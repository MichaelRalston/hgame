{-# Language NamedFieldPuns, OverloadedStrings, TupleSections #-}

module Engine.Mechanics
	( processInput
	, runTick
	, getPlayerState
	, isFinished
	) where
	
import Engine.Types
import System.Time (TimeDiff)
import Control.Concurrent.MVar (modifyMVar, withMVar)
import Data.Aeson (object, (.=), encode, decode', Value(..))
import Network.WebSockets (DataMessage (..))
import Control.Applicative ((<$>))
import Data.Maybe (mapMaybe)

import Debug.Trace
traceS a = trace (show a) a
traceSs str a = trace str $ traceS a
	
isFinished :: Game -> IO Bool
isFinished (Game {state, finished}) = do
	withMVar state (return . finished)
	
runTick :: Game -> TimeDiff -> IO [(PlayerIndex, DataMessage)]
runTick (Game {state, tick, getPlayers, playerRenderer}) timeDelta = do
	modifyMVar state process
  where
	process state' = (newGameState,) <$> result
	  where
		result = buildResult playerList playerRenderer newGameState logs
		playerList = getPlayers newGameState
		(newGameState, logs) = tick state' timeDelta
		
processInput :: Game -> DataMessage -> PlayerIndex -> IO [(PlayerIndex, DataMessage)]
processInput (Game {state, handleInput, getPlayers, playerRenderer}) (Text m) pid = 
	case decode' m of
		Just eid -> 
			modifyMVar state process
			  where
				process state' = (newGameState,) <$> result
				  where
					(newGameState, logs) = handleInput state' (traceS pid) (traceS eid)
					playerList = getPlayers newGameState
					result = buildResult playerList playerRenderer newGameState logs
		Nothing -> 
			trace "processInput got invalid json" $ trace (show m) $ return [] -- TODO: handle error.
processInput _ _ _ =
	trace "processInput got invalid input" $ return [] -- TODO: handle error.

buildResult :: (EntityId entity, ZoneId zone, GameState state) => [PlayerIndex] -> (state -> PlayerIndex -> Screen zone entity) -> state -> [Gamelog entity zone] -> IO [(PlayerIndex, DataMessage)]
buildResult playerList playerRenderer newGameState logs =
	sequence $ zipWith (\a b -> (a,) <$> b) playerList (map (getPlayerUpdate playerRenderer newGameState logs) playerList)

getNameForPid :: {- whatever parameters are required -} PlayerIndex -> IO String -- TO CONSIDER: kill this, put it in a transform on the /screen/, and let the frontend do the translation.
getNameForPid pid = return $ "Player " ++ (show $ pid+1)

getPlayerState :: (GameState state, EntityId entity, ZoneId zone) => (state -> PlayerIndex -> Screen zone entity) -> state -> PlayerIndex -> IO DataMessage
getPlayerState renderer state pid = getPlayerUpdate renderer state [] pid
	
getPlayerUpdate :: (GameState state, EntityId entity, ZoneId zone) => (state -> PlayerIndex -> Screen zone entity) -> state -> [Gamelog entity zone] -> PlayerIndex -> IO DataMessage
getPlayerUpdate renderer state logs pid = do
	let screenObject = renderer state pid
	let gamelogs = filterGamelogs logs pid
	gamelogObject <- sequence $ map (gameLogToJson getNameForPid) gamelogs
	return $ Network.WebSockets.Text $ encode $ object ["screen" .= toJSON screenObject, "gamelogs" .= gamelogObject]
	
gameLogToJson :: (EntityId entity, ZoneId zone) => (PlayerIndex -> IO String) -> GamelogMessage entity zone -> IO Value
gameLogToJson _ (GLMDisplay str zone) = return $ object ["type" .= String "display", "display" .= toJSON str, "zone" .= zone]
gameLogToJson _ (GLMMove entities zone) = return $ object ["type" .= String "move", "move" .= object ["entities" .= toJSON (map toJSON entities), "zone" .= toJSON zone]]
gameLogToJson nameFinder (GLMPlayerAction pid str zone) = do
	name <- nameFinder pid
	return $ object ["type" .= String "action", "actor" .= name, "string" .= str, "zone" .= zone]
gameLogToJson nameFinder (GLMTwoPlayerAction pid str target zone) = do
	name <- nameFinder pid
	return $ object ["type" .= String "targetAction", "actor" .= name, "string" .= str, "target" .= target, "zone" .= zone]
		
filterGamelogs :: [Gamelog a b] -> PlayerIndex -> [GamelogMessage a b]
filterGamelogs logs pid = concat $ mapMaybe theFilter logs
  where
	theFilter (GLBroadcast l) = Just l
	theFilter (GLPrivate players l) = if pid `elem` players then Just l else Nothing
	theFilter (GLAllBut players l) = if pid `notElem` players then Just l else Nothing
