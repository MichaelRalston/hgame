{-# Language NamedFieldPuns, OverloadedStrings, TupleSections #-}

module Engine.Mechanics
	( processInput
	, getPlayerState
	, isFinished
	, handleDisconnection
	, handleConnection
	) where
	
import Engine.Types
--import System.Time (TimeDiff)
import Data.Aeson (object, (.=), encode, decode', Value(..))
import Network.WebSockets (DataMessage (..))
import Control.Applicative ((<$>))
import Data.Maybe (mapMaybe)

import Engine.Debug
	
isFinished :: Game -> WithMemory Bool
isFinished (Game {state, finished}) = do
	usingMemory state (return . finished)
	
handleDisconnection :: Game -> PlayerIndex -> WithMemory [(PlayerIndex, DataMessage)]
handleDisconnection (Game {state, handleInput, getPlayers, playerRenderer}) pid =
	modifyMemory state process
		  where
			process state' = do
				(newGameState, logs, gameMovement) <- handleInput state' pid UIDisconnected
				let playerList = getPlayers newGameState
				let result = buildResult playerList playerRenderer newGameState logs
				(newGameState,) <$> result
					
handleConnection :: Game -> PlayerIndex -> WithMemory [(PlayerIndex, DataMessage)]
handleConnection (Game {state, handleInput, getPlayers, playerRenderer}) pid =
	modifyMemory state process
		  where
			process state' = do
				(newGameState, logs, gameMovement) <- handleInput state' pid UIConnected
				let playerList = getPlayers newGameState
				let result = buildResult playerList playerRenderer newGameState logs
				(newGameState,) <$> result
					
processInput :: Game -> DataMessage -> PlayerIndex -> WithMemory ([(PlayerIndex, DataMessage)], [GameMovement])
processInput (Game {state, handleInput, getPlayers, playerRenderer}) (Text m) pid = 
	case decode' m of
		Just input -> 
			modifyMemory state process
				  where
					process state' = do
						(newGameState, logs, gameMovement) <- handleInput state' (traceS pid) (traceS input)
						let playerList = getPlayers newGameState
						let result = buildResult playerList playerRenderer newGameState logs
						(newGameState,) <$> (,gameMovement) <$> result
		Nothing -> 
			trace "processInput got invalid json" $ trace (show m) $ return ([], []) -- TODO: handle error.
processInput _ _ _ =
	trace "processInput got invalid input" $ return ([], []) -- TODO: handle error.

buildResult :: (EntityId entity, ZoneId zone, GameState state) => [PlayerIndex] -> (state -> PlayerIndex -> Screen zone entity) -> state -> [Gamelog entity zone] -> WithMemory [(PlayerIndex, DataMessage)]
buildResult playerList playerRenderer newGameState logs =
	sequence $ zipWith (\a b -> (a,) <$> b) playerList (map (getPlayerUpdate playerRenderer newGameState logs) playerList)

getNameForPid :: {- whatever parameters are required -} PlayerIndex -> WithMemory String -- TO CONSIDER: kill this, put it in a transform on the /screen/, and let the frontend do the translation.
getNameForPid pid = return $ "Player " ++ (show $ pid+1)

getPlayerState :: (GameState state, EntityId entity, ZoneId zone) => (state -> PlayerIndex -> Screen zone entity) -> state -> PlayerIndex -> WithMemory DataMessage
getPlayerState renderer state pid = getPlayerUpdate renderer state [] pid
	
getPlayerUpdate :: (GameState state, EntityId entity, ZoneId zone) => (state -> PlayerIndex -> Screen zone entity) -> state -> [Gamelog entity zone] -> PlayerIndex -> WithMemory DataMessage
getPlayerUpdate renderer state logs pid = do
	let screenObject = renderer state pid
	let gamelogs = filterGamelogs logs pid
	gamelogObject <- sequence $ map (gameLogToJson getNameForPid) gamelogs
	return $ Network.WebSockets.Text $ encode $ object ["screen" .= toJSON screenObject, "gamelogs" .= gamelogObject]
	
gameLogToJson :: (EntityId entity, ZoneId zone) => (PlayerIndex -> WithMemory String) -> GamelogMessage entity zone -> WithMemory Value
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
