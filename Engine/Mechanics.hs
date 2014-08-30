{-# Language NamedFieldPuns, OverloadedStrings, TupleSections #-}

module Engine.Mechanics
	( processInput
	, runTick
	, isFinished
	) where
	
import Engine.Types
import System.Time (TimeDiff)
import qualified Data.ByteString (concat)
import Control.Concurrent.MVar (modifyMVar, withMVar)
import Data.ByteString.Lazy (toChunks)
import Data.ByteString.Builder (toLazyByteString)
import Data.Aeson (object, (.=), encode, decode', Value)
import Data.Aeson.Encode (encodeToByteStringBuilder)
import Network.WebSockets (DataMessage (..))
import Control.Applicative ((<$>))
import qualified Network.WebSockets (DataMessage(Text))
import Data.Maybe (mapMaybe)

import Data.ByteString.Lazy.Char8 (unpack)

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
		result = buildResult playerList getPlayerUpdate playerRenderer newGameState logs
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
					result = buildResult playerList getPlayerUpdate playerRenderer newGameState logs
		Nothing -> 
			trace "processInput got invalid json" $ return [] -- TODO: handle error.
processInput _ _ _ =
	trace "processInput got invalid input" $ return [] -- TODO: handle error.

buildResult :: [PlayerIndex] -> ((state -> PlayerIndex -> Screen zone entity) -> state -> [Gamelog entity zone] -> PlayerIndex -> IO DataMessage) -> (state -> PlayerIndex -> Screen zone entity) -> state -> [Gamelog entity zone] -> IO [(PlayerIndex, DataMessage)]
buildResult playerList getPlayerUpdate playerRenderer newGameState logs =
	sequence $ zipWith (\a b -> (a,) <$> b) playerList (map (getPlayerUpdate playerRenderer newGameState logs) playerList)

getNameForPid :: PlayerIndex -> IO String
getNameForPid = undefined
	
getPlayerUpdate :: (GameState state, EntityId entity, ZoneId zone) => (state -> PlayerIndex -> Screen zone entity) -> state -> [Gamelog entity zone] -> PlayerIndex -> IO DataMessage
getPlayerUpdate renderer state logs pid = do
	let screenObject = renderer state pid
	let gamelogs = filterGamelogs logs pid
	gamelogObject <- sequence $ map (gameLogToJson getNameForPid) gamelogs
	return $ Network.WebSockets.Text $ encode $ object ["screen" .= toJSON screenObject, "gamelogs" .= gamelogObject]
	
gameLogToJson :: (EntityId entity, ZoneId zone) => (PlayerIndex -> IO String) -> GamelogMessage entity zone -> IO Value
gameLogToJson _ (GLMDisplay str) = return $ object ["display" .= toJSON str]
gameLogToJson _ (GLMMove entities zone) = return $ object ["move" .= object ["entities" .= toJSON (map toJSON entities), "zone" .= toJSON zone]]
gameLogToJson nameFinder (GLMPlayerAction pid str) = do
	name <- nameFinder pid
	return $ object ["actor" .= name, "string" .= str]
gameLogToJson nameFinder (GLMTwoPlayerAction pid str target) = do
	name <- nameFinder pid
	return $ object ["actor" .= name, "string" .= str, "target" .= target]
		

filterGamelogs :: [Gamelog a b] -> PlayerIndex -> [GamelogMessage a b]
filterGamelogs logs pid = concat $ mapMaybe theFilter logs
  where
	theFilter (GLBroadcast l) = Just l
	theFilter (GLPrivate players l) = if pid `elem` players then Just l else Nothing
	theFilter (GLAllBut players l) = if pid `notElem` players then Just l else Nothing
