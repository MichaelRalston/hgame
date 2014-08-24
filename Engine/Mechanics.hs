{-# Language NamedFieldPuns, OverloadedStrings #-}

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
import Data.Aeson (object, (.=), encode, decode')
import Data.Aeson.Encode (encodeToByteStringBuilder)
import Network.WebSockets (DataMessage (..))
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
	process state' = return (newGameState, result)
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
				process state' = return (newGameState, result)
				  where
					(newGameState, logs) = handleInput state' pid eid
					playerList = getPlayers newGameState
					result = buildResult playerList getPlayerUpdate playerRenderer newGameState logs
		Nothing -> 
			return [] -- TODO: handle error.
processInput _ _ _ =
	return [] -- TODO: handle error.

buildResult playerList getPlayerUpdate playerRenderer newGameState logs =
	zip playerList (map (getPlayerUpdate playerRenderer newGameState logs) playerList)

getPlayerUpdate :: (GameState state, EntityId entity, ZoneId zone) => (state -> PlayerIndex -> Screen zone entity) -> state -> [Gamelog entity zone] -> PlayerIndex -> DataMessage
getPlayerUpdate renderer state logs pid = Network.WebSockets.Text $ encode $ object ["screen" .= toJSON screenObject, "gamelogs" .= gamelogObject]
  where
	screenObject = renderer state pid
	gamelogObject = toJSON $ filterGamelogs logs pid

filterGamelogs :: [Gamelog a b] -> PlayerIndex -> [GamelogMessage a b]
filterGamelogs logs pid = concat $ mapMaybe theFilter logs
  where
	theFilter (GLBroadcast l) = Just l
	theFilter (GLPrivate players l) = if pid `elem` players then Just l else Nothing
	theFilter (GLAllBut players l) = if pid `notElem` players then Just l else Nothing
