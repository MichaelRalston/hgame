{-# Language NamedFieldPuns, OverloadedStrings, TupleSections #-}

{-
	Dependencies:
	Data.Aeson
	Network.WebSockets
	Control.Monad.Loops
-}

module Engine.Engine
	( wsApp
	) where

import Engine.Types
import Engine.Mechanics
import Engine.Statebags
import Rules.Lobby (LobbyState)

import Control.Concurrent.MVar (MVar)
import qualified Network.WebSockets as WS
import qualified Data.Map as Map
import qualified Rules.Lobby as Lobby
import Data.Maybe (mapMaybe)
import Control.Monad (void, join)
import Control.Monad.Loops (unfoldM_)
import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay, forkIO)
import System.Time (getClockTime, ClockTime, diffClockTimes)

fixUpdates :: Map.Map PlayerIndex ConnectionId -> [(PlayerIndex, a)] -> [(ConnectionId, a)]
fixUpdates map' list' = mapMaybe (\(k,v) -> (,v) <$> Map.lookup k map') list'

wsApp :: GameMap -> ConnectionMap -> GameId -> MVar LobbyState -> WS.ServerApp
wsApp gameMap connectionMap lobbyId lobbyState pendingConnection = do
	-- TODO: validate incoming connections?
	conn <- WS.acceptRequest pendingConnection
	-- TODO: validate any handshakery.
	connId <- Lobby.addPlayer connectionMap conn lobbyId lobbyState gameMap
	-- TODO: address the problem of creating a game with a player.
	-- ... although it might only be an issue here? And then later we already HAVE a connection
	-- and have to update it. And the game ... will need an interface and whatever. SORT OUT.	
	unfoldM_ $ handleConnectionInput gameMap connectionMap connId
	_ <- twiddle connId connectionMap
		(\(ConnectionInfo {gameData=(gameId, playerIndex)}) -> do
			twiddle gameId gameMap (\GameData {game, players}-> do
				updates <- handleDisconnection game playerIndex
				sendUpdates connectionMap $ fixUpdates players updates
				)
		)
	delete connId connectionMap
	
handleConnectionInput :: GameMap -> ConnectionMap -> ConnectionId -> IO (Maybe ())
handleConnectionInput gameMap connectionMap connectionId =
	join <$> twiddle connectionId connectionMap
		(\(ConnectionInfo {gameData=(gameId, playerIndex), connection}) -> do
			msg <- WS.receiveDataMessage connection
			twiddle gameId gameMap (\GameData {game, players}-> do
				updates <- processInput game msg playerIndex
				sendUpdates connectionMap $ fixUpdates players updates
				)
		)

-- this is the per-game tick loop.	
tickGame :: GameMap -> ConnectionMap -> GameId -> IO () -- TODO: figure out why ticking explodes :(
tickGame gameMap connectionMap gameId = do
	time <- getClockTime
	void $ forkIO $ doTick connectionMap gameMap gameId time
	
doTick :: ConnectionMap -> GameMap -> GameId -> ClockTime -> IO ()
doTick connectionMap gameMap gameId startTime = do
	threadDelay 1000000 -- microseconds: so one second.
	currentTime' <- join <$> twiddle gameId gameMap (\GameData {game, players} -> do
			currentTime <- getClockTime
			let timeDelta = diffClockTimes currentTime startTime
			updates <- runTick game timeDelta
			sendUpdates connectionMap $ fixUpdates players updates
			finished <- isFinished game
			case finished of
				True -> return Nothing
				False -> return $ Just currentTime
		)
	case currentTime' of
		Just currentTime -> doTick connectionMap gameMap gameId currentTime
		Nothing -> delete gameId gameMap
	
sendUpdates :: ConnectionMap -> [(ConnectionId, WS.DataMessage)] -> IO ()
sendUpdates connectionMap updates = void $ sequence $ map (uncurry $ sendMessageToPlayer connectionMap ) updates

sendMessageToPlayer :: ConnectionMap -> ConnectionId -> WS.DataMessage -> IO ()
sendMessageToPlayer connectionMap cid msg = void $ twiddle cid connectionMap (\(ConnectionInfo {connection}) ->
		WS.sendDataMessage connection msg
	)
