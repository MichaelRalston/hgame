{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module Rules.Lobby
	( makeLobby
	, addPlayer
	, LobbyState
	) where
	
import qualified Engine.Types as ET
import Control.Concurrent.MVar (newMVar, MVar)
import System.Time (TimeDiff)
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import qualified Network.WebSockets as WS
import Engine.Statebags
import Control.Concurrent.MVar (modifyMVar)
import Control.Applicative ((<$>))
import Data.List ((\\))


handleInput :: ET.InputHandler LobbyState Int Int
handleInput state _ (ET.UIClick input) = (state, [ET.GLBroadcast [ET.GLMMove [input] 0, ET.GLMPlayerAction 0 "clicked"]])
handleInput state _ (ET.UIDrag entity zone) = (state, [ET.GLBroadcast [ET.GLMMove [entity] zone, ET.GLMPlayerAction 0 "dragged"]])

finished :: LobbyState -> Bool
finished _ = False

playerRenderer :: LobbyState -> ET.PlayerIndex -> ET.Screen Int Int
playerRenderer _ _ = Map.empty

tick :: LobbyState -> TimeDiff -> ET.GameDelta LobbyState Int Int
tick state _ = (state, [])

getPlayers :: LobbyState -> [ET.PlayerIndex]
getPlayers LobbyState {activeParticipants} = activeParticipants

addPlayer :: ConnectionMap -> WS.Connection -> GameId -> MVar LobbyState -> GameMap -> IO ConnectionId
addPlayer connectionMap conn lobbyId lobbyState gameMap = modifyMVar lobbyState $ (\state@LobbyState {activeParticipants} -> do
		let name = "The Drama Llama"
		let newId = head $ [0..] \\ activeParticipants
		let connInfo = ConnectionInfo
				{	displayName = name
				,	connection = conn
				,	gameData = (lobbyId, newId)
				}
		connectionId <- insert connInfo connectionMap
		update lobbyId gameMap (\game@GameData {players} -> game {players = Map.insert newId connectionId players})
		return (LobbyState $ newId:activeParticipants, connectionId)
	)

data LobbyState = LobbyState
	{ activeParticipants :: [ET.PlayerIndex]
	}
	
instance ET.GameState LobbyState

instance ET.EntityId Int
instance ET.ZoneId Int

makeLobby :: GameMap -> IO (GameId, MVar LobbyState)
makeLobby gameMap = do
	state <- newMVar $ LobbyState []
	let game = ET.Game
		{ ET.playerRenderer = playerRenderer
		, ET.tick = tick
		, ET.handleInput = handleInput
		, ET.getPlayers = getPlayers
		, ET.state = state
		, ET.finished = finished
		}
	gameId <- insert GameData {game=game, players=Map.empty} gameMap
	return (gameId, state)
