{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module Rules.Lobby
	( makeLobby
	, addPlayer
	, LobbyState
	) where
	
import qualified Engine.Types as ET
import qualified Engine.Mechanics as EM
import qualified Engine.Statebags as ES
import Control.Concurrent.MVar (newMVar, MVar)
import System.Time (TimeDiff)
import qualified Data.Map as Map
import qualified Network.WebSockets as WS
import Engine.Statebags
import Control.Concurrent.MVar (modifyMVar)
import Data.List ((\\))
import qualified Data.List as List

gamelogZone :: Int -- zone ID.
gamelogZone = 0
inputZone :: Int -- also zone ID.
inputZone = 1

handleInput :: ET.InputHandler LobbyState Int Int
handleInput (state@LobbyState {pendingGame}) pid (ET.UIClick input) =
	case pendingGame of
		Just g -> (state, [])
		Nothing -> (state, [])
handleInput state pid (ET.UIText _ str) = (state, [ET.GLBroadcast [ET.GLMPlayerAction pid str gamelogZone]])
handleInput (state@LobbyState {activeParticipants}) pid ET.UIDisconnected = (state{activeParticipants=List.delete pid activeParticipants}, [ET.GLBroadcast [ET.GLMPlayerAction pid "disconnected" gamelogZone]])
handleInput state _ _ = (state, [])

finished :: LobbyState -> Bool
finished _ = False

playerRenderer :: LobbyState -> ET.PlayerIndex -> ET.Screen Int Int
playerRenderer _ _ = Map.fromList 
	[ (gamelogZone, (ET.ZDHorizFill 95,[]))
	, (inputZone, (ET.ZDHorizFill 5,[ET.SE 0 ET.SDTextInput (ET.SESPercent 100 100) True]))
	]

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
		initialMessage <- EM.getPlayerState playerRenderer state newId
		WS.sendDataMessage conn initialMessage
		update lobbyId gameMap (\game@GameData {players} -> game {players = Map.insert newId connectionId players})
		return (state {activeParticipants=newId:activeParticipants}, connectionId)
	)

data LobbyState = LobbyState
	{ activeParticipants :: [ET.PlayerIndex]
	, pendingGame :: Maybe ES.GameId
	}
	
instance ET.GameState LobbyState

instance ET.EntityId Int
instance ET.ZoneId Int

makeLobby :: GameMap -> IO (GameId, MVar LobbyState)
makeLobby gameMap = do
	state <- newMVar $ LobbyState [] Nothing
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
