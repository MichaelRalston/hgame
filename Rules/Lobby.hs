{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module Rules.Lobby
	( makeLobby
	, addPlayer
	, LobbyState
	) where
	
import qualified Engine.Types as ET
import qualified Engine.Mechanics as EM
import Control.Concurrent.MVar (newMVar, MVar)
import qualified Data.Map as Map
import qualified Network.WebSockets as WS
import System.Random (newStdGen, StdGen, split)
import Engine.Statebags
import Control.Concurrent.MVar (modifyMVar, modifyMVar_)
import Data.List ((\\))
import qualified Data.List as List

import Rules.CardTable.CardTable (makeCardTable)

gamelogZone :: Int -- zone ID.
gamelogZone = 1
inputZone :: Int -- also zone ID.
inputZone = 2
dataZone :: Int
dataZone = 0

handleInput :: ET.InputHandler LobbyState Int Int
handleInput (state@LobbyState {pendingGame, generator, gameMap, activeParticipants}) pid (ET.UIClick input) =
	case pendingGame of
		Just g -> return $ (state {pendingGame = Nothing, activeParticipants=List.delete pid activeParticipants}, [ET.GLBroadcast [ET.GLMPlayerAction pid "joined a game" gamelogZone]], [ET.GMMoveGame pid g 1])
		Nothing -> do
			let (gen, gen') = split generator
			newGame <- makeCardTable gen -- todo: make players real.
			newId <- insert (GameData {game=newGame, players=Map.empty}) gameMap
			return $ (state {generator = gen', pendingGame = Just newId, activeParticipants=List.delete pid activeParticipants}, [ET.GLBroadcast [ET.GLMPlayerAction pid "created a game" gamelogZone]], [ET.GMMoveGame pid newId 0])
handleInput state pid (ET.UIText _ str) = return $ (state, [ET.GLBroadcast [ET.GLMPlayerAction pid str gamelogZone]], [])
handleInput (state@LobbyState {activeParticipants}) pid ET.UIDisconnected = return $ (state{activeParticipants=List.delete pid activeParticipants}, [ET.GLBroadcast [ET.GLMPlayerAction pid "disconnected" gamelogZone]], [])
handleInput state _ _ = return $ (state, [], [])

finished :: LobbyState -> Bool
finished _ = False

playerRenderer :: LobbyState -> ET.PlayerIndex -> ET.Screen Int Int
playerRenderer _ _ = Map.fromList 
	[ (dataZone, (ET.ZDHorizFill 5,[ET.SE 1 (ET.SDText "MAEK GAEM") (ET.SESAutoWidth 95) True]))
	, (gamelogZone, (ET.ZDHorizFill 90,[]))
	, (inputZone, (ET.ZDHorizFill 5,[ET.SE 0 ET.SDTextInput (ET.SESPercent 100 100) True]))
	]

getPlayers :: LobbyState -> [ET.PlayerIndex]
getPlayers LobbyState {activeParticipants} = activeParticipants

addPlayer :: ConnectionMap -> WS.Connection -> ET.GameId -> MVar LobbyState -> GameMap -> IO ConnectionId
addPlayer connectionMap conn lobbyId lobbyState gameMap = modifyMVar lobbyState $ (\state@LobbyState {activeParticipants} -> do
		let name = "The Drama Llama"
		let newId = head $ [0..] \\ activeParticipants
		let connInfo = ConnectionInfo
				{	displayName = name
				,	connection = conn
				,	gameData = (lobbyId, newId)
				}
		connectionId <- ET.useMemory $ insert connInfo connectionMap
		initialMessage <- ET.useMemory $ EM.getPlayerState playerRenderer state newId
		WS.sendDataMessage conn initialMessage
		ET.useMemory $ update lobbyId gameMap (\game@GameData {players} -> game {players = Map.insert newId connectionId players})
		return (state {activeParticipants=newId:activeParticipants}, connectionId)
	)

data LobbyState = LobbyState
	{ activeParticipants :: [ET.PlayerIndex]
	, pendingGame :: Maybe ET.GameId
	, generator :: StdGen
	, gameMap :: GameMap
	, selfId :: Maybe ET.GameId
	}
	
instance ET.GameState LobbyState

instance ET.EntityId Int
instance ET.ZoneId Int

makeLobby :: GameMap -> IO (ET.GameId, MVar LobbyState)
makeLobby gameMap = do
	gen <- newStdGen
	state <- newMVar $ LobbyState [] Nothing gen gameMap Nothing
	let game = ET.Game
		{ ET.playerRenderer = playerRenderer
		, ET.handleInput = handleInput
		, ET.getPlayers = getPlayers
		, ET.state = state
		, ET.finished = finished
		}
	gameId <- ET.useMemory $ insert GameData {game=game, players=Map.empty} gameMap
	modifyMVar_ state (\s -> return $ s {selfId = Just gameId})
	return (gameId, state)
