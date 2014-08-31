import Network.WebSockets as WS
import Engine.Statebags (makeModMap)
import Engine.Engine (wsApp)
import Rules.Lobby (makeLobby)
import Engine.Types (useMemory)

main :: IO ()
main = do
	gameMap <- useMemory $ makeModMap
	connectionMap <- useMemory $ makeModMap
	(lobbyId, lobbyState) <- makeLobby gameMap
	WS.runServer "0.0.0.0" 6116 (wsApp gameMap connectionMap lobbyId lobbyState)
