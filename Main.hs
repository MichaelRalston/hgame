import Network.WebSockets as WS
import Engine.Statebags (makeModMap)
import Engine.Engine (wsApp)
import Rules.Lobby (makeLobby)

main :: IO ()
main = do
	gameMap <- makeModMap
	connectionMap <- makeModMap
	(lobbyId, lobbyState) <- makeLobby gameMap
	WS.runServer "0.0.0.0" 6116 (wsApp gameMap connectionMap lobbyId lobbyState)
