import Network.WebSockets as WS
import Engine.Statebags
import Engine.Engine

main :: IO ()
main = do
	gameMap <- makeModMap
	connectionMap <- makeModMap
	WS.runServer "0.0.0.0" 6116 (wsApp gameMap connectionMap)
