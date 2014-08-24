module Rules.Glue
	( genGlueGame
	) where
	
import qualified Engine.Types as ET
import Control.Concurrent.MVar (newMVar)
import System.Time (TimeDiff)
import Data.ByteString (ByteString)
import qualified Data.Map as Map

handleInput :: ET.InputHandler GlueState Int Int
handleInput state _ (ET.UIClick input) = (state, [ET.GLBroadcast [ET.GLMMove [input] 0, ET.GLMPlayerAction 0 "clicked"]])
handleInput state _ (ET.UIDrag entity zone) = (state, [ET.GLBroadcast [ET.GLMMove [entity] zone, ET.GLMPlayerAction 0 "dragged"]])

finished :: GlueState -> Bool
finished _ = False

playerRenderer :: GlueState -> ET.PlayerIndex -> ET.Screen Int Int
playerRenderer _ _ = Map.empty

tick :: GlueState -> TimeDiff -> ET.GameDelta GlueState Int Int
tick state _ = (state, [ET.GLBroadcast [ET.GLMDisplay "tick tock"]])

getPlayers :: GlueState -> [ET.PlayerIndex]
getPlayers _ = [0]

data GlueState = GlueState
	{
	}
	
instance ET.GameState GlueState

instance ET.EntityId Int
instance ET.ZoneId Int

genGlueGame :: IO ET.Game
genGlueGame = do
	state <- newMVar $ GlueState {}
	return ET.Game
		{ ET.playerRenderer = playerRenderer
		, ET.tick = tick
		, ET.handleInput = handleInput
		, ET.getPlayers = getPlayers
		, ET.state = state
		, ET.finished = finished
		}
