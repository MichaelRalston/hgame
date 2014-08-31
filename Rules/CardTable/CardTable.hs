module Rules.CardTable.CardTable
	( makeCardTable
	) where

import Rules.CardTable.Types
import Engine.Types
import System.Random (StdGen)
import Control.Concurrent (newMVar)

renderer :: CardTableState -> PlayerIndex -> Screen CardZone CardEntity
renderer = undefined

inputHandler :: InputHandler CardTableState CardEntity CardZone
inputHandler = undefined
	
makeCardTable :: StdGen -> IO Game
makeCardTable generator = do
	let deck = [CECard suit rank | suit <- ["red", "blue", "purple", "yellow"], rank <- [1..15]]
	tableState <- newMVar $ CTS
		{ hands = [[],[]]
		, decks = [deck,[]]
		, tables = [[],[]]
		, discards = [[],[]]
		, rng = generator
		}		
	return Game
		{ playerRenderer = renderer
		, tick = undefined
		, getPlayers = (\_ -> [0, 1])
		, handleInput = inputHandler
		, state = tableState
		, finished = (\_ -> False)
		}
