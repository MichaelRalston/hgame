{-# LANGUAGE NamedFieldPuns #-}

module Rules.CardTable.CardTable
	( makeCardTable
	) where

import Rules.CardTable.Types
import Engine.Types
import System.Random (StdGen)
import qualified Data.Map as Map

renderer :: CardTableState -> PlayerIndex -> Screen CardZone CardEntity
renderer (CTS {hands, decks, tables, discards}) pid = Map.fromList (hands' ++ decks' ++ tables' ++ discards')
  where
	discards' = zipWith (renderZone (ConcealExcept pid) CZDiscard) [0..] discards
	tables' = zipWith (renderZone (Show) CZPlay) [0..] tables
	decks' = zipWith (renderZone (ConcealAll) CZDeck) [0..] decks
	hands' = zipWith (renderZone (ConcealExcept pid) CZHand) [0..] hands

data RenderType = ConcealAll | ConcealExcept PlayerIndex | Show
	
renderZone
	:: RenderType
	-> CardZoneType
	-> PlayerIndex -- controller of the zone in question.
	-> [CardEntity]
	-> (CardZone, (ZoneDisplayData CardZone CardEntity, [ScreenEntity CardEntity]))
renderZone Show t p cards = (CZ t p, (zoneDisplay t p, map renderCard cards))
renderZone ConcealAll t p cards = (CZ t p, (zoneDisplay t p, map (blankCard p t) cards))
renderZone (ConcealExcept pid) t p cards = (CZ t p, (zoneDisplay t p
	, map (if pid == p then renderCard else (blankCard p t)) cards
	))

renderCard :: CardEntity -> ScreenEntity CardEntity
renderCard c@(CECard s r) =
	SE
		{ eId = c
		, eDisplay = SDText $ s ++ " " ++ (show r)
		, eSize = SESAutoWidth 25
		, eActive = True
		}
		
blankCard :: PlayerIndex -> CardZoneType -> CardEntity -> ScreenEntity CardEntity
blankCard p t _ = SE { eId = CECard "blank" (p*100 + (fromEnum t)*10), eDisplay = SDText "FACEDOWN", eSize = SESAutoWidth 25, eActive = True}

zoneDisplay :: CardZoneType -> PlayerIndex -> ZoneDisplayData CardZone CardEntity
zoneDisplay CZDiscard pid = ZDD {display=ZDNested (ZDRight 5) (CZ CZPlay pid), order=pid*10+4}
zoneDisplay CZPlay pid = ZDD {display = ZDHorizFill 45, order=pid*10+2}
zoneDisplay CZDeck pid = ZDD {display = ZDNested (ZDLeft 5) (CZ CZDiscard pid), order=pid*10+3}
zoneDisplay CZHand pid = ZDD {display = ZDHorizFill 5, order=pid*20}

inputHandler :: InputHandler CardTableState CardEntity CardZone
inputHandler s _ UIConnected = return (s, [], [])
	
makeCardTable :: StdGen -> WithMemory Game
makeCardTable generator = do
	let deck = [CECard suit rank | suit <- ["red", "blue", "purple", "yellow"], rank <- [1..15]]
	tableState <- alloc $ CTS
		{ hands = [[],[]]
		, decks = [deck,[]]
		, tables = [[],[]]
		, discards = [[],[]]
		, rng = generator
		}		
	return Game
		{ playerRenderer = renderer
		, getPlayers = (\_ -> [0, 1])
		, handleInput = inputHandler
		, state = tableState
		, finished = (\_ -> False)
		}
