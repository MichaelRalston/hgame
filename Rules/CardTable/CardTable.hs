{-# LANGUAGE NamedFieldPuns #-}

module Rules.CardTable.CardTable
	( makeCardTable
	) where

import Rules.CardTable.Types
import Engine.Types
import System.Random (StdGen)
import qualified Data.Map as Map
import Safe (atMay)
import System.Random (randomR)

renderer :: CardTableState -> PlayerIndex -> Screen CardZone CardEntity
renderer (CTS {hands, decks, tables, discards}) pid = Map.fromList (hands' ++ decks' ++ tables' ++ discards' ++ gamelog)
  where
	discards' = zipWith (renderZone (ConcealExcept pid) CZDiscard) [0..] discards
	tables' = zipWith (renderZone (Show) CZPlay) [0..] tables
	decks' = zipWith (renderZone (ConcealAll) CZDeck) [0..] decks
	hands' = zipWith (renderZone (ConcealExcept pid) CZHand) [0..] hands
	gamelog = [(CZGamelog, (ZDD {display=ZDRight 20, order= -100}, []))]

data RenderType = ConcealAll | ConcealExcept PlayerIndex | Show
	
renderZone
	:: RenderType
	-> CardZoneType
	-> PlayerIndex -- controller of the zone in question.
	-> [CardEntity]
	-> (CardZone, (ZoneDisplayData CardZone CardEntity, [ScreenEntity CardEntity]))
renderZone Show t p cards = (CZ t p, (zoneDisplay t p, map renderCard cards))
renderZone ConcealAll t p cards = (CZ t p, (zoneDisplay t p, (blankCard p t 0 undefined):zipWith (blankCard p t) [1..] cards))
renderZone (ConcealExcept pid) t p cards = (CZ t p, (zoneDisplay t p
	, (if pid == p then (map renderCard) else (zipWith (blankCard p t) [0..])) cards
	))

renderCard :: CardEntity -> ScreenEntity CardEntity
renderCard c@(CECard s r) =
	SE
		{ eId = c
		, eDisplay = SDText $ s ++ " " ++ (show r)
		, eSize = SESAutoWidth 25
		, eActive = True
		}
		
blankCard :: PlayerIndex -> CardZoneType -> Int -> CardEntity -> ScreenEntity CardEntity
blankCard p t num _ = SE { eId = CECard "blank" (p*1000 + (fromEnum t)*100 + num), eDisplay = SDText "FACEDOWN", eSize = SESAutoWidth 25, eActive = True}

zoneDisplay :: CardZoneType -> PlayerIndex -> ZoneDisplayData CardZone CardEntity
zoneDisplay CZDiscard pid = ZDD {display=ZDNested (ZDRight 20) (CZ CZPlay pid), order=pid*10+4}
zoneDisplay CZPlay pid = ZDD {display = ZDHorizFill 40, order=pid*10+2}
zoneDisplay CZDeck pid = ZDD {display = ZDNested (ZDLeft 20) (CZ CZPlay pid), order=pid*10+3}
zoneDisplay CZHand pid = ZDD {display = ZDHorizFill 10, order=pid*20}



inputHandler :: InputHandler CardTableState CardEntity CardZone
inputHandler s _ UIConnected = return (s, [], [])
inputHandler s pid (UIClick (CECard "blank" bidx)) = return $ processClick s (toEnum $ bidx `mod` 1000 `div` 100) (bidx `div` 1000)

updateForIdx :: [a] -> Int -> a -> [a]
updateForIdx list idx updater = take idx list ++ [updater] ++ drop (idx+1) list

processClick s CZHand _ = (s, [], [])
processClick s CZPlay _ = (s, [], [])
processClick s CZDiscard _ = (s, [], [])
processClick s CZDeck pt = case (decks s) `atMay` pt of
		Just [] -> (s {decks = updateForIdx (decks s) pt newDeck, discards = updateForIdx (discards s) pt [], rng = rng'}, [], [])
		  where
			(newDeck, rng') = shuffle (discards s !! pt) (rng s)
		Just (card:newDeck) -> (s {decks = updateForIdx (decks s) pt newDeck, hands = updateForIdx (hands s) pt (card:(hands s !! pt))}, [], [])
		_ -> (s, [], [])

shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle [] r = ([], r)
shuffle a r = (elem:rest, r')
  where
	(idx, r'') = randomR (0, length a - 1) r 
	elem = a !! idx
	rest' = take idx a ++ drop (idx+1) a
	(rest, r') = shuffle rest' r''
	
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
