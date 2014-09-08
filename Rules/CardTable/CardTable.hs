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
import Data.List (delete)

renderer :: CardTableState -> PlayerIndex -> Screen CardZone CardEntity
renderer (CTS {hands, decks, tables, discards}) pid = Map.fromList (hands' ++ decks' ++ tables' ++ discards' ++ gamelog)
  where
	discards' = zipWith (renderZone (ConcealExcept pid) CZDiscard) [0..] discards
	tables' = zipWith (renderZone (Show) CZPlay) [0..] tables
	decks' = zipWith (renderZone (ConcealAll) CZDeck) [0..] decks
	hands' = zipWith (renderZone (ConcealExcept pid) CZHand) [0..] hands
	gamelog = [(CZGamelog, (ZDD {display=ZDRight 20, order= -100, classNames=[]}, []))]

data RenderType = ConcealAll | ConcealExcept PlayerIndex | Show
	
renderZone
	:: RenderType
	-> CardZoneType
	-> PlayerIndex -- controller of the zone in question.
	-> [CardEntity]
	-> (CardZone, (ZoneDisplayData CardZone CardEntity, [ScreenEntity CardEntity]))
renderZone Show t p cards = (CZ t p, (zoneDisplay t p, map (renderCard t) cards))
renderZone ConcealAll t p cards = (CZ t p, (zoneDisplay t p, (blankCard p t 0 undefined):zipWith (blankCard p t) [1..] cards))
renderZone (ConcealExcept pid) t p cards = (CZ t p, (zoneDisplay t p
	, (if pid == p then (map $ renderCard t) else (zipWith (blankCard p t) [0..])) cards
	))

renderCard :: CardZoneType -> CardEntity -> ScreenEntity CardEntity
renderCard t c@(CECard s r) =
	SE
		{ eId = c
		, eDisplay = SDImage "images/placeholder.jpg"
		, eSize = SESAutoWidth $ 
			case t of 
				CZHand -> 95
				CZPlay -> 22
				CZDeck -> 95
				CZDiscard -> 22
		, eActive = True
		}
		
blankCard :: PlayerIndex -> CardZoneType -> Int -> CardEntity -> ScreenEntity CardEntity
blankCard p t num _ = SE { eId = CECard "blank" (p*1000 + (fromEnum t)*100 + num), eDisplay = SDText "FACEDOWN", eSize = SESAutoWidth 25, eActive = True}

zoneDisplay :: CardZoneType -> PlayerIndex -> ZoneDisplayData CardZone CardEntity
zoneDisplay CZDiscard pid = ZDD {display=ZDNested (ZDRight 20) (CZ CZPlay pid), order=pid*10+4, classNames = ["margin-onepct"]}
zoneDisplay CZPlay pid = ZDD {display = ZDHorizFill 40, order=pid*10+2, classNames = ["display-inline", "margin-onepct"]}
zoneDisplay CZDeck pid = ZDD {display = ZDNested (ZDLeft 20) (CZ CZPlay pid), order=pid*10+3, classNames = ["display-block", "display-stacked"]}
zoneDisplay CZHand pid = ZDD {display = ZDHorizFill 10, order=pid*20, classNames = ["display-inline", "margin-onepct"]}


nameCard :: CardEntity -> String
nameCard card = show card

nameZone :: CardZone -> String
nameZone zone = show zone

inputHandler :: InputHandler CardTableState CardEntity CardZone
inputHandler s _ UIConnected = return (s, [], [])
inputHandler s pid (UIClick (CECard "blank" bidx)) = return $ processClick s (toEnum $ bidx `mod` 1000 `div` 100) (bidx `div` 1000)
inputHandler s _ (UIDrag (CECard "blank" _) _) = return (s, [], [])
inputHandler s pid (UIDrag card zone) = return (moveCard s card zone, [GLBroadcast [GLMMove [card] zone, GLMPlayerAction pid ("moved " ++ nameCard card ++ " to " ++ nameZone zone) CZGamelog]], []) -- TODO: update that to respect visibilities.
inputHandler s _ (UIClick (CECard _ _)) = return (s, [], []) -- TODO: tap/untap?

moveCard s _ CZGamelog = s
moveCard s card (CZ CZHand idx) = (deleteCard s card) { hands = insertForIdx (hands $ deleteCard s card) idx card }
moveCard s card (CZ CZDeck idx) = (deleteCard s card) { decks = insertForIdx (decks $ deleteCard s card) idx card }
moveCard s card (CZ CZDiscard idx) = (deleteCard s card) { discards = insertForIdx (discards $ deleteCard s card) idx card }
moveCard s card (CZ CZPlay idx) = (deleteCard s card) { tables = insertForIdx (tables $ deleteCard s card) idx card }

deleteCard s card = s
	{ hands = map (delete card) $ hands s
	, tables = map (delete card) $ tables s
	, discards = map (delete card) $ discards s
	, decks = map (delete card) $ decks s
	}
	
insertForIdx :: [[a]] -> Int -> a -> [[a]]
insertForIdx list idx updater = take idx list ++ [(updater:(head $ drop idx list))] ++ drop (idx+1) list

updateForIdx :: [a] -> Int -> a -> [a]
updateForIdx list idx updater = take idx list ++ [updater] ++ drop (idx+1) list

processClick s CZHand _ = (s, [], [])
processClick s CZPlay _ = (s, [], [])
processClick s CZDiscard _ = (s, [], [])
processClick s CZDeck pt = case (decks s) `atMay` pt of
		Just [] -> (s {decks = updateForIdx (decks s) pt newDeck, discards = updateForIdx (discards s) pt [], rng = rng'}, [GLBroadcast [GLMPlayerAction pt "reshuffled" CZGamelog]], [])
		  where
			(newDeck, rng') = shuffle (discards s !! pt) (rng s)
		Just (card:newDeck) -> (s {decks = updateForIdx (decks s) pt newDeck, hands = updateForIdx (hands s) pt (card:(hands s !! pt))}, [GLBroadcast [GLMPlayerAction pt "drew a card" CZGamelog]], [])
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
