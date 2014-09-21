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
renderer (CTS {hands, decks, tables, discards, subEntities, codexes}) pid = Map.fromList (tokens ++ hands' ++ decks' ++ tables' ++ discards' ++ playmats' ++ codexes' ++ codexrows' ++ gamelog)
  where
	tokens = [(CZTokens, (ZDD {display = ZDHorizFill 10, order= -1, classNames = ["display-inline", "margin-onepx", "bordered"]}, map (renderToken Nothing) [CESubEntity "gold" 0, CESubEntity "plusone" 0, CESubEntity "minusone" 0, CESubEntity "time" 0, CESubEntity "sword" 0]))]
	discards' = map ($ []) $ zipWith ($) (map (renderZone (ConcealExcept pid) CZDiscard) [0..]) discards
	tables' = map ($ subEntities) $ zipWith ($) (map (renderZone (Show) CZPlay) [0..]) tables
	decks' = map ($ []) $ zipWith ($) (map (renderZone (ConcealAll) CZDeck) [0..]) decks
	hands' = map ($ []) $ zipWith ($) (map (renderZone (ConcealExcept pid) CZHand) [0..]) hands
	codexes' = map ($ []) $ zipWith ($) (map (renderZone (ConcealExcept pid) CZCodex) [0..]) [[], []]
	codexrows' = map ($ []) $ zipWith ($) (map (\idx -> renderZone (ConcealExcept $ (pid*3) + (idx `mod` 3)) CZCodexRow idx) [0..]) (concat codexes)
	gamelog = [(CZGamelog, (ZDD {display=ZDRight 20, order= -100, classNames=[]}, []))]
	playmats' = concatMap ($ subEntities) $ map makePlaymat [0, 1] 

data RenderType = ConcealAll | ConcealExcept PlayerIndex | Show
	
makePlaymat :: PlayerIndex -> [(CardEntity, CardEntity)] -> [(CardZone, (ZoneDisplayData CardZone CardEntity, [ScreenEntity CardEntity]))]
makePlaymat pid tokens =
	[ (CZ CZCodexHolder pid, (zoneDisplay CZCodexHolder pid, [(renderCard CZPlaymat $ CECard "codex_card" pid) {eSize = SESPercent 100 10}]))
	, (CZ CZPlaymat pid, (zoneDisplay CZPlaymat pid, map (renderCard CZPlaymat) playmatCards ++ 
		map (\(token, card) -> renderToken (Just card) token) (filter (\(_, card) -> elem card playmatCards) tokens)))
	]
  where
	playmatCards =
		[ CECard "playmat_card" $ pid*100 + 0
		, CECard "playmat_card" $ pid*100 + 1
		, CECard "playmat_card" $ pid*100 + 2
		, CECard "playmat_card" $ pid*100 + 3
		, CECard "playmat_card" $ pid*100 + 4
		, CECard "playmat_card" $ pid*100 + 5
		, CECard "playmat_card" $ pid*100 + 6
		, CECard "playmat_card" $ pid*100 + 7
		, CECard "playmat_card" $ pid*100 + 8
		, CECard "playmat_card" $ pid*100 + 9
		]
	
	
renderZone
	:: RenderType
	-> CardZoneType
	-> PlayerIndex -- controller of the zone in question.
	-> [CardEntity]
	-> [(CardEntity, CardEntity)]
	-> (CardZone, (ZoneDisplayData CardZone CardEntity, [ScreenEntity CardEntity]))
renderZone Show t p cards tokens = (CZ t p, (zoneDisplay t p, map (renderCard t) cards ++ 
	map (\(token, card) -> renderToken (Just card) token) (filter (\(_, card) -> elem card cards) tokens)
	))
renderZone ConcealAll t p cards _ = (CZ t p, (zoneDisplay t p, [cardCounter p t (length cards)]))
renderZone (ConcealExcept pid) t p cards _ = (CZ t p, (zoneDisplay t p
	, (if pid == p then (map (renderCard t) cards) else [cardCounter p t (length cards)])
	))

renderCard :: CardZoneType -> CardEntity -> ScreenEntity CardEntity
renderCard t c@(CECard cardType idx) =
	SE
		{ eId = c
		, eDisplay = SDImage "images/placeholder.jpg"
		, eSize = if cardType == "playmat_card"
			then
				case idx `mod` 100 of
					0 -> SESPercent 20 33
					1 -> SESPercent 20 33
					2 -> SESPercent 20 33
					3 -> SESPercent 20 33
					4 -> SESPercent 20 33
					5 -> SESPercent 20 33
					6 -> SESPercent 33 33
					7 -> SESPercent 33 33
					8 -> SESPercent 33 33
					9 -> SESPercent 25 95
			else
				SESAutoWidth $ 
				case t of 
					CZHand -> 95
					CZPlay -> 44
					CZDeck -> 95
					CZDiscard -> 44
					CZCodex -> 80
					CZCodexRow -> 80
					CZPlaymat -> 95
					CZCodexHolder -> 95
		, eEntitiesDropOn = elem t [CZPlay, CZPlaymat]
		, eDropOnEntities = False
		, eActive = case cardType of
			"playmat_card" -> False
			"codex_card" -> False
			_ -> True
		, eNestOnEntity = Nothing
		, eClasses = [] -- TODO: address rotation by 90 degrees.
		}

renderToken :: Maybe CardEntity -> CardEntity -> ScreenEntity CardEntity
renderToken card token =
	SE
		{ eId = token
		, eDisplay = SDImage "images/placeholder.jpg"
		, eSize = SESAutoWidth 23
		, eEntitiesDropOn = False
		, eDropOnEntities = True
		, eActive = True
		, eNestOnEntity = card
		, eClasses = ["rotate180"] -- TODO: replace with real things?
		}
		
cardCounter :: PlayerIndex -> CardZoneType -> Int -> ScreenEntity CardEntity
cardCounter p t count = SE { eId = CECard "blank" (p*1000 + (fromEnum t)*100), eDisplay = SDText $ show count ++ " CARDS", eSize = SESAutoWidth (if t == CZDiscard then 22 else 95), eActive = True, eEntitiesDropOn = False, eDropOnEntities = False, eClasses = [], eNestOnEntity = Nothing}

zoneDisplay :: CardZoneType -> PlayerIndex -> ZoneDisplayData CardZone CardEntity
zoneDisplay CZDiscard pid = ZDD {display=ZDNested (ZDRight 20) (CZ CZPlay pid), order=pid*10+6, classNames = ["margin-onepx", "bordered"]}
zoneDisplay CZPlay pid = ZDD {display = ZDHorizFill 35, order=pid*10+2, classNames = ["display-inline", "margin-onepx"]}
zoneDisplay CZDeck pid = ZDD {display = ZDNested (ZDLeft 10) (CZ CZPlay pid), order=pid*10+3, classNames = []}
zoneDisplay CZPlaymat pid = ZDD {display = ZDNested (ZDLeft 20) (CZ CZPlay pid), order=pid*10+4, classNames = []}
zoneDisplay CZCodexHolder pid = ZDD {display = ZDNested (ZDRight 5) (CZ CZPlay pid), order=pid*20+5, classNames = []}
zoneDisplay CZHand pid = ZDD {display = ZDHorizFill 10, order=pid*20, classNames = ["display-inline", "margin-onepx", "bordered"]}
zoneDisplay CZCodex pid = ZDD {display = ZDShelf $ CECard "codex_card" pid, order=pid*20+6, classNames = []}
zoneDisplay CZCodexRow pid = ZDD {display = ZDNested (ZDHorizFill 33) (CZ CZCodex $ pid `div` 3), order=(pid `div` 3)*20+(pid `mod` 3) + 5, classNames = ["display-inline", "margin-onepx", "stretch-horiz"]}


nameCard :: CardEntity -> String
nameCard card = show card

nameZone :: CardZone -> String
nameZone zone = show zone

inputHandler :: InputHandler CardTableState CardEntity CardZone
inputHandler s _ UIConnected = return (s, [], [])
inputHandler s _ (UIClick (CECard "blank" bidx)) = return $ processClick s (toEnum $ bidx `mod` 1000 `div` 100) (bidx `div` 1000)
inputHandler s _ (UIDrag (CECard "blank" _) _) = return (s, [], [])
inputHandler s pid (UIDrag card@(CECard _ _) zone) = return (moveCard s card zone, moveCardDisplay s pid card zone, [])
inputHandler s pid (UIDrag se@(CESubEntity _ _) zone) = return (removeSubEntity s se, moveCardDisplay s pid se zone, [])
inputHandler s _ (UIClick (CECard _ _)) = return (s, [], []) -- TODO: tap/untap?
inputHandler s _ (UIClick (CESubEntity _ _)) = return (s, [], []) -- TODO: click on the underlying?
inputHandler s _ (UIDragEntity _ (CESubEntity _ _)) = return (s, [], []) -- TODO: pass to the underlying?
inputHandler s pid (UIDragEntity entity card@(CECard cardtype _)) = return $ if inPlay s card || cardtype == "playmat_card"
	then (s { subEntities = (newSubEntity s entity, card):(subEntities $ removeSubEntity s entity)}, [GLBroadcast [GLMPlayerAction pid ("put " ++ nameCard entity ++ " on " ++ nameCard card) CZGamelog]], [])
	else (s, [], [])
	
newSubEntity :: CardTableState -> CardEntity -> CardEntity
newSubEntity s (CESubEntity subEntityType 0) = head $ filter (\se -> all ((/= se) . fst) $ subEntities s) $ map (CESubEntity subEntityType) [1..]
newSubEntity _ c = c -- this is either a move-around or a non-sub-entity.

moveCardDisplay :: CardTableState -> PlayerIndex -> CardEntity -> CardZone -> [Gamelog CardEntity CardZone]
moveCardDisplay _ pid card zone@(CZ CZPlay _) = [GLBroadcast [GLMMove [card] zone, GLMPlayerAction pid ("moved " ++ nameCard card ++ " to " ++ nameZone zone) CZGamelog]]
moveCardDisplay s pid card zone@(CZ _ px) = if inPlay s card
	then [GLBroadcast [GLMMove [card] zone, GLMPlayerAction pid ("moved " ++ nameCard card ++ " to " ++ nameZone zone) CZGamelog]]
	else [GLPrivate [px] [GLMMove [card] zone, GLMPlayerAction pid ("moved " ++ nameCard card ++ " to " ++ nameZone zone) CZGamelog], GLAllBut [px] [GLMPlayerAction pid ("moved a card to " ++ nameZone zone) CZGamelog]]
moveCardDisplay _ _ _ _ = [] -- move fails for this..

removeSubEntity :: CardTableState -> CardEntity -> CardTableState
removeSubEntity s se = s {subEntities = filter ((/= se) . fst) (subEntities s)}

inPlay :: CardTableState -> CardEntity -> Bool
inPlay s card = True `elem` map (elem card) (tables s)

moveCard :: CardTableState -> CardEntity -> CardZone -> CardTableState
moveCard s (CECard "codex_card" _) _ = s
moveCard s (CECard "playmat_card" _) _ = s
moveCard s card (CZ CZHand idx) = (removeSubEntitiesForCard (deleteCard s card) card) { hands = insertForIdx (hands $ deleteCard s card) idx card }
moveCard s card (CZ CZDeck idx) = (removeSubEntitiesForCard (deleteCard s card) card) { decks = insertForIdx (decks $ deleteCard s card) idx card }
moveCard s card (CZ CZDiscard idx) = (removeSubEntitiesForCard (deleteCard s card) card) { discards = insertForIdx (discards $ deleteCard s card) idx card }
moveCard s card (CZ CZPlay idx) = (deleteCard s card) { tables = insertForIdx (tables $ deleteCard s card) idx card }
moveCard s _ _ = s

deleteCard :: CardTableState -> CardEntity -> CardTableState
deleteCard s card = s
	{ hands = map (delete card) $ hands s
	, tables = map (delete card) $ tables s
	, discards = map (delete card) $ discards s
	, decks = map (delete card) $ decks s
	}
	
removeSubEntitiesForCard :: CardTableState -> CardEntity -> CardTableState
removeSubEntitiesForCard s card = s { subEntities = filter ((/= card) . snd) $ subEntities s }
	
insertForIdx :: [[a]] -> Int -> a -> [[a]]
insertForIdx list idx updater = take idx list ++ [(updater:(head $ drop idx list))] ++ drop (idx+1) list

updateForIdx :: [a] -> Int -> a -> [a]
updateForIdx list idx updater = take idx list ++ [updater] ++ drop (idx+1) list

processClick :: CardTableState -> CardZoneType -> PlayerIndex -> GameDelta CardTableState CardEntity CardZone
processClick s CZDeck pt = case (decks s) `atMay` pt of
		Just [] -> (s {decks = updateForIdx (decks s) pt newDeck, discards = updateForIdx (discards s) pt [], rng = rng'}, [GLBroadcast [GLMPlayerAction pt "reshuffled" CZGamelog]], [])
		  where
			(newDeck, rng') = shuffle (discards s !! pt) (rng s)
		Just (card:newDeck) -> (s {decks = updateForIdx (decks s) pt newDeck, hands = updateForIdx (hands s) pt (card:(hands s !! pt))}, [GLBroadcast [GLMPlayerAction pt "drew a card" CZGamelog]], [])
		_ -> (s, [], [])
processClick s _ _ = (s, [], [])

shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle [] r = ([], r)
shuffle a r = (element:rest, r')
  where
	(idx, r'') = randomR (0, length a - 1) r 
	element = a !! idx
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
		, subEntities = []
		, codexes = [[[], [], []], [[], [], []]]
		, rng = generator
		}		
	return Game
		{ playerRenderer = renderer
		, getPlayers = (\_ -> [0, 1])
		, handleInput = inputHandler
		, state = tableState
		, finished = (\_ -> False)
		}
