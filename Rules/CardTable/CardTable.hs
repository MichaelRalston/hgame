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
import Data.List (delete, elemIndices, sort, zipWith4)
import Control.Applicative ((<$>))

import Engine.Debug

{- TODO LIST:
	- Real images!
	- Hero zone.
	- Worker zone.
	
	- Replace the awful map-chaining in the renderer with list comprehensions.
-}

withIndexes :: [a] -> [(Int, a)]
withIndexes list = zip [0..] list

renderer :: CardTableState -> PlayerIndex -> Screen CardZone CardEntity
renderer (CTS {hands, decks, tables, discards, tokens, codexes, exhaustedCards, heroes, specs, workers}) pid = Map.fromList (tokens' ++ hands' ++ decks' ++ tables' ++ discards' ++ playmats' ++ codexes' ++ codexrows' ++ workers' ++ gamelog)
  where
	tokens' =
		[ (CZTokens, (ZDD {display = ZDHorizFill 10, order= -1, classNames = ["display-inline", "margin-onepx", "bordered"], droppable = False}
			, [renderToken Nothing $ Token tokenType $ TI 0 | tokenType <- [Gold .. Sword]]
			++ [renderCard CZPlaymat [] $ Card (CodexCard spec SpecToken) $ CI 0 | spec <- [NeutralSpec .. Demonology]] )
		  )
		]
	discards' = [renderZone (ConcealExcept pid) CZDiscard [] idx discard [] | (idx, discard) <- withIndexes discards]
	tables' = [renderZone (Show) CZPlay exhaustedCards idx table tokens | (idx, table) <- withIndexes tables]
	decks' = [renderZone (ConcealAll) CZDeck [] idx deck [] | (idx, deck) <- withIndexes decks]
	hands' = [renderZone (ConcealExcept pid) CZHand [] idx hand [] | (idx, hand) <- withIndexes hands]
	codexes' = [renderZone (ConcealExcept pid) CZCodex [] idx codex [] | (idx, codex) <- withIndexes [[], []]]
	codexrows' = [renderZone (ConcealExcept $ (pid*3) + (idx `mod` 3)) CZCodexRow [] idx codexRow [] | (idx, codexRow) <- withIndexes $ concat codexes]
	workers' = [renderZone (ConcealAll) CZWorkers [] idx workerZone [] | (idx, workerZone) <- withIndexes workers]
	gamelog = [(CZGamelog, (ZDD {display=ZDRight 20, order= -100, classNames=[], droppable=False}, []))]
	playmats' = concat $ zipWith4 makePlaymat [0, 1] (repeat tokens) heroes specs

data RenderType = ConcealAll | ConcealExcept PlayerIndex | Show
	
makePlaymat :: PlayerIndex -> [(Token, Card)] -> [Card] -> [CardSpec] -> [(CardZone, (ZoneDisplayData CardZone CardEntity, [ScreenEntity CardEntity]))]
makePlaymat pid tokens heroes specs =
	[ (CZ CZCodexHolder pid, (zoneDisplay CZCodexHolder pid, [(renderCard CZPlaymat [] $ Card (UtilityCard CodexHolder) $ CI pid) {eSize = SESPercent 100 10}]))
	, (CZ CZPlaymat pid, (zoneDisplay CZPlaymat pid, map (renderCard CZPlaymat []) (commandCards ++ playmatCards) ++ renderTokensForCards tokens (playmatCards ++ heroes)))
	]
  where
	playmatCards =
		[ Card (UtilityCard Tech_3_1_Building) $ CI pid
		, Card (UtilityCard Tech_3_2_Building) $ CI pid
		, Card (UtilityCard Tech_3_3_Building) $ CI pid
		, Card (UtilityCard Tech_2_1_Building) $ CI pid
		, Card (UtilityCard Tech_2_2_Building) $ CI pid
		, Card (UtilityCard Tech_2_3_Building) $ CI pid
		, Card (UtilityCard SurplusBuilding) $ CI pid
		, Card (UtilityCard Tech_1_Building) $ CI pid
		, Card (UtilityCard TowerBuilding) $ CI pid
		, Card (UtilityCard BaseBuilding) $ CI pid
		]
	commandCards = zipWith (\def card -> if card `elem` heroes then card else def)
		(map (\card -> Card (UtilityCard card) (CI pid)) [Hero_1_Holder .. Hero_3_Holder])
		(map (\spec -> Card (CodexCard spec Hero) (CI pid)) specs)
	
renderTokensForCards :: [(Token, Card)] -> [Card] -> [ScreenEntity CardEntity]
renderTokensForCards tokens cards = map (\(token, card) -> renderToken (Just card) token) (filter (\(_, card) -> elem card cards) tokens)
	
renderZone
	:: RenderType
	-> CardZoneType
	-> [Card]
	-> PlayerIndex -- controller of the zone in question.
	-> [Card]
	-> [(Token, Card)]
	-> (CardZone, (ZoneDisplayData CardZone CardEntity, [ScreenEntity CardEntity]))
renderZone Show t exhaustedCards p cards tokens = (CZ t p, (zoneDisplay t p, map (renderCard t exhaustedCards) cards ++ renderTokensForCards tokens cards))
renderZone ConcealAll t _ p cards _ = (CZ t p, (zoneDisplay t p, [cardCounter p t (length cards)]))
renderZone (ConcealExcept pid) t _ p cards _ = (CZ t p, (zoneDisplay t p
	, (if pid == p then (map (renderCard t []) cards) else [cardCounter p t (length cards)])
	))
	
renderCard :: CardZoneType -> [Card] -> Card -> ScreenEntity CardEntity
renderCard t exhaustedCards c@(Card cardType _)  =
	SE
		{ eId = CECard c
		, eDisplay = cardImage cardType
		, eSize = case (cardType, t) of
			(UtilityCard Hero_1_Holder, _) -> SESPercent 20 33
			(UtilityCard Hero_2_Holder, _) -> SESPercent 20 33
			(UtilityCard Hero_3_Holder, _) -> SESPercent 20 33
			(UtilityCard Tech_3_1_Building, _) -> SESPercent 20 33
			(UtilityCard Tech_3_2_Building, _) -> SESPercent 20 33
			(UtilityCard Tech_3_3_Building, _) -> SESPercent 20 33
			(UtilityCard Tech_2_1_Building, _) -> SESPercent 20 33
			(UtilityCard Tech_2_2_Building, _) -> SESPercent 20 33
			(UtilityCard Tech_2_3_Building, _) -> SESPercent 20 33
			(UtilityCard SurplusBuilding, _) -> SESPercent 20 33
			(UtilityCard Tech_1_Building, _) -> SESPercent 20 33
			(UtilityCard TowerBuilding, _) -> SESPercent 20 33
			(UtilityCard BaseBuilding, _) -> SESPercent 19 95
			(CodexCard _ Hero, CZPlaymat) -> SESPercent 20 33
			(_, CZHand) -> SESAutoWidth 95
			(_, CZPlay) -> SESAutoWidth 44
			(_, CZDeck) -> SESAutoWidth 95
			(_, CZDiscard) -> SESAutoWidth 44
			(_, CZCodex) -> SESAutoWidth 80
			(_, CZCodexRow) -> SESAutoWidth 80
			(_, CZPlaymat) -> SESAutoWidth 95
			(_, CZCodexHolder) -> SESAutoWidth 95
			(_, CZWorkers) -> SESAutoWidth 95
		, eEntitiesDropOn = elem t [CZPlay, CZPlaymat]
		, eDropOnEntities = False
		, eClickable = case cardType of
			UtilityCard BlankCard -> True
			UtilityCard _ -> False
			_ -> True
		, eDraggable = case cardType of
			UtilityCard _ -> False
			CodexCard _ SpecToken -> False
			_ -> True
		, eNestOnEntity = Nothing
		, eClasses = if c `elem` exhaustedCards then ["rotate90"] else []
		}

renderToken :: Maybe Card -> Token -> ScreenEntity CardEntity
renderToken card token =
	SE
		{ eId = CEToken token
		, eDisplay = SDImage "images/placeholder.jpg"
		, eSize = SESAutoWidth 23
		, eEntitiesDropOn = False
		, eDropOnEntities = True
		, eDraggable = True
		, eClickable = False
		, eNestOnEntity = CECard <$> card
		, eClasses = ["rotate180"] -- TODO: replace with real things?
		}
		
cardCounter :: PlayerIndex -> CardZoneType -> Int -> ScreenEntity CardEntity
cardCounter p t count = SE { eId = CECard $ Card (UtilityCard BlankCard) $ CI (p*1000 + (fromEnum t)*100), eDisplay = SDText $ show count ++ " CARDS", eSize = SESAutoWidth (if t == CZDiscard then 22 else 95), eClickable = True, eDraggable = False, eEntitiesDropOn = False, eDropOnEntities = False, eClasses = [], eNestOnEntity = Nothing}

zoneDisplay :: CardZoneType -> PlayerIndex -> ZoneDisplayData CardZone CardEntity
zoneDisplay CZDiscard pid = ZDD {display=ZDNested (ZDRight 20) (CZ CZPlay pid), order=pid*10+7, classNames = ["margin-onepx", "bordered"], droppable = True}
zoneDisplay CZPlay pid = ZDD {display = ZDHorizFill 35, order=pid*10+2, classNames = ["display-inline", "margin-onepx"], droppable = True}
zoneDisplay CZDeck pid = ZDD {display = ZDNested (ZDLeft 10) (CZ CZPlay pid), order=pid*10+3, classNames = [], droppable = True}
zoneDisplay CZWorkers pid = ZDD {display = ZDNested (ZDLeft 10) (CZ CZPlay pid), order=pid*10+4, classNames = [], droppable = True}
zoneDisplay CZPlaymat pid = ZDD {display = ZDNested (ZDLeft 20) (CZ CZPlay pid), order=pid*10+5, classNames = [], droppable = True}
zoneDisplay CZCodexHolder pid = ZDD {display = ZDNested (ZDRight 5) (CZ CZPlay pid), order=pid*10+6, classNames = [], droppable = True}
zoneDisplay CZHand pid = ZDD {display = ZDHorizFill 10, order=pid*20, classNames = ["display-inline", "margin-onepx", "bordered"], droppable = True}
zoneDisplay CZCodex pid = ZDD {display = ZDShelf $ CECard $ Card (UtilityCard CodexHolder) $ CI pid, order=pid*20+8, classNames = [], droppable = False}
zoneDisplay CZCodexRow pid = ZDD {display = ZDNested (ZDHorizFill 33) (CZ CZCodex $ pid `div` 3), order=(pid `div` 3)*20+(pid `mod` 3) + 9, classNames = ["display-inline", "margin-onepx", "stretch-horiz"], droppable = False}


nameCard :: Card -> String
nameCard (Card (SDCard NeutralColor SDCard0) _) = "Timely Messenger"
nameCard (Card (SDCard NeutralColor SDCard1) _) = "Younger Brother"
nameCard (Card (SDCard NeutralColor SDCard2) _) = "Older Brother"
nameCard (Card (SDCard NeutralColor SDCard3) _) = "Brick Thief"
nameCard (Card (SDCard NeutralColor SDCard4) _) = "Helpful Turtle"
nameCard (Card (SDCard NeutralColor SDCard5) _) = "Granfalloon Flagbearer"
nameCard (Card (SDCard NeutralColor SDCard6) _) = "Fruit Ninja"
nameCard (Card (SDCard NeutralColor SDCard7) _) = "Spark"
nameCard (Card (SDCard NeutralColor SDCard8) _) = "Bloom"
nameCard (Card (SDCard NeutralColor SDCard9) _) = "Wither"

nameCard (Card (SDCard Red SDCard0) _) = "Nautical Dog"
nameCard (Card (SDCard Red SDCard1) _) = "Demolisher"
nameCard (Card (SDCard Red SDCard2) _) = "Bombaster"
nameCard (Card (SDCard Red SDCard3) _) = "Careless Musketeer"
nameCard (Card (SDCard Red SDCard4) _) = "Bloodrage Ogre"
nameCard (Card (SDCard Red SDCard5) _) = "Makeshift Rambaster"
nameCard (Card (SDCard Red SDCard6) _) = "Bloodburn"
nameCard (Card (SDCard Red SDCard7) _) = "Scorch"
nameCard (Card (SDCard Red SDCard8) _) = "Frenzy"
nameCard (Card (SDCard Red SDCard9) _) = "Shell Shock"

nameCard (Card (SDCard Green SDCard0) _) = "Merfolk Prospector"
nameCard (Card (SDCard Green SDCard1) _) = "Tiger Cub"
nameCard (Card (SDCard Green SDCard2) _) = "Young Treant"
nameCard (Card (SDCard Green SDCard3) _) = "Playful Panda"
nameCard (Card (SDCard Green SDCard4) _) = "Ironbark Treant"
nameCard (Card (SDCard Green SDCard5) _) = "Spore Shambler"
nameCard (Card (SDCard Green SDCard6) _) = "Verdant Tree"
nameCard (Card (SDCard Green SDCard7) _) = "Rich Earth"
nameCard (Card (SDCard Green SDCard8) _) = "Rampant Growth"
nameCard (Card (SDCard Green SDCard9) _) = "Forest's Favor"

nameCard (Card (CodexCard NeutralSpec Hero) _) = "Pearl Harper"
nameCard (Card (CodexCard Anarchy Hero) _) = "Captain Zane"
nameCard (Card (CodexCard Blood Hero) _) = "Drakk Ramhorn"
nameCard (Card (CodexCard Fire Hero) _) = "Jaina Stormborne"
nameCard (Card (CodexCard Balance Hero) _) = "Master Midori"
nameCard (Card (CodexCard Feral Hero) _) = "Calamandra Moss"
nameCard (Card (CodexCard Growth Hero) _) = "Argagarg Garg"

nameCard card = show card

cardImage :: CardIdentifier -> ScreenDisplay
cardImage (SDCard NeutralColor SDCard0) = SDImage "images/cards/_0002_timely%20messenger.jpg"
cardImage (SDCard NeutralColor SDCard1) = SDImage "images/cards/_0003_younger%20brother.jpg"
cardImage (SDCard NeutralColor SDCard2) = SDImage "images/cards/_0004_older%20brother.jpg"
cardImage (SDCard NeutralColor SDCard3) = SDImage "images/cards/_0005_brick%20thief.jpg"
cardImage (SDCard NeutralColor SDCard4) = SDImage "images/cards/_0006_helpful%20turtle.jpg"
cardImage (SDCard NeutralColor SDCard5) = SDImage "images/cards/_0007_granfalloon%20flagbearer.jpg"
cardImage (SDCard NeutralColor SDCard6) = SDImage "images/cards/_0008_fruit%20ninja.jpg"
cardImage (SDCard NeutralColor SDCard7) = SDImage "images/cards/_0009_spark.jpg"
cardImage (SDCard NeutralColor SDCard8) = SDImage "images/cards/_0010_bloom.jpg"
cardImage (SDCard NeutralColor SDCard9) = SDImage "images/cards/_0011_wither.jpg"

cardImage (SDCard Red SDCard0) = SDImage "images/cards/_0001_nautical%20dog.jpg"
cardImage (SDCard Red SDCard1) = SDImage "images/cards/_0002_demolisher.jpg"
cardImage (SDCard Red SDCard2) = SDImage "images/cards/_0003_bombaster.jpg"
cardImage (SDCard Red SDCard3) = SDImage "images/cards/_0004_careless%20musketeer.jpg"
cardImage (SDCard Red SDCard4) = SDImage "images/cards/_0005_bloodrage%20ogre.jpg"
cardImage (SDCard Red SDCard5) = SDImage "images/cards/_0006_makeshift%20rambaster.jpg"
cardImage (SDCard Red SDCard6) = SDImage "images/cards/_0007_bloodburn.jpg"
cardImage (SDCard Red SDCard7) = SDImage "images/cards/_0008_scorch.jpg"
cardImage (SDCard Red SDCard8) = SDImage "images/cards/_0009_frenzy.jpg"
cardImage (SDCard Red SDCard9) = SDImage "images/cards/_0010_shell%20shock.jpg"

cardImage (SDCard Green SDCard0) = SDImage "images/cards/_0000_merfolk%20prospector.jpg"
cardImage (SDCard Green SDCard1) = SDImage "images/cards/_0001_tiger%20cub.jpg"
cardImage (SDCard Green SDCard2) = SDImage "images/cards/_0002_young%20treant.jpg"
cardImage (SDCard Green SDCard3) = SDImage "images/cards/_0003_playful%20panda.jpg"
cardImage (SDCard Green SDCard4) = SDImage "images/cards/_0004_ironbark%20treant.jpg"
cardImage (SDCard Green SDCard5) = SDImage "images/cards/_0005_spore%20shambler.jpg"
cardImage (SDCard Green SDCard6) = SDImage "images/cards/_0006_verdant%20tree.jpg"
cardImage (SDCard Green SDCard7) = SDImage "images/cards/_0007_rich%20earth.jpg"
cardImage (SDCard Green SDCard8) = SDImage "images/cards/_0008_rampant%20growth.jpg"
cardImage (SDCard Green SDCard9) = SDImage "images/cards/_0009_forests%20favor.jpg"

cardImage (CodexCard NeutralSpec Hero) = SDImage "images/cards/_0019_neutral%20hero.jpg"
cardImage (CodexCard Anarchy Hero) = SDImage "images/cards/_0000_anarchy%20hero.jpg"
cardImage (CodexCard Blood Hero) = SDImage "images/cards/_0001_blood%20hero.jpg"
cardImage (CodexCard Fire Hero) = SDImage "images/cards/_0002_fire%20hero.jpg"
cardImage (CodexCard Balance Hero) = SDImage "images/cards/_0003_balance%20hero.jpg"
cardImage (CodexCard Feral Hero) = SDImage "images/cards/_0004_feral%20hero.jpg"
cardImage (CodexCard Growth Hero) = SDImage "images/cards/_0005_growth%20hero.jpg"

cardImage _ = SDImage "images/placeholder.jpg"

nameToken :: Token -> String
nameToken token = show token

nameZone :: CardZone -> String
nameZone zone = show zone

nameSpec :: CardSpec -> String
nameSpec spec = show spec

nameColor :: CardColor -> String
nameColor color = show color

unsafeTwiddleList :: [a] -> Int -> (a -> a) -> [a]
unsafeTwiddleList list idx twiddler = take idx list ++ [twiddler (list !! idx)] ++ drop (idx+1) list

addSpecToCodex :: [[[Card]]] -> PlayerIndex -> CardSpec -> [[[Card]]]
addSpecToCodex codexes pid spec = unsafeTwiddleList codexes pid $ (\codex -> unsafeTwiddleList codex (newRowId codex) (\[] -> newRow))
  where
	newRowId codex = head $ elemIndices [] codex
	newRow = sort $ [Card (CodexCard spec t) (CI (pid*10 + idx)) | idx <- [0, 1], t <- [Spell_1 .. Tech_3]]

addStartingDeck :: [[Card]] -> StdGen -> PlayerIndex -> CardColor -> ([[Card]], StdGen)
addStartingDeck decks rng pid color = (unsafeTwiddleList decks pid (\[] -> deck), rng')
  where
	(deck, rng') = shuffle (map (\t -> Card (SDCard color t) (CI pid)) [SDCard0 .. SDCard9]) rng

addSpec :: [[CardSpec]] -> PlayerIndex -> CardSpec -> [[CardSpec]]
addSpec specs pid spec = unsafeTwiddleList specs pid (++ [spec])

selectSpec :: CardTableState -> CardSpec -> PlayerIndex -> GameDelta CardTableState CardEntity CardZone
selectSpec s@(CTS{codexes, specs, decks, rng, heroes}) spec pid = case length (specs !! pid) of
	0 ->
		( s {codexes = addSpecToCodex codexes pid spec, specs = addSpec specs pid spec, decks = newDecks, rng = newRng, heroes = unsafeTwiddleList heroes pid (++ [Card (CodexCard spec Hero) (CI pid)])}
		, [GLBroadcast [GLMPlayerAction pid ("chose the " ++ nameSpec spec ++ " spec, giving them the " ++ nameColor (colorOfSpec spec) ++ " starting deck.") CZGamelog]]
		, []
		)
	  where
		(newDecks, newRng) = addStartingDeck decks rng pid $ colorOfSpec spec
	3 -> (s, [], [])
	_ ->
		( s {codexes = addSpecToCodex codexes pid spec, specs = addSpec specs pid spec, heroes = unsafeTwiddleList heroes pid (++ [Card (CodexCard spec Hero) (CI pid)])}
		, [GLBroadcast [GLMPlayerAction pid ("chose the " ++ nameSpec spec ++ " spec.") CZGamelog]]
		, []
		)
		
inputHandler :: InputHandler CardTableState CardEntity CardZone
inputHandler s _ UIConnected = return (s, [], [])
inputHandler s _ (UIClick (CECard (Card (UtilityCard BlankCard) (CI bidx)))) = return $ processClick s (toEnum $ bidx `mod` 1000 `div` 100) (bidx `div` 1000)
inputHandler s pid (UIClick (CECard (Card (CodexCard spec SpecToken) _))) = return $ selectSpec s spec pid
inputHandler s _ (UIDrag (CECard (Card (UtilityCard BlankCard) _)) _) = return (s, [], [])
inputHandler s pid (UIDrag cardEntity@(CECard card) zone) = return (moveCard s card zone, moveCardEntityDisplay s pid cardEntity zone, [])
inputHandler s pid (UIDrag ce@(CEToken token) zone) = return (removeToken s token, moveCardEntityDisplay s pid ce zone, [])
inputHandler s@(CTS{exhaustedCards}) pid (UIClick (CECard card)) = return $ case (inPlay s card, inCodex s card) of 
	(True, _) -> ( s { exhaustedCards = if card `elem` exhaustedCards then delete card exhaustedCards else card:exhaustedCards }
		 , [GLBroadcast [GLMPlayerAction pid ((if card `elem` exhaustedCards then "readied " else "exhausted ") ++ nameCard card) CZGamelog]]
		 , []
		 )
	(False, True) -> (moveCard s card $ CZ CZDiscard pid, moveCardEntityDisplay s pid (CECard card) $ CZ CZDiscard pid, [])
	_ -> (s, [], [])
inputHandler s _ (UIClick (CEToken _)) = return (s, [], []) -- TODO: click on the underlying?
inputHandler s _ (UIDragEntity _ (CEToken _)) = return (s, [], []) -- TODO: pass to the underlying?
inputHandler s pid (UIDragEntity (CEToken entity) (CECard card)) = return $ if inSight s card
	then (s { tokens = (newToken s entity, card):(tokens $ removeToken s entity)}, [GLBroadcast [GLMPlayerAction pid ("put " ++ nameToken entity ++ " on " ++ nameCard card) CZGamelog]], [])
	else (s, [], [])
inputHandler s _ (UIDragEntity (CEToken _) (CECard (Card (UtilityCard CodexHolder) _))) = return (s, [], [])
inputHandler s pid (UIDragEntity (CEToken entity) (CECard card)) = return (s { tokens = (newToken s entity, card):(tokens $ removeToken s entity)}, [GLBroadcast [GLMPlayerAction pid ("put " ++ nameToken entity ++ " on " ++ nameCard card) CZGamelog]], [])
inputHandler s _ (UIDragEntity (CECard _) (CECard _)) = return (s, [], [])
	
newToken :: CardTableState -> Token -> Token
newToken s (Token subEntityType (TI 0)) = head $ filter (\se -> all ((/= se) . fst) $ tokens s) $ map (Token subEntityType) $ map TI [1..]
newToken _ c = c -- this is either a move-around or a non-sub-entity.

moveCardEntityDisplay :: CardTableState -> PlayerIndex -> CardEntity -> CardZone -> [Gamelog CardEntity CardZone]
moveCardEntityDisplay _ pid cardEntity zone@(CZ CZPlay _) = [GLBroadcast [GLMMove [cardEntity] zone, GLMPlayerAction pid ("moved " ++ nameCardEntity cardEntity ++ " to " ++ nameZone zone) CZGamelog]]
moveCardEntityDisplay s pid (CECard card) (CZ CZCodexHolder px) = if inSight s card
	then [GLBroadcast [GLMPlayerAction pid ("moved " ++ nameCard card ++ " to their codex.") CZGamelog]]
	else [GLPrivate [px] [GLMPlayerAction pid ("moved " ++ nameCard card ++ " to your codex.") CZGamelog], GLAllBut [px] [GLMPlayerAction pid ("moved a card to their codex.") CZGamelog]]
moveCardEntityDisplay s pid (CECard card) zone@(CZ _ px) = if inSight s card
	then [GLBroadcast [GLMMove (map CECard [card]) zone, GLMPlayerAction pid ("moved " ++ nameCard card ++ " to " ++ nameZone zone) CZGamelog]]
	else [GLPrivate [px] [GLMMove (map CECard [card]) zone, GLMPlayerAction pid ("moved " ++ nameCard card ++ " to " ++ nameZone zone) CZGamelog], GLAllBut [px] [GLMPlayerAction pid ("moved a card to " ++ nameZone zone) CZGamelog]]
moveCardEntityDisplay _ _ _ _ = [] -- move fails for this.

nameCardEntity :: CardEntity -> String
nameCardEntity (CECard c) = nameCard c
nameCardEntity (CEToken t) = nameToken t

removeToken :: CardTableState -> Token -> CardTableState
removeToken s se = s {tokens = filter ((/= se) . fst) (tokens s)}

inPlay :: CardTableState -> Card -> Bool
inPlay s card = True `elem` map (elem card) (tables s)

inSight :: CardTableState -> Card -> Bool
inSight s card = inPlay s card || (True `elem` map (elem card) (heroes s))

inCodex :: CardTableState -> Card -> Bool
inCodex s card = True `elem` map (elem card) (concat $ codexes s)

moveCard :: CardTableState -> Card -> CardZone -> CardTableState
moveCard s (Card (UtilityCard _) _) _ = s
moveCard s card (CZ CZHand idx) = (removeSubEntitiesForCard (deleteCard s card) card) { hands = insertForIdx (hands $ deleteCard s card) idx card }
moveCard s card (CZ CZDeck idx) = (removeSubEntitiesForCard (deleteCard s card) card) { decks = insertForIdx (decks $ deleteCard s card) idx card }
moveCard s card (CZ CZWorkers idx) = (removeSubEntitiesForCard (deleteCard s card) card) { workers = insertForIdx (workers $ deleteCard s card) idx card }
moveCard s card (CZ CZDiscard idx) = (removeSubEntitiesForCard (deleteCard s card) card) { discards = insertForIdx (discards $ deleteCard s card) idx card }
moveCard s card@(Card (CodexCard _ Hero) _) (CZ CZPlaymat idx) = (removeSubEntitiesForCard (deleteCard s card) card) { heroes = insertForIdx (heroes $ deleteCard s card) idx card }
moveCard s card@(Card (CodexCard spec _) _) (CZ CZCodexHolder idx) = (removeSubEntitiesForCard (deleteCard s card) card) { codexes = unsafeTwiddleList (codexes $ deleteCard s card) idx (\codex -> (sort $ insertForIdx codex (head $ traceSs "possible spec indices" $ elemIndices spec ((specs s) !! idx)) card)) }
moveCard s card (CZ CZPlay idx) = (deleteCard s card) { tables = insertForIdx (tables $ deleteCard s card) idx card }
moveCard s _ _ = s

deleteCard :: CardTableState -> Card -> CardTableState
deleteCard s card = s
	{ hands = map (delete card) $ hands s
	, tables = map (delete card) $ tables s
	, discards = map (delete card) $ discards s
	, decks = map (delete card) $ decks s
	, codexes = [map (delete card) codex | codex <- codexes s]
	, heroes = map (delete card) $ heroes s
	}
	
removeSubEntitiesForCard :: CardTableState -> Card -> CardTableState
removeSubEntitiesForCard s card = s { tokens = filter ((/= card) . snd) $ tokens s, exhaustedCards = delete card $ exhaustedCards s }
	
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
	
colorOfSpec :: CardSpec -> CardColor
colorOfSpec NeutralSpec = NeutralColor
colorOfSpec Anarchy = Red
colorOfSpec Blood = Red
colorOfSpec Fire = Red
colorOfSpec Balance = Green
colorOfSpec Feral = Green
colorOfSpec Growth = Green
colorOfSpec Law = Blue
colorOfSpec Peace = Blue
colorOfSpec Truth = Blue
colorOfSpec Past = Purple
colorOfSpec Present = Purple
colorOfSpec Future = Purple
colorOfSpec Discipline = White
colorOfSpec Ninjitsu = White
colorOfSpec Strength = White
colorOfSpec Disease = Black
colorOfSpec Necromancy = Black
colorOfSpec Demonology = Black
	
makeCardTable :: StdGen -> WithMemory Game
makeCardTable generator = do
	let deck = []
	tableState <- alloc $ CTS
		{ hands = [[],[]]
		, decks = [deck,[]]
		, tables = [[],[]]
		, discards = [[],[]]
		, tokens = []
		, codexes = [[[], [], []], [[], [], []]]
		, exhaustedCards = []
		, specs = [[], []]
		, heroes = [[], []]
		, workers = [[Card (UtilityCard BlankCard) (CI 40),Card (UtilityCard BlankCard) (CI 41),Card (UtilityCard BlankCard) (CI 42),Card (UtilityCard BlankCard) (CI 43)], [Card (UtilityCard BlankCard) (CI 44),Card (UtilityCard BlankCard) (CI 45),Card (UtilityCard BlankCard) (CI 46),Card (UtilityCard BlankCard) (CI 47)]]
		, rng = generator
		}		
	return Game
		{ playerRenderer = renderer
		, getPlayers = (\_ -> [0, 1])
		, handleInput = inputHandler
		, state = tableState
		, finished = (\_ -> False)
		}
