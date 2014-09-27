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
import Data.List (delete, elemIndices, sort)
import Control.Applicative ((<$>))

{- TODO LIST:
	- Real images!
	- Clicking on a codex card should put it into your discard.
	- Dragging something onto the codex should put it into the codex of the row that matches the spec.
	- Hero zone.
	- Worker zone.
	
	- Replace the awful map-chaining in the renderer with list comprehensions.
-}

renderer :: CardTableState -> PlayerIndex -> Screen CardZone CardEntity
renderer (CTS {hands, decks, tables, discards, tokens, codexes, exhaustedCards}) pid = Map.fromList (tokens' ++ hands' ++ decks' ++ tables' ++ discards' ++ playmats' ++ codexes' ++ codexrows' ++ gamelog)
  where
	tokens' =
		[ (CZTokens, (ZDD {display = ZDHorizFill 10, order= -1, classNames = ["display-inline", "margin-onepx", "bordered"]}
			,  (map (renderToken Nothing) $ map (\tokenType -> Token tokenType $ TI 0) [Gold .. Sword])
			++ (map (renderCard CZPlaymat []) $ map (\spec -> Card (CodexCard spec SpecToken) (CI 0)) [NeutralSpec .. Demonology] )
		  ))
		]
	discards' = map ($ []) $ zipWith ($) (map (renderZone (ConcealExcept pid) CZDiscard []) [0..]) discards
	tables' = map ($ tokens) $ zipWith ($) (map (renderZone (Show) CZPlay exhaustedCards) [0..]) tables
	decks' = map ($ []) $ zipWith ($) (map (renderZone (ConcealAll) CZDeck []) [0..]) decks
	hands' = map ($ []) $ zipWith ($) (map (renderZone (ConcealExcept pid) CZHand []) [0..]) hands
	codexes' = map ($ []) $ zipWith ($) (map (renderZone (ConcealExcept pid) CZCodex []) [0..]) [[], []]
	codexrows' = map ($ []) $ zipWith ($) (map (\idx -> renderZone (ConcealExcept $ (pid*3) + (idx `mod` 3)) CZCodexRow [] idx) [0..]) (concat codexes)
	gamelog = [(CZGamelog, (ZDD {display=ZDRight 20, order= -100, classNames=[]}, []))]
	playmats' = concatMap ($ tokens) $ map makePlaymat [0, 1] 

data RenderType = ConcealAll | ConcealExcept PlayerIndex | Show
	
makePlaymat :: PlayerIndex -> [(Token, Card)] -> [(CardZone, (ZoneDisplayData CardZone CardEntity, [ScreenEntity CardEntity]))]
makePlaymat pid tokens =
	[ (CZ CZCodexHolder pid, (zoneDisplay CZCodexHolder pid, [(renderCard CZPlaymat [] $ Card (UtilityCard CodexHolder) $ CI pid) {eSize = SESPercent 100 10}]))
	, (CZ CZPlaymat pid, (zoneDisplay CZPlaymat pid, map (renderCard CZPlaymat []) playmatCards ++ 
		map (\(token, card) -> renderToken (Just card) token) (filter (\(_, card) -> elem card playmatCards) tokens)))
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
	
	
renderZone
	:: RenderType
	-> CardZoneType
	-> [Card]
	-> PlayerIndex -- controller of the zone in question.
	-> [Card]
	-> [(Token, Card)]
	-> (CardZone, (ZoneDisplayData CardZone CardEntity, [ScreenEntity CardEntity]))
renderZone Show t exhaustedCards p cards tokens = (CZ t p, (zoneDisplay t p, map (renderCard t exhaustedCards) cards ++ 
	map (\(token, card) -> renderToken (Just card) token) (filter (\(_, card) -> elem card cards) tokens)
	))
renderZone ConcealAll t _ p cards _ = (CZ t p, (zoneDisplay t p, [cardCounter p t (length cards)]))
renderZone (ConcealExcept pid) t _ p cards _ = (CZ t p, (zoneDisplay t p
	, (if pid == p then (map (renderCard t []) cards) else [cardCounter p t (length cards)])
	))

renderCard :: CardZoneType -> [Card] -> Card -> ScreenEntity CardEntity
renderCard t exhaustedCards c@(Card cardType _)  =
	SE
		{ eId = CECard c
		, eDisplay = SDImage "images/placeholder.jpg"
		, eSize = case cardType of
			UtilityCard Hero_1_Holder -> SESPercent 95 33
			UtilityCard Hero_2_Holder -> SESPercent 95 33
			UtilityCard Hero_3_Holder -> SESPercent 95 33
			UtilityCard Tech_3_1_Building -> SESPercent 20 33
			UtilityCard Tech_3_2_Building -> SESPercent 20 33
			UtilityCard Tech_3_3_Building -> SESPercent 20 33
			UtilityCard Tech_2_1_Building -> SESPercent 20 33
			UtilityCard Tech_2_2_Building -> SESPercent 20 33
			UtilityCard Tech_2_3_Building -> SESPercent 20 33
			UtilityCard SurplusBuilding -> SESPercent 33 33
			UtilityCard Tech_1_Building -> SESPercent 33 33
			UtilityCard TowerBuilding -> SESPercent 33 33
			UtilityCard BaseBuilding -> SESPercent 25 95
			_ ->
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
zoneDisplay CZDiscard pid = ZDD {display=ZDNested (ZDRight 20) (CZ CZPlay pid), order=pid*10+6, classNames = ["margin-onepx", "bordered"]}
zoneDisplay CZPlay pid = ZDD {display = ZDHorizFill 35, order=pid*10+2, classNames = ["display-inline", "margin-onepx"]}
zoneDisplay CZDeck pid = ZDD {display = ZDNested (ZDLeft 10) (CZ CZPlay pid), order=pid*10+3, classNames = []}
zoneDisplay CZPlaymat pid = ZDD {display = ZDNested (ZDLeft 20) (CZ CZPlay pid), order=pid*10+4, classNames = []}
zoneDisplay CZCodexHolder pid = ZDD {display = ZDNested (ZDRight 5) (CZ CZPlay pid), order=pid*10+5, classNames = []}
zoneDisplay CZHand pid = ZDD {display = ZDHorizFill 10, order=pid*20, classNames = ["display-inline", "margin-onepx", "bordered"]}
zoneDisplay CZCodex pid = ZDD {display = ZDShelf $ CECard $ Card (UtilityCard CodexHolder) $ CI pid, order=pid*20+6, classNames = []}
zoneDisplay CZCodexRow pid = ZDD {display = ZDNested (ZDHorizFill 33) (CZ CZCodex $ pid `div` 3), order=(pid `div` 3)*20+(pid `mod` 3) + 7, classNames = ["display-inline", "margin-onepx", "stretch-horiz"]}


nameCard :: Card -> String
nameCard card = show card

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
selectSpec s@(CTS{codexes, specs, decks, rng}) spec pid = case length (specs !! pid) of
	0 ->
		( s {codexes = addSpecToCodex codexes pid spec, specs = addSpec specs pid spec, decks = newDecks, rng = newRng}
		, [GLBroadcast [GLMPlayerAction pid ("chose the " ++ nameSpec spec ++ " spec, giving them the " ++ nameColor (colorOfSpec spec) ++ " starting deck.") CZGamelog]]
		, []
		)
	  where
		(newDecks, newRng) = addStartingDeck decks rng pid $ colorOfSpec spec
	3 -> (s, [], [])
	_ ->
		( s {codexes = addSpecToCodex codexes pid spec, specs = addSpec specs pid spec}
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
inputHandler s pid (UIDragEntity (CEToken entity) (CECard card)) = return $ if inPlay s card
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
moveCardEntityDisplay s pid (CECard card) zone@(CZ _ px) = if inPlay s card
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

inCodex :: CardTableState -> Card -> Bool
inCodex s card = True `elem` map (elem card) (concat $ codexes s)

moveCard :: CardTableState -> Card -> CardZone -> CardTableState
moveCard s (Card (UtilityCard _) _) _ = s
moveCard s card (CZ CZHand idx) = (removeSubEntitiesForCard (deleteCard s card) card) { hands = insertForIdx (hands $ deleteCard s card) idx card }
moveCard s card (CZ CZDeck idx) = (removeSubEntitiesForCard (deleteCard s card) card) { decks = insertForIdx (decks $ deleteCard s card) idx card }
moveCard s card (CZ CZDiscard idx) = (removeSubEntitiesForCard (deleteCard s card) card) { discards = insertForIdx (discards $ deleteCard s card) idx card }
moveCard s card@(Card (CodexCard spec _) _) (CZ CZCodexHolder idx) = (removeSubEntitiesForCard (deleteCard s card) card) { codexes = unsafeTwiddleList (codexes $ deleteCard s card) idx (\codex -> (sort $ insertForIdx codex (head $ elemIndices spec ((specs s) !! idx)) card)) }
moveCard s card (CZ CZPlay idx) = (deleteCard s card) { tables = insertForIdx (tables $ deleteCard s card) idx card }
moveCard s _ _ = s

deleteCard :: CardTableState -> Card -> CardTableState
deleteCard s card = s
	{ hands = map (delete card) $ hands s
	, tables = map (delete card) $ tables s
	, discards = map (delete card) $ discards s
	, decks = map (delete card) $ decks s
	, codexes = [map (delete card) codex | codex <- codexes s]
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
		, rng = generator
		}		
	return Game
		{ playerRenderer = renderer
		, getPlayers = (\_ -> [0, 1])
		, handleInput = inputHandler
		, state = tableState
		, finished = (\_ -> False)
		}
