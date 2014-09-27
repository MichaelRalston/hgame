{-# LANGUAGE NamedFieldPuns, OverloadedStrings, TemplateHaskell #-}

module Rules.CardTable.Types
	( CardZone(..)
	, CardTableState (..)
	, CardEntity (..)
	, CardZoneType (..)
	, Card (..)
	, Token (..)
	, TokenIndex (..)
	, CardIndex (..)
	, CardSpec (..)
	, CardColor (..)
	, CardType (..)
	, TokenType (..)
	, UtilityCardType (..)
	, CardIdentifier (..)
	, SDCardType (..)
	) where
	
import Engine.Types
import Data.Aeson (ToJSON, FromJSON, Value(..), parseJSON)
import Data.Text (pack, unpack, breakOn, breakOnEnd, drop, Text, append)
import Text.Read (readMaybe)
import Control.Monad (mzero)
import System.Random (StdGen)
import Control.Applicative ((<$>))
import Rules.CardTable.TypesGenerators

data CardTableState = CTS
	{ hands :: [[Card]]
	, decks :: [[Card]]
	, tables :: [[Card]]
	, discards :: [[Card]]
	, tokens :: [(Token, Card)]
	, codexes :: [[[Card]]]
	, exhaustedCards :: [Card]
	, specs :: [[CardSpec]]
	, rng :: StdGen
	}
	
data CardZone
	= CZ CardZoneType PlayerIndex
	| CZGamelog
	| CZTokens
	deriving (Show, Eq, Ord)
	
data CardZoneType
	= CZHand
	| CZPlay
	| CZDeck
	| CZDiscard
	| CZCodex
	| CZCodexRow
	| CZCodexHolder
	| CZPlaymat
	deriving (Show, Eq, Ord, Enum)
	
instance ZoneId CardZone

instance ToJSON CardZone where
	toJSON (CZ CZHand idx) = String $ pack $ "hand-" ++ show idx
	toJSON (CZ CZPlay idx) = String $ pack $ "play-" ++ show idx
	toJSON (CZ CZDeck idx) = String $ pack $ "deck-" ++ show idx
	toJSON (CZ CZDiscard idx) = String $ pack $ "discard-" ++ show idx
	toJSON (CZ CZCodex idx) = String $ pack $ "codex-" ++ show idx
	toJSON (CZ CZCodexRow idx) = String $ pack $ "codexrow-" ++ show idx
	toJSON (CZ CZCodexHolder idx) = String $ pack $ "codexholder-" ++ show idx
	toJSON (CZ CZPlaymat idx) = String $ pack $ "playmat-" ++ show idx
	toJSON (CZGamelog) = String $ pack $ "gamelog"
	toJSON (CZTokens) = String $ pack $ "tokens"
	
instance FromJSON CardZone where
	parseJSON (String "tokens") = return $ CZTokens
	parseJSON (String "gamelog") = return $ CZGamelog
	parseJSON (String v) = 
		case (pfx, idx) of
			(_, Nothing) -> mzero
			("hand", Just index) -> return $ CZ CZHand index
			("play", Just index) -> return $ CZ CZPlay index
			("deck", Just index) -> return $ CZ CZDeck index
			("discard", Just index) -> return $ CZ CZDiscard index
			("codex", Just index) -> return $ CZ CZCodex index
			("playmat", Just index) -> return $ CZ CZPlaymat index
			("codexrow", Just index) -> return $ CZ CZCodexRow index
			("codexholder", Just index) -> return $ CZ CZCodexRow index
			(_, Just _) -> mzero
		  where
			idx = readMaybe $ unpack num
			(_, num) = breakOnEnd "-" rst
			(pfx, rst) = breakOn "-" v
	parseJSON _ = mzero
	
data CardIndex = CI Int deriving (Show, Eq)
data TokenIndex = TI Int deriving (Show, Eq)

data CardEntity
	= CECard Card
	| CEToken Token
	deriving (Eq, Show)

instance EntityId CardEntity

data CardSpec
	= NeutralSpec
	| Anarchy
	| Blood
	| Fire
	| Balance
	| Feral
	| Growth
	| Law
	| Peace
	| Truth
	| Past
	| Present
	| Future
	| Discipline
	| Ninjitsu
	| Strength
	| Disease
	| Necromancy
	| Demonology
	deriving (Eq, Show, Enum, Ord)
	
data CardColor
	= NeutralColor
	| Red
	| Green
	| Blue
	| Purple
	| White
	| Black
	deriving (Eq, Show, Ord)
	
data CardType
	= Hero
	| Spell_1
	| Spell_2
	| Spell_3
	| Spell_Ult
	| Tech_1_1
	| Tech_1_2
	| Tech_2_1
	| Tech_2_2
	| Tech_2_3
	| Tech_2_4
	| Tech_2_5
	| Tech_3
	| SpecToken
	deriving (Eq, Show, Enum, Ord)

data UtilityCardType
	= CodexHolder
	| Hero_1_Holder
	| Hero_2_Holder
	| Hero_3_Holder
	| Tech_3_1_Building
	| Tech_3_2_Building
	| Tech_3_3_Building
	| Tech_2_1_Building
	| Tech_2_2_Building
	| Tech_2_3_Building
	| SurplusBuilding
	| Tech_1_Building
	| TowerBuilding
	| BaseBuilding
	| BlankCard
	deriving (Eq, Show, Enum, Ord)
	
data SDCardType
	= SDCard0
	| SDCard1
	| SDCard2
	| SDCard3
	| SDCard4
	| SDCard5
	| SDCard6
	| SDCard7
	| SDCard8
	| SDCard9
	deriving (Eq, Show, Enum, Ord)
	
data TokenType
	= Gold
	| PlusOne
	| MinusOne
	| Time
	| Cooldown
	| Sword
	deriving (Eq, Show, Enum, Ord)
	
data Card = Card CardIdentifier CardIndex
	deriving (Show, Eq)
data CardIdentifier
	= CodexCard CardSpec CardType
	| UtilityCard UtilityCardType
	| SDCard CardColor SDCardType
	deriving (Show, Eq)
data Token = Token TokenType TokenIndex -- String for type? Int for 'index'. index 0 is the special global one.
	deriving (Show, Eq)

instance Ord Card where
	compare (CodexCard s t) (CodexCard s2 t2) = case compare s s2 of
		GT -> GT
		LT -> LT
		WQ -> compare t t2
	compare (CodexCard _ _) _ = GT
	compare (UtilityCard t) (UtilityCard t2) = compare t t2
	compare (UtilityCard _) (SDCard _ _) = GT
	compare (SDCard c t) (SDCard c2 t2) = case compare c c2 of
		GT -> GT
		LT -> LT
		EQ -> compare t t2
	
class Encodable a where
	encode :: a -> Text
	decode :: Text -> Maybe a
	
makeEncodable ''CardSpec
	[ ('NeutralSpec, "neutral")
	, ('Anarchy, "anarchy")
	, ('Blood, "blood")
	, ('Fire, "fire")
	, ('Balance, "balance")
	, ('Feral, "feral")
	, ('Growth, "growth")
	, ('Law, "law")
	, ('Peace, "peace")
	, ('Truth, "truth")
	, ('Past, "past")
	, ('Present, "present")
	, ('Future, "future")
	, ('Discipline, "discipline")
	, ('Ninjitsu, "ninjitsu")
	, ('Strength, "strength")
	, ('Disease, "disease")
	, ('Necromancy, "necromancy")
	, ('Demonology, "demonology")
	]
	
instance Encodable CardIndex where
	encode (CI i) = pack $ show i
	decode s = CI <$> (readMaybe $ unpack s)
	
instance Encodable TokenIndex where
	encode (TI i) = pack $ show i
	decode s = TI <$> (readMaybe $ unpack s)

makeEncodable ''CardType
	[ ('Hero, "hero")
	, ('Spell_1, "spell1")
	, ('Spell_2, "spell2")
	, ('Spell_3, "spell3")
	, ('Spell_Ult, "spellult")
	, ('Tech_1_1, "tech11")
	, ('Tech_1_2, "tech12")
	, ('Tech_2_1, "tech21")
	, ('Tech_2_2, "tech22")
	, ('Tech_2_3, "tech23")
	, ('Tech_2_4, "tech24")
	, ('Tech_2_5, "tech25")
	, ('Tech_3, "tech3")
	, ('SpecToken, "spec")
	]
	
makeEncodable ''SDCardType
	[ ('SDCard0, "sdcard0")
	, ('SDCard1, "sdcard1")
	, ('SDCard2, "sdcard2")
	, ('SDCard3, "sdcard3")
	, ('SDCard4, "sdcard4")
	, ('SDCard5, "sdcard5")
	, ('SDCard6, "sdcard6")
	, ('SDCard7, "sdcard7")
	, ('SDCard8, "sdcard8")
	, ('SDCard9, "sdcard9")
	]
	
makeEncodable ''CardColor
	[ ('NeutralColor, "neutral")
	, ('Red, "red")
	, ('Blue, "blue")
	, ('Green, "green")
	, ('Purple, "purple")
	, ('White, "white")
	, ('Black, "black")
	]
	
makeEncodable ''UtilityCardType
	[ ('CodexHolder, "codex")
	, ('Hero_1_Holder, "hero1")
	, ('Hero_2_Holder, "hero2")
	, ('Hero_3_Holder, "hero3")
	, ('Tech_3_1_Building, "tech31")
	, ('Tech_3_2_Building, "tech32")
	, ('Tech_3_3_Building, "tech33")
	, ('Tech_2_1_Building, "tech21")
	, ('Tech_2_2_Building, "tech22")
	, ('Tech_2_3_Building, "tech23")
	, ('SurplusBuilding, "surplus")
	, ('Tech_1_Building, "tech1")
	, ('TowerBuilding, "tower")
	, ('BaseBuilding, "base")
	, ('BlankCard, "blank")
	]
	
makeEncodable ''TokenType
	[ ('Gold, "gold")
	, ('PlusOne, "plus")
	, ('MinusOne, "minus")
	, ('Time, "time")
	, ('Cooldown, "cooldown")
	, ('Sword, "sword")
	]
	
instance ToJSON CardEntity where
	toJSON (CECard (Card (CodexCard spec rank) idx)) = String $ "codexcard-" `append` encode spec `append` "-" `append` encode rank `append` "-" `append` encode idx
	toJSON (CECard (Card (UtilityCard uct) idx)) = String $ "utility-" `append` encode uct `append` "-" `append` encode idx
	toJSON (CECard (Card (SDCard color sdtype) idx)) = String $ "sdcard-" `append` encode color `append` "-" `append` encode sdtype `append` "-" `append` encode idx
	toJSON (CEToken (Token setype idx)) = String $ "token-" `append` encode setype `append` "-" `append` encode idx
	
instance FromJSON CardEntity where
	parseJSON (String v) =
		case entityType of
			"codexcard" -> case (idx, decode rnk, decode suit) of
				(Just index, Just rank, Just suit') -> return $ CECard $ Card (CodexCard suit' rank) (CI index)
				_ -> mzero
			  where
				idx = readMaybe $ unpack num
				(rnk, _) = breakOn "-" $ Data.Text.drop 1 $ rest
				(_, num) = breakOnEnd "-" rest
				(suit, rest) = breakOn "-" $ Data.Text.drop 1 $ rst
			"sdcard" -> case (idx, decode rnk, decode suit) of
				(Just index, Just rank, Just suit') -> return $ CECard $ Card (SDCard suit' rank) (CI index)
				_ -> mzero
			  where
				idx = readMaybe $ unpack num
				(rnk, _) = breakOn "-" $ Data.Text.drop 1 $ rest
				(_, num) = breakOnEnd "-" rest
				(suit, rest) = breakOn "-" $ Data.Text.drop 1 $ rst
			"utility" -> case (idx, decode suit) of
				(Just index, Just suit') -> return $ CECard $ Card (UtilityCard suit') (CI index)
				_ -> mzero
			  where
				idx = readMaybe $ unpack num
				(_, num) = breakOnEnd "-" rest
				(suit, rest) = breakOn "-" $ Data.Text.drop 1 $ rst
			"token" -> case (idx, decode setype) of
				(Just index, Just setype') -> return $ CEToken $ Token setype' (TI index)
				_ -> mzero
			  where
				idx = readMaybe $ unpack num
				(_, num) = breakOnEnd "-" rest
				(setype, rest) = breakOn "-" $ Data.Text.drop 1 $ rst
			_ -> mzero
		  where			
			(entityType, rst) = breakOn "-" v
	parseJSON _ = mzero

	
instance GameState CardTableState
