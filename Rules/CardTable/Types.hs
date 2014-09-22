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
	, UtilityCardType (..)
	, CardIdentifier (..)
	, SDCardType (..)
	, makeEncodable
	, runQ
	) where
	
import Engine.Types
import Data.Aeson (ToJSON, FromJSON, Value(..), parseJSON)
import Data.Text (pack, unpack, breakOn, breakOnEnd, drop, Text, append)
import Text.Read (readMaybe)
import Control.Monad (mzero)
import System.Random (StdGen)
import Control.Applicative ((<*>), (<$>))
import Rules.CardTable.TypesGenerators
import Language.Haskell.TH (runQ)

data CardTableState = CTS
	{ hands :: [[Card]]
	, decks :: [[Card]]
	, tables :: [[Card]]
	, discards :: [[Card]]
	, tokens :: [(Token, Card)]
	, codexes :: [[[Card]]]
	, exhaustedCards :: [Card]
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
	deriving (Eq, Show)
	
data CardColor
	= NeutralColor
	| Red
	deriving (Eq, Show)
	
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
	deriving (Eq, Show)

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
	deriving (Eq, Show)
	
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
	deriving (Eq, Show)
	
data Card = Card CardIdentifier CardIndex
	deriving (Show, Eq)
data CardIdentifier
	= CodexCard CardSpec CardType
	| UtilityCard UtilityCardType
	| SDCard CardColor SDCardType
	deriving (Show, Eq)
data Token = Token String TokenIndex -- String for type? Int for 'index'. index 0 is the special global one.
	deriving (Show, Eq)

class Encodable a where
	encode :: a -> Text
	decode :: Text -> Maybe a
	

instance Encodable CardSpec where
makeEncodable NeutralSpec "neutral"
--	encode NeutralSpec = "neutral"
--	decode "neutral" = Just NeutralSpec
	encode Anarchy = "anarchy"
	encode Blood = "blood"
	encode Fire = "fire"	
	-- decode _ = Nothing
	
instance Encodable CardIndex where
	encode (CI i) = pack $ show i
	decode s = CI <$> (readMaybe $ unpack s)
	
instance Encodable CardType where
	encode Hero = "hero"
	decode "hero" = Just Hero
	
instance Encodable SDCardType where
	encode SDCard0 = "sdcard0"
	decode "sdcard0" = Just SDCard0
	
instance Encodable CardColor where
	encode NeutralColor = "neutral"
	decode "neutral" = Just NeutralColor
	
instance Encodable UtilityCardType where
	encode CodexHolder = "codex"
	decode "codex" = Just CodexHolder
	
instance ToJSON CardEntity where
	toJSON (CECard (Card (CodexCard spec rank) idx)) = String $ "codexcard-" `append` encode spec `append` "-" `append` encode rank `append` "-" `append` encode idx
	toJSON (CECard (Card (UtilityCard uct) idx)) = String $ "utility-" `append` encode uct `append` "-" `append` encode idx
	toJSON (CECard (Card (SDCard color sdtype) idx)) = String $ "sdcard-" `append` encode color `append` "-" `append` encode sdtype `append` "-" `append` encode idx
	toJSON (CEToken (Token setype idx)) = String $ pack $ "token-" ++ setype ++ "-" ++ show idx
	
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
			"token" -> case idx of
				Just index -> return $ CEToken $ Token (unpack setype) (TI index)
				Nothing -> mzero
			  where
				idx = readMaybe $ unpack num
				(_, num) = breakOnEnd "-" rest
				(setype, rest) = breakOn "-" $ Data.Text.drop 1 $ rst
			_ -> mzero
		  where			
			(entityType, rst) = breakOn "-" v
	parseJSON _ = mzero

	
instance GameState CardTableState