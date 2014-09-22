{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Rules.CardTable.Types
	( CardZone(..)
	, CardTableState (..)
	, CardEntity (..)
	, CardZoneType (..)
	, Card (..)
	, Token (..)
	, TokenIndex (..)
	, CardIndex (..)
	) where
	
import Engine.Types
import Data.Aeson (ToJSON, FromJSON, Value(..), parseJSON)
import Data.Text (pack, unpack, breakOn, breakOnEnd, drop)
import Text.Read (readMaybe)
import Control.Monad (mzero)
import System.Random (StdGen)

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
	
data Card = Card String CardIndex -- suit, "rank"
	deriving (Show, Eq)
data Token = Token String TokenIndex -- String for type? Int for 'index'. index 0 is the special global one.
	deriving (Show, Eq)
	
instance ToJSON CardEntity where
	toJSON (CECard (Card suit rank)) = String $ pack $ "card-" ++ suit ++ "-" ++ show rank
	toJSON (CEToken (Token setype idx)) = String $ pack $ "token-" ++ setype ++ "-" ++ show idx
	
instance FromJSON CardEntity where
	parseJSON (String v) =
		case entityType of
			"card" -> case rnk of
				Just rank -> return $ CECard $ Card (unpack suit) (CI rank)
				Nothing -> mzero
			  where
				rnk = readMaybe $ unpack num
				(_, num) = breakOnEnd "-" rest
				(suit, rest) = breakOn "-" $ Data.Text.drop 1 $ rst
			_ -> mzero
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