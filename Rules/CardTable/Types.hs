{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Rules.CardTable.Types
	( CardZone(..)
	, CardTableState (..)
	, CardEntity (..)
	) where
	
import Engine.Types
import Data.Aeson (ToJSON, FromJSON, Value(..), parseJSON)
import Data.Text (pack, unpack, breakOn, breakOnEnd)
import Text.Read (readMaybe)
import Control.Monad (mzero)
import System.Random (StdGen)

data CardTableState = CTS
	{ hands :: [[CardEntity]]
	, decks :: [[CardEntity]]
	, tables :: [[CardEntity]]
	, discards :: [[CardEntity]]
	, rng :: StdGen
	}
	
data CardZone
	= CZHand PlayerIndex
	| CZPlay PlayerIndex
	| CZDeck PlayerIndex
	| CZDiscard PlayerIndex
	deriving (Show, Eq)
	
instance ZoneId CardZone

instance ToJSON CardZone where
	toJSON (CZHand idx) = String $ pack $ "hand-" ++ show idx
	toJSON (CZPlay idx) = String $ pack $ "play-" ++ show idx
	toJSON (CZDeck idx) = String $ pack $ "deck-" ++ show idx
	toJSON (CZDiscard idx) = String $ pack $ "discard-" ++ show idx
	
instance FromJSON CardZone where
	parseJSON (String v) = 
		case (pfx, idx) of
			(_, Nothing) -> mzero
			("hand", Just index) -> return $ CZHand index
			("play", Just index) -> return $ CZPlay index
			("deck", Just index) -> return $ CZDeck index
			("discard", Just index) -> return $ CZDiscard index
			(_, Just _) -> mzero
		  where
			idx = readMaybe $ unpack num
			(_, num) = breakOnEnd "-" rst
			(pfx, rst) = breakOn "-" v
	parseJSON _ = mzero
	
data CardEntity
	= CECard String Int -- suit, "rank"
	deriving (Show, Eq)
	
instance EntityId CardEntity
	
instance ToJSON CardEntity where
	toJSON (CECard suit rank) = String $ pack $ "card-" ++ suit ++ "-" ++ show rank
	
instance FromJSON CardEntity where
	parseJSON (String v) =
		case entityType of
			"card" -> case rnk of
				Just rank -> return $ CECard (unpack suit) rank
				Nothing -> mzero
			  where
				rnk = readMaybe $ unpack num
				(_, num) = breakOnEnd "-" rest
				(suit, rest) = breakOn "-" rst
			_ -> mzero
		  where			
			(entityType, rst) = breakOn "-" v
	parseJSON _ = mzero
	
instance GameState CardTableState