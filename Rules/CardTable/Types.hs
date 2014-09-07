{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Rules.CardTable.Types
	( CardZone(..)
	, CardTableState (..)
	, CardEntity (..)
	, CardZoneType (..)
	) where
	
import Engine.Types
import Data.Aeson (ToJSON, FromJSON, Value(..), parseJSON)
import Data.Text (pack, unpack, breakOn, breakOnEnd)
import Text.Read (readMaybe)
import Control.Monad (mzero)
import System.Random (StdGen)

import Debug.Trace
traceSs str showable = trace (str ++ " " ++ (show showable)) showable

data CardTableState = CTS
	{ hands :: [[CardEntity]]
	, decks :: [[CardEntity]]
	, tables :: [[CardEntity]]
	, discards :: [[CardEntity]]
	, rng :: StdGen
	}
	
data CardZone
	= CZ CardZoneType PlayerIndex
	| CZGamelog
	deriving (Show, Eq, Ord)
	
data CardZoneType
	= CZHand
	| CZPlay
	| CZDeck
	| CZDiscard
	deriving (Show, Eq, Ord, Enum)
	
instance ZoneId CardZone

instance ToJSON CardZone where
	toJSON (CZ CZHand idx) = String $ pack $ "hand-" ++ show idx
	toJSON (CZ CZPlay idx) = String $ pack $ "play-" ++ show idx
	toJSON (CZ CZDeck idx) = String $ pack $ "deck-" ++ show idx
	toJSON (CZ CZDiscard idx) = String $ pack $ "discard-" ++ show idx
	toJSON (CZGamelog) = String $ pack $ "gamelog"
	
instance FromJSON CardZone where
	parseJSON (String v) = 
		case (pfx, idx) of
			(_, Nothing) -> mzero
			("hand", Just index) -> return $ CZ CZHand index
			("play", Just index) -> return $ CZ CZPlay index
			("deck", Just index) -> return $ CZ CZDeck index
			("discard", Just index) -> return $ CZ CZDiscard index
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
				rnk = traceSs "readMaybe of rnk" $ readMaybe $ unpack num
				(_, num) = traceSs "break of rest" $ breakOnEnd "-" rest
				(suit, rest) = traceSs "break of rst" $ breakOn "-" $ drop 1 rst
			_ -> mzero
		  where			
			(entityType, rst) = traceSs "break of v" $ breakOn "-" v
	parseJSON _ = mzero
	
instance GameState CardTableState