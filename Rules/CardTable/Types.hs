{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Rules.CardTable.Types
	( CardZone(..)
	, CardTableState (..)
	, CardEntity (..)
	, CardZoneType (..)
	) where
	
import Engine.Types
import Data.Aeson (ToJSON, FromJSON, Value(..), parseJSON)
import Data.Text (pack, unpack, breakOn, breakOnEnd, drop)
import Text.Read (readMaybe)
import Control.Monad (mzero)
import System.Random (StdGen)

data CardTableState = CTS
	{ hands :: [[CardEntity]]
	, decks :: [[CardEntity]]
	, tables :: [[CardEntity]]
	, discards :: [[CardEntity]]
	, subEntities :: [(CardEntity, CardEntity)] -- subentity first, then what it's a subentity of.
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
	deriving (Show, Eq, Ord, Enum)
	
instance ZoneId CardZone

instance ToJSON CardZone where
	toJSON (CZ CZHand idx) = String $ pack $ "hand-" ++ show idx
	toJSON (CZ CZPlay idx) = String $ pack $ "play-" ++ show idx
	toJSON (CZ CZDeck idx) = String $ pack $ "deck-" ++ show idx
	toJSON (CZ CZDiscard idx) = String $ pack $ "discard-" ++ show idx
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
			(_, Just _) -> mzero
		  where
			idx = readMaybe $ unpack num
			(_, num) = breakOnEnd "-" rst
			(pfx, rst) = breakOn "-" v
	parseJSON _ = mzero
	
data CardEntity
	= CECard String Int -- suit, "rank"
	| CESubEntity String Int -- String for type? Int for 'index'. index 0 is the special global one.
	deriving (Show, Eq)
	
instance EntityId CardEntity
	
instance ToJSON CardEntity where
	toJSON (CECard suit rank) = String $ pack $ "card-" ++ suit ++ "-" ++ show rank
	toJSON (CESubEntity setype idx) = String $ pack $ "subentity-" ++ setype ++ "-" ++ show idx
	
instance FromJSON CardEntity where
	parseJSON (String v) =
		case entityType of
			"card" -> case rnk of
				Just rank -> return $ CECard (unpack suit) rank
				Nothing -> mzero
			  where
				rnk = readMaybe $ unpack num
				(_, num) = breakOnEnd "-" rest
				(suit, rest) = breakOn "-" $ Data.Text.drop 1 $ rst
			"subentity" -> case idx of
				Just index -> return $ CESubEntity (unpack setype) index
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