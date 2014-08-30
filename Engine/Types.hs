{-# LANGUAGE FlexibleInstances, NamedFieldPuns, OverloadedStrings, ExistentialQuantification #-}

module Engine.Types
	( ZoneId
	, PlayerIndex
	, GameDelta
	, EntityId
	, ScreenDisplay (..)
	, ZoneDisplay (..)
	, GameState
	, ScreenEntity (..)
	, ScreenEntitySize (..)
	, Screen
	, InputHandler
	, Gamelog (..)
	, GamelogMessage (..)
	, UserInput (..)
	, toJSON
	, Game (..)
	) where
	
import Data.Map (Map, assocs)
import System.Time (TimeDiff)
--import System.Random (StdGen)
import Control.Concurrent.MVar (MVar)
import Control.Applicative ((<*>), (<$>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON, ToJSON, toJSON, object, Value(..), (.=), (.:), parseJSON)

data Game = forall state entity zone. (GameState state, EntityId entity, ZoneId zone) => Game
	{ playerRenderer :: state -> PlayerIndex -> Screen zone entity
--	, newGame :: [PlayerIndex] -> StdGen -> Maybe a -- TODO: reevaluate. Maybe this needs to go?
	, tick :: state -> TimeDiff -> GameDelta state entity zone -- TODO: nuke this.
-- TODO: replace 'tick' with "player time remaining", fabricate "UITimeout" events.
	, handleInput :: InputHandler state entity zone
	, getPlayers :: state -> [PlayerIndex] -- TODO: reevaluate. 
	, state :: MVar state
--	, parseEntity :: ByteString -> Maybe b
	, finished :: state -> Bool
	}
	
type GameDelta state entity zone = (state, [Gamelog entity zone])
type Screen zone entity = Map (zone) (ZoneDisplay zone entity, [ScreenEntity entity])
type InputHandler state entity zone = state -> PlayerIndex -> UserInput entity zone -> GameDelta state entity zone

class (Eq entity, FromJSON entity, ToJSON entity, Show entity) => EntityId entity
class (Eq zone, FromJSON zone, ToJSON zone, Show zone) => ZoneId zone
class GameState state

-- This is just an entry into a big lookup table.
type PlayerIndex = Int -- TODO: verify that this is int.

data Gamelog entity zone
    = GLBroadcast [GamelogMessage entity zone]
    | GLPrivate
        { glPlayer :: [PlayerIndex]
        , content :: [GamelogMessage entity zone]
        }
    | GLAllBut
        { glPlayerList :: [PlayerIndex]
        , content :: [GamelogMessage entity zone]
        }
	deriving Show

data UserInput entity zone
	= UIClick entity
	| UIDrag entity zone	
	| UIText entity String
	deriving Show

data GamelogMessage entity zone
	= GLMDisplay String zone
	| GLMMove [entity] zone -- TODO: maybe include a way to say a 'source zone', for facedown->faceups?
    | GLMPlayerAction PlayerIndex String zone
    | GLMTwoPlayerAction PlayerIndex String PlayerIndex zone -- "first did something to second".
	deriving Show

data ScreenDisplay
	= SDImage String
	| SDText String	
	| SDTextInput
	deriving Show

data ZoneDisplay zone entity
	= ZDRight Int -- Float on the right, X% wide.
	| ZDHorizFill Int -- 100% wide, X% tall
	| ZDNested (ZoneDisplay zone entity) zone
	| ZDShelf entity -- Hidden by default, but the entity whose ID is entity shows it.
	| ZDDialog -- dialog!
	deriving Show

data ScreenEntitySize
	= SESPercent Int Int -- height% width%
	deriving Show
	
data ScreenEntity entity = SE
	{ eId :: entity
	, eDisplay :: ScreenDisplay
	, eSize :: ScreenEntitySize
	-- TODO: Visual modifiers?
	, eActive :: Bool
	}
	deriving Show

instance (EntityId entity, ZoneId zone) => FromJSON (UserInput entity zone) where
	parseJSON (Object v) = do
		action <- v .: "action"
		case (action :: String) of
			"click" -> UIClick <$> v .: "entity"
			"drag" -> UIDrag <$> v .: "entity" <*> v .: "zone"
			"text" -> UIText <$> v .: "entity" <*> v .: "text"
			_ -> fail "unknown action"
	parseJSON _ = mzero
	
instance ToJSON ScreenDisplay where
	toJSON (SDImage u) = object ["type" .= String "image", "uri" .= toJSON u]
	toJSON (SDText t) = object ["type" .= String "text", "text" .= toJSON t]
	toJSON (SDTextInput) = object ["type" .= String "textInput"]

instance (EntityId entity, ZoneId zone) => ToJSON (ZoneDisplay zone entity) where
	toJSON (ZDRight i) = object ["type" .= String "floatRight", "width" .= i]
	toJSON (ZDHorizFill i) = object ["type" .= String "horizFill", "height" .= i]
	toJSON (ZDNested nested zoneId) = object ["type" .= String "nested", "display" .= nested, "zone" .= zoneId]
	toJSON (ZDShelf entityId) = object ["type" .= String "shelf", "entity" .= entityId]
	toJSON ZDDialog = object ["type" .= String "dialog"]

instance ToJSON (ScreenEntitySize) where
	toJSON (SESPercent h w) = object ["type" .= String "percent", "height" .= h, "width" .= w]
	
instance EntityId entity => ToJSON (ScreenEntity entity) where
	toJSON SE {eId, eDisplay, eSize, eActive} = object ["entityId" .= eId, "display" .= eDisplay, "active" .= eActive, "size" .= eSize]

instance (EntityId entity, ZoneId zone) => ToJSON (Screen zone entity) where
	toJSON screen = toJSON $ assocs screen

instance (EntityId entity, ZoneId zone) => ToJSON (ZoneDisplay zone entity, [ScreenEntity entity]) where
	toJSON (zd, ses) = object ["display" .= zd, "entities" .= ses]
