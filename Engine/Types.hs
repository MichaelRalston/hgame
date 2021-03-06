{-# LANGUAGE FlexibleInstances, NamedFieldPuns, OverloadedStrings, ExistentialQuantification #-}

module Engine.Types
	( ZoneId
	, PlayerIndex
	, GameDelta
	, EntityId
	, GameMovement (..)
	, ScreenDisplay (..)
	, ZoneDisplay (..)
	, ZoneDisplayData (..)
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
	, GameId
	, WithMemory
	, alloc
	, useMemory
	, modifyMemory
	, usingMemory
	) where

import Data.Map (Map, assocs)
--import System.Time (TimeDiff)
--import System.Random (StdGen)
import Control.Concurrent.MVar (MVar)
import Control.Applicative ((<*>), (<$>))
import Control.Monad (mzero)
import Engine.InternalTypes

import Data.Aeson (FromJSON, ToJSON, toJSON, object, Value(..), (.=), (.:), parseJSON)

data Game = forall state entity zone. (GameState state, EntityId entity, ZoneId zone) => Game
	{ playerRenderer :: state -> PlayerIndex -> Screen zone entity
--	, newGame :: [PlayerIndex] -> StdGen -> Maybe a -- TODO: reevaluate. Maybe this needs to go?
	, handleInput :: InputHandler state entity zone
	, state :: MVar state

-- TODO: replace 'tick' with "player time remaining", fabricate "UITimeout" events.
	, getPlayers :: state -> [PlayerIndex] -- TODO: reevaluate.
--	, parseEntity :: ByteString -> Maybe b
	, finished :: state -> Bool
	}

type GameDelta state entity zone = (state, [Gamelog entity zone], [GameMovement])
type Screen zone entity = Map zone (ZoneDisplayData zone entity, [ScreenEntity entity])
type InputHandler state entity zone = state -> PlayerIndex -> UserInput entity zone -> WithMemory (GameDelta state entity zone)

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

data GameMovement
	= GMLeaveGame PlayerIndex
	| GMMoveGame PlayerIndex GameId PlayerIndex

data UserInput entity zone
	= UIClick entity
	| UIDrag entity zone
	| UIDragEntity entity entity Int Int -- dragee, target, x% y%
	| UIText entity String
	| UIDisconnected
	| UIConnected
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

data ZoneDisplayData zone entity = ZDD
	{ display :: ZoneDisplay zone entity
	, order :: Int
	, droppable :: Bool
	, classNames :: [String]
	}

data ZoneDisplay zone entity
	= ZDRight Int -- Float on the right, X% wide.
	| ZDLeft Int -- Float on the left, X% wide.
	| ZDHorizFill Int -- 100% wide, X% tall
	| ZDNested (ZoneDisplay zone entity) zone
	| ZDShelf entity -- Hidden by default, but the entity whose ID is entity shows it.
	| ZDDialog -- dialog!
	| ZDAbsolute Int Int -- absolutely positioned
	deriving Show

data ScreenEntitySize
	= SESPercent Int Int -- height% width%
	| SESAutoWidth Int -- width auto, height%.
	deriving Show

data ScreenEntity entity = SE
	{ eId :: entity
	, eDisplay :: ScreenDisplay
	, eSize :: ScreenEntitySize
	, ePosition :: Maybe (Int, Int)
	, eDropOnEntities :: Bool
	, eEntitiesDropOn :: Bool
	, eDraggable :: Bool
	, eClickable :: Bool
	, eClasses :: [String]
	, eNestOnEntity :: Maybe entity
	}
	deriving Show

instance (EntityId entity, ZoneId zone) => FromJSON (UserInput entity zone) where
	parseJSON (Object v) = do
		action <- v .: "action"
		case (action :: String) of
			"click" -> UIClick <$> v .: "entity"
			"drag" -> UIDrag <$> v .: "entity" <*> v .: "zone"
			"dragEntity" -> UIDragEntity <$> v .: "entity" <*> v .: "target" <*> v .: "leftOffset" <*> v .: "topOffset"
			"text" -> UIText <$> v .: "entity" <*> v .: "text"
			_ -> fail "unknown action"
	parseJSON _ = mzero

instance ToJSON ScreenDisplay where
	toJSON (SDImage u) = object ["type" .= String "image", "uri" .= toJSON u]
	toJSON (SDText t) = object ["type" .= String "text", "text" .= toJSON t]
	toJSON (SDTextInput) = object ["type" .= String "textInput"]

instance (EntityId entity, ZoneId zone) => ToJSON (ZoneDisplay zone entity) where
	toJSON (ZDAbsolute x y) = object ["type" .= String "absolute", "left" .= x, "top" .= y]
	toJSON (ZDRight i) = object ["type" .= String "floatRight", "width" .= i]
	toJSON (ZDLeft i) = object ["type" .= String "floatLeft", "width" .= i]
	toJSON (ZDHorizFill i) = object ["type" .= String "horizFill", "height" .= i]
	toJSON (ZDNested nested zoneId) = object ["type" .= String "nested", "display" .= nested, "zone" .= zoneId]
	toJSON (ZDShelf entityId) = object ["type" .= String "shelf", "entity" .= entityId]
	toJSON ZDDialog = object ["type" .= String "dialog"]

instance (EntityId entity, ZoneId zone) => ToJSON (ZoneDisplayData zone entity) where
	toJSON (ZDD {display, order, classNames, droppable}) = object ["display" .= display, "order" .= order, "classNames" .= classNames, "droppable" .= droppable]

instance ToJSON (ScreenEntitySize) where
	toJSON (SESPercent h w) = object ["type" .= String "percent", "height" .= h, "width" .= w]
	toJSON (SESAutoWidth h) = object ["type" .= String "autoWidth", "height" .= h]

instance EntityId entity => ToJSON (ScreenEntity entity) where
	toJSON SE {eId, eDisplay, eSize, eClickable, eDraggable, eDropOnEntities, eEntitiesDropOn, eClasses, eNestOnEntity, ePosition} =
		object
			["entityId" .= eId
			, "display" .= eDisplay
			, "clickable" .= eClickable
			, "draggable" .= eDraggable
			, "size" .= eSize
			, "dropOnEntities" .= eDropOnEntities
			, "entitiesDropOn" .= eEntitiesDropOn
			, "classes" .= eClasses
			, "nestedEntity" .= maybe (Bool False) toJSON eNestOnEntity
			, "position" .= maybe (Bool False) (\(l, t) -> object ["left" .= l, "top" .= t]) ePosition
			]

instance (EntityId entity, ZoneId zone) => ToJSON (Screen zone entity) where
	toJSON screen = toJSON $ map renderScreenAssocs $ assocs screen

renderScreenAssocs
	:: (ZoneId zone, EntityId entity)
	=> (zone, (ZoneDisplayData zone entity, [ScreenEntity entity]))
	-> Value
renderScreenAssocs (zid, (zd, ses)) = object ["zoneId" .= zid, "display" .= zd, "entities" .= ses]
