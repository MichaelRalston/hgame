{-# LANGUAGE FlexibleInstances, NamedFieldPuns, OverloadedStrings, ExistentialQuantification #-}

module Engine.Types
	( ZoneId
	, PlayerIndex
	, GameDelta
	, EntityId
	, ScreenDisplay
	, ZoneDisplay
	, GameState
	, ScreenEntity
	, Screen
	, InputHandler
	, Gamelog (..)
	, GamelogMessage (..)
	, UserInput (..)
	, toJSON
	, Game (..)
	) where
	
import Data.Map (Map)
import System.Time (TimeDiff)
import System.Random (StdGen)
import Data.ByteString (ByteString)
import Control.Concurrent.MVar (MVar)
import Control.Applicative ((<*>), (<$>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON, ToJSON, toJSON, object, Value(..), (.=), (.:), encode, parseJSON)

data Game = forall state entity zone. (GameState state, EntityId entity, ZoneId zone) => Game
	{ playerRenderer :: state -> PlayerIndex -> Screen zone entity
--	, newGame :: [PlayerIndex] -> StdGen -> Maybe a -- TODO: reevaluate. Maybe this needs to go?
	, tick :: state -> TimeDiff -> GameDelta state entity zone
	, handleInput :: InputHandler state entity zone
	, getPlayers :: state -> [PlayerIndex] -- TODO: reevaluate. 
	, state :: MVar state
--	, parseEntity :: ByteString -> Maybe b
	, finished :: state -> Bool
	}
	
type GameDelta state entity zone = (state, [Gamelog entity zone])
type Screen zone entity = Map (zone) (ZoneDisplay, [ScreenEntity entity])
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
	
data GamelogMessage entity zone
	= GLMDisplay String
	| GLMMove [entity] zone
    | GLMPlayerAction PlayerIndex String
    | GLMTwoPlayerAction PlayerIndex String PlayerIndex -- "first did something to second".
	deriving Show

data ScreenDisplay
	= SDImage String
	| SDText String	
	deriving Show

data ZoneDisplay
	= ZDGuess -- "guess where it goes"; to be extended with something actually useful.
	deriving Show

data ScreenEntity entity = SE
	{ eId :: entity
	, eDisplay :: ScreenDisplay
	, eActive :: Bool
	}
	deriving Show

instance (EntityId entity, ZoneId zone) => FromJSON (UserInput entity zone) where
	parseJSON (Object v) = do
		action <- v .: "action"
		case (action :: String) of
			"click" -> UIClick <$> v .: "entity"
			"drag" -> UIDrag <$> v .: "entity" <*> v .: "zone"
			otherwise -> fail "unknown action"
	parseJSON _ = mzero
	
instance ToJSON ScreenDisplay where
	toJSON (SDImage u) = object ["uri" .= toJSON u]

instance ToJSON ZoneDisplay where
	toJSON ZDGuess = String "guess"
	
instance EntityId entity => ToJSON (ScreenEntity entity) where
	toJSON SE {eId, eDisplay, eActive} = object ["entityId" .= toJSON eId, "display" .= toJSON eDisplay, "active" .= toJSON eActive]

instance (EntityId entity, ZoneId zone) => ToJSON (GamelogMessage entity zone) where
	toJSON (GLMDisplay str) = object ["display" .= toJSON str]
	toJSON (GLMMove entities zone) = object ["move" .= object ["entities" .= toJSON (map toJSON entities), "zone" .= toJSON zone]]
	toJSON (GLMPlayerAction pid str) = object ["actor" .= pid, "string" .= str] -- TODO: shouldn't this be converting the pid to a name?
	toJSON (GLMTwoPlayerAction pid str target) = object ["actor" .= pid, "string" .= str, "target" .= target] -- TODO: shouldn't this be converting the pids to names?
	
instance (EntityId entity, ZoneId zone) => ToJSON (Screen zone entity) where
	toJSON = toJSON

instance EntityId entity => ToJSON (ZoneDisplay, [ScreenEntity entity]) where
	toJSON (zd, ses) = object ["display" .= toJSON zd, "entities" .= toJSON ses]