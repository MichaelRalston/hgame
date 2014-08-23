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
	, GamelogMessage
	, toJSON
	, Game (..)
	) where
	
import Data.Map (Map)
import System.Time (TimeDiff)
import System.Random (StdGen)
import Data.ByteString (ByteString)
import Control.Concurrent.MVar (MVar)
import Data.Aeson (FromJSON, ToJSON, toJSON, object, Value(..), (.=), encode)

data Game = forall a b c. (GameState a, EntityId b, ZoneId c) => Game
	{ playerRenderer :: a -> PlayerIndex -> Screen c b
	, newGame :: [PlayerIndex] -> StdGen -> Maybe a -- TODO: reevaluate. Maybe this needs to go?
	, tick :: a -> TimeDiff -> GameDelta a b c
	, handleInput :: InputHandler a b c
	, getPlayers :: a -> [PlayerIndex] -- TODO: reevaluate. 
	, state :: MVar a
	, parseEntity :: ByteString -> Maybe b
	, finished :: a -> Bool
	}
	
type GameDelta a b c = (a, [Gamelog b c])
type Screen a b = Map (a) (ZoneDisplay, [ScreenEntity b])
type InputHandler a b c = a -> PlayerIndex -> b -> GameDelta a b c

class (Eq a, FromJSON a, ToJSON a) => EntityId a
class (Eq a, ToJSON a) => ZoneId a
class GameState a

-- This is just an entry into a big lookup table.
type PlayerIndex = Int -- TODO: verify that this is int.

data Gamelog a b
    = GLBroadcast [GamelogMessage a b]
    | GLPrivate
        { glPlayer :: [PlayerIndex]
        , content :: [GamelogMessage a b]
        }
    | GLAllBut
        { glPlayerList :: [PlayerIndex]
        , content :: [GamelogMessage a b]
        }
             
data GamelogMessage a b -- where a is EntityId, b is ZoneId
	= GLMDisplay String
	| GLMMove [a] b
    | GLMPlayerAction PlayerIndex String
    | GLMTwoPlayerAction PlayerIndex String PlayerIndex -- "first did something to second".

data ScreenDisplay
	= SDImage String
	| SDText String	

data ZoneDisplay
	= ZDGuess -- "guess where it goes"; to be extended with something actually useful.

data ScreenEntity a = SE
	{ eId :: a
	, eDisplay :: ScreenDisplay
	, eActive :: Bool
	}

instance ToJSON ScreenDisplay where
	toJSON (SDImage u) = object ["uri" .= toJSON u]

instance ToJSON ZoneDisplay where
	toJSON ZDGuess = String "guess"
	
instance EntityId a => ToJSON (ScreenEntity a) where
	toJSON SE {eId, eDisplay, eActive} = object ["entityId" .= toJSON eId, "display" .= toJSON eDisplay, "active" .= toJSON eActive]

instance (EntityId a, ZoneId b) => ToJSON (GamelogMessage a b) where
	toJSON (GLMDisplay str) = object ["display" .= toJSON str]
	toJSON (GLMMove entities zone) = object ["move" .= object ["entities" .= toJSON (map toJSON entities), "zone" .= toJSON zone]]
	
instance (EntityId b, ZoneId a) => ToJSON (Screen a b) where
	toJSON = toJSON

instance EntityId a => ToJSON (ZoneDisplay, [ScreenEntity a]) where
	toJSON (zd, ses) = object ["display" .= toJSON zd, "entities" .= toJSON ses]