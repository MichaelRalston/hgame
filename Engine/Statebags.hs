module Engine.Statebags
	( GameId
	, ConnectionId
	, ConnectionInfo (..)
	, GameData (..)
	, ConnectionMap
	, GameMap
	, ModMap
	, insert
	, delete
	, twiddle
	, makeModMap
	) where
	
import Engine.Types
import qualified Network.WebSockets as WS
import qualified Data.Map as Map
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar (modifyMVar, modifyMVar_, readMVar, newMVar)
import Data.Traversable (forM)
import Control.Monad.State.Lazy (StateT)
import Control.Monad.Trans (lift)
import Data.IORef (newIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)

data GameId = GameId Int
data ConnectionId = ConnectionId Int

type ConnectionMap = ModMap ConnectionId ConnectionInfo
type GameMap = ModMap GameId GameData

class Keyable a where 
	toInt :: a -> Int
	fromInt :: Int -> a
	
instance Keyable ConnectionId where
	toInt (ConnectionId i) = i
	fromInt i = ConnectionId i

instance Keyable GameId where
	toInt (GameId i) = i
	fromInt i = GameId i

-- This is not to be held anywhere, only accessed via direct lookup functions.
data ConnectionInfo = ConnectionInfo
	{	displayName :: String
	,   connection :: WS.Connection
	,   gameData :: (GameId, PlayerIndex)
	}
	
data GameData = GameData
	{	game :: Game
	,	players :: Map.Map PlayerIndex ConnectionId
	}

data ModMap k a = ModMap 
	{	nextKey :: MVar k
	,   internalMap :: MVar (Map.Map Int a)
	}

insert :: Keyable k => a -> ModMap k a -> IO k
insert elem theMap = do
	key <- modifyMVar (nextKey theMap) (\i -> return (fromInt $ toInt i+1, i))
	modifyMVar_ (internalMap theMap) (\map' -> return $ Map.insert (toInt key) elem map')
	return key
	
delete :: Keyable k => k -> ModMap k a -> IO ()
delete elem theMap = modifyMVar_ (internalMap theMap) (\map' -> return $ Map.delete (toInt elem) map')

twiddle :: Keyable k => k -> ModMap k a -> (a -> IO r) -> IO (Maybe r)
twiddle elem theMap twiddler = do
	map' <- readMVar (internalMap theMap)
	forM (Map.lookup (toInt elem) map') twiddler
	
makeModMap :: Keyable k => IO (ModMap k a)
makeModMap = do
	key <- newMVar $ fromInt 1
	map' <- newMVar Map.empty
	return $ ModMap key map'
