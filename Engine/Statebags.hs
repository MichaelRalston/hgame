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
	, update
	, makeModMap
	) where
	
import Engine.Types
import Engine.InternalTypes
import qualified Network.WebSockets as WS
import qualified Data.Map as Map
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar (modifyMVar, modifyMVar_, readMVar, newMVar)
import Data.Traversable (forM)

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

insert :: Keyable k => a -> ModMap k a -> WithMemory k
insert e theMap = WM $ do
	key <- modifyMVar (nextKey theMap) (\i -> return (fromInt $ toInt i+1, i))
	modifyMVar_ (internalMap theMap) (\map' -> return $ Map.insert (toInt key) e map')
	return key
	
delete :: Keyable k => k -> ModMap k a -> WithMemory ()
delete e theMap = WM $ modifyMVar_ (internalMap theMap) (\map' -> return $ Map.delete (toInt e) map')

update :: Keyable k => k -> ModMap k a -> (a -> a) -> WithMemory ()
update e theMap updater = WM $ do
	let adjustment = Map.adjust updater (toInt e)
	modifyMVar_ (internalMap theMap) $ return . adjustment

twiddle :: Keyable k => k -> ModMap k a -> (a -> WithMemory r) -> WithMemory (Maybe r)
twiddle e theMap twiddler = do
	map' <- WM $ readMVar (internalMap theMap)
	forM (Map.lookup (toInt e) map') twiddler
	
makeModMap :: Keyable k => WithMemory (ModMap k a)
makeModMap = WM $ do
	key <- newMVar $ fromInt 1
	map' <- newMVar Map.empty
	return $ ModMap key map'
