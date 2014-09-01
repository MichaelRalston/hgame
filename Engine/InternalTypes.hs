{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Engine.InternalTypes
	( WithMemory (..)
	, useMemory
	, alloc
	, modifyMemory
	, usingMemory
	) where
	
import Control.Applicative (Applicative)
import Control.Concurrent (MVar, newMVar, modifyMVar, withMVar)
	
newtype WithMemory a = WM (IO a)
	deriving (Monad, Applicative, Functor)
	
useMemory :: WithMemory a -> IO a
useMemory (WM action) = action

alloc :: a -> WithMemory (MVar a)
alloc a = WM $ newMVar a

modifyMemory :: MVar a -> (a -> WithMemory (a, b)) -> WithMemory b
modifyMemory var fxn = WM $ modifyMVar var (useMemory . fxn)

usingMemory :: MVar a -> (a -> WithMemory b) -> WithMemory b
usingMemory var fxn = WM $ withMVar var (useMemory . fxn)