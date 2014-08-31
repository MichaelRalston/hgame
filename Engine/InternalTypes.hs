{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Engine.InternalTypes
	( WithMemory (..)
	, useMemory
	, alloc
	) where
	
import Control.Applicative (Applicative)
import Control.Concurrent (MVar, newMVar)
	
newtype WithMemory a = WM (IO a)
	deriving (Monad, Applicative, Functor)
	
useMemory :: WithMemory a -> IO a
useMemory (WM action) = action

alloc :: a -> WithMemory (MVar a)
alloc a = WM $ newMVar a