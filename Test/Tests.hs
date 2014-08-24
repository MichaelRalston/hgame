{-# Language OverloadedStrings #-}
module Test.Tests
	( test
	) where

import Data.Aeson (decode', Value, encode, object, (.=))
import Network.WebSockets (DataMessage (..))
import Control.Concurrent.MVar (modifyMVar, withMVar, newMVar)
import Control.Monad (void)
import Control.Concurrent (forkIO)

test = forkIO $ void $ do
	a <- newMVar 5
	r <- modifyMVar a (\a' ->
		return $ (a'+1, Network.WebSockets.Text $ encode $ object ["screen" .= (a' :: Int), "thing" .= (6 :: Int)]))
	putStrLn $ show r

