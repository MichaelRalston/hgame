{-# LANGUAGE TemplateHaskell #-}

module Rules.CardTable.TypesGeneratorsInternal where

import Language.Haskell.TH
import Control.Applicative ((<$>))

encodings :: [(Name, String)] -> Q [Dec]
encodings pairs = concat <$> sequence
	[
		[d|encode typeName = string |]
		| (typeName, string) <- pairs
	]
	
decodings :: [(Name, String)] -> Q [Dec]
decodings pairs = do
	l1 <- sequence decs
	l2 <- [d|decode _ = Nothing |]
	return ((concat l1) ++ l2)
  where
	decs :: [Q [Dec]]
	decs = [ [d|decode $(litP $ stringL string) = $(conE typeName) |] | (typeName, string) <- pairs ]
	