{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Rules.CardTable.TypesGenerators where

import Language.Haskell.TH
import Rules.CardTable.TypesGeneratorsInternal

makeEncodable :: Name -> [(Name, String)] -> Q [Dec]
makeEncodable className pairs = [d|instance Encodable $(varP className) where $(encodings pairs) $(decodings pairs) |]
