{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Rules.CardTable.TypesGenerators where

import Language.Haskell.TH

encodings :: [(Name, String)] -> Q [Dec]
encodings pairs = return [ FunD (mkName "encode") [(Clause [ConP typeName []] (NormalB $ LitE $ StringL string) []) | (typeName, string) <- pairs ] ]
	
decodings :: [(Name, String)] -> Q [Dec]
decodings pairs = return
	[ FunD (mkName "decode") 
		( [(Clause [LitP (StringL string)] (NormalB $ AppE (ConE $ mkName "Just") (ConE typeName)) []) | (typeName, string) <- pairs ]
		++ [(Clause [WildP] (NormalB $ ConE $ mkName "Nothing") [])]
		)
	]

makeEncodable :: Name -> [(Name, String)] -> Q [Dec]
makeEncodable className pairs = do
	enc <- encodings pairs
	dec <- decodings pairs
	return $ [InstanceD [] (AppT (ConT (mkName "Encodable")) $ ConT className) $ enc ++ dec]
