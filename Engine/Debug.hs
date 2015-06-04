module Engine.Debug
	( traceSs
	, traceS
	, trace
	) where

import Debug.Trace
traceSs :: Show a => [Char] -> a -> a
traceSs str showable = trace (str ++ " " ++ (show showable)) showable
traceS :: Show a => a -> a
traceS a = trace (show a) a
