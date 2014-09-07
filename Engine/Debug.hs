module Engine.Debug
	( traceSs
	, traceS
	, trace
	) where

import Debug.Trace
traceSs str showable = trace (str ++ " " ++ (show showable)) showable
traceS a = trace (show a) a
