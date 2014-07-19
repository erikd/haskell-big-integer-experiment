{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples #-}

module Common.GHC.Integer.Debug where

import Data.Word
import qualified Debug.Trace as Debug
import qualified Numeric

import Common.GHC.Integer.StrictPrim

hexShowW :: Word -> String
hexShowW w = "0x" ++ Numeric.showHex w ""

debugPrint :: Int -> String -> StrictPrim s ()
debugPrint line s = trace (show line ++ " : " ++ s) $ return ()

wibbleInt :: Int
wibbleInt = 1

showHexW :: Word -> String
showHexW x = Numeric.showHex x ""

showHexI :: Int -> String
showHexI x = Numeric.showHex x ""

errorLine :: Int -> String -> a
errorLine linenum s = error $ "Line " ++ show linenum ++ ": " ++ s

trace :: String -> a -> a
trace = Debug.trace
