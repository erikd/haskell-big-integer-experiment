
import Data.Primitive (ByteArray)
import Numeric

import New.GHC.Integer.Array

main :: IO ()
main = do
    arr <- createWordArray
    indexWordArrayM arr 0 >>= hexPrint
    indexWordArrayM arr 1 >>= hexPrint
    indexWordArrayM arr 2 >>= hexPrint
    indexWordArrayM arr 3 >>= hexPrint

    indexHalfWordArrayM arr 0 >>= hexPrint
    indexHalfWordArrayM arr 1 >>= hexPrint
    indexHalfWordArrayM arr 2 >>= hexPrint
    indexHalfWordArrayM arr 3 >>= hexPrint

    hwarr <- createHalfWordArray
    indexHalfWordArrayM hwarr 0 >>= hexPrint
    indexHalfWordArrayM hwarr 1 >>= hexPrint
    indexHalfWordArrayM hwarr 2 >>= hexPrint
    indexHalfWordArrayM hwarr 3 >>= hexPrint


createWordArray :: IO ByteArray
createWordArray = do
	marr <- newWordArray 4
	writeWordArray marr 0 0x1111111122222222
	writeWordArray marr 1 0x3333333344444444
	writeWordArray marr 2 0x5555555566666666
	writeWordArray marr 3 0x7777777788888888
	unsafeFreezeWordArray marr

createHalfWordArray :: IO ByteArray
createHalfWordArray = do
	marr <- newHalfWordArray 4
	writeHalfWordArray marr 0 0x11111111
	writeHalfWordArray marr 1 0x22222222
	writeHalfWordArray marr 2 0x33333333
	writeHalfWordArray marr 3 0x44444444
	unsafeFreezeHalfWordArray marr

hexPrint :: (Integral a, Show a) => a -> IO ()
hexPrint x = putStrLn $ "0x" ++ showHex x ""
