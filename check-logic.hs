
import Data.Bits
import Numeric

main :: IO ()
main = mapM_ printResult tests


type LogicTest = (Integer, Integer, String)


tests :: [ LogicTest ]
tests =
    [ ( 0, 1, ".|." )
    , ( 0, -1, ".|." )
    , ( -1, 1, ".|." )
    , ( -1, -1, ".|." )


    , ( -1 * 0xa, 0xf, ".|." )
    , ( 0xf, -1 * 0xa, ".|." )
    , ( 0x3ff002, -1 * 0x3ff802, ".|." )

    , ( -1, -2, ".|." )


    , ( 0, 10, ".|." )
    , ( 0, -10, ".|." )

    , ( 3, 1, ".&." )
    , ( 3, -1, ".&." )
    , ( -1, 3, ".&." )

    , ( 3, 1, ".^." )
    , ( 3, -1, ".^." )
    , ( -1, 3, ".^." )
    ]


printResult :: LogicTest -> IO ()
printResult (a, b, op) =
    putStrLn $ hexShow a ++ " " ++ op ++ " " ++ hexShow b ++ " -> " ++ hexShow ((lookupOp op) a b)


hexShow :: Integer -> String
hexShow i
    | i < 0 = "-0x" ++ showHex (abs i) ""
    | otherwise = "+0x" ++ showHex i ""


lookupOp :: String -> Integer -> Integer -> Integer
lookupOp ".|." = (.|.)
lookupOp ".&." = (.&.)
lookupOp ".^." = xor
lookupOp x = error $ "lookupOp '" ++ x ++ "'."
