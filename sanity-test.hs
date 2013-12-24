
import Control.Monad (when)
import New.Integer ()
import qualified GMP.Integer as G
import qualified New.GHC.Integer.Type as N


main :: IO ()
main = do
    print $ (G.mkInteger False [0x7fffffff], N.mkInteger False [0x7fffffff])
    print $ (G.shiftLInteger (G.mkInteger False [0x7fffffff]) 1#, N.shiftLInteger (N.mkInteger False [0x7fffffff]) 1#)
    print $ (G.mkInteger False [-10], N.mkInteger False [-10])

    when False $ do
    putStrLn "Small"
    print $ (N.Small 0x1234)
    print $ (N.Small 0x12346789)

    putStrLn "\nmkInteger"
    print $ N.mkInteger True [1]
    print $ N.mkInteger True [0, 2]
    print $ N.mkInteger False [1]
    print $ N.negateInteger (N.mkInteger False [1])
    print $ N.mkInteger True [1000000000000000000]

    print $ N.mkInteger True [0x7fffffff]
    print $ N.mkInteger True [0x80000000]

    putStrLn "\nshiftLInteger"
    print $ N.shiftLInteger (N.mkInteger True [0]) 0#
    print $ N.shiftLInteger (N.mkInteger False [0]) 1#
    print $ N.shiftLInteger (N.mkInteger False [1]) 1#
    print $ N.shiftLInteger (N.mkInteger True [0x10000000]) 1#
    print $ N.shiftLInteger (N.mkInteger True [0x10000000]) 2#
    print $ N.shiftLInteger (N.mkInteger True [0x10000000]) 3#
    print $ N.shiftLInteger (N.mkInteger True [0x12345678]) 4#
    print $ N.shiftLInteger (N.mkInteger True [0x12345678]) 8#
    print $ N.shiftLInteger (N.mkInteger True [0x12345678]) 12#
    print $ N.shiftLInteger (N.mkInteger True [0x12345678]) 16#
    print $ N.shiftLInteger (N.mkInteger True [0x12345678]) 32#
    print $ N.shiftLInteger (N.mkInteger True [0x12345678]) 60#

    print $ N.shiftLInteger (N.mkInteger False [0x7fffffff00000001]) 1#
    print $ N.shiftLInteger (N.mkInteger False [0x7fffffff00000001]) 2#
    print $ N.shiftLInteger (N.mkInteger False [0x7fffffff00000001]) 4#

    putStrLn "\ntimesLInteger"
    printProduct (N.mkInteger True [1]) (N.Small 1)
    printProduct (N.mkInteger True [0x1001]) (N.Small 0x1001)
    printProduct (N.mkInteger True [0x100000001]) (N.Small 0x100000001)


printProduct :: N.Integer -> N.Integer -> IO ()
printProduct a b = putStrLn $
    show a ++ " * " ++ show b ++ " = " ++ show (N.timesInteger a b)
