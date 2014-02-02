
import qualified Criterion.Main as C

import New3.GHC.Integer.Internals
import New3.Integer ()

import Check.Helpers


main :: IO ()
main = do
    list <- fmap (map (mkNatural . snd)) $ mkLargeIntegerList 20 (10, 10)

    C.defaultMain
        [ C.bgroup
                ( "Product of " ++ show (length list)
                    ++ " * 10 Word Natural numbers"
                )
            [ C.bench "Old" $ C.whnf (foldl1 timesNatural) list
            , C.bench "New" $ C.whnf (foldl1 timesNaturalNew) list
            , C.bench "Newest" $ C.whnf (foldl1 timesNaturalNewest) list
            ]
        ]
