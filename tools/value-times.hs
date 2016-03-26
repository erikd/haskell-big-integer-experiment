{-# LANGUAGE StrictData #-}

import Control.Monad
import Data.List
import Data.Map (Map)
import System.Exit ( exitFailure )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


data Source
    = Source Word Char
    deriving (Eq, Ord, Show)


data ValueType
    = Product
    | ProdCarry
    | Sum
    | SumCarry
    deriving (Eq, Ord, Show)


data Value
    = Value ValueType Word Int
    deriving (Eq, Ord, Show)

vType :: Value -> ValueType
vType (Value t _ _) = t

vIndex :: Value -> Word
vIndex (Value _ w _) = w

vName :: Value -> Int
vName (Value _ _ n) = n


data Operation
    = LoadSource Source
    | TimesWord2 (Source, Source) (Value, Value)
    | PlusWord2 (Value, Value) (Value, Value)
    | PlusWord (Value, Value) Value
    | StoreValue Value
    deriving Show


data Times = Times
    { ops :: [Operation]
    , vals :: Map Word [Value]
    , names :: Map Word [Int]
    }
    deriving Show


printTimes :: Times -> IO ()
printTimes times = do
    putStrLn "-------------------------------------------------------------------------"
    mapM_ print $ ops times
    putStrLn "-------------------------------------------------------------------------"
    mapM_ (\ (k, v) -> if null v then return () else putStrLn $ show k ++ " : " ++ show v) . Map.toList $ vals times
    putStrLn "-------------------------------------------------------------------------"


timesEmpty :: Times
timesEmpty = Times [] Map.empty Map.empty


insertResults :: Map Word [Value] -> Operation -> Map Word [Value]
insertResults vmap op =
    case op of
        TimesWord2 _ (v1, v2) -> addResult (++) v1 v2
        PlusWord2 _ (v1, v2) -> addResult appender v1 v2
        PlusWord _ v -> Map.insertWith appender (vIndex v) [v] vmap
        _ -> error $ "insertResults " ++ show op
  where
    appender xs ys = ys ++ xs
    addResult apf v1 v2 =
        Map.insertWith apf (vIndex v1) [v1]
            $ Map.insertWith apf (vIndex v2) [v2] vmap


insertNames :: Map Word [Int] -> Operation -> Map Word [Int]
insertNames vmap op =
    case op of
        TimesWord2 _ (v1, v2) -> addNames v1 v2
        PlusWord2 _ (v1, v2) -> addNames v1 v2
        PlusWord _ v -> Map.insertWith (++) (vIndex v) [vName v] vmap
        _ -> error $ "insertNames " ++ show op
  where
    addNames v1 v2 =
        Map.insertWith (++) (vIndex v1) [vName v1]
            $ Map.insertWith (++) (vIndex v2) [vName v2] vmap



fixName :: Times -> Operation -> Operation
fixName times op =
    case op of
        TimesWord2 x (v1, v2) -> TimesWord2 x (fixValName v1, fixValName v2)
        PlusWord2 x (v1, v2) -> PlusWord2 x (fixValName v1, fixValName v2)
        PlusWord x v -> PlusWord x (fixValName v)
        _ -> error $ "fixName " ++ show op
  where
    newName :: ValueType -> Word -> [Int] -> Value
    newName t i xs =
        case xs of
            [] -> Value t i 0
            _ -> Value t i $ succ (maximum xs)
    fixValName :: Value -> Value
    fixValName (Value t i 0) =
        maybe (Value t i 0) (newName t i) $ Map.lookup i (names times)
    fixValName v = v


appendOp :: Operation -> Times -> Times
appendOp origOp times =
    let op = fixName times origOp
    in times
        { ops = ops times ++ [op]
        , vals = insertResults (vals times) op
        , names = insertNames (names times) op
        }


insertOp :: Operation -> Times -> Times
insertOp origOp times =
    let op = fixName times origOp
    in times
        { ops = op : ops times
        , vals = insertResults (vals times) op
        }


initializeProducts :: Word -> Word -> Times
initializeProducts left right =
    insertLoads $ foldl' generate timesEmpty prodIndices
  where
    generate times (x, y) =
        let idx = x + y
            prod = Value Product idx 0
            carry = Value ProdCarry (idx + 1) 0
        in appendOp (TimesWord2 (Source x 'x', Source y 'y') (carry, prod)) times

    prodIndices :: [(Word, Word)]
    prodIndices =
        let ys = [0 .. left - 1]
            compf (a, b) (c, d) =
                case compare (a + b) (c + d) of
                    EQ -> compare a b
                    cx -> cx
        in sortBy compf $ concatMap (zip ys . replicate (fromIntegral left)) [0 .. right - 1]


insertLoads :: Times -> Times
insertLoads times =
    times { ops = insert Set.empty Set.empty (ops times) }
  where
    insert xload yload (op@(TimesWord2 (x, y) _):ops) =
        case (Set.member x xload, Set.member y yload) of
            (True, True) -> op : insert xload yload ops
            (True, False) -> LoadSource y : op : insert xload (Set.insert y yload) ops
            (False, True) -> LoadSource x : op : insert (Set.insert x xload) yload ops
            (False, False) -> LoadSource x : LoadSource y : op : insert (Set.insert x xload) (Set.insert y yload) ops
    insert xload yload (op:ops) = op : insert xload yload ops
    insert _ _ [] = []


insertSums :: Times -> Times
insertSums times =
    foldl' (flip insertIndexSums) times [ 0 .. (2 + maximum (Map.keys (vals times))) ]
  where
    getIndexVals :: Word -> Times -> (Times, [Value])
    getIndexVals i times =
        let vmap = vals times
            getVals values =
                case splitAt 2 values of
                    ([a], []) -> (times { vals = Map.insert i [] vmap } , [a])
                    ([a, b], rest) -> (times { vals = Map.insert i rest vmap } , [a, b])
                    _ -> (times, [])
        in maybe (times, []) getVals $ Map.lookup i vmap

    insertIndexSums :: Word -> Times -> Times
    insertIndexSums index times =
        case getIndexVals index times of
            (newtimes, [a, b]) -> insertIndexSums index $ appendOp (makeSum a b) newtimes
            (newtimes, [a]) -> newtimes { ops = ops newtimes ++ [StoreValue a] }
            (_, _) -> times


makeSum :: Value -> Value -> Operation
makeSum v1 v2 =
    case (v1, v2) of
        -- Sums that can produce a carry.
        (Value Product i _, Value Product _ _) ->
            PlusWord2 (v1, v2) (Value SumCarry (i + 1) 0, Value Sum i 0)
        (Value ProdCarry i _, Value Sum _ _) ->
            PlusWord2 (v1, v2) (Value SumCarry (i + 1) 0, Value Sum i 0)
        (Value Product i _, Value ProdCarry _ _) ->
            PlusWord2 (v1, v2) (Value SumCarry (i + 1) 0, Value Sum i 0)
        (Value SumCarry i _, Value Sum _ _) ->
            PlusWord2 (v1, v2) (Value SumCarry (i + 1) 0, Value Sum i 0)
        (Value Sum i _, Value Sum _ _) ->
            PlusWord2 (v1, v2) (Value SumCarry (i + 1) 0, Value Sum i 0)
        (Value ProdCarry i _, Value ProdCarry _ _) ->
            PlusWord2 (v1, v2) (Value SumCarry (i + 1) 0, Value Sum i 0)

        -- Sums that *will not* produce a carry.
        (Value ProdCarry i _, Value SumCarry _ _) ->
            PlusWord (v1, v2) (Value Sum i 0)
        (Value SumCarry i _, Value SumCarry _ _) ->
            PlusWord (v1, v2) (Value Sum i 0)

        x -> error $ "makeSum " ++ show x


validateValueUsage :: Times -> IO ()
validateValueUsage times = do
    results <- sequence
                [ valuesAreEmpty
                , valuesAreUniqueAndUsedOnce $ extractValues (ops times)
                ]
    when (or results) $ do
        putStrLn "Terminating"
        exitFailure
    putStrLn "Looks good to me!"
  where
    valuesAreEmpty =
        let elems = concat . Map.elems $ vals times
        in if null elems
            then return False
            else do
                putStrLn $ "validateValueUsage found unused values : " ++ show elems
                return True

    valuesAreUniqueAndUsedOnce (outvals, invals) = do
        let out_ok = length (nub outvals) == length outvals
            in_ok = length (nub invals) == length invals
            unused_in = filter (\ i -> i `notElem` outvals) invals
            unused_out = filter (\ i -> i `notElem` invals) outvals

        when (not in_ok) $
            putStrLn $ "validateValueUsage found duplicate inputs : " ++ show invals ++ "\n"
        when (not out_ok) $
            putStrLn $ "validateValueUsage found duplicate outputs : " ++ show outvals ++ "\n"

        when (not $ null unused_in) $
            putStrLn $ "validateValueUsage unused inputs : " ++ show unused_in ++ "\n"
        when (not $ null unused_out) $
            putStrLn $ "validateValueUsage unused outputs : " ++ show unused_out ++ "\n"

        return (in_ok && out_ok && not (null unused_in) && not (null unused_out))


extractValues :: [Operation] -> ([Value], [Value])
extractValues =
    sortLR . foldl' extract ([], [])
  where
    sortLR (a, b) = (sort a, sort b)
    extract (ins, outs) op =
        case op of
            LoadSource _ -> (ins, outs)
            TimesWord2 _ (o1, o2) -> (ins, o1 : o2 : outs)
            PlusWord2 (i1, i2) (o1, o2) -> (i1 : i2 : ins, o1 : o2 : outs)
            PlusWord (i1, i2) o -> (i1 : i2 : ins, o : outs)
            StoreValue i -> (i : ins, outs)
