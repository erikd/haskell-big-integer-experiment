{-# LANGUAGE StrictData #-}

import Data.List
import Data.Map (Map)
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
    deriving (Eq, Show)


data Value
    = Value ValueType Word Int
    deriving Show

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
    putStrLn "--------------------------------------------------------"
    mapM_ print $ ops times
    putStrLn "------------"
    mapM_ (\ (k, v) -> putStrLn $ show k ++ " : " ++ show v) . Map.toList $ vals times
    putStrLn "--------------------------------------------------------"


timesEmpty :: Times
timesEmpty = Times [] Map.empty Map.empty


insertResults :: Map Word [Value] -> Operation -> Map Word [Value]
insertResults vmap op =
    case op of
        TimesWord2 _ (v1, v2) -> addResult (++) v1 v2
        PlusWord2 _ (v1, v2) -> addResult (appender) v1 v2
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
    insertLoads . snd $ foldl' generate ((0, 0), timesEmpty) prodIndices
  where
    generate ((lidx, n), times) (x, y) =
        let idx = x + y
            ni = if idx > lidx then 0 else n
            prod = Value Product idx ni
            carry = Value ProdCarry (idx + 1) ni
        in ((idx, succ ni), appendOp (TimesWord2 (Source x 'x', Source y 'y') (carry, prod)) times)

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
    let indices = [ 1 .. (1 + maximum (Map.keys (vals times))) ]
    in foldl' (\t i -> insertIndexSums i t) times indices


insertIndexSums :: Word -> Times -> Times
insertIndexSums index =
    recurse
  where
    recurse times =
        case getIndexVals index times of
            (_, Nothing) -> times
            (newtimes, Just (a, b)) -> recurse $ appendOp (makeSum a b) newtimes

getIndexVals :: Word -> Times -> (Times, Maybe (Value, Value))
getIndexVals i times =
    maybe (times, Nothing) getVals $ Map.lookup i vmap
  where
    vmap = vals times
    getVals values =
        case splitAt 2 values of
            ([a, b], rest) -> (times { vals = Map.insert i rest vmap } , Just (a, b))
            _ -> (times, Nothing)

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
