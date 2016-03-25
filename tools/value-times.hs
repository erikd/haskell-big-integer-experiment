
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
    deriving Show


data Value
    = Value ValueType Word Char
    deriving Show

vIndex :: Value -> Word
vIndex (Value _ w _) = w

data Operation
    = LoadSource Source
    | TimesWord2 (Source, Source) (Value, Value)
    | SumProducts (Value, Value) (Value, Value)
    | PlusWord2 (Value, Value) (Value, Value)
    | PlusWord (Value, Value) Value
    | StoreSum Value
    | StoreProd Value
    deriving Show

data Times = Times
    { ops :: [Operation]
    , vals :: Map Word [Value]
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
timesEmpty = Times [] Map.empty

insertResults :: Map Word [Value] -> Operation -> Map Word [Value]
insertResults vmap op =
    case op of
        TimesWord2 _ (v1, v2) -> addResult (++) v1 v2
        PlusWord2 _ (v1, v2) -> addResult (appender) v1 v2
        _ -> error $ "insertResults " ++ show op
  where
    appender xs ys = ys ++ xs
    addResult apf v1 v2 =
        Map.insertWith apf (vIndex v1) [v1]
            $ Map.insertWith apf (vIndex v2) [v2] vmap


appendOp :: Operation -> Times -> Times
appendOp op times = times
    { ops = ops times ++ [op]
    , vals = insertResults (vals times) op
    }

insertOp :: Operation -> Times -> Times
insertOp op times = times
    { ops = op : ops times
    , vals = insertResults (vals times) op
    }

getIndexVals :: Word -> Times -> (Times, Maybe (Value, Value))
getIndexVals i times =
    maybe (times, Nothing) getVals $ Map.lookup i vmap
  where
    vmap = vals times
    getVals values =
        case splitAt 2 values of
            ([a, b], rest) -> (times { vals = Map.insert i rest vmap } , Just (a, b))
            _ -> (times, Nothing)


initializeProducts :: Word -> Word -> Times
initializeProducts left right =
    insertLoads . snd $ foldl' generate ((0, 'a'), timesEmpty) prodIndices
  where
    generate ((lidx, n), times) (x, y) =
        let idx = x + y
            ni = if idx > lidx then 'a' else n
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
insertSums times = times



insertSum :: Word -> Times -> Times
insertSum index =
    recurse
  where
    recurse times =
        case getIndexVals index times of
            (_, Nothing) -> times
            (newtimes, Just (a, b)) -> recurse $ appendOp (makeSum a b) newtimes


makeSum :: Value -> Value -> Operation
makeSum v1 v2 =
    case (v1, v2) of
        (Value Product i n1, Value Product _ n2) ->
            let n = max n1 n2
            in PlusWord2 (v1, v2) (Value SumCarry (i + 1) n, Value Sum i n)
        (Value ProdCarry i n1, Value Sum _ n2) ->
            let n = succ (max n1 n2)
            in PlusWord2 (v1, v2) (Value SumCarry (i + 1) n, Value Sum i n)

        x -> error $ "makeSum " ++ show x
