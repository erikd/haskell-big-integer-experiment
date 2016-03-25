
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

timesEmpty :: Times
timesEmpty = Times [] Map.empty

insertResults :: Map Word [Value] -> Operation -> Map Word [Value]
insertResults vmap (TimesWord2 _ (v1, v2)) =
    Map.insertWith (++) (vIndex v1) [v1] $ Map.insertWith (++) (vIndex v2) [v2] vmap
insertResults _ op =
    error $ "insertResults " ++ show op


appendOp :: Operation -> Times -> Times
appendOp op times = times
    { ops = ops times ++ [op]
    , vals = insertResults (vals times) op
    }


generateProducts :: Word -> Word -> Times
generateProducts left right =
    snd $ foldl' generate ((0, 'a'), timesEmpty) prodIndices
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


insertLoads :: [Operation] -> [Operation]
insertLoads =
    insert Set.empty Set.empty
  where
    insert xload yload (op@(TimesWord2 (x, y) _):ops) =
        case (Set.member x xload, Set.member y yload) of
            (True, True) -> op : insert xload yload ops
            (True, False) -> LoadSource y : op : insert xload (Set.insert y yload) ops
            (False, True) -> LoadSource x : op : insert (Set.insert x xload) yload ops
            (False, False) -> LoadSource x : LoadSource y : op : insert (Set.insert x xload) (Set.insert y yload) ops
    insert xload yload (op:ops) = op : insert xload yload ops
    insert _ _ [] = []

