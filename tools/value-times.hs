{-# LANGUAGE StrictData #-}

import Control.Monad
import Data.Char
import Data.List
import Data.Map (Map)
import System.Environment
import System.Exit ( exitFailure, exitSuccess )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


main :: IO ()
main = do
    args <- getArgs
    case args of
        [x, y] -> runProgram (read x) (read y) Nothing
        [x, y, fname] -> runProgram (read x) (read y) $ Just fname
        _ -> usageExit


usageExit :: IO ()
usageExit = do
    progname <- getProgName
    putStrLn $ unlines
        [ ""
        , "Usage : " ++ progname ++ " x y <filename>"
        , ""
        , "where:"
        , "    x and y are numbers >= 2, x and y < 20 and x > y"
        , "    filename is optional (stdout will be used if missing)"
        , ""
        ]
    exitSuccess


runProgram :: Word -> Word -> Maybe String -> IO ()
runProgram x y mfname
    | x < y || x < 2 || y < 2 = do
        putStrLn $ "Error : x (" ++ show x ++ ") < y (" ++ show y ++ ")"
        usageExit
    | Just fname <- mfname =
        putStrLn $ "runProgram " ++ fname
    | otherwise =
        printTimes $ reorderOperations $ insertSums $ initializeProducts x y

-- -----------------------------------------------------------------------------
-- Implementation follows:

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


opProvidesOutput :: Value -> Operation -> Bool
opProvidesOutput vt op =
    case op of
        LoadSource _ -> False
        TimesWord2 _ (v1, v2) -> v1 == vt || v2 == vt
        PlusWord2 _ (v1, v2) -> v1 == vt || v2 == vt
        PlusWord _ v -> v == vt
        StoreValue _ -> False

class Ppr a where
    ppr :: a -> String

instance Ppr Int where
    ppr i
        | i < 26 = [chr (i + ord 'a')]
        | otherwise =
                let (q, r) = i `quotRem` 26
                in ppr q ++ ppr r


instance Ppr Value where
    ppr (Value t w i) = ppr t ++ show w ++ ppr i

instance Ppr ValueType where
    ppr Product = "prod"
    ppr ProdCarry = "pc"
    ppr Sum = "sum"
    ppr SumCarry = "sc"

instance Ppr Source where
    ppr (Source w c) = c : show w

instance Ppr Operation where
    ppr (LoadSource (Source i c)) = [c] ++ show i ++ " <- indexWordArrayM " ++ [c] ++ "arr " ++ show i
    ppr (TimesWord2 (s0, s1) (co, po)) = "let (# " ++ ppr co ++ ", " ++ ppr po ++ " #) = timesWord2 " ++ ppr s0 ++ " " ++ ppr s1
    ppr (PlusWord2 (i1, i2) (co, so)) = "let (# " ++ ppr co ++ ", " ++ ppr so ++ " #) = plusWord2 " ++ ppr i1 ++ " " ++ ppr i2
    ppr (PlusWord (i1, i2) o) = "let " ++ ppr o ++ " = plusWord " ++ ppr i1 ++ " " ++ ppr i2
    ppr (StoreValue v) = "writeWordArray marr " ++ show (vIndex v) ++ " " ++ ppr v


-- | Compare Operations based on the Index of the primay output values. Using
-- this as the compare function passed to Data.List.sortBy should result in a
-- list of Operations which is still valid (as long as the list was valid
-- beforehand). Specifically, it groups all operations with lower value output
-- indices in front ot operations with higher value output indices. Ordering
-- within a given output index should remain unchanged.
opCompare :: Operation -> Operation -> Ordering
opCompare op1 op2 =
    compare (opIndex op1) (opIndex op2)
  where
    opIndex :: Operation -> Word
    opIndex (LoadSource (Source w _)) = w
    opIndex (TimesWord2 _ (_, v)) = vIndex v
    opIndex (PlusWord2 _ (_, v)) = vIndex v
    opIndex (PlusWord _ v) = vIndex v
    opIndex (StoreValue v) = vIndex v


data Times = Times
    { x :: Word
    , y :: Word
    , len :: Word
    , ops :: [Operation]
    , vals :: Map Word [Value]
    , names :: Map Word [(ValueType, Int)]
    }
    deriving Show


displayTimes :: Times -> IO ()
displayTimes times = do
    putStrLn "-------------------------------------------------------------------------"
    mapM_ print $ ops times
    putStrLn "-------------------------------------------------------------------------"
    mapM_ (\ (k, v) -> unless (null v) (putStrLn $ show k ++ " : " ++ show v)) . Map.toList $ vals times
    putStrLn "-------------------------------------------------------------------------"


timesEmpty :: Word -> Word -> Times
timesEmpty x y = Times x y (x + y) [] Map.empty Map.empty


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


insertNames :: Map Word [(ValueType, Int)] -> Operation -> Map Word [(ValueType, Int)]
insertNames vmap op =
    case op of
        TimesWord2 _ (v1, v2) -> addNames v1 v2
        PlusWord2 _ (v1, v2) -> addNames v1 v2
        PlusWord _ v -> Map.insertWith (++) (vIndex v) [(vType v, vName v)] vmap
        _ -> error $ "insertNames " ++ show op
  where
    vTypeName v = (vType v, vName v)
    addNames v1 v2 =
        Map.insertWith (++) (vIndex v1) [vTypeName v1]
            $ Map.insertWith (++) (vIndex v2) [vTypeName v2] vmap



fixName :: Times -> Operation -> Operation
fixName times op =
    case op of
        TimesWord2 x (v1, v2) -> TimesWord2 x (fixValName v1, fixValName v2)
        PlusWord2 x (v1, v2) -> PlusWord2 x (fixValName v1, fixValName v2)
        PlusWord x v -> PlusWord x (fixValName v)
        _ -> error $ "fixName " ++ show op
  where
    newName :: ValueType -> Word -> [(ValueType, Int)] -> Value
    newName t i xs =
        case map snd (filter (\vt -> fst vt == t) xs) of
            [] -> Value t i 0
            ys -> Value t i $ succ (maximum ys)
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
        { ops = ops times
        , vals = insertResults (vals times) op
        , names = insertNames (names times) op
        }


initializeProducts :: Word -> Word -> Times
initializeProducts left right =
    insertLoads $ foldl' generate (timesEmpty left right) prodIndices
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
            (newtimes, [a, b]) -> insertIndexSums index $ appendOp (makeSum (len times - 1) a b) newtimes
            (newtimes, [a]) -> newtimes { ops = ops newtimes ++ [StoreValue a] }
            (_, _) -> times


reorderOperations :: Times -> Times
reorderOperations times = times { ops = sortBy opCompare $ ops times }


makeSum :: Word -> Value -> Value -> Operation
makeSum maxIndex v1 v2 =
    case (v1, v2) of
        -- Sums that can produce a carry.
        (Value Product i _, Value Product _ _) ->
            PlusWord2 (v1, v2) (Value SumCarry (i + 1) 0, Value Sum i 0)
        (Value ProdCarry i _, Value Sum _ _) ->
            PlusWord2 (v1, v2) (Value SumCarry (i + 1) 0, Value Sum i 0)
        (Value Product i _, Value ProdCarry _ _) ->
            PlusWord2 (v1, v2) (Value SumCarry (i + 1) 0, Value Sum i 0)
        (Value ProdCarry i _, Value ProdCarry _ _) ->
            PlusWord2 (v1, v2) (Value SumCarry (i + 1) 0, Value Sum i 0)

        (Value Sum _ _, Value SumCarry i _) -> lastPlusWord i
        (Value SumCarry i _, Value Sum _ _) -> lastPlusWord i
        (Value Sum i _, Value Sum _ _) -> lastPlusWord i


        -- Sums that *will not* produce a carry.
        (Value ProdCarry i _, Value SumCarry _ _) ->
            PlusWord (v1, v2) (Value Sum i 0)
        (Value SumCarry i _, Value SumCarry _ _) ->
            PlusWord (v1, v2) (Value Sum i 0)

        x -> error $ "makeSum " ++ show x
  where
    lastPlusWord i =
        if i >= maxIndex
            then PlusWord (v1, v2) (Value Sum i 0)
            else PlusWord2 (v1, v2) (Value SumCarry (i + 1) 0, Value Sum i 0)


validateValueUsage :: Times -> IO ()
validateValueUsage times = do
    results <- sequence
                [ valuesAreEmpty
                , valuesAreUniqueAndUsedOnce $ extractValues (ops times)
                , validateOperationOrdering $ ops times
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
            unused_in = filter (`notElem` outvals) invals
            unused_out = filter (`notElem` invals) outvals

        unless in_ok $
            putStrLn $ "validateValueUsage found duplicate inputs : " ++ show invals ++ "\n"
        unless out_ok $
            putStrLn $ "validateValueUsage found duplicate outputs : " ++ show outvals ++ "\n"

        unless (null unused_in) $
            putStrLn $ "validateValueUsage unused inputs : " ++ show unused_in ++ "\n"
        unless (null unused_out) $
            putStrLn $ "validateValueUsage unused outputs : " ++ show unused_out ++ "\n"

        return (in_ok && out_ok && not (null unused_in) && not (null unused_out))

    setInsert2 (a, b) = Set.insert a . Set.insert b

    setMembers2 (a, b) set = Set.member a set && Set.member b set

    opCheckArgs (ok, vals) op =
        case op of
            LoadSource _ -> return (ok, vals)
            TimesWord2 _ outs -> return (ok, setInsert2 outs vals)
            PlusWord2 ins outs -> return (ok && setMembers2 ins vals, setInsert2 outs vals)
            PlusWord ins out -> return (ok && setMembers2 ins vals, Set.insert out vals)
            StoreValue _ -> return (ok, vals)

    validateOperationOrdering oplist =
        fst <$> foldM opCheckArgs (False, Set.empty) oplist

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


pprTimes :: Times -> [String]
pprTimes times =
    [ ""
    , "{-# INLINE " ++ name ++ " #-}"
    , name ++ " :: WordArray -> WordArray -> Natural"
    , name ++ " !xarr !yarr ="
    , "    runStrictPrim $ do"
    , "        marr <- newWordArray " ++ show maxlen
    ]
    ++ map (indent8 . ppr) ( ops times)
    ++ map indent8
        [ "narr <- unsafeFreezeWordArray marr"
        , "let !len = " ++ show (maxlen - 1) ++ " + boxInt# (neWord# (unboxWord " ++ lastCarry ++ ") 0##)"
        , "return $! Natural len narr"
        , ""
        ]
  where
    name = "timesNat" ++ show (x times) ++ "x" ++ show (y times)
    indent8 s = "        " ++ s
    maxlen = (x times) + (y times)
    lastCarry =
        case last (ops times) of
            StoreValue v -> ppr v
            x -> error $ "lastCarry " ++ show x


printTimes :: Times -> IO ()
printTimes = mapM_ putStrLn . pprTimes
