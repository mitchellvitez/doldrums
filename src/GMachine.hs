{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module GMachine
  ( gMachine
  , gMachineOutput
  )
where

-- TODO: page 135, 3.8.6
-- this will add data constructors along with casing on them

import Data.Foldable
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void
import Heap
import Language

gMachine :: [(Name, [Name], Expr)] -> Text
gMachine = showResults . eval . compile

gMachineOutput :: [(Name, [Name], Expr)] -> Text
gMachineOutput = getOutput . last . eval . compile

data GmState = GmState
  { getOutput :: GmOutput
  , getCode :: GmCode
  , getStack :: GmStack
  , getDump :: GmDump
  , getHeap :: GmHeap
  , getGlobals :: GmGlobals
  , getStats :: GmStats
  }

type GmOutput = Text

type GmCode = [Instruction]

type GmStack = [Addr]

type GmDump = [GmDumpItem]
type GmDumpItem = (GmCode, GmStack)

type GmHeap = Heap Node

type GmGlobals = [(Name, Addr)]

type GmStats = Int

statInitial :: GmStats
statInitial = 0

statIncSteps :: GmStats -> GmStats
statIncSteps s = s + 1

statGetSteps :: GmStats -> Int
statGetSteps = id

putOutput :: GmOutput -> GmState -> GmState
putOutput output state = state { getOutput = output }

putCode :: GmCode -> GmState -> GmState
putCode code state = state { getCode = code }

putStack :: GmStack -> GmState -> GmState
putStack stack state = state { getStack = stack }

putDump :: GmDump -> GmState -> GmState
putDump dump state = state { getDump = dump }

putHeap :: GmHeap -> GmState -> GmState
putHeap heap state = state { getHeap = heap }

-- putGlobals :: GmGlobals -> GmState -> GmState
-- putGlobals globals state = state { getGlobals = globals }

putStats :: GmStats -> GmState -> GmState
putStats stats state = state { getStats = stats }

data Instruction
  = Unwind
  | Pushglobal Name
  | Pushint Int
  | Push Int
  | Mkap
  | Slide Int
  | Update Int
  | Pop Int
  | Alloc Int
  | Eval
  | Add | Sub | Mul | Div | Neg
  | Eq | Ne | Lt | Le | Gt | Ge
  | Cond GmCode GmCode
  | Pack Int Int
  | Casejump [(Int, GmCode)]
  | Split Int
  | Print
  deriving (Show, Eq)

data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GmCode
  | NInd Addr
  | NConstr Int [Addr]
  deriving Eq

eval :: GmState -> [GmState]
eval state = state : restStates
  where
    restStates
      | gmFinal state = []
      | otherwise = eval nextState
    nextState = doAdmin $ step state

doAdmin s@GmState{..} = putStats (statIncSteps getStats) s

gmFinal :: GmState -> Bool
gmFinal GmState{..} = null getCode

step :: GmState -> GmState
step state@GmState{..} = dispatch code $ putCode codes state
  where (code:codes) = getCode

dispatch :: Instruction -> GmState -> GmState
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint n)    = pushint n
dispatch Mkap           = mkap
dispatch (Push n)       = push n
dispatch (Slide n)      = slide n
dispatch Unwind         = unwind
dispatch (Update n)     = update n
dispatch (Pop n)        = pop n
dispatch (Alloc n)      = alloc n
dispatch Eval           = evalInstruction
dispatch Add            = arithmetic2 (+)
dispatch Sub            = arithmetic2 (-)
dispatch Mul            = arithmetic2 (*)
dispatch Div            = arithmetic2 (div)
dispatch Neg            = arithmetic1 (negate)
dispatch Eq             = comparison (==)
dispatch Ne             = comparison (/=)
dispatch Lt             = comparison (<)
dispatch Le             = comparison (<=)
dispatch Gt             = comparison (>)
dispatch Ge             = comparison (>=)
dispatch (Cond c1 c2)   = cond c1 c2
dispatch (Pack t a)     = pack t a
dispatch (Casejump cs)  = casejump cs
dispatch (Split n)      = split n
dispatch Print          = print'

evalInstruction :: GmState -> GmState
evalInstruction state@GmState{..} =
  putDump dump $ putStack stack $ putCode code $ state
  where
    code = [Unwind]
    stack = [head getStack]
    dump = (getCode, tail getStack) : getDump

pack :: Int -> Int -> GmState -> GmState
pack tag arity state@GmState{..} = putHeap heap' $ putStack (a:stack') $ state
  where stack = getStack
        args = take arity stack
        stack' = drop arity stack
        (heap', a) = hAlloc getHeap (NConstr tag args)

casejump :: [(Int, GmCode)] -> GmState -> GmState
casejump [] state = error "Non-exhaustive pattern matching in case statement"
casejump ((tag, code):cs) state@GmState{..} =
  let (a:_) = getStack
      (NConstr constTag _constArgs) = hLookup getHeap a
  in if tag == constTag
        then putCode (code <> getCode) state
        else casejump cs state

split :: Int -> GmState -> GmState
split n state@GmState{..} =
  let (a:stack) = getStack
      (NConstr _constTag constArgs) = hLookup getHeap a
   in if length constArgs /= n
         then error "This shouldn't happen"
         else putStack (constArgs <> stack) state

print' :: GmState -> GmState
print' state@GmState{..} =
 case node of
    (NNum n) -> putOutput (getOutput <> tshow n) $ putStack stack state
    (NConstr tag args) ->
      let i' = concat $ replicate (length args) [Eval, Print]
       in putOutput getOutput $
          putCode (i' <> getCode) $
          putStack (args <> stack) $ state
    _ -> error "print'"
  where
    (a:stack) = getStack
    node = hLookup getHeap a

pop :: Int -> GmState -> GmState
pop n state@GmState{..} =
  putStack (drop n getStack) state

boxInteger :: Int -> GmState -> GmState
boxInteger n state@GmState{..} =
  putStack (a : getStack) $ putHeap h' state
  where (h', a) = hAlloc getHeap $ NNum n

unboxInteger :: Addr -> GmState -> Int
unboxInteger a state@GmState{..} =
  ub $ hLookup getHeap a
  where ub (NNum i) = i
        ub _ = error $ "Unboxing a non-integer"

primitive1
  :: (b -> GmState -> GmState)
  -> (Addr -> GmState -> a)
  -> (a -> b)
  -> (GmState -> GmState)
primitive1 box unbox op state =
  box (op $ unbox a state) (putStack as state)
  where (a:as) = getStack state

primitive2
  :: (b -> GmState -> GmState)
  -> (Addr -> GmState -> a)
  -> (a -> a -> b)
  -> (GmState -> GmState)
primitive2 box unbox op state =
  box (op (unbox a0 state) (unbox a1 state)) (putStack as state)
  where (a0:a1:as) = getStack state

arithmetic1 :: (Int -> Int) -> (GmState -> GmState)
arithmetic1 = primitive1 boxInteger unboxInteger

arithmetic2 :: (Int -> Int -> Int) -> (GmState -> GmState)
arithmetic2 = primitive2 boxInteger unboxInteger

comparison :: (Int -> Int -> Bool) -> (GmState -> GmState)
comparison = primitive2 boxBoolean unboxInteger

-- TODO: fix these up until MARK so they work on non-ints
boxBoolean :: Bool -> GmState -> GmState
boxBoolean b state = (putStack (a:getStack state)) . (putHeap heap') $ state
  where (heap', a) = hAlloc (getHeap state) (NNum b')
        b' | b         = 1
           | otherwise = 0

cond :: GmCode -> GmCode -> GmState -> GmState
cond i1 i2 state = (putStack stack') . (putCode (code' ++ getCode state)) $ state
  where (a:stack') = getStack state
        node = hLookup (getHeap state) a
        code' = case node of
                     (NNum 1) -> i1
                     (NNum 0) -> i2
                     _        -> error "Non exhaustive case expression"
-- MARK

update :: Int -> GmState -> GmState
update n state@GmState{..} =
  -- overwrite the n+1th stack item with an indirection to the item on top of the stack
  putStack as $ putHeap newHeap state
  where
    (a:as) = getStack
    newHeap = hUpdate getHeap (as !! n) (NInd a)

pushglobal :: Name -> GmState -> GmState
pushglobal f state@GmState{..} =
  putStack (a : getStack) state
  where a = aLookup getGlobals f

pushint :: Int -> GmState -> GmState
pushint n state@GmState{..} =
  putHeap heap' $ putStack (a : getStack) state
  where (heap', a) = hAlloc getHeap $ NNum n

mkap :: GmState -> GmState
mkap state@GmState{..} =
  putHeap heap' $ putStack (a : as') state
  where (heap', a) = hAlloc getHeap $ NAp a1 a2
        (a1:a2:as') = getStack

push :: Int -> GmState -> GmState
push n state@GmState{..} =
  putStack (a:as) state
  where as = getStack
        a = as !! n

slide :: Int -> GmState -> GmState
slide n state@GmState{..} =
  putStack (a : drop n as) state
  where (a:as) = getStack

unwind :: GmState -> GmState
unwind state = newState (hLookup heap a)
  where (a:as) = getStack state
        heap = getHeap state
        newState :: Node -> GmState
        newState (NNum n)      = if null $ getDump state
                                    then state
                                    else let ((code', stack'):ds) = getDump state
                                          in (putDump ds) . (putCode code') . (putStack (a:stack')) $ state
        newState (NConstr n as) = if null $ getDump state
                                     then state
                                     else let ((code', stack'):ds) = getDump state
                                           in (putDump ds) . (putCode code') . (putStack (a:stack')) $ state
        newState (NAp a1 a2)   = (putCode [Unwind]) . (putStack (a1:a:as)) $ state
        newState (NGlobal n c) = if length as < n
                                    then error "Unwinding with too few arguments"
                                    else let stack' = rearrange n (getHeap state) (getStack state)
                                          in (putStack stack') . (putCode c) $ state
        newState (NInd a')     = (putCode [Unwind]) . (putStack (a':as)) $ state

-- TODO: 3.29
-- unwind :: GmState -> GmState
-- unwind state@GmState{..} =
--   newState $ hLookup getHeap a
--   where (a:as) = getStack
--         newState (NNum n) = if null getDump then state else
--           let ((code', stack'):ds) = getDump
--            in putDump ds $ putCode code' $ putStack (a:stack') $ state
--         newState (NAp a1 a2) = putCode [Unwind] $ putStack (a1:a:as) state
--         newState (NGlobal n c)
--           | length as < n = error "Unwinding with too few arguments"
--           | otherwise = putStack stack' $ putCode c state
--               where stack' = rearrange n getHeap getStack
--         newState (NConstr n as) =
--           if null getDump
--             then state
--             else let ((code', stack'):ds) = getDump
--                   in putDump ds $ putCode code' $ putStack (a:stack') state
--         newState (NInd addr) =
--           putStack (a:s) state
--           where
--             (a0:s) = getStack
--             (NInd a) = hLookup getHeap a0

rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as =
  take n as' ++ drop n as
  where as' = map (getArg . hLookup heap) (tail as)
        getArg (NAp _ a2) = a2
        getArg _ = error "getArg"

alloc :: Int -> GmState -> GmState
alloc n state@GmState{..} = putHeap heap' $ putStack (stack' <> getStack) state
  where (heap', stack') = allocNodes n getHeap

allocNodes :: Int -> GmHeap -> (GmHeap, GmStack)
allocNodes 0 heap = (heap, [])
allocNodes n heap = (heap2, a:as)
  where (heap1, as) = allocNodes (n - 1) heap
        (heap2, a) = hAlloc heap1 $ NInd hNull
        hNull = -1

compile :: [(Name, [Name], Expr)] -> GmState
compile program =
  GmState "" initialCode [] [] heap globals statInitial
  where (heap, globals) = buildInitialHeap program

-- buildInitialHeap :: [(Name, [Name], Expr)] -> (GmHeap, GmGlobals)
-- buildInitialHeap program =
--   mapAccumL allocatePrim compiledPrimitives $
--   mapAccumL allocateSc hInitial compiled
--   where compiled = map compileSc program

buildInitialHeap :: [(Name, [Name], Expr)] -> (GmHeap, GmEnvironment)
buildInitialHeap = flip (foldr allocatePrim) compiledPrimitives . foldr allocateSc (hInitial, []) . (map compileSc)

allocateSc :: GmCompiledSC -> (GmHeap, GmEnvironment) -> (GmHeap, GmEnvironment)
allocateSc (name, nargs, instructions) (heap, env) = (heap', (name, addr):env)
  where (heap', addr) = hAlloc heap (NGlobal nargs instructions)

allocatePrim :: GmCompiledSC -> (GmHeap, GmEnvironment) -> (GmHeap, GmEnvironment)
allocatePrim (name, nargs, instructions) (heap, env) = (heap', (name, addr):env)
  where (heap', addr) = hAlloc heap (NGlobal nargs instructions)

type GmCompiledSC = (Name, Int, GmCode)

-- allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
-- allocateSc heap (name, nargs, instns) =
--   (heap', (name, addr))
--   where (heap', addr) = hAlloc heap $ NGlobal nargs instns

initialCode :: GmCode
initialCode = [Pushglobal "main", Eval, Print]

compileSc :: (Name, [Name], Expr) -> GmCompiledSC
compileSc (name, env, body) =
  (name, length env, compileR body $ zip env [0..])

compileR :: GmCompiler
compileR e env = compileE e env <> [Update d, Pop d, Unwind]
  where d = length env

aDomain = map fst

lookup' :: Eq a => [(a, b)] -> a -> b
lookup' xs a = fromJust $ lookup a xs
  where fromJust (Just x) = x
        fromJust _ = error "lookup'"

compileE :: GmCompiler
compileE (ExprInt n) args = [Pushint $ fromIntegral n]
compileE (ExprLet name def exp) args = compileLetRec compileE [(name, def)] exp args
compileE (ExprApplication (ExprVariable "negate") e) args = compileE e args ++ [Neg]
compileE (ExprApplication (ExprApplication (ExprApplication (ExprVariable "if") e0) e1) e2) args = compileE e0 args ++ [Cond (compileE e1 args) (compileE e2 args)]
compileE e@(ExprApplication (ExprApplication (ExprVariable op) e0) e1) args = if op `elem` aDomain builtInDyadic
                                                 then compileE e0 args ++
                                                      compileE e1 (argOffset 1 args) ++
                                                      [lookup' builtInDyadic op]
                                                 else compileC e args ++ [Eval]
compileE (ExprCase exp alts) args = compileE exp args ++ [Casejump (compileD compileE' alts args)]
compileE e args = let (core, exps) = decompose e
                  in case core of
                          (ExprConstructor t a) -> concatMap (\(exp, offset) -> compileC exp (argOffset offset args))
                                                     (zip (reverse exps) [0..])
                                           ++ [Pack t a]
                          _             -> compileC e args ++ [Eval]

compileE' :: Int -> GmCompiler
compileE' offset expr args =
  [Split offset] <> compileE expr args <> [Slide offset]

compileD :: (Int -> GmCompiler) -> [CaseAlternative Void] -> GmEnvironment -> [(Int, GmCode)]
compileD comp alts env
  = [(tag, comp (length names) body (zip names [0..] ++ argOffset (length names) env)) | (tag, names, body) <- alts]

decompose :: Expr -> (Expr, [Expr])
decompose e = decompose' e []
  where decompose' :: Expr -> [Expr] -> (Expr, [Expr])
        decompose' e args = case e of
                                 (ExprApplication l r) -> decompose' l (r:args)
                                 _         -> (e, args)

type GmCompiler = Expr -> GmEnvironment -> GmCode

type GmEnvironment = [(Name, Int)]

compileC :: GmCompiler
compileC (ExprVariable v) env
  | v `elem` map fst env = [Push n]
  | otherwise = [Pushglobal v]
  where n = aLookup env v
compileC (ExprInt n) env = [Pushint $ fromIntegral n]
compileC (ExprApplication e1 e2) env = compileC e2 env <> compileC e1 (argOffset 1 env) <> [Mkap]
compileC (ExprLet name def e) env = compileLetRec compileC [(name, def)] e env
compileC _ _ = error "compileC"

compileLetRec :: GmCompiler -> [(Name, Expr)] -> GmCompiler
compileLetRec comp defs exp env = [Alloc (length defs)] ++ compileLetRec' defs env' ++ comp exp env' ++ [Slide (length defs)]
  where env' = compileArgs defs env

compileLetRec' [] env = []
compileLetRec' ((name, expr):defs) env = compileC expr env ++ [Update (length defs)] ++ compileLetRec' defs env

compileArgs :: [(Name, Expr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env = zip (map fst defs) [n - 1, n - 2 .. 0] ++ argOffset n env
  where n = length defs

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v,m) <- env]

compiledPrimitives :: [GmCompiledSC]
compiledPrimitives =
  [ ("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind])
  , ("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind])
  , ("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind])
  , ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind])
  , ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind])
  , ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind])
  , ("!=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind])
  , ("<", 2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind])
  , ("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind])
  , (">", 2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind])
  , (">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind])
  , ("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
  ]

builtInDyadic :: [(Name, Instruction)]
builtInDyadic =
  [ ("+", Add)
  , ("-", Sub)
  , ("*", Mul)
  , ("/", Div)
  , ("==", Eq)
  , ("!=", Ne)
  , (">=", Ge)
  , (">", Gt)
  , ("<=", Le)
  , ("<", Lt)
  ]

-- SHOW --

showResults :: [GmState] -> Text
showResults states = fold
  [ "Supercombinator definitions\n"
  , T.intercalate "\n" $ map (showSC s) (getGlobals s)
  , "\n\nState transitions\n\n"
  , T.intercalate "\n" $ map showState states
  , "\n"
  , showStats $ last states
  ]
  where (s:_) = states

showSC :: GmState -> (Name, Addr) -> Text
showSC s (name, addr) = fold
  [ "Code for "
  , name
  , "\n"
  , showInstructions code
  ]
  where (NGlobal _arity code) = hLookup (getHeap s) addr

showInstructions :: GmCode -> Text
showInstructions is = fold
  [ "  Code:{ "
  , T.intercalate ", " $ map showInstruction is
  , " }\n"
  ]

tshow :: Show a => a -> Text
tshow = T.pack . show

showInstruction :: Instruction -> Text
showInstruction = tshow

showState :: GmState -> Text
showState s = fold
  [ showOutput s
  , "\n"
  , showStack s
  , "\n"
  , showDump s
  , "\n"
  -- , showHeap s
  -- , "\n"
  , showInstructions $ getCode s
  ]

showOutput :: GmState -> Text
showOutput GmState{..} = fold
  [ "  Output:\""
  , getOutput
  , "\""
  ]

showStack :: GmState -> Text
showStack s = fold
  [ "  Stack:[ "
  , T.intercalate ", " $ map (showStackItem s) (reverse $ getStack s)
  , " ]"
  ]

showStackItem :: GmState -> Addr -> Text
showStackItem s a = fold
  [ showAddr a
  , ": "
  , showNode s a $ hLookup (getHeap s) a
  ]

showDump :: GmState -> Text
showDump s = fold
  [ "  Dump:[ "
  , T.intercalate ", " $ map showDumpItem $ reverse $ getDump s
  , " ]"
  ]

showDumpItem :: GmDumpItem -> Text
showDumpItem (code, stack) = fold
  [ "<"
  , shortShowInstructions 3 code
  , ", "
  , shortShowStack stack
  , ">"
  ]

shortShowInstructions :: Int -> GmCode -> Text
shortShowInstructions number code = fold
  [ "{"
  , T.intercalate "; " dotcodes
  , "}"
  ]
  where codes = map showInstruction $ take number code
        dotcodes | length code > number = codes <> ["..."]
                 | otherwise = codes

shortShowStack :: GmStack -> Text
shortShowStack stack = fold
  [ "["
  , T.intercalate ", " $ map showAddr stack
  , "]"
  ]

-- showHeap :: GmState -> Text
-- showHeap s = fold
--   [ "  Heap:( "
--   , T.intercalate ", " $ map (\(addr, val) -> showAddr addr <> ": " <> showNode s addr val) (reverse . hToList $ getHeap s)
--   , " )"
--   ]

showNode :: GmState -> Addr -> Node -> Text
showNode s a (NNum n) = tshow n
showNode s a (NGlobal n g) = "Global " <> v
  where v = head [n | (n,b) <- getGlobals s, a == b]
showNode s a (NAp a1 a2) = fold
  [ "Ap "
  , showAddr a1
  , " "
  , showAddr a2
  ]
showNode s a (NInd n) = "Indirection " <> showAddr n
showNode s a (NConstr t as) = fold
  [ "Cons "
  , tshow t
  , " ["
  , T.intercalate ", " $ map showAddr as
  , " ]"
  ]

showAddr :: Addr -> Text
showAddr n = "@" <> tshow n

showStats :: GmState -> Text
showStats s = fold
  [ "Steps taken = "
  , tshow $ statGetSteps $ getStats s
  ]
