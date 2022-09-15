{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module GMachine
  ( gMachine
  )
where

import Data.Foldable
import Data.List (mapAccumL)
import qualified Data.Text as T
import Data.Text (Text)
import Heap
import Language

gMachine :: [(Name, [Name], Expr)] -> Text
gMachine = showResults . eval . compile

data GmState = GmState
  { getCode :: GmCode
  , getStack :: GmStack
  , getHeap :: GmHeap
  , getGlobals :: GmGlobals
  , getStats :: GmStats
  }

type GmCode = [Instruction]

type GmStack = [Addr]

type GmHeap = Heap Node

type GmGlobals = [(Name, Addr)]

type GmStats = Int

statInitial :: GmStats
statInitial = 0

statIncSteps :: GmStats -> GmStats
statIncSteps s = s + 1

statGetSteps :: GmStats -> Int
statGetSteps = id

putCode :: GmCode -> GmState -> GmState
putCode code state = state { getCode = code }

putStack :: GmStack -> GmState -> GmState
putStack stack state = state { getStack = stack }

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
  deriving (Show, Eq)

data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GmCode
  | NInd Addr
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
step state@GmState{..} = dispatch i (putCode is state)
  where (i:is) = getCode

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

pop :: Int -> GmState -> GmState
pop n state@GmState{..} =
  putStack (drop n getStack) state

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
unwind state@GmState{..} =
  newState $ hLookup getHeap a
  where (a:as) = getStack
        newState (NNum n) = state
        newState (NAp a1 a2) = putCode [Unwind] $ putStack (a1:a:as) state
        newState (NGlobal n c)
          | length as < n = error "Unwinding with too few arguments"
          | otherwise = putStack stack' $ putCode c state
              where stack' = rearrange n getHeap getStack
        newState (NInd addr) =
          putStack (a:s) state
          where
            (a0:s) = getStack
            (NInd a) = hLookup getHeap a0

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
  GmState initialCode [] heap globals statInitial
  where (heap, globals) = buildInitialHeap program

buildInitialHeap :: [(Name, [Name], Expr)] -> (GmHeap, GmGlobals)
buildInitialHeap program =
  mapAccumL allocateSc hInitial compiled
  where compiled = map compileSc program

type GmCompiledSC = (Name, Int, GmCode)

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) =
  (heap', (name, addr))
  where (heap', addr) = hAlloc heap $ NGlobal nargs instns

initialCode :: GmCode
initialCode = [Pushglobal "main", Unwind]

compileSc :: (Name, [Name], Expr) -> GmCompiledSC
compileSc (name, env, body) =
  (name, length env, compileR body $ zip env [0..])

compileR :: GmCompiler
compileR e env = compileC e env <> [Update d, Pop d, Unwind]
  where d = length env

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

-- compiledPrimitives :: [GmCompiledSC]
-- compiledPrimitives = []

showResults :: [GmState] -> Text
showResults states = fold
  [ "Supercombinator definitions\n"
  , T.intercalate "\n" $ map (showSC s) (getGlobals s)
  , "\n\nState transitions\n\n"
  , T.intercalate "\n" $ map showState states
  , "\n\n"
  , showStats $ last states
  ]
  where (s:_) = states

showSC :: GmState -> (Name, Addr) -> Text
showSC s (name, addr) = fold
  [ "Code for "
  , name
  , "\n"
  , showInstructions code
  , "\n\n"
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
  [ showStack s
  , "\n"
  , showHeap s
  , "\n"
  , showInstructions $ getCode s
  , "\n"
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

showHeap :: GmState -> Text
showHeap s = fold
  [ "  Heap:( "
  , T.intercalate ", " $ map (\(addr, val) -> showAddr addr <> ": " <> showNode s addr val) (reverse . hToList $ getHeap s)
  , " )"
  ]

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

showAddr :: Addr -> Text
showAddr n = "@" <> tshow n

showStats :: GmState -> Text
showStats s = fold
  [ "Steps taken = "
  , tshow $ statGetSteps $ getStats s
  ]
