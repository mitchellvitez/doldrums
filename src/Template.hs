module Template
 ( eval
 , compile
 , showResults
 , showFinalResults
 )
where

import Language
import Heap

import Data.List (mapAccumL, sort)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Void (Void)

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

data TiDump = DummyTiDump

initialTiDump :: TiDump
initialTiDump = DummyTiDump

type TiHeap = Heap Node

type Assoc a b = [(a, b)]

type TiGlobals = Assoc Text Addr

data TiStats = TiStats Int

data Node = NAp Addr Addr
          | NSupercomb Text [Text] CoreExpr
          | NNum Int
          | NInd Addr

assocLookup :: Eq a => a -> Assoc a b -> Text -> b
assocLookup _ [] errMsg = error $ show errMsg
assocLookup a ((x, y):xs) errMsg =
  if a == x then y else assocLookup a xs errMsg

compile :: CoreProgram -> CoreProgram -> TiState
compile prelude program = (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
  where
    initialStack = [addressOfMain]
    (initialHeap, globals) = buildInitialHeap scDefs
    addressOfMain = assocLookup "main" globals "main is not defined"
    scDefs = program ++ prelude

buildInitialHeap :: [CoreSupercombinatorDefinition] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = mapAccumL allocateSc hInitial scDefs

allocateSc :: TiHeap -> CoreSupercombinatorDefinition -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
  where
    (heap', addr) = hAlloc heap (NSupercomb name args body)

eval :: TiState -> [TiState]
eval state = state : restStates
  where
    restStates | tiFinal state = []
               | otherwise     = eval nextState
    nextState = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

tiFinal :: TiState -> Bool
tiFinal ([soleAddr], _, heap, _, _) = isDataNode (hLookup heap soleAddr)
tiFinal ([], _, _, _, _) = error "Empty stack"
tiFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

step :: TiState -> TiState
step state@(stack, _, heap, _, _) =
  dispatch (hLookup heap (head stack))
  where
    dispatch (NNum n )                 = numStep state n
    dispatch (NAp a1 a2)               = apStep state a1 a2
    dispatch (NSupercomb sc args body) = scStep state sc args body
    dispatch (NInd addr)               = indStep state addr

numStep :: TiState -> Int -> TiState
numStep _ n = error $ "Number applied as a function " ++ show n

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 _ =
  (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc argNames body
     = (newStack, dump, newHeap, globals, stats)
  where
    newStack = drop (length argNames) stack
    root = head newStack
    newHeap = instantiateAndUpdate body root heap (bindings ++ globals)
    bindings = zip argNames (getArgs heap stack)

indStep :: TiState -> Addr -> TiState
indStep (a : stack, dump, heap, globals, stats) a' =
  (a' : stack, dump, heap, globals, stats)

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (_ : stack) = map getArg stack
  where
    getArg addr = arg where (NAp _ arg) = hLookup heap addr
getArgs _ [] = []

instantiate :: CoreExpr -> TiHeap -> Assoc Text Addr -> (TiHeap, Addr)
instantiate (ExprNumber n) heap _ = hAlloc heap (NNum n)
instantiate (ExprApplication e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap  env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (ExprVariable v) heap env = (heap, assocLookup v env ("Undefined name " <> v))
instantiate (ExprConstructor tag arity) heap env = instantiateConstr tag arity heap env
instantiate (ExprLet defs body) heap oldEnv =
  instantiate body heap1 newEnv
  where
    (heap1, extraBindings) = mapAccumL instantiateRhs heap defs
    newEnv = oldEnv ++ extraBindings
    instantiateRhs heap3 (name, rhs) = (heap2, (name, addr))
      where (heap2, addr) = instantiate rhs heap3 newEnv
instantiate (ExprCase _ _) _ _ = error "Can't instantiate case exprs"
instantiate (ExprLambda _ _ ) _ _ = error "Can't instantiate lambda exprs"

instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> Assoc Text Addr -> TiHeap
instantiateAndUpdate (ExprNumber n) updateAddr heap env =
  hUpdate heap updateAddr (NNum n)
instantiateAndUpdate (ExprApplication a b) updateAddr heap env =
  hUpdate heap2 updateAddr (NAp x y)
  where
    (heap1, x) = instantiate a heap env
    (heap2, y) = instantiate b heap1 env
instantiateAndUpdate (ExprVariable v) updateAddr heap env =
  hUpdate heap updateAddr (NInd varAddr)
  where
    varAddr = assocLookup v env ("Undefined name " <> v)
instantiateAndUpdate (ExprLet defs body) updateAddr heap oldEnv =
  instantiateAndUpdate body updateAddr heap1 newEnv
  where
    (heap1, extraBindings) = mapAccumL instantiateRhs heap defs
    newEnv = extraBindings ++ oldEnv
    instantiateRhs heap (name, rhs) =
      (heap1, (name, addr))
      where
        (heap1, addr) = instantiate rhs heap newEnv
instantiateAndUpdate (ExprConstructor tag arity) updateAddr heap env =
  error "can't instantiateAndUpdate constructors yet"

instantiateConstr :: p1 -> p2 -> p3 -> p4 -> a
-- instantiateConstr tag arity heap env = undefined
instantiateConstr _ _ _ _ = error "Can't instantiate construtors yet"

-- | Show results
showResults :: [TiState] -> Text
showResults states = Text.concat $ Prelude.map showState states <> [showStats (last states)]

showFinalResults :: [TiState] -> Text
showFinalResults states = showFinalState $ last states

showFinalState :: TiState -> Text
showFinalState (stack, _, heap, _, _) = showFinalStack heap stack

showFinalStack :: TiHeap -> TiStack -> Text
showFinalStack heap stack = Text.intercalate "\n" (map showFinalStackItem stack)
  where showFinalStackItem addr = showFinalNode $ hLookup heap addr

showState :: TiState -> Text
showState (stack, _, heap, _, _) = Text.concat [showStack heap stack, "\n"]

showStack :: TiHeap -> TiStack -> Text
showStack heap stack = Text.concat [
    "Stk [",
    Text.intercalate "\n" (map showStackItem stack),
    " ]"
    ]
  where
    showStackItem addr = Text.concat [ showFWAddr addr, ": ",
                                   showStkNode heap (hLookup heap addr)]

showStkNode :: TiHeap -> Node -> Text
showStkNode heap (NAp funAddr argAddr) =
    Text.concat ["NAp ", showFWAddr funAddr, showDetail funAddr,
             " ", showFWAddr argAddr, showDetail argAddr]
  where
    showDetail addr = Text.concat [" (", showNode (hLookup heap addr), ")"]
showStkNode _ node = showNode node

tshow = pack . show

showNode :: Node -> Text
showNode (NAp a1 a2) = Text.concat ["NAp ", showAddr a1, " ", showAddr a2]
showNode (NSupercomb name _ _) = "NSupercomb " <> name
showNode (NNum n) = "NNum " <> tshow n
showNode (NInd addr) = "NInd " <> tshow addr

showFinalNode :: Node -> Text
showFinalNode (NAp a1 a2) = Text.concat ["<application:", showAddr a1, ",", showAddr a2, ">"]
showFinalNode (NSupercomb name _ _) = "<combinator:" <> name <> ">"
showFinalNode (NInd addr) = "<indirection:" <> tshow addr <> ">"
showFinalNode (NNum n) = tshow n

showAddr :: Addr -> Text
showAddr addr = tshow addr

showFWAddr :: Addr -> Text
showFWAddr addr = genSpaces (4 - Text.length str) <> str
  where
    str = tshow addr
    genSpaces n = pack $ replicate n ' '

showStats :: TiState -> Text
showStats (_, _, heap, _, stats) =
    Text.concat ["Total number of steps = ",
             tshow (tiStatGetSteps stats),
             "\n\n",
             showHeap heap]

showHeap :: TiHeap -> Text
showHeap heap = Text.concat ["Heap:",
    (Text.concat $ map showHeapAddr addrs)]
  where
    addrs = sort $ hAddresses heap
    showHeapAddr addr = Text.concat ["\n", tshow addr, " ", showNode (hLookup heap addr)]

tiStatInitial :: TiStats
tiStatInitial = TiStats 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps (TiStats n) = TiStats $ n + 1

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps (TiStats n) = n

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats statsFun (stack, dump, heap, scDefs, stats) =
  (stack, dump, heap, scDefs, statsFun stats)
