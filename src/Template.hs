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

type TiDump = [TiStack]

initialTiDump :: TiDump
initialTiDump = []

type TiHeap = Heap Node

type Assoc a b = [(a, b)]

type TiGlobals = Assoc Text Addr

data TiStats = TiStats Int

data Primitive = Neg | Add | Sub | Mul | Div | Eq | Neq | Gt | Lt | Geq | Leq | And | Or
  deriving Show

data Node = NAp Addr Addr
          | NSupercomb Text [Text] CoreExpr
          | NNum Int
          | NInd Addr
          | NPrim Name Primitive

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
buildInitialHeap scDefs =
  (heap2, scAddrs ++ primAddrs)
  where
    (heap1, scAddrs) = mapAccumL allocateSc hInitial scDefs
    (heap2, primAddrs) = mapAccumL allocatePrim heap1 primitives

primitives :: Assoc Name Primitive
primitives =
  [ ("negate", Neg)
  , ("+", Add)
  , ("-", Sub)
  , ("*", Mul)
  , ("/", Div)
  , ("==", Eq)
  , ("!=", Neq)
  , (">", Gt)
  , ("<", Lt)
  , (">=", Geq)
  , ("<=", Leq)
  , ("&&", And)
  , ("||", Or)
  ]

allocateSc :: TiHeap -> CoreSupercombinatorDefinition -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
  where
    (heap', addr) = hAlloc heap (NSupercomb name args body)

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim) =
  (heap2, (name, addr))
  where (heap2, addr) = hAlloc heap $ NPrim name prim

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
    dispatch (NPrim name prim)         = primStep state prim

numStep :: TiState -> Int -> TiState
numStep (stack, stack2:dump, heap, globals, stats) n =
  (stack2, dump, heap, globals, stats)

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2 =
  apDispatch (hLookup heap a2)
  where
    apDispatch (NInd a3) = (stack, dump, heap2, globals, stats)
      where heap2 = hUpdate heap apNode (NAp a1 a3)
            apNode = head stack
    apDispatch node = (a1 : stack, dump, heap, globals, stats)

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

-- data Primitive = Neg | Add | Sub | Mul | Div | Eq | Neq | Gt | Lt | Geq | Leq | And | Or
primStep :: TiState -> Primitive -> TiState
primStep state Neg = primUnary state negateNode
  where negateNode (NNum x) = (NNum (-x))
primStep state op = primBinary state $ case op of
  Add -> \(NNum a) (NNum b) -> (NNum $ a + b)
  Sub -> \(NNum a) (NNum b) -> (NNum $ a - b)
  Mul -> \(NNum a) (NNum b) -> (NNum $ a * b)
  Div -> \(NNum a) (NNum b) -> (NNum $ a `div` b)
  Eq  -> \(NNum a) (NNum b) -> (NNum $ if a == b then 1 else 0) -- TODO: introduce real booleans
  Neq -> \(NNum a) (NNum b) -> (NNum $ if a /= b then 1 else 0)
  Gt  -> \(NNum a) (NNum b) -> (NNum $ if a > b then 1 else 0)
  Lt  -> \(NNum a) (NNum b) -> (NNum $ if a < b then 1 else 0)
  Geq -> \(NNum a) (NNum b) -> (NNum $ if a >= b then 1 else 0)
  Leq -> \(NNum a) (NNum b) -> (NNum $ if a <= b then 1 else 0)
  And -> \(NNum a) (NNum b) -> (NNum $ if a /= 0 && b /= 0 then 1 else 0)
  Or -> \(NNum a) (NNum b) -> (NNum $ if a /= 0 || b /= 0 then 1 else 0)

primUnary :: TiState -> (Node -> Node) -> TiState
primUnary (stack, dump, heap, globals, stats) op
  | length args /= 1 = error $ "wrong number of args for unary operator"
  | not (isDataNode argNode) = ([argAddr], newStack:dump, heap, globals, stats)
  | otherwise = (newStack, dump, newHeap, globals, stats)
  where
    args = getArgs heap stack
    [argAddr] = args
    argNode = hLookup heap argAddr
    newStack = drop 1 stack
    rootOfRedex = head newStack
    newHeap = hUpdate heap rootOfRedex $ op argNode

primBinary :: TiState -> (Node -> Node -> Node) -> TiState
primBinary (stack, dump, heap, globals, stats) op
  | length args /= 2 = error $ "wrong number of args for binary operator"
  | not (isDataNode arg1Node) = ([arg1addr], newStack:dump, heap, globals, stats)
  | not (isDataNode arg2Node) = ([arg2addr], newStack:dump, heap, globals, stats)
  | otherwise = (newStack, dump, newHeap, globals, stats)
  where
    args = getArgs heap stack
    [arg1addr, arg2addr] = args
    arg1Node = hLookup heap arg1addr
    arg2Node = hLookup heap arg2addr
    newStack = drop 2 stack
    rootOfRedex = head newStack
    newHeap = hUpdate heap rootOfRedex $ op arg1Node arg2Node

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
showState (stack, dump, heap, _, _) = Text.concat [showStack heap stack, "\n", showDump dump, "\n"]

showStack :: TiHeap -> TiStack -> Text
showStack heap stack = Text.concat [
    "Stk [",
    Text.intercalate "\n" (map showStackItem stack),
    " ]"
    ]
  where
    showStackItem addr = Text.concat [ showFWAddr addr, ": ",
                                   showStkNode heap (hLookup heap addr)]

showDump :: TiDump -> Text
showDump dump = "Dump depth " <> tshow (length dump)

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
showNode (NPrim name prim) = "NPrim " <> name

showFinalNode :: Node -> Text
showFinalNode (NAp a1 a2) = Text.concat ["<application:", showAddr a1, ",", showAddr a2, ">"]
showFinalNode (NSupercomb name _ _) = "<combinator:" <> name <> ">"
showFinalNode (NInd addr) = "<indirection:" <> tshow addr <> ">"
showFinalNode (NNum n) = tshow n
showFinalNode (NPrim name prim) = "<primitive:(" <> name <> ")>"

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
