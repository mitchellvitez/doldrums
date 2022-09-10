{-# language FlexibleInstances #-}

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

instance {-# OVERLAPPING #-} Show TiState where
  show (stack, dump, _heap, globals, stats) = show (stack, dump, globals, stats)

type TiStack = [Addr]

type TiDump = [TiStack]

initialTiDump :: TiDump
initialTiDump = []

type TiHeap = Heap Node

type Assoc a b = [(a, b)]

type TiGlobals = Assoc Text Addr

newtype TiStats = TiStats Int
  deriving Show

type Primitive = TiState -> TiState

instance Show Primitive where
  show _ = "<<primitive>>"

type Tag = Int

data Node
  = NAp Addr Addr
  | NSupercomb Text [Text] Expr
  | NLit Value
  | NInd Addr
  | NPrim Name Primitive
  | NData Tag [Addr]
  deriving Show

assocLookup :: Eq a => a -> Assoc a b -> Text -> b
assocLookup _ [] errMsg = error $ show errMsg
assocLookup a ((x, y):xs) errMsg =
  if a == x then y else assocLookup a xs errMsg

compile :: Program -> Program -> TiState
compile prelude program = (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
  where
    initialStack = [addressOfMain]
    (initialHeap, globals) = buildInitialHeap scDefs
    addressOfMain = assocLookup "main" globals "main is not defined"
    scDefs = program ++ prelude

buildInitialHeap :: [SupercombinatorDefinition] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs =
  (heap2, scAddrs ++ primAddrs)
  where
    (heap1, scAddrs) = mapAccumL allocateSc hInitial scDefs
    (heap2, primAddrs) = mapAccumL allocatePrim heap1 primitives

allocateSc :: TiHeap -> SupercombinatorDefinition -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, Annotated body _) = (heap', (name, addr))
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
tiFinal ([soleAddr], [], heap, _, _) = isDataNode (hLookup heap soleAddr)
tiFinal ([], _, _, _, _) = error "Empty stack"
tiFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NLit _)    = True
isDataNode (NData t c) = True
isDataNode _           = False

step :: TiState -> TiState
step state@(stack, _, heap, _, _) =
  dispatch (hLookup heap (head stack))
  where
    dispatch (NLit n)                 = literalStep state n
    dispatch (NAp a1 a2)               = apStep state a1 a2
    dispatch (NSupercomb sc args body) = scStep state sc args body
    dispatch (NInd addr)               = indStep state addr
    dispatch (NPrim name prim)         = primStep state prim
    dispatch (NData tag compts)        = dataStep state tag compts

literalStep :: TiState -> Value -> TiState
literalStep (stack, stack2:dump, heap, globals, stats) n =
  (stack2, dump, heap, globals, stats)

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2 =
  apDispatch (hLookup heap a2)
  where
    apDispatch (NInd a3) = (stack, dump, heap2, globals, stats)
      where heap2 = hUpdate heap apNode (NAp a1 a3)
            apNode = head stack
    apDispatch node = (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> Expr -> TiState
scStep (stack, dump, heap, globals, stats) sc argNames body
     = (newStack, dump, newHeap, globals, stats)
  where
    newStack = drop (length argNames) stack
    root = head newStack
    newHeap = instantiateAndUpdate body root heap (bindings ++ globals)
    bindings = zip argNames (getArgs heap stack)

indStep :: TiState -> Addr -> TiState
indStep (a : stack, dump, heap, globals, stats) a2 =
  (a2 : stack, dump, heap, globals, stats)

dataStep :: TiState -> Tag -> [Addr] -> TiState
dataStep (stack, stack2:dump, heap, globals, stats) tag compts =
  (stack2, dump, heap, globals, stats)

primStep :: TiState -> Primitive -> TiState
primStep state prim = prim state

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (_ : stack) = map getArg stack
  where
    getArg addr = arg where (NAp _ arg) = hLookup heap addr
getArgs _ [] = []

instantiate :: Expr -> TiHeap -> Assoc Text Addr -> (TiHeap, Addr)
instantiate (ExprLiteral v) heap _ = hAlloc heap (NLit v)
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
instantiate (ExprLambda _ _ ) _ _ = error "Can't instantiate lambda exprs"

instantiateAndUpdate :: Expr -> Addr -> TiHeap -> Assoc Text Addr -> TiHeap
instantiateAndUpdate (ExprLiteral v) updateAddr heap env =
  hUpdate heap updateAddr (NLit v)
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
  hUpdate heap updateAddr . NPrim "Cons" $ primConstr tag arity

instantiateConstr :: Tag -> Int -> TiHeap -> Assoc Text Addr -> (TiHeap, Addr)
instantiateConstr tag arity heap env =
  hAlloc heap . NPrim "Cons" $ primConstr tag arity

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

tshow :: Show a => a -> Text
tshow = pack . show

showNode :: Node -> Text
showNode (NAp a1 a2) = Text.concat ["NAp ", showAddr a1, " ", showAddr a2]
showNode (NSupercomb name _ _) = "NSupercomb " <> name
showNode (NLit n) = "NLit " <> tshow n
showNode (NInd addr) = "NInd " <> tshow addr
showNode (NPrim name prim) = "NPrim " <> name
showNode (NData tag compts) = "NData " <> tshow tag <> " [" <> Text.intercalate "," (map showAddr compts) <> "]"

showFinalNode :: Node -> Text
showFinalNode (NAp a1 a2) = Text.concat ["<application:", showAddr a1, ",", showAddr a2, ">"]
showFinalNode (NSupercomb name _ _) = "<combinator:" <> name <> ">"
showFinalNode (NInd addr) = "<indirection:" <> tshow addr <> ">"
showFinalNode (NLit n) = case n of
  ValueInt x -> tshow x
  ValueDouble x -> tshow x
  ValueBool x -> tshow x
  ValueString x -> tshow x
showFinalNode (NPrim name prim) = "<primitive:(" <> name <> ")>"
showFinalNode node@(NData tag compts) = "<data:{" <> tshow tag <> "," <> Text.intercalate "\n" (map tshow compts) <> "}>"

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

-----------------------------------------------------------
-- PRIMITIVES ---------------------------------------------
-----------------------------------------------------------

primitives :: Assoc Name Primitive
primitives =
  [ ("negate", primMonadic negateNode)
  , ("~", primMonadic negateNode)
  , ("!", primMonadic notNode)

  , ("+", primArith (+))
  , ("-", primArith (-))
  , ("*", primArith (*))
  , ("/", primArith (div))

  , ("+.", primArithFloat (+))
  , ("-.", primArithFloat (-))
  , ("*.", primArithFloat (*))
  , ("/.", primArithFloat (/))

  , ("==", primComp (==))
  , ("!=", primComp (/=))
  , (">", primComp (>))
  , ("<", primComp (<))
  , (">=", primComp (>=))
  , ("<=", primComp (<=))

  , ("$", primApply)

  , ("&&", primBool (&&))
  , ("||", primBool (||))

  , ("if", primIf)
  , ("casePair", primCasePair)
  , ("caseList", primCaseList)
  , ("abort", error "encountered abort primitive")
  ]
  where negateNode (NLit (ValueInt x)) = NLit (ValueInt (negate x))
        negateNode (NLit (ValueDouble x)) = NLit (ValueDouble (negate x))
        negateNode _ = error "bad value for negateNode"
        notNode (NLit (ValueBool x)) = NLit (ValueBool (not x))
        notNode _ = error "bad value for notNode"

primArith :: (Integer -> Integer -> Integer) -> TiState -> TiState
primArith op state = primDyadic op' state
  where op' (NLit (ValueInt n)) (NLit (ValueInt m)) = NLit (ValueInt (op n m))
        op' _ _ = error "bad type passed to arithmetic operator"

primArithFloat :: (Double -> Double -> Double) -> TiState -> TiState
primArithFloat op state = primDyadic op' state
  where op' (NLit (ValueDouble n)) (NLit (ValueDouble m)) = NLit (ValueDouble (op n m))
        op' _ _ = error "bad type passed to arithmetic operator"

doldrumsTrue = NData 2 []
doldrumsFalse = NData 1 []

toDoldrumsBool :: Bool -> Node
toDoldrumsBool x = if x then doldrumsTrue else doldrumsFalse

fromDoldrumsBool :: Node -> Bool
fromDoldrumsBool x = case x of
  NData 2 [] -> True
  NData 1 [] -> False
  _ -> error "tried to evaluate a non-bool as a bool"

primComp :: (Value -> Value -> Bool) -> TiState -> TiState
primComp op state = primDyadic op' state
  where op' (NLit n) (NLit m) = toDoldrumsBool $ op n m
        op' _ _ = error "bad type passed to arithmetic operator"

primBool :: (Bool -> Bool -> Bool) -> TiState -> TiState
primBool op state = primDyadic op' state
  where op' a@(NData _ []) b@(NData _ []) =
          toDoldrumsBool $ op (fromDoldrumsBool a) (fromDoldrumsBool b)
        op' _ _ = error "bad type passed to arithmetic operator"

primMonadic :: (Node -> Node) -> TiState -> TiState
primMonadic op (stack, dump, heap, globals, stats)
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

primDyadic :: (Node -> Node -> Node) -> TiState -> TiState
primDyadic op (stack, dump, heap, globals, stats)
  | length args /= 2 = error $ "wrong number of args for dyadic operator"
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

primApply :: TiState -> TiState
primApply (stack, dump, heap, globals, stats)
  | length args < 2 = error $ "wrong number of args for $"
  | otherwise = (newStack, dump, newHeap, globals, stats)
  where
    args = getArgs heap stack
    (arg1addr:arg2addr:restArgs) = args
    arg1node = hLookup heap arg1addr
    arg2node = hLookup heap arg2addr
    newStack = drop 2 stack
    rootOfRedex = head newStack
    newHeap = hUpdate heap rootOfRedex $ NAp arg1addr arg2addr

primIf :: TiState -> TiState
primIf (stack, dump, heap, globals, stats)
  | length args < 3 = error "wrong number of args for if"
  | not (isDataNode arg1node) = ([arg1addr], newStack:dump, heap, globals, stats)
  | otherwise = (newStack, dump, newHeap, globals, stats)
  where
    args = getArgs heap stack
    (arg1addr:arg2addr:arg3addr:restArgs) = args
    arg1node = hLookup heap arg1addr
    newStack = drop 3 stack
    rootOfRedex = head newStack
    NData tag [] = arg1node
    resultAddr
      | tag == 2 = arg2addr
      | otherwise = arg3addr
    newHeap = hUpdate heap rootOfRedex $ NInd resultAddr

primCasePair :: TiState -> TiState
primCasePair (stack, dump, heap, globals, stats)
  | length args < 2 = error "wrong number of arguments to casePair"
  | not (isDataNode arg1node) = ([arg1addr], newStack:dump, heap, globals, stats)
  | otherwise = (newStack, dump, newHeap, globals, stats)
  where
    args = getArgs heap stack
    (arg1addr:arg2addr:restArgs) = args
    arg1node = hLookup heap arg1addr
    newStack = drop 2 stack
    rootOfRedex = head newStack
    NData tag [fst,snd] = arg1node
    newHeap = hUpdate heap1 rootOfRedex (NAp tempAddr snd)
      where (heap1, tempAddr) = hAlloc heap (NAp arg2addr fst)

-- TODO: generalize this somehow to allow `caseX` as a primitive
primCaseList :: TiState -> TiState
primCaseList (stack, dump, heap, globals, stats)
  | length args < 3 = error "wrong number of arguments to caseList"
  | not (isDataNode arg1node) = ([arg1addr], newStack:dump, heap, globals, stats)
  | otherwise = (newStack, dump, newHeap, globals, stats)
  where
    args = getArgs heap stack
    (arg1addr:arg2addr:arg3addr:restArgs) = args
    arg1node = hLookup heap arg1addr
    newStack = drop 3 stack
    rootOfRedex = Prelude.head newStack
    NData tag compts = arg1node
    [head,tail] = compts
    newHeap
      | tag == 1 = hUpdate heap rootOfRedex (NInd arg2addr)
      | otherwise = hUpdate heap1 rootOfRedex (NAp tempAddr tail)
        where (heap1, tempAddr) = hAlloc heap (NAp arg3addr head)

primConstr :: Tag -> Int -> TiState -> TiState
primConstr tag arity (stack, dump, heap, globals, stats)
  | length args < arity = error "wrong number of args to constructor"
  | otherwise = (newStack, dump, newHeap, globals, stats)
    where
      args = getArgs heap stack
      newStack = drop arity stack
      rootOfRedex = head newStack
      newHeap = hUpdate heap rootOfRedex (NData tag args)
