module Heap
   (Heap
   , Addr
   , hInitial
   , hAlloc
   , hUpdate
   , hSize
   , hFree
   , hAddresses
   , hLookup
   )
   where

type Addr = Int

data Heap a = Heap Addr [Addr] [(Addr, a)]

hInitial :: Heap a
hInitial = Heap 0 [1..] []

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap size (next : free) xs) x =  (Heap (size + 1) free  ((next, x) : xs), next)
hAlloc (Heap _ [] _) _ = error "Heap.hs:hAlloc - Empty free list"

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (Heap size free xs) a x =
  Heap size free ((a,x) : remove xs a)

hFree :: Heap a -> Addr -> Heap a
hFree (Heap size free xs) a =
  Heap (size - 1) (a:free) (remove xs a)

remove :: [(Int, a)] -> Int -> [(Int, a)]
remove [] adr = error ("Heap.remove - Attemot to update or free nonexistent address"
              ++ show adr)
remove ((a, x) : xs) adr
    | a == adr  = xs
    | otherwise = (a,x) : remove xs adr

hSize :: Heap a -> Int
hSize (Heap size _ _) = size

hAddresses :: Heap a -> [Addr]
hAddresses (Heap _ _ xs) = [addr | (addr, _) <- xs]

hLookup :: Heap a -> Addr -> a
hLookup (Heap _ _ xs) a = aLookup xs a

aLookup :: (Eq a, Show a) => [(a, b)] -> a -> b
aLookup [] a = error ("Heap lookup can't find address " ++ show a)
aLookup ((x, y):xs) a = if a == x then y else aLookup xs a
