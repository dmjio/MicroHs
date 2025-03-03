{-# OPTIONS_GHC -Wno-name-shadowing #-}
--
-- Balanced binary trees
-- Similar to Data.Map
-- Based on https://ufal.mff.cuni.cz/~straka/papers/2011-bbtree.pdf
--
module MicroHs.IdentMap(
  Map,
  empty, singleton,
  insertWith, insert,
  fromListWith, fromList,
  delete,
  lookup,
  size,
  toList, elems, keys,
  ) where
import Prelude hiding(lookup)
import MicroHs.Ident

data Map a
  = Nil           -- empty tree
  | One Ident a   -- singleton
  | Node          -- tree node
    (Map a)        -- left subtree
    Int            -- size of this tree
    Ident          -- key stored in the node
    a              -- element stored in the node
    (Map a)        -- right subtree
  deriving(Show)

empty :: forall a . Map a
empty = Nil

singleton :: forall a . Ident -> a -> Map a
singleton i a = One i a

elems :: forall v . Map v -> [v]
elems = map snd . toList

keys :: forall v . Map v -> [Ident]
keys = map fst . toList

toList :: forall v . Map v -> [(Ident, v)]
toList t = to t []
  where
    to Nil q = q
    to (One k v) q = (k, v):q
    to (Node l _ k v r) q = to l ((k, v) : to r q)

fromList :: forall v . [(Ident, v)] -> Map v
fromList = fromListWith const

fromListWith :: forall v . (v -> v -> v) -> [(Ident, v)] -> Map v
fromListWith comb = foldr (uncurry (insertWith comb)) empty

size :: forall a . Map a -> Int
size Nil = 0
size (One _ _) = 1
size (Node _ s _ _ _) = s

node :: forall a . Map a -> Ident -> a -> Map a -> Map a
node Nil  key val Nil   = One key val
node left key val right = Node left (size left + 1 + size right) key val right

lookup :: forall a . Ident -> Map a -> Maybe a
lookup k = look
  where
    look Nil = Nothing
    look (One key val) =
      case compare k key of
        EQ -> Just val
        _  -> Nothing
    look (Node left _ key val right) =
      case k `compare` key of
        LT -> look left
        EQ -> Just val
        GT -> look right

insert :: forall a . Ident -> a -> Map a -> Map a
insert = insertWith const

insertWith :: forall a . (a -> a -> a) -> Ident -> a -> Map a -> Map a
insertWith comb k v = ins
  where
    ins Nil = One k v
    ins (One a v) = ins (Node Nil 1 a v Nil)
    ins (Node left _ key val right) =
      case k `compare` key of
        LT -> balance (ins left) key val right
        EQ -> node left k (comb v val) right
        GT -> balance left key val (ins right)

delete :: forall a . Ident -> Map a -> Map a
delete k = del
  where
    del Nil = Nil
    del t@(One a _) | (k `compare` a) == EQ = Nil
                    | otherwise        = t
    del (Node left _ key val right) =
      case k `compare` key of
        LT -> balance (del left) key val right
        EQ -> glue left right
        GT -> balance left key val (del right)
      where
        glue Nil right = right
        glue left Nil = left
        glue left right
          | size left > size right =
            let (key', val', left') = extractMax left
            in node left' key' val' right
          | otherwise =
            let (key', val', right') = extractMin right
            in node left key' val' right'

extractMin :: forall a . Map a -> (Ident, a, Map a)
extractMin Nil = undefined
extractMin (One key val) = (key, val, Nil)
extractMin (Node Nil _ key val right) = (key, val, right)
extractMin (Node left _ key val right) =
  case extractMin left of
    (min, vmin, left') -> (min, vmin, balance left' key val right)

extractMax :: forall a . Map a -> (Ident, a, Map a)
extractMax Nil = undefined
extractMax (One key val) = (key, val, Nil)
extractMax (Node left _ key val Nil) = (key, val, left)
extractMax (Node left _ key val right) =
  case extractMax right of
    (max, vmax, right') -> (max, vmax, balance left key val right')

omega :: Int
omega = 3
alpha :: Int
alpha = 2
delta :: Int
delta = 0

balance :: forall a . Map a -> Ident -> a -> Map a -> Map a
balance left key val right
  | size left + size right <= 1 = node left key val right
balance (One k v) key val right = balance (Node Nil 1 k v Nil) key val right
balance left key val (One k v)  = balance left key val (Node Nil 1 k v Nil)
balance left key val right
  | size right > omega * size left + delta =
      case right of
        (Node rl _ _ _ rr) | size rl < alpha*size rr -> singleL left key val right
                           | otherwise -> doubleL left key val right
        _ -> undefined
  | size left > omega * size right + delta =
      case left of
        (Node ll _ _ _ lr) | size lr < alpha*size ll -> singleR left key val right
                           | otherwise -> doubleR left key val right
        _ -> undefined
  | otherwise = node left key val right

singleL :: forall a . Map a -> Ident -> a -> Map a -> Map a
singleL l k v (Node rl _ rk rv rr) = node (node l k v rl) rk rv rr
singleL _ _ _ _ = undefined

singleR :: forall a . Map a -> Ident -> a -> Map a -> Map a
singleR (Node ll _ lk lv lr) k v r = node ll lk lv (node lr k v r)
singleR _ _ _ _ = undefined

doubleL :: forall a . Map a -> Ident -> a -> Map a -> Map a
doubleL l k v (Node (Node rll _ rlk rlv rlr) _ rk rv rr) = node (node l k v rll) rlk rlv (node rlr rk rv rr)
doubleL l k v (Node (One        rlk rlv    ) _ rk rv rr) = node (node l k v Nil) rlk rlv (node Nil rk rv rr)
doubleL _ _ _ _ = undefined

doubleR :: forall a . Map a -> Ident -> a -> Map a -> Map a
doubleR (Node ll _ lk lv (Node lrl _ lrk lrv lrr)) k v r = node (node ll lk lv lrl) lrk lrv (node lrr k v r)
doubleR (Node ll _ lk lv (One        lrk lrv    )) k v r = node (node ll lk lv Nil) lrk lrv (node Nil k v r)
doubleR _ _ _ _ = undefined
