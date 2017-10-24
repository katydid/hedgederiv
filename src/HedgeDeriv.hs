module HedgeDeriv (
    Pattern(..), Refs(..), Var(..), Name(..), reachable, newRef, union, isLinear
) where

import qualified Data.Map.Strict as DataMap
import qualified Data.Set as DataSet
import qualified Debug.Trace as Debug

newtype Var = Var String -- x
    deriving (Show, Ord, Eq)

newtype Name = Name String -- n
    deriving (Show, Ord, Eq)

data Pattern a = EmptySet -- 0
    | EmptyPattern -- 1
    | NodePattern a Name -- a[n]
    | Concat (Pattern a) (Pattern a) -- p . p
    | Or (Pattern a) (Pattern a) -- p + p
    | ZeroOrMore (Pattern a) -- p*
    | Capture (Pattern a) Var -- p@x
    deriving (Show, Eq)

data Hedge a = Empty -- \epsilon
    | Node a (Hedge a) (Hedge a) -- a[h] h
    deriving (Show, Eq)

newtype Refs a = Refs (DataMap.Map Name (Pattern a))
    deriving (Show, Eq)

-- |
-- lookupRef looks up a pattern in the reference map, given a reference name.
lookupRef :: Refs a -> Name -> Pattern a
lookupRef (Refs m) name = m DataMap.! name

-- |
-- reverseLookupRef returns the reference name for a given pattern.
reverseLookupRef :: (Eq a) => Pattern a -> Refs a -> Maybe Name
reverseLookupRef p (Refs m) = case DataMap.keys $ DataMap.filter (== p) m of
    []      -> Nothing
    (k:_)  -> Just k

-- |
-- newRef returns a new reference map given a single pattern and its reference name.
newRef :: Name -> Pattern a -> Refs a
newRef key value = Refs $ DataMap.singleton key value

-- |
-- emptyRef returns an empty reference map.
emptyRef :: Refs a
emptyRef = Refs DataMap.empty

-- |
-- union returns the union of two reference maps.
union :: Refs a -> Refs a -> Refs a
union (Refs m1) (Refs m2) = Refs $ DataMap.union m1 m2 

reachable :: Refs a -> Pattern a -> DataSet.Set Var
reachable refs EmptySet = DataSet.empty
reachable refs EmptyPattern = DataSet.empty
reachable refs (NodePattern _ name) = reachable refs (lookupRef refs name)
reachable refs (Concat p1 p2) = DataSet.union (reachable refs p1) (reachable refs p2)
reachable refs (Or p1 p2) = DataSet.union (reachable refs p1) (reachable refs p2)
reachable refs (ZeroOrMore p) = reachable refs p
reachable refs (Capture p x) = DataSet.singleton x `DataSet.union` (reachable refs p)

isLinear :: Refs a -> Pattern a -> Bool
isLinear refs EmptySet = True
isLinear refs EmptyPattern = True
isLinear refs (NodePattern _ name) = isLinear refs (lookupRef refs name)
isLinear refs (Concat p1 p2) =
    let vp1 = reachable refs p1
        vp2 = reachable refs p2
    in DataSet.null (vp1 `DataSet.intersection` vp2) && isLinear refs p1 && isLinear refs p2
isLinear refs (Or p1 p2) =
    let vp1 = reachable refs p1
        vp2 = reachable refs p2
    in ((debug vp1) == (debug vp2)) && isLinear refs p1 && isLinear refs p2
isLinear refs (ZeroOrMore p1) = DataSet.null (reachable refs p1)
isLinear refs (Capture p1 x) =
    let vp1 = reachable refs p1
    in (not $ DataSet.member x vp1) && isLinear refs p1

debug :: (Show a) => a -> a
debug v = Debug.trace (show v) v