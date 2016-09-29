This module provides type-level finite maps.
The implementation is similar to that shown in the paper.
 "Embedding effect systems in Haskell" Orchard, Petricek 2014

> {-# LANGUAGE TypeOperators, PolyKinds, DataKinds, KindSignatures,
>              TypeFamilies, UndecidableInstances, MultiParamTypeClasses,
>              FlexibleInstances #-}

> module Data.Type.FiniteMap (Union, DisjointUnion, Map(..),
>                             Lookup, Member, Combine, Cmp, (:\), (:++)) where

> import Data.Type.Set hiding (X, Y, Z, (:->), Nub, Union, Append)

> -- Mappings
> infixr 4 :->
> data Map k v = k :-> v

Throughout, type variables
   'k' ranges over "keys"
   'v'  ranges over "values"
   'kvp' ranges over "key-value-pairs"
   'm', 'n' range over "maps"

> -- Append type-level lists
> type family (:++) (x :: [a]) (y :: [a]) :: [a] where
>             '[]       :++ xs = xs
>             (x ': xs) :++ ys = x ': (xs :++ ys)

> type family Combine (a :: v) (b :: v) :: v

> -- Combines repeated channel mappings
> type family Nub t where
>    Nub '[]                             = '[]
>    Nub '[kvp]                          = '[kvp]
>    Nub ((k :-> v1) ': (k :-> v2) ': m) = Nub ((k :-> Combine v1 v2) ': m)
>    Nub (kvp1 ': kvp2 ': s)             = kvp1 ': Nub (kvp2 ': s)

> -- Removes repeated channel mappings
> type family NubEq t where
>    NubEq '[]                             = '[]
>    NubEq '[kvp]                          = '[kvp]
>    NubEq ((k :-> v) ': (k :-> v) ': m)   = NubEq ((k :-> v) ': m)
>    NubEq (kvp1 ': kvp2 ': s)             = kvp1 ': NubEq (kvp2 ': s)

Union of two finite maps

> type Union m n = Nub (Sort (m :++ n))

Union of two finite maps that have either different keys, or when the keys
conincide so must the values

> type DisjointUnion m n = NubEq (Sort (m :++ n))

Delete elements from a map

> type family (m :: [Map k v]) :\ (c :: k) :: [Map k v] where
>      '[]               :\ k = '[]
>      ((k :-> v) ': m) :\ k  = m :\ k
>      (kvp ': m)         :\ k = kvp ': (m :\ k)

Lookup elements from a map

> type family Lookup (m :: [Map k v]) (c :: k) :: Maybe v where
>             Lookup '[]              k = Nothing
>             Lookup ((k :-> v) ': m) k = Just v
>             Lookup (kvp ': m)       k = Lookup m k

Membership

> type family Member (c :: k) (m :: [Map k v]) :: Bool where
>             Member k '[]              = False
>             Member k ((k :-> v) ': m) = True
>             Member k (kvp ': m)       = Member k m
