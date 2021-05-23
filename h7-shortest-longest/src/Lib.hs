{-# LANGUAGE ViewPatterns #-}

module Lib ( shortestLongest ) where

import Control.Arrow ( (&&&) )

data Nat = Z | S Nat
  deriving Eq

instance Ord Nat where
  compare Z     Z     = EQ
  compare Z     (S _) = LT
  compare (S _) Z     = GT
  compare (S n) (S m) = compare n m

  max Z n = n
  max n Z = n
  max (S n) (S m) = S (max n m)

  min Z n = Z
  min n Z = Z
  min (S n) (S m) = S (min n m)

len :: [a] -> Nat
len = foldr (const S) Z

shortestLongest :: [[[a]]] -> [[a]]
shortestLongest = s . fmap l
  where
    l (id &&& maximum . fmap len -> (x, mx)) = (mx, filter ((== mx) . len) x)
    s (id &&& minimum . fmap fst -> (x, mn)) = foldMap snd $ filter ((== mn) . fst) x
