{-# LANGUAGE BlockArguments, LambdaCase #-}

module Lib ( Tree (..), materializeForcedBy ) where

import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.Functor ( ($>), void )
import Control.Exception ( evaluate )
import Data.Bool ( bool )

data Tree
    = Leaf
    | Fork Tree Int Tree
    deriving (Show, Eq)

materializeForcedBy :: (Tree -> Int) -> Tree -> Tree
materializeForcedBy f tree_ = unsafePerformIO do
  ctag <- newIORef False
  vtag <- newIORef False

  let
    tagged tag v = unsafePerformIO $ writeIORef tag True $> v

    tagTree = \case
      Leaf       -> tagged ctag Leaf
      Fork l v r -> tagged ctag $ Fork (tagTree l) (tagged vtag v) (tagTree r)

    tree = tagTree tree_
    {-# NOINLINE tree #-}

    isUnevaluated tag x = do
      writeIORef tag False
      void $ evaluate x
      readIORef tag

    trimTree n = isUnevaluated ctag n >>= \cu -> if cu
      then pure Leaf
      else case n of
        Leaf       -> pure Leaf
        Fork l v r -> Fork <$> trimTree l <*> (bool v 0 <$> isUnevaluated vtag v) <*> trimTree r

  void $ evaluate $ f tree

  trimTree tree
{-# NOINLINE materializeForcedBy #-}
