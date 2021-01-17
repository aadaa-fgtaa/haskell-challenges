{-# LANGUAGE BlockArguments, RankNTypes #-}

module Lib ( forceElems ) where

newtype UCont a = UCont { runUCont :: forall r . (a -> r) -> r }

instance Functor UCont where
  fmap f x = UCont ($ runUCont x f)

instance Applicative UCont where
  pure x = UCont ($ x)
  (<*>) = fmap . flip runUCont id

forceElems :: Traversable t => t a -> t a
forceElems = flip runUCont id . traverse \a -> UCont ($! a)
