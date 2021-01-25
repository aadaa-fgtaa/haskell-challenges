{-# LANGUAGE GADTs, ViewPatterns #-}

module Lib where

import Data.Proxy ( Proxy(..) )
import Type.Reflection ( Typeable )

data Scheme a where
  Res :: Typeable a => Proxy a -> Scheme a
  Arg :: Typeable a => Proxy a -> Scheme b -> Scheme (a -> b)

data Function = forall a . Function (Scheme a) a

newtype Wrap a = Wrap { unWrap :: a }

data WScheme a b where
  WRes :: Typeable a => Proxy a -> WScheme (Wrap a) a
  WArg :: Typeable a => Proxy a -> WScheme wb b -> WScheme (Wrap a -> wb) (a -> b)

data SomeWScheme a = forall b . SomeWScheme (WScheme b a)

wrapWithWScheme :: WScheme a b -> b -> a
wrapWithWScheme (WRes _)   f = Wrap f
wrapWithWScheme (WArg _ r) f = wrapWithWScheme r . f . unWrap

toWScheme :: Scheme a -> SomeWScheme a
toWScheme (Res p) = SomeWScheme $ WRes p
toWScheme (Arg p (toWScheme -> SomeWScheme r)) = SomeWScheme $ WArg p r

fromWScheme :: WScheme a b -> Scheme a
fromWScheme (WRes _) = Res Proxy
fromWScheme (WArg _ (fromWScheme -> r)) = Arg Proxy r

wrapFunction :: Function -> Function
wrapFunction (Function (toWScheme -> SomeWScheme w) f) = Function (fromWScheme w) (wrapWithWScheme w f)
