{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Test (Foo (..)) where

import Plutus.V1.Ledger.Value (Value)
import PlutusTx.Prelude
import PlutusTx.Skeleton.Internal (
  Skeletal (skeletize),
  Skeleton (Skeleton),
  runSkeleton,
 )

data Foo = Foo
  { bar :: Integer
  , baz :: Value
  , quux :: [BuiltinString]
  }

instance Eq Foo where
  {-# INLINEABLE (==) #-}
  x == y =
    bar x == bar y
      && baz x == baz y
      && quux x == quux y

instance Skeletal Foo where
  skeletize (Foo x y z) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let x' = runSkeleton (skeletize x) bCont iCont sCont conCont recCont tupCont lsCont
              y' = runSkeleton (skeletize y) bCont iCont sCont conCont recCont tupCont lsCont
              z' = runSkeleton (skeletize z) bCont iCont sCont conCont recCont tupCont lsCont
           in conCont "Foo" [x', y', z']
      )
