{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize #-}

module PlutusTx.Skeleton.Builder (renderSkeleton) where

import PlutusTx.Prelude
import PlutusTx.Skeleton.Internal (Skeleton (runSkeleton))
import PlutusTx.Skeleton.String (intToString)

{-
newtype Builder = Builder [BuiltinString]
  deriving (Semigroup, Monoid) via [BuiltinString]

{-# INLINEABLE build #-}
build :: Builder -> BuiltinString
build (Builder xs) = foldr go "" xs
  where
    go :: BuiltinString -> BuiltinString -> BuiltinString
    go x acc = x <> acc
-}

{-# INLINEABLE renderSkeleton #-}
renderSkeleton :: Skeleton -> BuiltinString
renderSkeleton sk =
  runSkeleton
    sk
    renderBool
    renderInteger
    renderString
    renderCon
    renderRec
    renderTup
    renderList
  where
    renderBool :: Bool -> BuiltinString
    renderBool b = if b then "true" else "false"
    renderInteger :: Integer -> BuiltinString
    renderInteger = intToString
    renderString :: BuiltinString -> BuiltinString
    renderString s = "\"" <> s <> "\""
    renderCon :: BuiltinString -> [BuiltinString] -> BuiltinString
    renderCon name args =
      "{ \"tag\": \""
        <> name
        <> "\", \"arguments\": "
        <> renderArray args
        <> " }"
    renderRec :: BuiltinString -> [(BuiltinString, BuiltinString)] -> BuiltinString
    renderRec name fieldVals =
      "{ \"recordTag\": \""
        <> name
        <> "\", \"fields\": "
        <> renderFieldVals fieldVals
        <> " }"
    renderTup :: BuiltinString -> BuiltinString -> Maybe BuiltinString -> BuiltinString
    renderTup x y mz =
      "{ \"fst\": " <> x <> ", \"snd\": " <> y <> case mz of
        Nothing -> " }"
        Just z -> ", \"thd\": " <> z <> " }"
    renderList :: [BuiltinString] -> BuiltinString
    renderList = renderArray

{-# INLINEABLE renderArray #-}
renderArray :: [BuiltinString] -> BuiltinString
renderArray xs = "[ " <> go xs <> " ]"
  where
    go :: [BuiltinString] -> BuiltinString
    go = \case
      [] -> ""
      (y : ys) -> y <> ", " <> go ys

{-# INLINEABLE renderFieldVals #-}
renderFieldVals :: [(BuiltinString, BuiltinString)] -> BuiltinString
renderFieldVals fieldVals = "{ " <> go fieldVals <> " }"
  where
    go :: [(BuiltinString, BuiltinString)] -> BuiltinString
    go = \case
      [] -> ""
      (k, v) : kvs -> "\"" <> k <> "\": " <> v <> ", " <> go kvs

{-
renderSkeleton = \case
  BoolS b -> embed (if b then "true" else "false")
  IntegerS i -> embed (intToString i)
  StringS bis -> embed ("\"" <> bis <> "\"")
  ConS conName conArgs -> renderTagged conName conArgs
  RecS recConName fieldVals -> renderKV recConName fieldVals
  TupleS x y z -> renderTuple x y z
  ListS xs -> renderArray xs

{-# INLINEABLE renderTagged #-}
renderTagged :: BuiltinString -> [Skeleton] -> Builder
renderTagged name args =
  embed "{ \"tag\":"
    <+> embed ("\"" <> name <> "\",")
    <+> embed "\"arguments\":"
    <+> renderArray args
    <+> embed "}"

{-# INLINEABLE renderKV #-}
renderKV :: BuiltinString -> [(BuiltinString, Skeleton)] -> Builder
renderKV name fieldVals =
  embed "{ \"recordTag\":"
    <+> embed ("\"" <> name <> "\",")
    <+> embed "\"fields\":"
    <+> renderFieldVals fieldVals
    <+> embed "}"

{-# INLINEABLE renderTuple #-}
renderTuple :: Skeleton -> Skeleton -> Maybe Skeleton -> Builder
renderTuple x y mz =
  embed "{ \"fst\":"
    <+> (renderSkeleton x <> embed ",")
    <+> embed "\"snd\":"
    <+> ( renderSkeleton y <> case mz of
            Nothing -> embed " }"
            Just z ->
              embed ", \"thd\":"
                <+> renderSkeleton z
                <+> embed "}"
        )

{-# INLINEABLE renderArray #-}
renderArray :: [Skeleton] -> Builder
renderArray xs = embed "[" <+> go xs <+> embed "]"
  where
    go :: [Skeleton] -> Builder
    go = \case
      [] -> mempty
      (y : ys) -> renderSkeleton y <> embed ", " <> go ys

{-# INLINEABLE renderFieldVals #-}
renderFieldVals :: [(BuiltinString, Skeleton)] -> Builder
renderFieldVals fieldVals = embed "{" <+> go fieldVals <+> embed "}"
  where
    go :: [(BuiltinString, Skeleton)] -> Builder
    go = \case
      [] -> mempty
      (k, v) : kvs -> embed ("\"" <> k <> "\":") <+> renderSkeleton v <+> embed "," <+> go kvs

-- Space
{-# INLINEABLE (<+>) #-}
(<+>) :: Builder -> Builder -> Builder
xs <+> ys = xs <> embed " " <> ys

infixl 6 <+>

{-# INLINEABLE embed #-}
embed :: BuiltinString -> Builder
embed s = Builder [s] -}
