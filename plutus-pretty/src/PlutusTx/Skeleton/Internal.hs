{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- needed for Eq (,,)
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-specialize #-}

module PlutusTx.Skeleton.Internal (
  Skeleton (..),
  Skeletal (..),
) where

import Data.Kind (Type)
import Plutus.V1.Ledger.Address (Address (Address))
import Plutus.V1.Ledger.Contexts (
  ScriptContext (ScriptContext),
  ScriptPurpose (Certifying, Minting, Rewarding, Spending),
  TxInInfo (TxInInfo),
  TxInfo (
    txInfoDCert,
    txInfoData,
    txInfoFee,
    txInfoId,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoSignatories,
    txInfoValidRange,
    txInfoWdrl
  ),
  TxOut (TxOut),
  TxOutRef (TxOutRef),
 )
import Plutus.V1.Ledger.Credential (
  Credential (PubKeyCredential, ScriptCredential),
  StakingCredential (StakingHash, StakingPtr),
 )
import Plutus.V1.Ledger.Crypto (PubKeyHash (PubKeyHash))
import Plutus.V1.Ledger.DCert (
  DCert (
    DCertDelegDeRegKey,
    DCertDelegDelegate,
    DCertDelegRegKey,
    DCertGenesis,
    DCertMir,
    DCertPoolRegister,
    DCertPoolRetire
  ),
 )
import Plutus.V1.Ledger.Interval (
  Extended (Finite, NegInf, PosInf),
  Interval (Interval),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
 )
import Plutus.V1.Ledger.Scripts (
  Datum (Datum),
  DatumHash (DatumHash),
  ValidatorHash (ValidatorHash),
 )
import Plutus.V1.Ledger.Time (POSIXTime (POSIXTime))
import Plutus.V1.Ledger.TxId (TxId (TxId))
import Plutus.V1.Ledger.Value (
  AssetClass (AssetClass),
  CurrencySymbol (CurrencySymbol),
  TokenName (TokenName),
  Value (Value),
 )
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins (matchData)
import PlutusTx.Prelude
import PlutusTx.Skeleton.String (bsToString, intToString)

{- | An \'essentials\' representation of a value's structure, as well as
 content.

 @since 2.1
-}
newtype Skeleton = Skeleton
  { runSkeleton ::
      forall (r :: Type).
      -- Boolean only
      (Bool -> r) ->
      -- Integer only
      (Integer -> r) ->
      -- String only
      (BuiltinString -> r) ->
      -- Constructor (name and args)
      (BuiltinString -> [r] -> r) ->
      -- Record (name and field-value pairs)
      (BuiltinString -> [(BuiltinString, r)] -> r) ->
      -- Tuple (2 or 3)
      (r -> r -> Maybe r -> r) ->
      -- List
      ([r] -> r) ->
      r
  }

{-
data Skeleton
  = BoolS Bool
  | IntegerS Integer
  | StringS BuiltinString
  | ConS BuiltinString [Skeleton]
  | RecS BuiltinString [(BuiltinString, Skeleton)]
  | TupleS Skeleton Skeleton (Maybe Skeleton)
  | ListS [Skeleton]
  deriving stock
    ( -- | @since 2.1
      Prelude.Eq
    , -- | @since 2.1
      Prelude.Show
    )

-- | @since 2.1
instance Eq Skeleton where
  {-# INLINEABLE (==) #-}
  sk == sk' = case (sk, sk') of
    (BoolS b, BoolS b') -> b == b'
    (IntegerS i, IntegerS i') -> i == i'
    (StringS s, StringS s') -> s == s'
    (ConS nam sks, ConS nam' sks') ->
      nam == nam' && sks == sks'
    (RecS nam keyVals, RecS nam' keyVals') ->
      keyVals == keyVals'
        && nam == nam'
    (TupleS x y z, TupleS x' y' z') ->
      x == x' && y == y' && z == z'
    (ListS xs, ListS xs') -> xs == xs'
    _ -> False
-}

{- | Indicates the ability of a type's values to be converted to an
 \'essentials\' structural representation (a 'Skeleton').

 You /cannot/ define instances of this type class directly: instead, use
 'PlutusTx.Skeleton.QQ.makeSkeletal' via TH to construct such instances
 automatically.

 = Laws

 'skeletize' must be a representable functor. Specifically, this means that
 for any @x, y@, @x == y@ if and only if @skeletize x == skeletize y@. All
 instances provided by us, as well as defined via
 'PlutusTx.Skeleton.QQ.makeSkeletal', follow this law.

 @since 2.1
-}
class (Eq a) => Skeletal a where
  skeletize :: a -> Skeleton

-- | @since 2.2
instance (Eq a, Eq b, Eq c) => Eq (a, b, c) where
  {-# INLINEABLE (==) #-}
  (x, y, z) == (x', y', z') = x == x' && y == y' && z == z'

-- | @since 2.2
instance (Skeletal a, Skeletal b, Skeletal c) => Skeletal (a, b, c) where
  {-# INLINEABLE skeletize #-}
  skeletize (x, y, z) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let x' = runSkeleton (skeletize x) bCont iCont sCont conCont recCont tupCont lsCont
              y' = runSkeleton (skeletize y) bCont iCont sCont conCont recCont tupCont lsCont
              z' = runSkeleton (skeletize z) bCont iCont sCont conCont recCont tupCont lsCont
           in tupCont x' y' (Just z')
      )

-- | @since 2.2
instance Skeletal AssetClass where
  {-# INLINEABLE skeletize #-}
  skeletize (AssetClass xs) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let xs' = runSkeleton (skeletize xs) bCont iCont sCont conCont recCont tupCont lsCont
           in conCont "AssetClass" [xs']
      )

-- | @since 2.1
instance Skeletal BuiltinData where
  {-# INLINEABLE skeletize #-}
  skeletize dat = matchData dat mkConstr mkMap mkList mkI mkB
    where
      mkConstr :: Integer -> [BuiltinData] -> Skeleton
      mkConstr ix xs =
        Skeleton
          ( \bCont iCont sCont conCont recCont tupCont lsCont ->
              let conName = "Constr " <> intToString ix
                  xs' = fmap (\x -> runSkeleton (skeletize x) bCont iCont sCont conCont recCont tupCont lsCont) xs
               in conCont conName xs'
          )
      mkMap :: [(BuiltinData, BuiltinData)] -> Skeleton
      mkMap xs =
        Skeleton
          ( \bCont iCont sCont conCont recCont tupCont lsCont ->
              let xs' = runSkeleton (skeletize xs) bCont iCont sCont conCont recCont tupCont lsCont
               in conCont "Map" [xs']
          )
      mkList :: [BuiltinData] -> Skeleton
      mkList xs =
        Skeleton
          ( \bCont iCont sCont conCont recCont tupCont lsCont ->
              let xs' = runSkeleton (skeletize xs) bCont iCont sCont conCont recCont tupCont lsCont
               in conCont "List" [xs']
          )
      mkI :: Integer -> Skeleton
      mkI i = Skeleton (\_ iCont _ conCont _ _ _ -> conCont "I" [iCont i])
      mkB :: BuiltinByteString -> Skeleton
      mkB b =
        Skeleton
          ( \bCont iCont sCont conCont recCont tupCont lsCont ->
              let b' = runSkeleton (skeletize b) bCont iCont sCont conCont recCont tupCont lsCont
               in conCont "B" [b']
          )

-- | @since 2.1
instance Skeletal BuiltinString where
  {-# INLINEABLE skeletize #-}
  skeletize s = Skeleton (\_ _ f _ _ _ _ -> f s)

-- | @since 2.1
instance Skeletal Integer where
  {-# INLINEABLE skeletize #-}
  skeletize i = Skeleton (\_ f _ _ _ _ _ -> f i)

-- | @since 2.1
instance Skeletal Bool where
  {-# INLINEABLE skeletize #-}
  skeletize b = Skeleton (\f _ _ _ _ _ _ -> f b)

-- | @since 2.1
instance Skeletal BuiltinByteString where
  {-# INLINEABLE skeletize #-}
  skeletize bs = Skeleton (\_ _ f _ _ _ _ -> f . bsToString $ bs)

-- | @since 2.1
instance (Skeletal a) => Skeletal (Maybe a) where
  {-# INLINEABLE skeletize #-}
  skeletize m =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont -> case m of
          Nothing -> conCont "Nothing" []
          Just x ->
            let x' = runSkeleton (skeletize x) bCont iCont sCont conCont recCont tupCont lsCont
             in conCont "Just" [x']
      )

-- | @since 2.1
instance (Skeletal a) => Skeletal [a] where
  {-# INLINEABLE skeletize #-}
  skeletize xs =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let xs' = fmap (\x -> runSkeleton (skeletize x) bCont iCont sCont conCont recCont tupCont lsCont) xs
           in lsCont xs'
      )

{- | We represent a 'AssocMap.Map' as a list of tuples of its key-value pairs.

 @since 2.1
-}
instance (Skeletal k, Skeletal v) => Skeletal (AssocMap.Map k v) where
  {-# INLINEABLE skeletize #-}
  skeletize kvs =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let kvs' = fmap (\x -> runSkeleton (skeletize x) bCont iCont sCont conCont recCont tupCont lsCont) . AssocMap.toList $ kvs
           in lsCont kvs'
      )

-- | @since 2.1
instance (Skeletal a, Skeletal b) => Skeletal (a, b) where
  {-# INLINEABLE skeletize #-}
  skeletize (x, y) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let x' = runSkeleton (skeletize x) bCont iCont sCont conCont recCont tupCont lsCont
              y' = runSkeleton (skeletize y) bCont iCont sCont conCont recCont tupCont lsCont
           in tupCont x' y' Nothing
      )

-- | @since 2.1
instance Skeletal TxId where
  {-# INLINEABLE skeletize #-}
  skeletize (TxId bbs) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let bbs' = runSkeleton (skeletize bbs) bCont iCont sCont conCont recCont tupCont lsCont
           in conCont "TxId" [bbs']
      )

-- | @since 2.1
instance Skeletal TxOutRef where
  {-# INLINEABLE skeletize #-}
  skeletize (TxOutRef txi txix) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let txi' = runSkeleton (skeletize txi) bCont iCont sCont conCont recCont tupCont lsCont
              txix' = runSkeleton (skeletize txix) bCont iCont sCont conCont recCont tupCont lsCont
           in recCont
                "TxOutRef"
                [ ("txOutRefId", txi')
                , ("txOutRefIdx", txix')
                ]
      )

-- | @since 2.1
instance Skeletal PubKeyHash where
  {-# INLINEABLE skeletize #-}
  skeletize (PubKeyHash bbs) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let bbs' = runSkeleton (skeletize bbs) bCont iCont sCont conCont recCont tupCont lsCont
           in conCont "PubKeyHash" [bbs']
      )

-- | @since 2.1
instance Skeletal ValidatorHash where
  {-# INLINEABLE skeletize #-}
  skeletize (ValidatorHash bbs) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let bbs' = runSkeleton (skeletize bbs) bCont iCont sCont conCont recCont tupCont lsCont
           in conCont "ValidatorHash" [bbs']
      )

-- | @since 2.1
instance Skeletal Credential where
  {-# INLINEABLE skeletize #-}
  skeletize cred =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          case cred of
            PubKeyCredential pkh ->
              let pkh' = runSkeleton (skeletize pkh) bCont iCont sCont conCont recCont tupCont lsCont
               in conCont "PubKeyCredential" [pkh']
            ScriptCredential vh ->
              let vh' = runSkeleton (skeletize vh) bCont iCont sCont conCont recCont tupCont lsCont
               in conCont "ScriptCredential" [vh']
      )

-- | @since 2.1
instance Skeletal StakingCredential where
  {-# INLINEABLE skeletize #-}
  skeletize sc =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          case sc of
            StakingHash cred ->
              let cred' = runSkeleton (skeletize cred) bCont iCont sCont conCont recCont tupCont lsCont
               in conCont "StakingHash" [cred']
            StakingPtr i j k ->
              let i' = runSkeleton (skeletize i) bCont iCont sCont conCont recCont tupCont lsCont
                  j' = runSkeleton (skeletize j) bCont iCont sCont conCont recCont tupCont lsCont
                  k' = runSkeleton (skeletize k) bCont iCont sCont conCont recCont tupCont lsCont
               in conCont "StakingPtr" [i', j', k']
      )

-- | @since 2.1
instance Skeletal Address where
  {-# INLINEABLE skeletize #-}
  skeletize (Address cred stakingCred) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let cred' = runSkeleton (skeletize cred) bCont iCont sCont conCont recCont tupCont lsCont
              stakingCred' = runSkeleton (skeletize stakingCred) bCont iCont sCont conCont recCont tupCont lsCont
           in recCont
                "Address"
                [ ("addressCredential", cred')
                , ("addressStakingCredential", stakingCred')
                ]
      )

-- | @since 2.1
instance Skeletal CurrencySymbol where
  {-# INLINEABLE skeletize #-}
  skeletize (CurrencySymbol bbs) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let bbs' = runSkeleton (skeletize bbs) bCont iCont sCont conCont recCont tupCont lsCont
           in conCont "CurrencySymbol" [bbs']
      )

-- | @since 2.1
instance Skeletal TokenName where
  {-# INLINEABLE skeletize #-}
  skeletize (TokenName bbs) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let bbs' = runSkeleton (skeletize bbs) bCont iCont sCont conCont recCont tupCont lsCont
           in conCont "TokenName" [bbs']
      )

-- | @since 2.1
instance Skeletal Value where
  {-# INLINEABLE skeletize #-}
  skeletize (Value x) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let x' = runSkeleton (skeletize x) bCont iCont sCont conCont recCont tupCont lsCont
           in conCont "Value" [x']
      )

-- | @since 2.1
instance Skeletal DatumHash where
  {-# INLINEABLE skeletize #-}
  skeletize (DatumHash bbs) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let bbs' = runSkeleton (skeletize bbs) bCont iCont sCont conCont recCont tupCont lsCont
           in conCont "DatumHash" [bbs']
      )

-- | @since 2.1
instance Skeletal TxOut where
  {-# INLINEABLE skeletize #-}
  skeletize (TxOut addr val mHash) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let addr' = runSkeleton (skeletize addr) bCont iCont sCont conCont recCont tupCont lsCont
              val' = runSkeleton (skeletize val) bCont iCont sCont conCont recCont tupCont lsCont
              mHash' = runSkeleton (skeletize mHash) bCont iCont sCont conCont recCont tupCont lsCont
           in recCont
                "TxOut"
                [ ("txOutAddress", addr')
                , ("txOutValue", val')
                , ("txOutDatumHash", mHash')
                ]
      )

-- | @since 2.1
instance Skeletal TxInInfo where
  {-# INLINEABLE skeletize #-}
  skeletize (TxInInfo txoRef txo) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let txoRef' = runSkeleton (skeletize txoRef) bCont iCont sCont conCont recCont tupCont lsCont
              txo' = runSkeleton (skeletize txo) bCont iCont sCont conCont recCont tupCont lsCont
           in recCont
                "TxInInfo"
                [ ("txInInfoOutRef", txoRef')
                , ("txInInfoResolved", txo')
                ]
      )

-- | @since 2.1
instance Skeletal DCert where
  {-# INLINEABLE skeletize #-}
  skeletize dc =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont -> case dc of
          DCertDelegRegKey sc ->
            let sc' = runSkeleton (skeletize sc) bCont iCont sCont conCont recCont tupCont lsCont
             in conCont "DCertDelegRegKey" [sc']
          DCertDelegDeRegKey sc ->
            let sc' = runSkeleton (skeletize sc) bCont iCont sCont conCont recCont tupCont lsCont
             in conCont "DCertDeDelegRegKey" [sc']
          DCertDelegDelegate sc pkh ->
            let sc' = runSkeleton (skeletize sc) bCont iCont sCont conCont recCont tupCont lsCont
                pkh' = runSkeleton (skeletize pkh) bCont iCont sCont conCont recCont tupCont lsCont
             in conCont "DCertDelegDelegate" [sc', pkh']
          DCertPoolRegister pkh vfr ->
            let pkh' = runSkeleton (skeletize pkh) bCont iCont sCont conCont recCont tupCont lsCont
                vfr' = runSkeleton (skeletize vfr) bCont iCont sCont conCont recCont tupCont lsCont
             in conCont "DCertPoolRegister" [pkh', vfr']
          DCertPoolRetire pkh i ->
            let pkh' = runSkeleton (skeletize pkh) bCont iCont sCont conCont recCont tupCont lsCont
                i' = runSkeleton (skeletize i) bCont iCont sCont conCont recCont tupCont lsCont
             in conCont "DCertPoolRetire" [pkh', i']
          DCertGenesis -> conCont "DCertGenesis" []
          DCertMir -> conCont "DCertMir" []
      )

-- | @since 2.1
instance (Skeletal a) => Skeletal (Extended a) where
  {-# INLINEABLE skeletize #-}
  skeletize ext =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont -> case ext of
          NegInf -> conCont "NegInf" []
          Finite x ->
            let x' = runSkeleton (skeletize x) bCont iCont sCont conCont recCont tupCont lsCont
             in conCont "Finite" [x']
          PosInf -> conCont "PosInf" []
      )

-- | @since 2.1
instance (Skeletal a) => Skeletal (LowerBound a) where
  {-# INLINEABLE skeletize #-}
  skeletize (LowerBound ext clos) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let ext' = runSkeleton (skeletize ext) bCont iCont sCont conCont recCont tupCont lsCont
              clos' = runSkeleton (skeletize clos) bCont iCont sCont conCont recCont tupCont lsCont
           in conCont "LowerBound" [ext', clos']
      )

-- | @since 2.1
instance (Skeletal a) => Skeletal (UpperBound a) where
  {-# INLINEABLE skeletize #-}
  skeletize (UpperBound ext clos) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let ext' = runSkeleton (skeletize ext) bCont iCont sCont conCont recCont tupCont lsCont
              clos' = runSkeleton (skeletize clos) bCont iCont sCont conCont recCont tupCont lsCont
           in conCont "UpperBound" [ext', clos']
      )

-- | @since 2.1
instance (Skeletal a) => Skeletal (Interval a) where
  {-# INLINEABLE skeletize #-}
  skeletize (Interval from to) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let from' = runSkeleton (skeletize from) bCont iCont sCont conCont recCont tupCont lsCont
              to' = runSkeleton (skeletize to) bCont iCont sCont conCont recCont tupCont lsCont
           in recCont
                "Interval"
                [ ("ivFrom", from')
                , ("ivTo", to')
                ]
      )

-- | @since 2.1
instance Skeletal POSIXTime where
  {-# INLINEABLE skeletize #-}
  skeletize (POSIXTime i) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let i' = runSkeleton (skeletize i) bCont iCont sCont conCont recCont tupCont lsCont
           in conCont "POSIXTime" [i']
      )

-- | @since 2.1
instance Skeletal Datum where
  {-# INLINEABLE skeletize #-}
  skeletize (Datum bd) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let bd' = runSkeleton (skeletize bd) bCont iCont sCont conCont recCont tupCont lsCont
           in conCont "Datum" [bd']
      )

-- | @since 2.1
instance Skeletal TxInfo where
  {-# INLINEABLE skeletize #-}
  skeletize txi =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          recCont
            "TxInfo"
            [ ("txInfoInputs", runSkeleton (skeletize . txInfoInputs $ txi) bCont iCont sCont conCont recCont tupCont lsCont)
            , ("txInfoOutputs", runSkeleton (skeletize . txInfoOutputs $ txi) bCont iCont sCont conCont recCont tupCont lsCont)
            , ("txInfoFee", runSkeleton (skeletize . txInfoFee $ txi) bCont iCont sCont conCont recCont tupCont lsCont)
            , ("txInfoMint", runSkeleton (skeletize . txInfoMint $ txi) bCont iCont sCont conCont recCont tupCont lsCont)
            , ("txInfoDCert", runSkeleton (skeletize . txInfoDCert $ txi) bCont iCont sCont conCont recCont tupCont lsCont)
            , ("txInfoWdrl", runSkeleton (skeletize . txInfoWdrl $ txi) bCont iCont sCont conCont recCont tupCont lsCont)
            , ("txInfoValidRange", runSkeleton (skeletize . txInfoValidRange $ txi) bCont iCont sCont conCont recCont tupCont lsCont)
            , ("txInfoSignatories", runSkeleton (skeletize . txInfoSignatories $ txi) bCont iCont sCont conCont recCont tupCont lsCont)
            , ("txInfoData", runSkeleton (skeletize . txInfoData $ txi) bCont iCont sCont conCont recCont tupCont lsCont)
            , ("txInfoId", runSkeleton (skeletize . txInfoId $ txi) bCont iCont sCont conCont recCont tupCont lsCont)
            ]
      )

-- | @since 2.1
instance Skeletal ScriptPurpose where
  {-# INLINEABLE skeletize #-}
  skeletize sp =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont -> case sp of
          Minting cs ->
            let cs' = runSkeleton (skeletize cs) bCont iCont sCont conCont recCont tupCont lsCont
             in conCont "Minting" [cs']
          Spending tor ->
            let tor' = runSkeleton (skeletize tor) bCont iCont sCont conCont recCont tupCont lsCont
             in conCont "Spending" [tor']
          Rewarding sc ->
            let sc' = runSkeleton (skeletize sc) bCont iCont sCont conCont recCont tupCont lsCont
             in conCont "Rewarding" [sc']
          Certifying dcert ->
            let dcert' = runSkeleton (skeletize dcert) bCont iCont sCont conCont recCont tupCont lsCont
             in conCont "Certifying" [dcert']
      )

-- | @since 2.1
instance Skeletal ScriptContext where
  {-# INLINEABLE skeletize #-}
  skeletize (ScriptContext txi sp) =
    Skeleton
      ( \bCont iCont sCont conCont recCont tupCont lsCont ->
          let txi' = runSkeleton (skeletize txi) bCont iCont sCont conCont recCont tupCont lsCont
              sp' = runSkeleton (skeletize sp) bCont iCont sCont conCont recCont tupCont lsCont
           in recCont
                "ScriptContext"
                [ ("scriptContextTxInfo", txi')
                , ("scriptContextPurpose", sp')
                ]
      )
