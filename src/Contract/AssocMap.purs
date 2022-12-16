-- | A module for a Plutus-style `AssocMap`
module Contract.AssocMap
  ( DatumMap(DatumMap)
  , module AssocMap
  ) where

import Contract.Prelude

import Data.Bifunctor (bimap)
import Ctl.Internal.Plutus.Types.AssocMap
  ( Map(Map)
  , delete
  , elems
  , empty
  , filter
  , insert
  , keys
  , lookup
  , mapMaybe
  , mapMaybeWithKey
  , mapThese
  , member
  , null
  , singleton
  , union
  , unionWith
  , values
  ) as AssocMap
import Ctl.Internal.Types.PlutusData
  ( PlutusData(DatumMap)
  ) as PlutusData
import Ctl.Internal.FromData
  ( class FromData
  , fromData
  )
import Ctl.Internal.ToData
  ( class ToData
  , toData
  )

newtype DatumMap (k :: Type) (v :: Type) = DatumMap (AssocMap.Map k v)

instance (ToData k, ToData v) => ToData (DatumMap k v) where
  toData (DatumMap (AssocMap.Map xs)) = PlutusData.DatumMap (bimap toData toData <$> xs)

instance (FromData k, FromData v) => FromData (DatumMap k v) where
  fromData (PlutusData.DatumMap mp) = do
    (DatumMap <<< AssocMap.Map) <$>
      ( for mp \(k /\ v) ->
          Tuple <$> fromData k <*> fromData v
      )
  fromData _ = Nothing