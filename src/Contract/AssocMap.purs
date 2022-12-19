-- | A module for a Plutus-style `AssocMap`
module Contract.AssocMap
  ( DatumMap(DatumMap)
  , module AssocMap
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Ctl.Internal.FromData
  ( class FromData
  , fromData
  )
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
import Ctl.Internal.ToData
  ( class ToData
  , toData
  )
import Ctl.Internal.Types.PlutusData
  ( PlutusData(DatumMap)
  ) as PlutusData
import Data.Bifunctor (bimap)

newtype DatumMap (k :: Type) (v :: Type) = DatumMap (AssocMap.Map k v)

derive instance Generic (DatumMap k v) _
derive instance Newtype (DatumMap k v) _
derive newtype instance (Eq k, Eq v) => Eq (DatumMap k v)
derive newtype instance (Ord k, Ord v) => Ord (DatumMap k v)
derive newtype instance
  ( EncodeAeson k
  , EncodeAeson v
  ) =>
  EncodeAeson (DatumMap k v)

derive newtype instance
  ( DecodeAeson k
  , DecodeAeson v
  ) =>
  DecodeAeson (DatumMap k v)

instance (Show k, Show v) => Show (DatumMap k v) where
  show = genericShow

instance (ToData k, ToData v) => ToData (DatumMap k v) where
  toData (DatumMap (AssocMap.Map xs)) = PlutusData.DatumMap
    (bimap toData toData <$> xs)

instance (FromData k, FromData v) => FromData (DatumMap k v) where
  fromData (PlutusData.DatumMap mp) = do
    (DatumMap <<< AssocMap.Map) <$>
      ( for mp \(k /\ v) ->
          Tuple <$> fromData k <*> fromData v
      )
  fromData _ = Nothing
