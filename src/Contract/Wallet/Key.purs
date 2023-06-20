-- | `KeyWallet` type and its utility functions.
module Contract.Wallet.Key
  ( module X
  , publicKeyFromPrivateKey
  ) where

import Prelude

import Ctl.Internal.Cardano.Types.Transaction (PublicKey, mkFromCslPubKey)
import Ctl.Internal.Serialization.Keys (publicKeyFromPrivateKey) as Internal
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Wallet.Key
  ( KeyWallet(KeyWallet)
  , keyWalletPrivatePaymentKey
  , keyWalletPrivateStakeKey
  , privateKeysToKeyWallet
  ) as X
import Ctl.Internal.Wallet.Spec
  ( Cip1852DerivationPath
  , StakeKeyPresence(WithStakeKey, WithoutStakeKey)
  , mkKeyWalletFromMnemonic
  ) as X

publicKeyFromPrivateKey :: PrivateKey -> PublicKey
publicKeyFromPrivateKey = mkFromCslPubKey <<< Internal.publicKeyFromPrivateKey
