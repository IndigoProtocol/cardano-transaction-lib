module Ctl.Internal.Test.ContractTest
  ( ContractTest(ContractTest)
  , withWallets
  , noWallet
  , ContractTestHandler
  , ContractTestPlan(ContractTestPlan)
  , ContractTestPlanHandler
  , sameWallets
  , groupContractTestPlans
  ) where

import Prelude

import Contract.Monad (Contract)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Test.UtxoDistribution (class UtxoDistribution)
import Data.Tuple.Nested ((/\))
import Mote.Monad (group, mapTest)
import Data.Tuple (fst, snd)

-- | Represents a `Contract` test suite that depend on *some* wallet
-- | `UtxoDistribution`.
newtype ContractTest = ContractTest
  ( forall (r :: Type)
     . ( forall (distr :: Type) (wallets :: Type)
          . ContractTestHandler distr wallets r
       )
    -> r
  )

-- | Store a wallet `UtxoDistribution` and a `Contract` that depends on those wallets
withWallets
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => distr
  -> (wallets -> Contract Unit)
  -> ContractTest
withWallets distr tests = ContractTest \h -> h distr tests

-- | Lift a `Contract` into `ContractTest`
noWallet :: Contract Unit -> ContractTest
noWallet = withWallets unit <<< const

-- | A runner for a test suite that supports funds distribution.
type ContractTestHandler :: Type -> Type -> Type -> Type
type ContractTestHandler distr wallets r =
  UtxoDistribution distr wallets => distr -> (wallets -> Contract Unit) -> r

-- | Represents `Contract`s in `TestPlanM` that depend on *some* wallet `UtxoDistribution`
newtype ContractTestPlan = ContractTestPlan
  ( forall (r :: Type)
     . ( forall (distr :: Type) (wallets :: Type)
          . ContractTestPlanHandler distr wallets r
       )
    -> r
  )

instance Semigroup ContractTestPlan where
  append (ContractTestPlan runContractTestPlan) (ContractTestPlan runContractTestPlan') =
    do
      runContractTestPlan \distr tests -> do
        runContractTestPlan'
          \distr' tests' -> ContractTestPlan \h -> h (distr /\ distr') do
            mapTest (_ <<< fst) tests
            mapTest (_ <<< snd) tests'

-- | Same as `ContractTestHandler`, but wrapped in a `TestPaln`.
type ContractTestPlanHandler :: Type -> Type -> Type -> Type
type ContractTestPlanHandler distr wallets r =
  UtxoDistribution distr wallets
  => distr
  -> TestPlanM (wallets -> Contract Unit) Unit
  -> r

-- | Store a wallet `UtxoDistribution` and `Contract`s that depend on that wallet
sameWallets 
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => distr
  -> TestPlanM (wallets -> Contract Unit) Unit
  -> ContractTestPlan
sameWallets distr tests = ContractTestPlan \h -> h distr tests

-- | Group `ContractTestPlans` together, so that they can be ran in the same Plutip instance
groupContractTestPlans :: String -> ContractTestPlan -> ContractTestPlan
groupContractTestPlans title (ContractTestPlan runContractTestPlan) = do
  runContractTestPlan \distr tests -> ContractTestPlan \h -> h distr do
    group title tests