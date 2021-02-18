{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Examples
  ( testAlonzoMIR,
  )
where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Rules.Deleg (DELEG, DelegEnv (..))
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import Data.Default.Class (def)
import Shelley.Spec.Ledger.API
  ( AccountState (..),
    DCert (..),
    DState (..),
    InstantaneousRewards (..),
    MIRCert (..),
    MIRPot,
    MIRTarget,
    Ptr (..),
  )
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Test.Cardano.Ledger.EraBuffet (TestCrypto)
import Test.Shelley.Spec.Ledger.Utils (applySTSTest, runShelleyBase)
import Test.Tasty.HUnit (Assertion, (@?=))

type AlonzoTest = AlonzoEra TestCrypto

ignoreAllButIRWD ::
  Either [[PredicateFailure (DELEG AlonzoTest)]] (DState TestCrypto) ->
  Either [[PredicateFailure (DELEG AlonzoTest)]] (InstantaneousRewards TestCrypto)
ignoreAllButIRWD (Left e) = Left e
ignoreAllButIRWD (Right dstate) = Right (_irwd dstate)

env :: AccountState -> DelegEnv
env acnt =
  DelegEnv
    { slotNo = SlotNo 50,
      ptr_ = Ptr (SlotNo 50) 0 0,
      acnt_ = acnt
    }

testAlonzoMIR ::
  MIRPot ->
  MIRTarget TestCrypto ->
  InstantaneousRewards TestCrypto ->
  AccountState ->
  Either [[PredicateFailure (DELEG AlonzoTest)]] (InstantaneousRewards TestCrypto) ->
  Assertion
testAlonzoMIR pot target ir acnt (Right expected) = do
  checkTrace @(DELEG AlonzoTest) runShelleyBase (env acnt) $
    (pure (def {_irwd = ir})) .- (DCertMir (MIRCert pot target)) .-> (def {_irwd = expected})
testAlonzoMIR pot target ir acnt predicateFailure@(Left _) = do
  let st =
        runShelleyBase $
          applySTSTest @(DELEG AlonzoTest)
            (TRC (env acnt, def {_irwd = ir}, DCertMir (MIRCert pot target)))
  (ignoreAllButIRWD st) @?= predicateFailure
