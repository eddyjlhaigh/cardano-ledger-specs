{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Examples.MirEmbargos
  ( testMIREmbargos,
  )
where

import Cardano.Ledger.Shelley (ShelleyEra)
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import Data.Default.Class (def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Shelley.Spec.Ledger.API
  ( AccountState (..),
    Credential (..),
    DCert (..),
    DELEG,
    DState (..),
    DelegEnv (..),
    InstantaneousRewards (..),
    MIRCert (..),
    MIRPot (..),
    MIRTarget (..),
    Ptr (..),
  )
import Shelley.Spec.Ledger.Coin (Coin (..), DeltaCoin (..))
import Shelley.Spec.Ledger.Keys
  ( KeyRole (..),
    hashKey,
  )
import Shelley.Spec.Ledger.STS.Deleg (DelegPredicateFailure (..))
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Shelley.Spec.Ledger.Utils (applySTSTest, mkKeyPair, runShelleyBase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

type ShelleyTest = ShelleyEra C_Crypto

ignoreAllButIRWD ::
  Either [[PredicateFailure (DELEG ShelleyTest)]] (DState C_Crypto) ->
  Either [[PredicateFailure (DELEG ShelleyTest)]] (InstantaneousRewards C_Crypto)
ignoreAllButIRWD (Left e) = Left e
ignoreAllButIRWD (Right dstate) = Right (_irwd dstate)

env :: AccountState -> DelegEnv
env acnt =
  DelegEnv
    { slotNo = SlotNo 50,
      ptr_ = Ptr (SlotNo 50) 0 0,
      acnt_ = acnt
    }

testPreAlonzoMIR ::
  MIRPot ->
  MIRTarget C_Crypto ->
  InstantaneousRewards C_Crypto ->
  AccountState ->
  Either [[PredicateFailure (DELEG ShelleyTest)]] (InstantaneousRewards C_Crypto) ->
  Assertion
testPreAlonzoMIR pot target ir acnt (Right expected) = do
  checkTrace @(DELEG ShelleyTest) runShelleyBase (env acnt) $
    (pure (def {_irwd = ir})) .- (DCertMir (MIRCert pot target)) .-> (def {_irwd = expected})
testPreAlonzoMIR pot target ir acnt predicateFailure@(Left _) = do
  let st =
        runShelleyBase $
          applySTSTest @(DELEG ShelleyTest)
            (TRC (env acnt, def {_irwd = ir}, DCertMir (MIRCert pot target)))
  (ignoreAllButIRWD st) @?= predicateFailure

alice :: Credential 'Staking C_Crypto
alice = (KeyHashObj . hashKey . snd) $ mkKeyPair (0, 0, 0, 0, 1)

aliceOnlyReward :: Integer -> Map (Credential 'Staking C_Crypto) Coin
aliceOnlyReward c = Map.fromList [(alice, Coin c)]

aliceOnlyDelta :: Integer -> Map (Credential 'Staking C_Crypto) DeltaCoin
aliceOnlyDelta c = Map.fromList [(alice, DeltaCoin c)]

testMIREmbargos :: TestTree
testMIREmbargos =
  testGroup
    "MIR cert embargos"
    [ testCase "embargo reserves to treasury transfer" $
        testPreAlonzoMIR
          ReservesMIR
          (OtherPot $ Coin 1)
          (InstantaneousRewards mempty mempty mempty mempty)
          (AccountState {_reserves = Coin 1, _treasury = Coin 0})
          (Left [[MIRTransferNotCurrentlyAllowed]]),
      testCase "embargo treasury to reserves transfer" $
        testPreAlonzoMIR
          TreasuryMIR
          (OtherPot $ Coin 1)
          (InstantaneousRewards mempty mempty mempty mempty)
          (AccountState {_reserves = Coin 0, _treasury = Coin 1})
          (Left [[MIRTransferNotCurrentlyAllowed]]),
      testCase "embargo decrements from reserves" $
        testPreAlonzoMIR
          ReservesMIR
          (StakeAddressesMIR $ aliceOnlyDelta (-1))
          (InstantaneousRewards (aliceOnlyReward 1) mempty mempty mempty)
          (AccountState {_reserves = Coin 1, _treasury = Coin 0})
          (Left [[MIRNegativesNotCurrentlyAllowed]]),
      testCase "embargo decrements from treasury" $
        testPreAlonzoMIR
          TreasuryMIR
          (StakeAddressesMIR $ aliceOnlyDelta (-1))
          (InstantaneousRewards mempty (aliceOnlyReward 1) mempty mempty)
          (AccountState {_reserves = Coin 0, _treasury = Coin 1})
          (Left [[MIRNegativesNotCurrentlyAllowed]])
    ]
