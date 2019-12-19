{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Delegation.Certificates
  ( DCert(..)
  , DelegCert(..)
  , PoolCert(..)
  , GenesisDelegate(..)
  , MIRCert(..)
  , StakeCreds(..)
  , StakePools(..)
  , PoolDistr(..)
  , delegCWitness
  , poolCWitness
  , genesisCWitness
  , dvalue
  , refund
  , releasing
  , allocating
  , dretire
  , dderegister
  , decayKey
  , decayPool
  , isRegKey
  , isDeRegKey
  , isDelegation
  , isGenesisDelegation
  , isRegPool
  , isRetirePool
  , isInstantaneousRewards
  , requiresVKeyWitness
  ) where

import           BaseTypes (FixedPoint, UnitInterval, fpEpsilon, intervalValue)
import           Cardano.Ledger.Shelley.Crypto
import           Coin (Coin (..))
import           Keys (GenKeyHash, Hash, KeyHash, VRFAlgorithm (VerKeyVRF))
import           Ledger.Core (Relation (..))
import           NonIntegral (exp')
import           PParams (PParams (..), keyDecayRate, keyDeposit, keyMinRefund, poolDecayRate,
                     poolDeposit, poolMinRefund)
import           Slot (Duration (..))
import           TxData (Credential (..), DCert (..), DelegCert (..), GenesisDelegate (..),
                     MIRCert (..), PoolCert (..), StakeCreds (..), StakePools (..), delegator,
                     poolPubKey)

import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Data.Map.Strict (Map)
import           Data.Ratio (approxRational)

import           Lens.Micro ((^.))

-- |Determine the certificate author
delegCWitness :: DelegCert crypto-> Credential crypto
delegCWitness (RegKey _)            = error "no witness in key registration certificate"
delegCWitness (DeRegKey hk)         = hk
delegCWitness (Delegate delegation) = delegation ^. delegator

poolCWitness :: PoolCert crypto -> Credential crypto
poolCWitness (RegPool pool)            = KeyHashObj $ pool ^. poolPubKey
poolCWitness (RetirePool k _)          = KeyHashObj k

genesisCWitness :: GenesisDelegate crypto -> GenKeyHash crypto
genesisCWitness (GenesisDelegate (gk, _)) = gk

-- |Retrieve the deposit amount for a certificate
dvalue :: DCert crypto-> PParams -> Coin
dvalue (DCertDeleg (RegKey _))  = flip (^.) keyDeposit
dvalue (DCertPool (RegPool _)) = flip (^.) poolDeposit
dvalue _ = const $ Coin 0

-- |Compute a refund on a deposit
refund :: Coin -> UnitInterval -> Rational -> Duration -> Coin
refund (Coin dval) dmin lambda delta = floor refund'
  where
    pow     = fromRational (-lambda * fromIntegral delta) :: FixedPoint
    refund' = fromIntegral dval * (dmin' + (1 - dmin') * dCay)
    dmin'   = intervalValue dmin
    dCay    = approxRational (exp' pow) fpEpsilon

-- | Check whether certificate is of releasing type, i.e., key deregistration or
-- pool retirement.
releasing :: DCert crypto-> Bool
releasing c = dderegister c || dretire c

dderegister :: DCert crypto-> Bool
dderegister (DCertDeleg (DeRegKey _)) = True
dderegister _            = False

dretire :: DCert crypto-> Bool
dretire (DCertPool (RetirePool _ _)) = True
dretire _                = False

-- | Check whether certificate is of allocating type, i.e, key or pool
-- registration.
allocating :: DCert crypto-> Bool
allocating (DCertDeleg _)  = True
allocating (DCertPool _) = True
allocating _           = False

-- | Check for `RegKey` constructor
isRegKey :: DCert crypto-> Bool
isRegKey (DCertDeleg (RegKey _)) = True
isRegKey _ = False

-- | Check for `DeRegKey` constructor
isDeRegKey :: DCert crypto-> Bool
isDeRegKey (DCertDeleg (DeRegKey _)) = True
isDeRegKey _ = False

-- | Check for `Delegation` constructor
isDelegation :: DCert crypto-> Bool
isDelegation (DCertDeleg (Delegate _)) = True
isDelegation _ = False

-- | Check for `GenesisDelegate` constructor
isGenesisDelegation :: DCert crypto-> Bool
isGenesisDelegation (DCertGenesis (GenesisDelegate _)) = True
isGenesisDelegation _ = False

-- | Check for `RegPool` constructor
isRegPool :: DCert crypto-> Bool
isRegPool (DCertPool (RegPool _)) = True
isRegPool _ = False

-- | Check for `RetirePool` constructor
isRetirePool :: DCert crypto -> Bool
isRetirePool (DCertPool (RetirePool _ _)) = True
isRetirePool _ = False

decayKey :: PParams -> (Coin, UnitInterval, Rational)
decayKey pc = (dval, dmin, lambdad)
    where dval    = fromIntegral $ pc ^. keyDeposit
          dmin    = pc ^. keyMinRefund
          lambdad = pc ^. keyDecayRate

decayPool :: PParams -> (Coin, UnitInterval, Rational)
decayPool pc = (pval, pmin, lambdap)
    where pval    = fromIntegral $ pc ^. poolDeposit
          pmin    = pc ^. poolMinRefund
          lambdap = pc ^. poolDecayRate

newtype PoolDistr crypto=
  PoolDistr (Map (KeyHash crypto) (Rational, Hash (HASH crypto) (VerKeyVRF (VRF crypto))))
  deriving (Show, Eq, NoUnexpectedThunks, Relation)

isInstantaneousRewards :: DCert crypto-> Bool
isInstantaneousRewards (DCertMir _) = True
isInstantaneousRewards _       = False

-- | Returns True for delegation certificates that require at least
-- one witness, and False otherwise. It is mainly used to ensure
-- that calling a variant of `cwitness` is safe.
requiresVKeyWitness :: DCert crypto-> Bool
requiresVKeyWitness (DCertMir (MIRCert _)) = False
requiresVKeyWitness (DCertDeleg (RegKey _)) = False
requiresVKeyWitness _ = True
