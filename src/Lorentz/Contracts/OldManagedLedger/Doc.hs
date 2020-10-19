{- SPDX-FileCopyrightText: 2020 Tocqueville Group
 -
 - SPDX-License-Identifier: LicenseRef-MIT-TQ
 -}

module Lorentz.Contracts.OldManagedLedger.Doc
  ( ML.contractDoc
  , ML.transferDoc
  , ML.approveDoc
  , ML.approveCASDoc
  , ML.getAllowanceDoc
  , ML.getBalanceDoc
  , ML.getTotalSupplyDoc
  , ML.setPauseDoc
  , ML.setAdministratorDoc
  , ML.getAdministratorDoc
  , ML.mintDoc
  , ML.burnDoc

  , ML.DTokenNotPausedOnly (..)
  , ML.DRequireRole (..)
  ) where

import qualified Lorentz.Contracts.ManagedLedger.Doc as ML
