{- SPDX-FileCopyrightText: 2020 Tocqueville Group
 -
 - SPDX-License-Identifier: LicenseRef-MIT-TQ
 -}

module Lorentz.Contracts.TZBTC.V1.ManagedLedger.Doc
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

import Lorentz.Contracts.ManagedLedger.Doc qualified as ML
