{- SPDX-FileCopyrightText: 2019 Bitcoin Suisse
 -
 - SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
 -}

-- | Documentation utilities.
module Lorentz.Contracts.TZBTC.Common.Doc
  ( licenseInfoDoc
  , additionalDeployNotes
  ) where

import Lorentz

licenseInfoDoc :: DComment
licenseInfoDoc = DComment $ mconcat
    [ "- SP"
    , "DX-FileCopyrightText: 2019 Bitcoin Suisse\n"
    , "-\n"
    , "- SP"
    , "DX-License-Identifier: LicenseRef-MIT-BitcoinSuisse"
    ]

additionalDeployNotes :: Markdown
additionalDeployNotes =
  "Initially originated contract has V0 which should be empty. However, it's possible\
  \ to originate contract with some entrypoints implementation, but such origination \
  \will highly likely exceed operation size limit, so it's recomended to originate \
  \empty V0 contract.\n\n Once the V0 is originated, it should be upgraded to V1 in order \
  \to be usable.\n\n The easiest way to originate and upgrade contract to V1 is to use \
  \`tzbtc-client deployTzbtcContract` command."
