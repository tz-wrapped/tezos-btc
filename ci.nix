# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
rec {
  sources = import ./nix/sources.nix;
  haskellNix = import sources."haskell.nix" {};
  pkgs = import sources.nixpkgs haskellNix.nixpkgsArgs;
  crossref-verifier = import sources.crossref-verifier;
  weeder-hacks = import sources.haskell-nix-weeder { inherit pkgs; };

  tzbtc-with-commit = commitInfo: import ./tzbtc.nix { inherit pkgs weeder-hacks commitInfo; };
  tzbtc-release = import ./tzbtc.nix { inherit pkgs weeder-hacks; release = true; };
  tzbtc = tzbtc-with-commit null;

  all-components = with tzbtc.components;
    [ library ] ++ pkgs.lib.attrValues exes ++ pkgs.lib.attrValues tests;

  # a derivation which generates a script for running weeder
  weeder-script = weeder-hacks.weeder-script {
    hs-pkgs = { tzbtc = tzbtc; };
    local-packages = [
      { name = "tzbtc"; subdirectory = "."; }
    ];
  };

  contract-doc-with-commit = commitInfo:
    let tzbtc-exe = (tzbtc-with-commit commitInfo).components.exes.tzbtc;
    in pkgs.runCommand "contract-doc" {} ''
      mkdir $out
      ${tzbtc-exe}/bin/tzbtc printContractDoc > $out/TZBTC-contract.md
    '';
  contract-doc-dev = contract-doc-with-commit null;
  contract-doc-release = { sha, date }@commitInfo: contract-doc-with-commit commitInfo;
}
