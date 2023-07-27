# SPDX-FileCopyrightText: 2023 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse

{
  description = "The tezos-btc flake";

  nixConfig.flake-registry = "https://gitlab.com/morley-framework/morley-infra/-/raw/main/flake-registry.json";

  inputs.morley-infra.url = "gitlab:morley-framework/morley-infra";

  outputs = { self, flake-utils, morley-infra, ... }:
    (flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = morley-infra.legacyPackages.${system};

        inherit (morley-infra.utils.${system}) ci-apps run-chain-tests;

        projectSrc = pkgs.haskell-nix.haskellLib.cleanGit {
          name = "tezos-btc";
          src = ./.;
        };

        local-packages = [
          { name = "tzbtc"; subdirectory = "./."; }
        ];

        hs-pkgs =
          args: import ./tzbtc.nix ({ inherit pkgs projectSrc; inherit (ci-apps) collect-hie; } // args);

        hs-pkgs-release = hs-pkgs { release = true; };

        hs-pkgs-development = hs-pkgs {};

        flake = hs-pkgs-development.flake {};

        release = import ./release.nix {
          pkgs = pkgs.extend (_:_: {
            tzbtc-static = hs-pkgs-release.tzbtc.components.exes.tzbtc-client;
          });
        };

      in pkgs.lib.lists.foldr pkgs.lib.recursiveUpdate {} [
        { inherit (flake) packages; }
        {
          legacyPackages = pkgs;

          inherit release;

          packages = {
            all-components = with hs-pkgs-development.tzbtc.components;
              pkgs.linkFarmFromDrvs "all-components" (
                [ library library.haddock ]
                ++ pkgs.lib.attrValues exes
                ++ pkgs.lib.attrValues tests
              );

            default = self.packages.${system}.all-components;

            contract-doc-dev = self.utils.${system}.contract-doc-with-commit null;
          };

          checks = {
            trailing-whitespace = pkgs.build.checkTrailingWhitespace ./.;

            reuse-lint = pkgs.build.reuseLint ./.;
          };

          utils = {
            inherit run-chain-tests;

            contract-doc-with-commit = commitInfo:
              let tzbtc-exe = (hs-pkgs { inherit commitInfo; }).tzbtc.components.exes.tzbtc;
              in pkgs.runCommand "contract-doc" {} ''
                mkdir $out
                ${tzbtc-exe}/bin/tzbtc printContractDoc -o $out/TZBTC-contract.md
              '';

            contract-doc-release = { sha, date }@commitInfo: self.utils.${system}.contract-doc-with-commit commitInfo;
          };

          apps = ci-apps.apps {
            hs-pkgs = hs-pkgs-development;
            inherit local-packages projectSrc;
          };
        }
      ]));
}
