# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
rec {
  sources = import ./nix/sources.nix;
  haskellNix = import sources."haskell.nix" {
    sourcesOverride = { hackage = sources."hackage.nix"; stackage = sources."stackage.nix"; };
  };
  pkgs = import sources.nixpkgs haskellNix.nixpkgsArgs;
  crossref-verifier = import sources.crossref-verifier;
  weeder-hacks = import sources.haskell-nix-weeder { inherit pkgs; };
  tezos-client = (import "${sources.tezos-packaging}/nix/build/pkgs.nix" {}).ocamlPackages.tezos-client;

  tzbtc-with-commit = commitInfo: import ./tzbtc.nix { inherit pkgs weeder-hacks commitInfo; };
  tzbtc-release = import ./tzbtc.nix { inherit pkgs weeder-hacks; release = true; };
  tzbtc = tzbtc-with-commit null;

  all-components = with tzbtc.components;
    [ library library.haddock ] ++ pkgs.lib.attrValues exes ++ pkgs.lib.attrValues tests;

  # nixpkgs has weeder 2, but we use weeder 1
  weeder-legacy = pkgs.haskellPackages.callHackageDirect {
    pkg = "weeder";
    ver = "1.0.9";
    sha256 = "0gfvhw7n8g2274k74g8gnv1y19alr1yig618capiyaix6i9wnmpa";
  } {};

  # a derivation which generates a script for running weeder
  weeder-script = weeder-hacks.weeder-script {
    weeder = weeder-legacy;
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
  # gh in the nixpkgs is quite old and doesn't support release managing, so we're doing
  # some ugly workarounds (because of https://github.com/NixOS/nixpkgs/issues/86349) to bump it
  gh = (pkgs.callPackage "${pkgs.path}/pkgs/applications/version-management/git-and-tools/gh" {
    buildGoModule = args: pkgs.buildGoModule (args // rec {
      version = "1.2.0";
      vendorSha256 = "0ybbwbw4vdsxdq4w75s1i0dqad844sfgs69b3vlscwfm6g3i9h51";
      src = pkgs.fetchFromGitHub {
        owner = "cli";
        repo = "cli";
        rev = "v${version}";
        sha256 = "17hbgi1jh4p07r4p5mr7w7p01i6zzr28mn5i4jaki7p0jwfqbvvi";
      };
    });
  });
}
