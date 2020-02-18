# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
{ static ? false }:
let
  sources = import ./nix/sources.nix;
  haskell-nix = import sources."haskell.nix";
  nixpkgs = import sources.nixpkgs haskell-nix;
  hn = if static then
    nixpkgs.pkgsCross.musl64.haskell-nix
  else
    nixpkgs.haskell-nix;
  inherit (nixpkgs) lib;
  project = hn.stackProject {
    src = hn.haskellLib.cleanGit { src = ./.; };
    modules = let
      staticLibs = with nixpkgs.pkgsStatic; [
        zlib
        gmp6
        libffi
      ];
    in [{
      packages.tzbtc.configureFlags = lib.optionals static [
        "--disable-executable-dynamic"
        "--disable-shared"
        "--ghc-option=-optl=-pthread"
        "--ghc-option=-optl=-static"
      ] ++ map (drv: "--ghc-option=-optl=-L${drv}/lib") staticLibs;
    }];
  };
in project.tzbtc.components.exes.tzbtc-client
