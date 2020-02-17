# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
let
  sources = import ./nix/sources.nix;
  haskell-nix = import sources."haskell.nix";
  pkgs-native = import sources.nixpkgs haskell-nix;
  inherit (pkgs-native) lib;
  pkgs-musl = import sources.nixpkgs
    (haskell-nix // { crossSystem = lib.systems.examples.musl64; });
  stackProjectCross = { crossPkgs, pkgs, ... }@args:
    with crossPkgs.haskell-nix;
    let
      stack = importAndFilterProject (pkgs.haskell-nix.callStackToNix args);
      pkg-set = mkStackPkgSet {
        stack-pkgs = stack.pkgs;
        pkg-def-extras = (args.pkg-def-extras or [ ]);
        modules = (args.modules or [ ])
          ++ lib.optional (args ? ghc) { ghc.package = args.ghc; }
          ++ lib.optional (args ? cache) (mkCacheModule args.cache);
      };
      p = {
        inherit (pkg-set.config) hsPkgs;
        stack-nix = stack.nix;
      };
    in p.hsPkgs // {
      inherit (p) stack-nix;
      shells.ghc = p.hsPkgs.shellFor { };
    };
  gpl = true; # false: not supported yet
  tezos-btc = { buildPkgs, hostPkgs }:
    stackProjectCross {
      src = hostPkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
      modules = let
        staticLibs = with buildPkgs;
          [
            zlib.static
            (libffi.overrideAttrs (oldAttrs: {
              dontDisableStatic = true;
              configureFlags = (oldAttrs.configureFlags or [ ])
                ++ [ "--enable-static" "--disable-shared" ];
            }))
          ] ++ lib.optional gpl (gmp6.override { withStatic = true; });
      in [{
        packages.tzbtc.components.exes.tzbtc-client.configureFlags =
          with buildPkgs;
          lib.optionals stdenv.hostPlatform.isMusl [
            "--disable-executable-dynamic"
            "--disable-shared"
            "--ghc-option=-optl=-pthread"
            "--ghc-option=-optl=-static"
          ] ++ map (drv: "--ghc-option=-optl=-L${drv}/lib") staticLibs;
      }];
      crossPkgs = buildPkgs;
      pkgs = hostPkgs;
    };
in { static ? false }:
(tezos-btc {
  hostPkgs = pkgs-native;
  buildPkgs = if static then pkgs-musl else pkgs-native;
}).tzbtc.components.exes.tzbtc-client
