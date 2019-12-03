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
  morley-repo = {
    name = "morley";
    url = "https://gitlab.com/morley-framework/morley.git";
    rev = "fba24c6271c2ae1ba4b2c956ab3d2e398d22297c";
    sha256 = "07dd2i9mh6r4bni77gpbk6nclimkp0hcqs3d2g2wq9nznm4rygj2";
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
      cache = [
        morley-repo
        (morley-repo // rec {
          name = "morley-prelude";
          subdir = "prelude";
        })
        (morley-repo // rec {
          name = "lorentz-contracts";
          subdir = name;
        })
        (morley-repo // rec {
          name = "lorentz-contracts-test";
          subdir = name;
        })
        rec {
          name = "tezos-bake-monitor-lib";
          url =
            "https://gitlab.com/obsidian.systems/tezos-bake-monitor-lib.git";
          rev = "656f8fe8c9ea5b45686b927f8706b8bfc6a616ac";
          sha256 = "02z8aqccfkbl0snvambaqjpjzcx18ag3apvx4xsfrvpn8l5dhj7c";
          subdir = name;
        }
      ];
      crossPkgs = buildPkgs;
      pkgs = hostPkgs;
    };
in { static ? false }:
(tezos-btc {
  hostPkgs = pkgs-native;
  buildPkgs = if static then pkgs-musl else pkgs-native;
}).tzbtc.components.exes.tzbtc-client
