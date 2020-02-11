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
    rev = "960b8c79e4d2e241d4600f8d2c6e82ff2cb9c8fe";
    sha256 = "0q42bsd8f4chmrsziw037kxsclwwj5cym708jkw2pmy5l98hnphh";
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
          name = "morley-ledgers";
          subdir = name;
        })
        (morley-repo // rec {
          name = "morley-ledgers-test";
          subdir = name;
        })
        (morley-repo // rec {
          name = "morley-multisig";
          subdir = name;
        })
        (morley-repo // rec {
          name = "morley-upgradeable";
          subdir = name;
        })
        (morley-repo // rec {
          name = "morley-nettest";
          subdir = name;
        })
        (morley-repo // rec {
          name = "indigo";
          subdir = name;
        })
        (morley-repo // rec {
          name = "tasty-hunit-compat";
          subdir = name;
        })

        rec {
          name = "tezos-bake-monitor-lib";
          url =
            "https://gitlab.com/obsidian.systems/tezos-bake-monitor-lib.git";
          rev = "19a9ce57a0510bc3ad8a3f639d0a968a65024b86";
          sha256 = "0ypf7z2c9w59rz7hymzdyx87783qdfp3kygs9jl8qnmbfslmi8jr";
          subdir = name;
        }
        # morley-nettest depends on caps
        rec {
            name = "caps";
            url =
              "https://github.com/int-index/caps.git";
            rev = "ab4345eabd58fc6f05d3b46bea2c5acdba3ec6f8";
            sha256 = "0r1zqa559gaxavsd8zynlpa2c5hi2qc20n8s4bhcdaw36hasg3kr";
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
