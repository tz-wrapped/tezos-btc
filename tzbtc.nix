# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
{
  pkgs, weeder-hacks, static ? true,
  release ? false,   # release build with optimizations enabled
  commitInfo ? null  # git commit sha and date
}:

with rec {
  haskell-nix =
    if static
    then pkgs.pkgsCross.musl64.haskell-nix
    else pkgs.haskell-nix;

  hs-pkgs = haskell-nix.stackProject {
    src = haskell-nix.haskellLib.cleanGit {
      name = "tezos-btc";
      src = ./.;
    };
    modules =
      [{
        packages.tzbtc = {
          package.ghcOptions = with pkgs.lib; concatStringsSep " " (concatLists [
            # error on warning
            [ "-Werror" ]

            # disable optimizations in non-release build
            (optional (!release) "-O0")

            # output *.dump-hi files (required for weeder)
            (optionals (!release) [ "-ddump-to-file" "-ddump-hi" ])
          ]);

          # collect all *.dump-hi files (required for weeder)
          postInstall = if release then "" else weeder-hacks.collect-dump-hi-files;

          # the code expects commit sha and date to be provided in build-time
          preBuild = if commitInfo != null then ''
            export MORLEY_DOC_GIT_COMMIT_SHA=${pkgs.lib.escapeShellArg commitInfo.sha}
            export MORLEY_DOC_GIT_COMMIT_DATE=${pkgs.lib.escapeShellArg commitInfo.date}
          '' else ''
            export MORLEY_DOC_GIT_COMMIT_SHA="UNSPECIFIED"
            export MORLEY_DOC_GIT_COMMIT_DATE="UNSPECIFIED"
          '';
        };

        # don't haddock dependencies, haddock our package in non-release build only
        doHaddock = false;
        packages.tzbtc.doHaddock = if release then false else true;

      }] ++ pkgs.lib.optional static {
        packages.tzbtc = {
          # library: no extra flags
          components.library.configureFlags = [];

          # other components: provide static libraries for linking
          configureFlags = [
            "--ghc-option=-optl=-L${pkgs.pkgsStatic.zlib}/lib"
          ];
        };
      };
  };

  tzbtc = hs-pkgs.tzbtc;
};

tzbtc
