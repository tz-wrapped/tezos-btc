# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
{
  pkgs, weeder-hacks, static ? true,
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
          package.ghcOptions = with pkgs.lib; concatStringsSep " " [
            "-Werror"

            # output *.dump-hi files (required for weeder)
            "-ddump-to-file" "-ddump-hi"
          ];

          # collect all *.dump-hi files (required for weeder)
          postInstall = weeder-hacks.collect-dump-hi-files;

          # the code expects commit sha and date to be provided in build-time
          preBuild = if commitInfo != null then ''
            export MORLEY_DOC_GIT_COMMIT_SHA=${pkgs.lib.escapeShellArg commitInfo.sha}
            export MORLEY_DOC_GIT_COMMIT_DATE=${pkgs.lib.escapeShellArg commitInfo.date}
          '' else ''
            export MORLEY_DOC_GIT_COMMIT_SHA="UNSPECIFIED"
            export MORLEY_DOC_GIT_COMMIT_DATE="UNSPECIFIED"
          '';
        };

        # don't haddock dependencies
        doHaddock = false;
        packages.tzbtc.doHaddock = true;

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
