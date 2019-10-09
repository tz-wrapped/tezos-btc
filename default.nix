# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
{ stack2nix-output-path ? "custom-stack2nix-output.nix" }:
let
  cabalPackageName = "tzbtc";
  compiler = "ghc865";

  static-haskell-nix = fetchTarball
    "https://github.com/nh2/static-haskell-nix/archive/475739275fbdfce025cd8a0e83f79164a7cdc66e.tar.gz";

  pkgs = import "${static-haskell-nix}/nixpkgs.nix";

  stack2nix-script = import
    "${static-haskell-nix}/static-stack2nix-builder/stack2nix-script.nix" {
      inherit pkgs;
      stack-project-dir = toString ./.;
      hackageSnapshot = "2019-10-08T00:00:00Z";
    };

  static-stack2nix-builder =
    import "${static-haskell-nix}/static-stack2nix-builder/default.nix" {
      normalPkgs = pkgs;
      inherit cabalPackageName compiler stack2nix-output-path;
    };

  fullBuildScript = pkgs.writeScript "stack2nix-and-build-script.sh" ''
    #!${pkgs.stdenv.shell}
    set -eu -o pipefail
    STACK2NIX_OUTPUT_PATH=$(${stack2nix-script})
    export NIX_PATH=nixpkgs=${pkgs.path}
    ${pkgs.nix}/bin/nix-build --no-link -A static_package --argstr stack2nix-output-path "$STACK2NIX_OUTPUT_PATH" "$@"
  '';

in rec {
  static_package = static-stack2nix-builder.static_package;
  inherit fullBuildScript;
}
