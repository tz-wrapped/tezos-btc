# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
let sources = import ./nix/sources.nix;
in { pkgs ? import sources.nixpkgs { } }:
with pkgs;

let
  tzbtc-static = import ./default.nix { static = true; };
  root = ./.;
  packageDesc = {
    project = "tzbtc-client";
    majorVersion = "0";
    minorVersion = "1";
    packageRevision = "1";
    bin = "${tzbtc-static}/bin/tzbtc-client";
    arch = "amd64";
    license = "Proprietary";
    dependencies = "";
    maintainer = "Serokell https://serokell.io";
    licenseFile = "${root}/LICENSE";
    description = "CLI client for managing TZBTC contract";
  };
  buildDeb =
    import ./packageDeb.nix { inherit stdenv writeTextFile; } packageDesc;
  buildRpm = import ./packageRpm.nix { inherit stdenv writeTextFile; }
    (packageDesc // { arch = "x86_64"; });

  inherit (vmTools)
    makeImageFromDebDist makeImageFromRPMDist debDistros rpmDistros;
  ubuntuImage = makeImageFromDebDist debDistros.ubuntu1804x86_64;
  fedoraImage = makeImageFromRPMDist rpmDistros.fedora27x86_64;

  mainnetInfo = {
    url = "https://gitlab.com/serokell/morley/tezos.git";
    rev = "e0ab0c74"; # rvem/mainnet-client-patched
    sha256 = "0b5mp7gms31qnqybph1r82v60sx6scigrv0w7y6r9ssb0v23vr4y";
  };

  babylonnetInfo = {
    url = "https://gitlab.com/serokell/morley/tezos.git";
    rev = "dd8a9c6b"; # rvem/babylonnet-client-patched
    sha256 = "1bpck6q1cgzkhbmgx16m118qff3rs2ndvrzq9bh2hzd6iqww75mj";
  };

  tezosPackaging = import (fetchFromGitHub {
    owner = "serokell";
    repo = "tezos-packaging";
    rev = "126f6dff28d255058c05bfd917bd6fb5988dd185";
    sha256 = "1pk5q5q51lgzic6pvvvnb0vskwy7vf2h90461fg3rmvxpzaz2c57";
  }) { inherit mainnetInfo babylonnetInfo; };

in rec {
  inherit (tezosPackaging) tezos-client-mainnet tezos-client-babylonnet;
  static = tzbtc-static;
  rpm = vmTools.runInLinuxImage
    (buildRpm.packageRpm // { diskImage = fedoraImage; });

  deb = vmTools.runInLinuxImage
    (buildDeb.packageDeb // { diskImage = ubuntuImage; });
}
