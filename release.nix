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
    minorVersion = "3";
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

  tezos-packaging = import (fetchFromGitHub {
    owner = "serokell";
    repo = "tezos-packaging";
    rev = "202002241125";
    sha256 = "1pqggiii21ip27l24y7fdrm1jiyhb9l0lrmkrqw5hgz978rjg9bs";
  }) { patches = [ ./patch/tezos-client.patch ]; };

  tezos-client = pkgs.runCommand "uncompress-tezos-client" { } ''
    mkdir tmp && cd tmp
    cp ${tezos-packaging.binaries}/*.tar.gz ./
    tar -xvzf *.tar.gz
    mkdir -p $out && cp tezos-client $out/
  '';

in rec {
  inherit tezos-client;
  static = tzbtc-static;
  rpm = vmTools.runInLinuxImage
    (buildRpm.packageRpm // { diskImage = fedoraImage; });

  deb = vmTools.runInLinuxImage
    (buildDeb.packageDeb // { diskImage = ubuntuImage; });
}
