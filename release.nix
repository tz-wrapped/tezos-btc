# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-MIT-BitcoinSuisse
let sources = import ./nix/sources.nix;
in { pkgs ? import sources.nixpkgs { } }:
with pkgs;

let
  tzbtc-static = (import ./ci.nix).tzbtc-release.components.exes.tzbtc-client;
  root = ./.;
  packageDesc = {
    project = "tzbtc-client";
    majorVersion = "0";
    minorVersion = "5";
    packageRevision = "0";
    bin = "${tzbtc-static}/bin/tzbtc-client";
    arch = "amd64";
    license = "MIT";
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

in rec {
  static = tzbtc-static;
  rpm = vmTools.runInLinuxImage
    (buildRpm.packageRpm // { diskImage = fedoraImage; });

  deb = vmTools.runInLinuxImage
    (buildDeb.packageDeb // { diskImage = ubuntuImage; });
}
