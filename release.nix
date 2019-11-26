# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
{ pkgs ? import <nixpkgs> { }, tzbtc-client-binary }:
with pkgs;

let
  root = ./.;
  packageDesc = {
    project = "tzbtc-client";
    majorVersion = "0";
    minorVersion = "1";
    packageRevision = "0";
    bin = tzbtc-client-binary;
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

in rec {
  inherit (vmTools) runInLinuxImage;

  packageIntoRpm = runInLinuxImage
    (buildRpm.packageRpm tzbtc-client-binary // { diskImage = fedoraImage; });

  packageIntoDeb = runInLinuxImage
    (buildDeb.packageDeb tzbtc-client-binary // { diskImage = ubuntuImage; });
}
