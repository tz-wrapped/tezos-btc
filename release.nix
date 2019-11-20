# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
{ pkgs ? import <nixpkgs> { }, tzbtc-client-binary ? "./bin/tzbtc-client"}:
with pkgs;

let
  root = ./.;
  staticProject = import ./default.nix {};
  tzbtc-client-static = stdenv.mkDerivation {
    name = "tzbtc-client";
    phases = ["buildPhase"];

    buildPhase = ''
    $(${staticProject.fullBuildScript}) -o ${root}/tzbtc-static
    cp ${root}/tzbtc-static/bin/tzbtc-client $out
    '';
  };
  packageDesc = {
    project = "tzbtc-client";
    majorVersion = "0";
    minorVersion = "1";
    packageRevision = "1";
    bin = tzbtc-client-static;
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
  inherit tzbtc-client-static;
  inherit (vmTools) runInLinuxImage;

  packageIntoRpm = runInLinuxImage
    (buildRpm.packageRpm tzbtc-client-binary // { diskImage = fedoraImage; });

  packageIntoDeb = runInLinuxImage
    (buildDeb.packageDeb tzbtc-client-binary // { diskImage = ubuntuImage; });
}
