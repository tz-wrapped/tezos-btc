# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
{ stdenv, writeTextFile }:
pkgDesc:

let
  project = pkgDesc.project;
  majorVersion = pkgDesc.majorVersion;
  minorVersion = pkgDesc.minorVersion;
  pkgRevision = pkgDesc.packageRevision;
  pkgArch = pkgDesc.arch;
  bin = pkgDesc.bin;
  licenseFile = pkgDesc.licenseFile;
  pkgName = "${project}_${majorVersion}.${minorVersion}-${pkgRevision}_${pkgArch}";

  writeControlFile = writeTextFile {
    name = "control";
    text = ''
      Package: ${project}
      Version: ${majorVersion}.${minorVersion}-${pkgRevision}
      Priority: optional
      Architecture: ${pkgArch}
      Depends: ${pkgDesc.dependencies}
      Maintainer: ${pkgDesc.maintainer}
      Description: ${project}
       ${pkgDesc.description}
    '';
  };

in rec {
  packageDeb =
    stdenv.mkDerivation {
      name = "${pkgName}.deb";

      phases = "packagePhase";

      packagePhase = ''
        mkdir ${pkgName}
        mkdir -p ${pkgName}/usr/local/bin
        cp ${bin} ${pkgName}/usr/local/bin/${project}

        mkdir ${pkgName}/DEBIAN
        cp ${writeControlFile} ${pkgName}/DEBIAN/control
        cp ${licenseFile} ${pkgName}/DEBIAN/copyright

        dpkg-deb --build ${pkgName}
        cp ${pkgName}.deb $out
      '';
    };
}
