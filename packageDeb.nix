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
  bin = pkgDesc.bin;
  pkgName = "${project}_${majorVersion}.${minorVersion}-${pkgRevision}";

  writeControlFile = writeTextFile {
    name = "control";
    text = ''
      Package: ${project}
      Version: ${majorVersion}.${minorVersion}-${pkgRevision}
      Priority: optional
      Architecture: ${pkgDesc.arch}
      Depends: ${pkgDesc.dependencies}
      Maintainer: ${pkgDesc.maintainer}
      Description: ${project}
       ${pkgDesc.description}
    '';
  };

in rec {
  packageDeb = binary:
    stdenv.mkDerivation {
      name = "${pkgName}.deb";

      phases = "packagePhase";

      packagePhase = ''
        mkdir ${pkgName}
        mkdir -p ${pkgName}/usr/local/bin
        cp ${bin} ${pkgName}/usr/local/bin/${project}

        mkdir ${pkgName}/DEBIAN
        cp ${writeControlFile} ${pkgName}/DEBIAN/control

        dpkg-deb --build ${pkgName}
        mv ${pkgName}.deb $out
      '';
    };
}
