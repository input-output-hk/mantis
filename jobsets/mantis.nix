{ stdenv, mantisSrc, scala, sbt, callPackage, unzip, sbtVerify }:

stdenv.mkDerivation {
  name = "mantis";
  requiredSystemFeatures = [ "ubuntu" ];
  src = mantisSrc;

  buildInputs = [ scala sbt sbtVerify unzip ];

  buildPhase = ''
    export HOME="$NIX_BUILD_TOP"
    cp -r ${sbtVerify} target
    chmod -R u+w target

    sbt 'set test in Test := {}' dist
  '';

  installPhase = ''
    mkdir $out
    unzip target/universal/mantis-1.0-daedalus-rc1.zip
    mv mantis-1.0-daedalus-rc1/* $out
  '';
}
