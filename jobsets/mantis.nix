{ stdenv, mantisSrc, scala, sbt, callPackage, unzip, sbtVerify }:

stdenv.mkDerivation {
  name = "mantis";
  requiredSystemFeatures = [ "ubuntu" ];
  src = mantisSrc;

  buildInputs = [ scala sbt sbtVerify unzip ];

  buildPhase = ''
    cp -r ${sbtVerify}/.ivy .
    cp -r ${sbtVerify}/.sbt .
    cp -r ${sbtVerify}/target .
    chmod -R u+w .ivy .sbt target

    sbt -Dsbt.global.base=.sbt/1.0 -Dsbt.ivy.home=.ivy 'set test in Test := {}' dist
  '';

  installPhase = ''
    mkdir $out
    unzip target/universal/mantis-1.0-daedalus-rc1.zip
    mv mantis-1.0-daedalus-rc1/* $out
  '';
}
