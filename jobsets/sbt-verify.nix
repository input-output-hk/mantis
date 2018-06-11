{ stdenv, sbtVerifySrc, scala, sbt }:

stdenv.mkDerivation {
  name = "sbtVerify";
  src = sbtVerifySrc;

  buildInputs = [ scala sbt ];

  configurePhase = ''
    export HOME="$NIX_BUILD_TOP"
  '';

  buildPhase = "
    sbt -Dsbt.global.base=.sbt/1.0 -Dsbt.ivy.home=.ivy publishLocal
  ";

  installPhase = ''
    mkdir $out
    cp -r .ivy .sbt target $out
  '';
}
