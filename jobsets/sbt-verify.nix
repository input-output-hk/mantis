{ stdenv, sbtVerifySrc, scala, sbt }:

stdenv.mkDerivation {
  name = "sbtVerify";
  requiredSystemFeatures = [ "ubuntu" ];
  src = sbtVerifySrc;

  buildInputs = [ scala sbt ];

  configurePhase = "export HOME=$NIX_BUILD_TOP";

  buildPhase = "sbt publishLocal";

  installPhase = "mv target $out";
}
