{ stdenv, pkgs, mantisSrc, sbtVerify, protobuf }:

stdenv.mkDerivation {
  name = "mantis";
  src = mantisSrc;

  buildInputs = with pkgs; [ scala sbt sbtVerify unzip protobuf openjdk8 ];

  outputs = [ "out" "zip" ];

  configurePhase = ''
    export HOME="$NIX_BUILD_TOP"
    export "_JAVA_OPTIONS=-Dsbt.global.base=.sbt/1.0 -Dsbt.ivy.home=.ivy"

    cp -r ${sbtVerify}/.ivy .
    cp -r ${sbtVerify}/.sbt .
    cp -r ${sbtVerify}/target .
    chmod -R u+w .ivy .sbt target

    # Get sbt to pre-fetch its dependencies. The cleanest way I've
    # found of doing this is to get it to list the available projects,
    # which it can only do once deps are downloaded.
    sbt projects

    # We have to patch the executable embedded inside protoc-jar for
    # the one nix provides. :-(
    # This would be unnecessary if the mantis project didn't use
    # protoc-jar, and just expected the binary to be on the path as a
    # pre-requisite.
    mkdir -p bin_340/linux/amd64/
    cp ${protobuf}/bin/protoc bin_340/linux/amd64/
    jar uf .ivy/cache/com.github.os72/protoc-jar/jars/protoc-jar-3.4.0.jar bin_340/linux/amd64/protoc
  '';

  buildPhase = ''
    sbt 'set test in Test := {}' dist
  '';

  installPhase = ''
    mkdir $out $zip
    cp target/universal/mantis-1.0-daedalus-rc1.zip $zip/
    unzip $zip/*.zip
    mv mantis-1.0-daedalus-rc1/* $out
  '';
}
