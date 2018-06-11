{ stdenv, pkgs, mantisSrc, sbtVerify, protobuf }:

stdenv.mkDerivation {
  name = "mantis";
  src = mantisSrc;

  buildInputs = with pkgs; [ scala sbt sbtVerify unzip protobuf openjdk8 ];

  outputs = [ "out" "zip" ];

  configurePhase = ''
    export HOME="$NIX_BUILD_TOP"
    cp -r ${sbtVerify}/.ivy .
    cp -r ${sbtVerify}/.sbt .
    cp -r ${sbtVerify}/target .
    chmod -R u+w .ivy .sbt target

    # Get sbt to pre-fetch its dependencies. The cleanest way I've
    # found of doing this is to get it to list the available projects,
    # which it can only do once deps are downloaded.
    sbt -Dsbt.global.base=.sbt/1.0 -Dsbt.ivy.home=.ivy projects

    # We have to patch the executable embedded inside protoc-jar for
    # the one nix provides. :-(
    mkdir -p bin/3.4.0
    cp ${protobuf}/bin/protoc bin/3.4.0/protoc-3.4.0-linux-x86_64.exe
    jar uf .ivy/cache/com.github.os72/protoc-jar/jars/protoc-jar-3.4.0.jar bin/3.4.0/protoc-3.4.0-linux-x86_64.exe
  '';

  buildPhase = ''
    sbt -Dsbt.global.base=.sbt/1.0 -Dsbt.ivy.home=.ivy 'set test in Test := {}' dist
  '';

  installPhase = ''
    cp target/universal/mantis-1.0-daedalus-rc1.zip $zip

    mkdir $out
    unzip $zip
    mv mantis-1.0-daedalus-rc1/* $out
  '';
}
