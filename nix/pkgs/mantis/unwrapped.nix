# this file originates from SBTix
{ src, stdenv, writeShellScriptBin, bash, protobuf, sbtix
, jdk, mantis-extvm-pb, runCommand, impure ? false }:

let
  inherit (stdenv.lib) optionalString makeLibraryPath;
  inherit (stdenv) cc isDarwin;

  LD_LIBRARY_PATH = makeLibraryPath [ cc.cc.lib ];

  unlessDarwin = optionalString (!stdenv.isDarwin);

  # sbt-protoc puts the scala plugin in /tmp/protobridge<some-random-number>.
  # it is in fact a shell script with a standard `#!/usr/bin/env sh` shebang
  # that makes the Nix sandbox ANGRY and breaks all the things in a cryptic,
  # hairpull-inducing way. So we gotta sed it out. Not the prettiest thing
  # but it works.
  protoc-wrapper = writeShellScriptBin "protoc" ''
    set -e

    for f in "$@"; do
      echo "''${f##*=}"
    done | grep protocbridge | xargs sed -i "1s|.*|#!${bash}/bin/bash|"

    exec ${protobuf}/bin/protoc "$@"
  '';

  extSrc = runCommand "mantis-src-ext" {} ''
    cp -r ${src} $out
    chmod -R u+w $out
    mkdir -p $out/src/main/protobuf/extvm
    cp ${mantis-extvm-pb}/msg.proto $out/src/main/protobuf/extvm/msg.proto
  '';

in sbtix.buildSbtProject {
  src = extSrc;

  name = "mantis";
  sbtOptions = "-Dnix=true";
  buildInputs = [ protoc-wrapper ];
  sbtixBuildInputs = [];

  repo = [
    (import ../../../repo.nix)
    (import ../../../project/repo.nix)
    (import ../../../manual-repo.nix)
  ];

  installPhase = ''
    sbt stage
    mkdir -p $out/
    cp target/universal/stage/* $out/ -r
    mkdir -p $out/share/mantis
    mv $out/{LICENSE,RELEASE,mantis_config.txt} $_
  '';

  dontPatchShebangs = impure;
}
