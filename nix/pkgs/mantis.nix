{ src
, lib
, stdenv
, makeWrapper
, writeShellScriptBin
, bash
, protobuf
, coreutils
, jdk8
, gawk
, sbt
, impure ? false
}:

let

  # sbt-protoc puts the scala plugin in /tmp/protobridge<some-random-number>.
  # it is in fact a shell script with a standard `#!/usr/bin/env sh` shebang
  # that makes the Nix sandbox ANGRY and breaks all the things in a cryptic,
  # hairpull-inducing way. So we gotta sed it out. Not the prettiest thing
  # but it works.
  #
  # This will be unnecessary in sbt-protoc>=1.0.0, as we can specify a file path
  # Also, https://github.com/thesamet/sbt-protoc/issues/209 will need to be resolved
  # for the plugins to not fail.
  protoc-wrapper = writeShellScriptBin "protoc" ''
    set -e

    for f in "$@"; do
      echo ''${f##*=}
    done | grep protocbridge | xargs sed -i "1s|.*|#!${bash}/bin/bash|"

    exec ${protobuf}/bin/protoc "$@"
  '';
  nativeBuildInputs = [ protoc-wrapper jdk8 makeWrapper ];

  # read version from build.sbt
  version = let
    buildSbt = builtins.readFile ../../build.sbt;
    captures = builtins.match ''.*version := "([^"]+)".*'' buildSbt;
  in builtins.elemAt captures 0;

  LD_LIBRARY_PATH = lib.makeLibraryPath [ stdenv.cc.cc.lib ];
  PATH = lib.makeBinPath [ coreutils jdk8.jre gawk ];

in sbt.mkDerivation {
  pname = "mantis";

  inherit src nativeBuildInputs version;

  # This sha represents the change dependencies of mantis.
  # Update this sha whenever you change the dependencies
  depsSha256 = "1zsf8yykr8a7p9za4lyw8l1rhqa7ppas049lawp9pn90sj0xkjh5";

  # this is the command used to to create the fixed-output-derivation
  depsWarmupCommand = "sbt compile --debug -Dnix=true";

  # just inherit build artifacts from fixed-output-derivation
  dontBuild = true;

  overrideDepsAttrs = oldAttrs: oldAttrs // {
    # $out is a directory, thus the archive will fail to write a file
    preInstall = ''
      rm -rf $out
    '';
  };

  installPhase = ''
    sbt stage -Dnix=true
    mkdir -p $out/
    cp -r target/universal/stage/* $out/
    mkdir -p $out/share/mantis
    mv $out/{LICENSE,RELEASE,mantis_config.txt} $_

    # wrap executable so that java is available at runtime
    for p in $(find $out/bin/* -executable); do
    wrapProgram "$p" \
      --prefix PATH : ${PATH} \
      ${lib.optionalString (!stdenv.isDarwin)
        "--prefix LD_LIBRARY_PATH : ${LD_LIBRARY_PATH}"
      }
    done
  '';

  dontPatchShebangs = impure;

  # limit warnings from trying to strip jar files
  dontStrip = true;
}
