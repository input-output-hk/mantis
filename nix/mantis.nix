{ src
, lib
, coreutils
, stdenv
, nix-gitignore
, solc
, makeWrapper
, runtimeShell
, jre
, sbt
, gawk
, gnused
, protobuf
, substituteAll
, writeBashBinChecked
, mantis-extvm-pb
, depsSha256
}:
let
  version =
    let
      versionSbt = builtins.readFile ../version.sbt;
      captures = builtins.match ''.*version in ThisBuild := "([^"]+)".*'' versionSbt;
    in
    builtins.elemAt captures 0;

  PATH = lib.makeBinPath [ jre solc coreutils gawk gnused ];
  LD_LIBRARY_PATH = ''''; #lib.makeLibraryPath [ libsonic ];

  # filter out mentions of protobridge, which is unable to execute
  protoc-wrapper = writeBashBinChecked "protoc" ''
    set -e

    for f in "$@"; do
      echo "''${f##*=}"
    done | grep protocbridge | xargs sed -i "1s|.*|#!${runtimeShell}|"

    exec ${protobuf}/bin/protoc "$@"
  '';

in
sbt.mkDerivation rec {
  pname = "mantis";
  inherit src version;

  nativeBuildInputs = [ solc protobuf makeWrapper ];

  preConfigure = ''
    HOME=$TMPDIR
    PROTOC_CACHE=.nix/protoc-cache

    chmod -R u+w src
    mkdir -p src/main/protobuf/extvm
    cp ${mantis-extvm-pb}/msg.proto src/main/protobuf/extvm/msg.proto
  '';

  # used by sbt-derivation to modify vendor derivation
  overrideDepsAttrs = oldAttrs: {
    inherit preConfigure;
  };
  PROTOCBRIDGE_SHELL = runtimeShell;

  patches = [
    (substituteAll {
      src = ./protoc.patch;
      protobuf = protoc-wrapper;
    })
  ];

  # This sha represents the change dependencies of mantis.
  # Update this sha whenever you change the dependencies using the
  # update-nix.sh script
  inherit depsSha256;

  # this is the command used to to create the fixed-output-derivation
  depsWarmupCommand = ''
    export PROTOC_CACHE=.nix/protoc-cache
    export HOME="$TMPDIR"
    export PATH="${PATH}:$PATH"

    chmod -R u+w src
    mkdir -p src/main/protobuf/extvm
    cp ${mantis-extvm-pb}/msg.proto src/main/protobuf/extvm/msg.proto

    sbt clean
    sbt compile --debug
  '';

  installPhase = ''
    sbt stage
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


}
