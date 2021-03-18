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
, writeShellScriptBin
}:

let
  version = let
    buildSbt = builtins.readFile ../build.sbt;
    captures = builtins.match ''.*version := "([^"]+)".*'' buildSbt;
  in builtins.elemAt captures 0;

  PATH = lib.makeBinPath [ jre solc coreutils gawk gnused ];
  LD_LIBRARY_PATH = ''''; #lib.makeLibraryPath [ libsonic ];

  # filter out mentions of protobridge, which is unable to execute
  protoc-wrapper = writeShellScriptBin "protoc" ''
    set -e

    for f in "$@"; do
      echo ''${f##*=}
    done | grep protocbridge | xargs sed -i "1s|.*|#!${runtimeShell}|"

    exec ${protobuf}/bin/protoc "$@"
  '';


in sbt.mkDerivation rec {
  pname = "mantis";
  inherit src version;

  nativeBuildInputs = [ solc protobuf makeWrapper ];

  preConfigure = ''
    HOME=$TMPDIR
    PROTOC_CACHE=.nix/protoc-cache
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

  #patchPhase = lib.optionalString (libsonic != null) ''
  #  rm -rf src/main/resources
  #  cp -r ${libsonic}/lib src/main/resources
  #'';

  # This sha represents the change dependencies of mantis.
  # Update this sha whenever you change the dependencies
  depsSha256 = "0n7vv4k73cxjwg40qggr7gnkkg7vn8a179sf0wxnz3absj1700jj";

  # this is the command used to to create the fixed-output-derivation
  depsWarmupCommand = "PROTOC_CACHE=.nix/protoc-cache; HOME=$TMPDIR; PATH=${PATH}:$PATH; sbt clean; sbt compile --debug";

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
