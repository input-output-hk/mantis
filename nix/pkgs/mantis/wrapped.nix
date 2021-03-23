{ lib, stdenv, mantis, makeWrapper, jre, gawk, gnused, kevm, coreutils }:
let
  inherit (stdenv.lib) optionalString makeLibraryPath;
  inherit (stdenv) cc isDarwin;
  LD_LIBRARY_PATH = makeLibraryPath [ cc.cc.lib ];
  PATH = lib.makeBinPath [ jre gawk gnused kevm coreutils ];
in stdenv.mkDerivation {
  pname = "mantis";
  version = let
    buildSbt = builtins.readFile ../../../build.sbt;
    captures = builtins.match ''.*version := "([^"]+)".*'' buildSbt;
  in builtins.elemAt captures 0;

  nativeBuildInputs = [ makeWrapper ];

  src = mantis;

  buildPhase = ":";

  installPhase = ''
    cp -r $src $out
    chmod -R u+rw $out

    for p in $(find $out/bin/* -executable); do
      wrapProgram "$p" \
        --set PATH ${PATH} \
        ${
          optionalString (!isDarwin)
          "--prefix LD_LIBRARY_PATH : ${LD_LIBRARY_PATH}"
        }
    done

    ln -s ${kevm}/bin/kevm-vm $out/bin
  '';
}
