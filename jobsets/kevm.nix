{ stdenv
, kevmSrc
, pkgs
}:

stdenv.mkDerivation {
  name = "kevm";
  src = kevmSrc;

  patches = [ ./kevm.patch ];
  buildInputs = with pkgs; [ pandoc openjdk8 ocaml opam maven z3 mpfr autoconf automake libtool ncurses unzip git curl rsync gcc perl which pkgconfig flex zlib python3 ];

  configurePhase = ''
    export HOME="$NIX_BUILD_TOP"
    export NIX_CFLAGS_COMPILE="-Wno-error=unused-result $NIX_CFLAGS_COMPILE"
    make deps
  '';

  installPhase = ''
    mkdir $out
    mv .build/local/lib $out/
    mv .build/vm $out/bin
  '';
}
