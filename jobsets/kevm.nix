{ stdenv
, kevmSrc
, git
, openjdk8
, pandoc
, opam
, ocaml
, maven
, z3
, mpfr
, autoconf
, automake
, libtool
, ncurses
, unzip
, curl
, rsync
, gcc
, perl
, which
, pkgconfig
, flex
, python3
, zlib
}:

stdenv.mkDerivation {
  name = "kevm";
  src = kevmSrc;

  patches = [ ./kevm.patch ];
  buildInputs = [ pandoc openjdk8 ocaml opam maven z3 mpfr autoconf automake libtool ncurses unzip git curl rsync gcc perl which pkgconfig flex zlib python3 ];

  configurePhase = ''
    export HOME="$NIX_BUILD_TOP"
  '';

  buildPhase = ''
    make deps
    make
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp -pr .build/local/lib $out/
    cp -pr .build/vm/* $out/bin/
  '';
}
