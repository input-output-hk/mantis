{ stdenv
, pkgs
, gmp
, secp256k1
, ieleSrc
}:

stdenv.mkDerivation {
  name = "iele";
  src = ieleSrc;

  buildInputs = with pkgs; [ autoconf automake libtool secp256k1 maven haskell.compiler.ghc802 stack perl flex git gcc opam ocaml pandoc curl rsync unzip which pkgconfig zlib ncurses z3 mpfr gmp openjdk8 python2 bash ];

  patches = [ ./iele-tests.patch ./iele-deps.patch ];

  configurePhase = ''
    export HOME=$NIX_BUILD_TOP
    export LD_LIBRARY_PATH=${gmp.out}/lib:$LD_LIBRARY_PATH
    export NIX_CFLAGS_COMPILE="-Wno-error=unused-result $NIX_CFLAGS_COMPILE"
    make deps
    eval $(opam config env)
  '';

  installPhase = ''
    make install
    mv $HOME/.local $out
  '';
}
