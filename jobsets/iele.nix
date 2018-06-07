{ stdenv
, pkgs
, gmp
, secp256k1
, ieleSrc
}:

stdenv.mkDerivation {
  name = "iele";
  requiredSystemFeatures = [ "ubuntu" ];
  src = ieleSrc;

  buildInputs = with pkgs; [ autoconf automake libtool maven stack perl flex git gcc opam ocaml pandoc curl rsync unzip which pkgconfig zlib ncurses z3 mpfr gmp openjdk8 python2 secp256k1 ];

  patches = [ ./iele-spaces.patch ];

  configurePhase = ''
    export HOME=$NIX_BUILD_TOP
    export LD_LIBRARY_PATH=${gmp.out}/lib:$LD_LIBRARY_PATH
    make deps
    eval $(opam config env)
  '';

  installPhase = ''
    make install
    mv $HOME/.local $out
  '';
}
