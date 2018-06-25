{ pkgs
, soliditySrc
, stdenv
}:

stdenv.mkDerivation {
  name = "solidity";
  src = soliditySrc;

  buildInputs = with pkgs; [
    cmake
    llvm
    boost
    libtool
    autoconf
    automake
    z3
  ];

  patches = [ ./solidity.patch ];

  configurePhase = ''
    mkdir build
    cd build
    cmake ..
  '';

  installPhase = ''
    mkdir -p $out/bin
    mv solc/solc $out/bin
  '';
}
