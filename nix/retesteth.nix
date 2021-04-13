{ stdenv, cacert, cmake, gcc, gnumake, perl, psmisc, solc }:

stdenv.mkDerivation rec {
  pname = "retesteth";
  version = "v0.1.0-accesslist_fix";
  src = builtins.fetchurl {
    url = "https://github.com/ethereum/retesteth/archive/refs/tags/${version}.tar.gz";
    sha256 = "0gi38ykdg207klx8sb1f8xwf76agcjj3c87hrqqvqgxp0ir8hk7c";
  };
  nativeBuildInputs = [ cacert cmake gcc gnumake perl psmisc ];
  buildInputs = [ solc ];
  configurePhase = ''
    mkdir build
    cd build
    export HUNTER_ROOT="$(pwd)/hunter"
    cmake .. -DCMAKE_BUILD_TYPE=Release
  '';
  buildPhase = ''
    make -j4
  '';
  installPhase = ''
    mkdir -p $out/bin
    mv retesteth/retesteth $out/bin/
  '';
  outputHashMode = "recursive";
  outputHashAlgo = "sha256";
  outputHash = "sha256-0e4a3riDsNfb1vpgSAGupSUnssIjZgYCfo16B7utjnI=";
}
