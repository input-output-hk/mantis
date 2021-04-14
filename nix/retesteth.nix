{ stdenv, cacert, cmake, gcc, gnumake, perl, psmisc, solc, lllc, boost, pkg-config }:

stdenv.mkDerivation rec {
  pname = "retesteth";
  version = "v0.1.0-accesslist_fix";
  src = builtins.fetchurl {
    url = "https://github.com/ethereum/retesteth/archive/refs/tags/${version}.tar.gz";
    sha256 = "0gi38ykdg207klx8sb1f8xwf76agcjj3c87hrqqvqgxp0ir8hk7c";
  };
  nativeBuildInputs = [ cacert cmake gcc gnumake perl psmisc boost pkg-config ];
  buildInputs = [ solc lllc boost ];

  # configurePhase = ''
  #   mkdir build
  #   cd build
  #   export HUNTER_ROOT="$(pwd)/hunter"
  #   cmake .. -DCMAKE_BUILD_TYPE=Release -DHUNTER_ENABLED=off -DBoost_USE_STATIC_LIBS=OFF
  # '';

  cmakeFlags = [
    "-DHUNTER_ROOT=./hunter"
    "-DCMAKE_BUILD_TYPE=Release"
    # "-DHUNTER_ENABLED=OFF"
    "-DBoost_USE_STATIC_LIBS=OFF"
  ];

  # buildPhase = ''
  #   make -j4
  # '';
  installPhase = ''
    mkdir -p $out/bin
    mv retesteth/retesteth $out/bin/
  '';

  __noChroot = true;
  # outputHashMode = "recursive";
  # outputHashAlgo = "sha256";
  # outputHash = "sha256-wRPuMIQs+6VKK40VCHNHb9hFFPIdUW6XlXtVlaeVBj0=";
}
