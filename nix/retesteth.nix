{ stdenv, boost175, cmake, solc, lllc, pkg-config, libyamlcpp, libscrypt }:
let
  cryptopp_cmake = stdenv.mkDerivation {
    name = "cryptopp_cmake";
    src = builtins.fetchurl {
      url = "https://github.com/noloader/cryptopp-cmake/archive/refs/tags/CRYPTOPP_8_5_0.tar.gz";
      sha256 = "sha256:1b4hdp6fk7188lh6pylsjkbrk3szvs567krzhy9njrsy804m4s0h";
    };

    installPhase = ''
      mkdir -p $out
      cp -r . $out
    '';
  };
in
stdenv.mkDerivation rec {
  pname = "retesteth";
  version = "v0.1.0-accesslist_fix";
  src = builtins.fetchurl {
    url = "https://github.com/ethereum/retesteth/archive/refs/tags/${version}.tar.gz";
    sha256 = "0gi38ykdg207klx8sb1f8xwf76agcjj3c87hrqqvqgxp0ir8hk7c";
  };
  nativeBuildInputs = [ cmake pkg-config ];
  buildInputs = [
    solc
    lllc
    boost175.dev
    libyamlcpp
    cryptopp_cmake
    libscrypt.src
  ];

  cmakeFlags = [
    "-DCMAKE_BUILD_TYPE=Release"
    "-DHUNTER_ENABLED=OFF"
    "-DBoost_DEBUG=ON"
  ];

  postPatch = ''
    sed -i "s/STATIC_LIBS ON/STATIC_LIBS OFF/g" CMakeLists.txt
  '';

  # buildPhase = ''
  #   make -j4
  # '';
  # installPhase = ''
  #   mkdir -p $out/bin
  #   mv retesteth/retesteth $out/bin/
  # '';

  # outputHashMode = "recursive";
  # outputHashAlgo = "sha256";
  # outputHash = "sha256-wRPuMIQs+6VKK40VCHNHb9hFFPIdUW6XlXtVlaeVBj0=";
}



