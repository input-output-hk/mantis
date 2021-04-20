{ stdenv, cmake, boost, fetchzip }:
let
  jsoncppURL = "https://github.com/open-source-parsers/jsoncpp/archive/1.8.4.tar.gz";
  jsoncpp = fetchzip {
    url = jsoncppURL;
    sha256 = "sha256-lX5sbu2WJuTfyFmFRkCbMOjlMQE62nmrjPN6adSRD/w=";
  };
in
stdenv.mkDerivation rec {

  pname = "lllc";
  version = "14c9d5de6c7e58f1d1b18a04b57ec9f190d7cd2f";

  src = builtins.fetchurl {
    url = "https://github.com/winsvega/solidity/archive/${version}.tar.gz";
    sha256 = "0s6nz0w8fr1347d05kx4z3z72b1k0kmcz6d4dwwc24rydjznkw6r";
  };

  postPatch = ''
    substituteInPlace cmake/jsoncpp.cmake \
      --replace "${jsoncppURL}" ${jsoncpp}
  '';

  preConfigure = ''
    echo ${version} > commit_hash.txt
  '';

  cmakeFlags = [
    "-DBoost_USE_STATIC_LIBS=OFF"
    "-DCMAKE_BUILD_TYPE=Release"
    "-DLLL=1"
  ];

  nativeBuildInputs = [ cmake ];
  buildInputs = [ boost ];

  buildPhase = ''
    make lllc
  '';

  installPhase = ''
    mkdir -p $out/bin
    mv lllc/lllc $out/bin/
  '';
}
