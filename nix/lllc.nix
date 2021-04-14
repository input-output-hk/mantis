{ stdenv, cmake, boost, fetchzip }:
let
  jsoncppURL = "https://github.com/open-source-parsers/jsoncpp/archive/1.9.2.tar.gz";
  jsoncpp = fetchzip {
    url = jsoncppURL;
    sha256 = "037d1b1qdmn3rksmn1j71j26bv4hkjv7sn7da261k853xb5899sg";
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

  # nativeBuildInputs = [ cacert cmake gcc gnumake perl psmisc ];

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
