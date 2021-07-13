{ stdenv
, lib
, cmake
, fetchFromGitHub
, pkg-config
, libyamlcpp
, boost175
, cryptopp
, curl
, writeTextFile
}:
let
  libscrypt = stdenv.mkDerivation
    rec {
      pname = "libscrypt";
      version = "0.0.1";

      src = fetchFromGitHub {
        owner = "hunter-packages";
        repo = pname;
        rev = "62755c372cdcb8e40f35cf779f3abb045aa39063";
        sha256 = "sha256-UBRiSG4VFiAADEMiK1klmH/RwL0y/ZLvA1DNaAk5U1o=";
      };

      nativeBuildInputs = [ cmake ];
    };

  secp256k1 = builtins.fetchurl {
    url = "https://github.com/chfast/secp256k1/archive/ac8ccf29b8c6b2b793bc734661ce43d1f952977a.tar.gz";
    sha256 = "02f8f05c9e9d2badc91be8e229a07ad5e4984c1e77193d6b00e549df129e7c3a";
  };
  mpir = builtins.fetchurl {
    url = "https://github.com/chfast/mpir/archive/cmake.tar.gz";
    sha256 = "d32ea73cb2d8115a8e59b244f96f29bad7ff03367162b660bae6495826811e06";
  };
  libff = builtins.fetchurl {
    url = "https://github.com/scipr-lab/libff/archive/03b719a7c81757071f99fc60be1f7f7694e51390.tar.gz";
    sha256 = "81b476089af43025c8f253cb1a9b5038a1c375baccffea402fa82042e608ab02";
  };


  cryptoPcFile = writeTextFile {
    name = "libcryptopp.pc";
    text = ''
      # Crypto++ package configuration file
      prefix=@out@
      libdir=''${prefix}/lib
      includedir=@dev@/include
      Name: Crypto++
      Description: Crypto++ cryptographic library
      Version: 5.6.5
      URL: https://cryptopp.com/
      Cflags: -I''${includedir}
      Libs: -L''${libdir} -lcryptopp
    '';
  };

  cryptopp_5_6_5 = cryptopp.overrideAttrs (oldAttrs: rec {
    version = "5.6.5";
    outputs = [ "out" "dev" ];
    src = fetchFromGitHub {
      owner = "weidai11";
      repo = "cryptopp";
      rev = "CRYPTOPP_5_6_5";
      sha256 = "sha256-h+7LK8nzk1NlkVB4Loc9VQpN79SUFvBYESSpTZyXZ/o=";
    };
    postPatch = "";
    preConfigure = " ";
    buildFlags = [ "static" "shared" ];
    installTargets = "";
    postInstall = ''
      mkdir -p $dev/lib/pkgconfig
      substituteAll ${cryptoPcFile} $dev/lib/pkgconfig/libcryptopp.pc
      ln -sr $out/lib/libcryptopp.so.${version} $out/lib/libcryptopp.so.${lib.versions.majorMinor version}
      ln -sr $out/lib/libcryptopp.so.${version} $out/lib/libcryptopp.so.${lib.versions.major version}
    '';
  });

in
stdenv.mkDerivation rec {
  pname = "retesteth";
  version = "v0.1.1-eip1559";

  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "retesteth";
    rev = "remove-hunter";
    sha256 = "sha256-NdiH01EPM9lHnWXgDj7DqZOt5GPIk3hmZSM2blj0+SM=";
  };

  nativeBuildInputs = [ cmake pkg-config ];

  buildInputs = [
    boost175
    libyamlcpp
    cryptopp_5_6_5
    curl
    libscrypt
  ];

  cmakeFlags = [
    "-DCMAKE_BUILD_TYPE=Release"
  ];

  preBuild = ''
    cp ${libff} deps/src/libff-03b719a7.tar.gz
    cp ${secp256k1} deps/src/secp256k1-ac8ccf29.tar.gz
    cp ${mpir} deps/src/mpir-cmake.tar.gz
  '';
}
