{ lib, stdenv, fetchFromGitHub, dockerTools, autoreconfHook, gmp5, mpfr, zlib }:
let
  libPath = lib.makeLibraryPath [ secp gmp5 mpfr zlib ];

  secp = stdenv.mkDerivation {
    name = "secp256k1";

    src = fetchFromGitHub {
      owner = "bitcoin-core";
      repo = "secp256k1";
      rev = "f532bdc9f77f7bbf7e93faabfbe9c483f0a9f75f";
      sha256 = "sha256-PyqNZGER9VypH35S/aU4EBeepieI3BGXrYsJ141os24=";
    };

    nativeBuildInputs = [ autoreconfHook ];
  };
in stdenv.mkDerivation {
  name = "kevm";

  src = dockerTools.pullImage {
    imageName = "inputoutput/mantis";
    imageDigest =
      "sha256:594ed009f1bc1f12b86e11136441602107c3d580476002d9bae58b258a74ac1b";
    sha256 = "sha256-JT+FarGQlgYoO392Ne1ofdqmishZLfH+OwV2CXTYwdA=";
  };

  installPhase = ''
    mkdir -p tmp $out/bin
    tar --delay-directory-restore -C tmp -xf layer.tar || true

    cp tmp/bin/kevm-vm $out/bin
    chmod 0755 $out/bin/kevm-vm

    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${libPath}" \
      "$out/bin/kevm-vm"
  '';
}
