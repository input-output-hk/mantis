{ lib, stdenv, dockerTools, secp256k1, gmp5, mpfr, zlib }:
let libPath = lib.makeLibraryPath [ secp256k1 gmp5 mpfr zlib ];
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
