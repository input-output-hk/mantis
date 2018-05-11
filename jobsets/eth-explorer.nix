{ stdenv, ethExplorerSrc, nodejs, yarn }:

stdenv.mkDerivation {
  src = ethExplorerSrc;

  name = "eth-explorer";
  buildInputs = [ nodejs yarn ];

  configurePhase = ''
    export HOME="$NIX_BUILD_TOP"
  '';

  buildPhase = ''
    yarn
    yarn run build
  '';

  installPhase = ''
    mkdir -p $out/lightweight-eth-explorer
    cp -r build/* $out/lightweight-eth-explorer/
  '';
}
