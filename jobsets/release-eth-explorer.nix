{ nixpkgs
, ethExplorerSrc
}:
let pkgs = import nixpkgs {};
in {
  ethExplorer = pkgs.callPackage ./eth-explorer.nix {
    inherit ethExplorerSrc;
  };
}
