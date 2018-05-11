{ nixpkgs
, kevmSrc
}:
let pkgs = import nixpkgs {};
in {
  kevm = pkgs.callPackage ./kevm.nix {
    inherit kevmSrc;
  };
}
