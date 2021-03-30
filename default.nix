{ system ? builtins.currentSystem }:
let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/a98302aa9b9628915878a6ea9776c40a0bb02950.tar.gz";
    sha256 = "01zq4hhfvpyrr17zk9n911zg406afkyvv97mrcn61ajfc30fypkb";
  };
  sbt-derivation = fetchTarball {
    url = "https://github.com/zaninime/sbt-derivation/archive/9666b2b589ed68823fff1cefa4cd8a8ab54956c1.tar.gz";
    sha256 = "17r74avh4i3llxbskfjhvbys3avqb2f26pzydcdkd8a9k204rg9z";
  };
  pkgs = import nixpkgs {
    inherit system;
    config = {};
    overlays = [
      (import sbt-derivation)
      (import ./nix/overlay.nix)
    ];
  };
in pkgs
