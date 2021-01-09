{ system ? builtins.currentSystem, sources ? import ./sources.nix, src ? ../. }:

let
  overlay = final: prev: let
    inherit (import sources.gitignore { inherit (prev) lib; }) gitignoreSource;
  in {
    inherit sources;

    mantis = final.callPackage ./pkgs/mantis.nix {
      inherit (prev.openjdk8_headless) jre;
      src = prev.lib.cleanSource (gitignoreSource src); #prev.lib.cleanSource src;
    };
  };
  sbt-derivation-overlay = import sources.sbt-derivation;
in import sources.nixpkgs {
  inherit system;
  overlays = [
    overlay
    sbt-derivation-overlay
  ];
}
