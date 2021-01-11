{ system ? builtins.currentSystem, sources ? import ./sources.nix, src ? ../. }:

let
  # we need to filter out the nix files in this repository as they will
  # affect the fixed derivation calculation from sbt-derivation
  overlay = final: prev: let
    inherit (import sources.gitignore { inherit (prev) lib; }) gitignoreSource;
    cleanedSrc = prev.lib.cleanSource (gitignoreSource src);
  in {
    inherit sources;

    mantis = final.callPackage ./pkgs/mantis.nix {
      inherit (prev.openjdk8_headless) jre;
      src = cleanedSrc;
    };
  };
  sbt-derivation-overlay = import sources.sbt-derivation;
in import sources.nixpkgs {
  inherit system;
  overlays = [
    sbt-derivation-overlay
    overlay
  ];
}
