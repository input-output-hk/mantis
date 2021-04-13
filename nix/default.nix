{ system ? builtins.currentSystem, sources ? import ./sources.nix, src ? ../. }:
let
  # we need to filter out the nix files in this repository as they will
  # affect the fixed derivation calculation from sbt-derivation
  overlay = final: prev:
    let
      inherit (import sources.gitignore { inherit (prev) lib; }) gitignoreSource;
      # If src isn't a path, it's likely a prefiltered store path, so use it directly
      # This also makes Nix flakes work by passing the flake source as the `src` arg
      cleanedSrc =
        if builtins.isPath src
        then prev.lib.cleanSource (gitignoreSource src)
        else src;
    in
    {
      inherit sources;

      # match java version used by devs, this should also change the version used by sbt
      jre = prev.jdk8.jre;

      mantis = final.callPackage ./pkgs/mantis.nix {
        src = cleanedSrc;
      };

      retesteth = final.callPackage ./retesteth.nix { };
      # lllc = final.callPackage ./lllc.nix { };
    };

  sbt-derivation-overlay = import sources.sbt-derivation;
in
import sources.nixpkgs {
  inherit system;
  overlays = [
    sbt-derivation-overlay
    overlay
  ];
}
