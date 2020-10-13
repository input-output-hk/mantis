{ system ? builtins.currentSystem, sources ? import ./sources.nix, src ? ../. }:

let
  overlay = final: prev: {
    inherit sources;
    inherit (import sources.gitignore { inherit (prev) lib; }) gitignoreSource;

    sbtix = prev.callPackage ../sbtix.nix {
      jdk = prev.openjdk8_headless;
      jre = prev.openjdk8_headless.jre;
    };

    mantisPkgs = final.callPackage ./pkgs/mantis {
      inherit (prev.openjdk8_headless) jre;
      inherit src;
    };

    inherit (final.mantisPkgs) mantis;

    mkSrc = import sources.nix-mksrc { inherit (final) lib; };
  };
in import sources.nixpkgs {
  inherit system;
  overlays = [ overlay ];
}
