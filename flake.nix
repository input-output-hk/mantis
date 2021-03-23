{
  description = "Mantis flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url =
      "github:nixos/nixpkgs?rev=a98302aa9b9628915878a6ea9776c40a0bb02950";
    nixpkgs-sbt = {
      url = "github:nixos/nixpkgs?rev=e2bb73ce5f786b83e984b80199112f86b8a6cc9d";
      flake = false;
    };
    sbtix = {
      url =
        "github:input-output-hk/Sbtix?rev=7b969a5641fce10500ca51cbe88af4ea160d7064";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, nixpkgs-sbt, ... }@inputs:
    let
      overlay = final: prev: {
        inherit (import nixpkgs-sbt { inherit (final) system; }) sbt;
        sbtix = final.callPackage ./sbtix.nix { };

        mantisPkgs = final.callPackage ./nix/pkgs/mantis {
          src = builtins.fetchGit {
            url = "https://github.com/input-output-hk/mantis";
            rev = self.rev or "482340d5e6ab635e5a5047e9b670d59b4ad366c2";
            ref = "develop";
            submodules = true;
          };
        };

        jdk = prev.openjdk8_headless;
        jre = prev.openjdk8_headless.jre;

        inherit (final.mantisPkgs) mantis;

        kevm = final.callPackage ./nix/pkgs/kevm.nix { };

        mantis-entrypoint = final.callPackage ./nix/entrypoint.nix { };
      };

      pkgsForSystem = system:
        (import nixpkgs) {
          inherit system;
          overlays = [ overlay ];
        };

      mkHydraUtils = mkPkgs:
        let
          # nothing in lib should really depend on the system
          libPkgs = mkPkgs "x86_64-linux";
          # [attrset] -> attrset
          recursiveMerge = libPkgs.lib.foldr libPkgs.lib.recursiveUpdate { };
          mkHydraJobsForSystem = attrs: system:
            recursiveMerge
            (map (n: { "${n}"."${system}" = (mkPkgs system)."${n}"; }) attrs);
        in {
          collectHydraSets = jobSets: { hydraJobs = recursiveMerge jobSets; };
          mkHydraSet = attrs: systems:
            recursiveMerge (map (mkHydraJobsForSystem attrs) systems);
        };

      hydraUtils = mkHydraUtils pkgsForSystem;
      inherit (hydraUtils) collectHydraSets mkHydraSet;

    in flake-utils.lib.eachDefaultSystem (system: rec {
      pkgs = pkgsForSystem system;
      legacyPackages = pkgs;

      defaultPackage = pkgs.mantis;
      devShell = pkgs.mkShell { nativeBuildInputs = with pkgs; [ solc sbt ]; };
      apps.mantis = flake-utils.lib.mkApp { drv = pkgs.mantis; };
      defaultApp = apps.mantis;
    }) // (collectHydraSets [
      (mkHydraSet [ "mantis" ] [ "x86_64-linux" "x86_64-darwin" ])
      (mkHydraSet [ "kevm" ] [ "x86_64-linux" ])
      (mkHydraSet [ "mantis-entrypoint" ] [ "x86_64-linux" ])
    ]);
}
