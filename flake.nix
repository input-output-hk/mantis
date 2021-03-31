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
    nixpkgs-kevm = {
      url = "github:nixos/nixpkgs?rev=df25e214c8e662d693ef89e45ce56bbf58d6c59e";
      flake = false;
    };
    sbtix = {
      url =
        "github:input-output-hk/Sbtix?rev=7b969a5641fce10500ca51cbe88af4ea160d7064";
      flake = false;
    };
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    mantis-faucet-web.url = "github:input-output-hk/mantis-faucet-web";
    mantis-explorer.url = "github:input-output-hk/mantis-explorer";
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    let
      overlay = import ./nix/overlay.nix inputs;

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
      (mkHydraSet [
        "mantis"
        "kevm"
        "iele"

        "mantis-entrypoint"
        "mantis-faucet-entrypoint"

        "mantis-explorer-evm"
        "mantis-explorer-iele"
        "mantis-explorer-kevm"

        "mantis-faucet-web-evm"
        "mantis-faucet-web-iele"
        "mantis-faucet-web-kevm"
      ] [ "x86_64-linux" ])
    ]);
}
