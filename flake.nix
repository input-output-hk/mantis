{
  description = "Mantis flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url =
    "github:nixos/nixpkgs?rev=a98302aa9b9628915878a6ea9776c40a0bb02950";
  inputs.sbt-derivation.url = "github:zaninime/sbt-derivation";
  inputs.kevm.url = "github:input-output-hk/kevm.flake/v1.0.1-fc35db2";
  inputs.mantis-explorer.url = "github:input-output-hk/mantis-explorer";
  inputs.mantis-faucet-web.url = "github:input-output-hk/mantis-faucet-web";

  outputs = inputs@{ self, nixpkgs, flake-utils, sbt-derivation, kevm, ...
    }: # , libsonic, libsonic-jnr }:
    let
      overlay = import ./nix/overlay.nix inputs;
      pkgsForSystem = system:
        (import nixpkgs) {
          inherit system;
          overlays = [
            #libsonic.overlay
            #libsonic-jnr.overlay
            sbt-derivation.overlay
            overlay
          ];
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
    }) // (collectHydraSets
      (map (name: mkHydraSet [ name ] [ "x86_64-linux" ]) [
        "jdk8"
        "lllc"
        "mantis"
        "mantis-entrypoint"
        "netcat-gnu"
        "retesteth"
        "sbt"
        "solc"
      ]));
}
