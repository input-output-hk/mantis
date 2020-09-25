{ cfg, pkgs, ... }:

with cfg.steps.commands;

let
  commonAttrs = {
    retry.automatic = true;
    agents.queue = "project42";
  };
in

{
  steps.commands = {
    compile = commonAttrs // {
      label = "compile everything";
      command = ''
        nix-shell --run '$SBT compile-all'
      '';
    };

    style = commonAttrs // {
      dependsOn = [ compile ];
      label = "scalastyle";
      command = ''
        nix-shell --run '$SBT scalastyle test:scalastyle'
      '';
    };

    test-unit = commonAttrs // {
      dependsOn = [ compile ];
      label = "unit tests";
      command = ''
        nix-shell --run '$SBT coverage test'
      '';
      artifactPaths = [
        "target/test-reports/**/*"
        "target/scala/2.12/scoverage-report/**/*"
        "target/scala/2.12/coverage-report/**/*"
      ];
    };

    test-evm = commonAttrs // {
      dependsOn = [ compile ];
      label = "EVM tests";
      command = ''
        nix-shell --run '$SBT coverage evm:test'
      '';
      artifactPaths = [
        "target/test-reports/**/*"
        "target/scala/2.12/scoverage-report/**/*"
        "target/scala/2.12/coverage-report/**/*"
      ];
    };

    test-integration = commonAttrs // {
      dependsOn = [ compile ];
      label = "integration tests";
      command = ''
        nix-shell --run '$SBT coverageOff it:test'
      '';
      artifactPaths = [ "target/test-reports/**/*" ];
    };

    test-ets = commonAttrs // {
      dependsOn = [ compile ];
      label = "ETS";
      command = ''
        nix-shell --run './test-ets.sh'
      '';
    };

    coverageReport = commonAttrs // {
      dependsOn = [ test-unit test-evm ];
      label = "coverage report";
      command = ''
        nix-shell --run '$SBT coverageReport coverageAggregate'
      '';
    };

    additional = commonAttrs // {
      dependsOn = [ compile ];
      label = "additional compilation & dist";
      command = ''
        nix-shell --run '$SBT benchmark:compile snappy:compile dist'
      '';
    };
  };
}
