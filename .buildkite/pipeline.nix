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
    nixExpr = commonAttrs // {
      label = "ensure Nix expressions are up-to-date";
      command = ''
        echo "Checking if Nix expressions are up-to-date..."

        nix-shell --run 'sbtix-gen-all2'

        set +e
        git diff --exit-code > nix-expr.patch
        if [ "$?" -eq "1" ]; then
          set -e
          echo "Nix expressions not up-to-date."
          echo "Download and apply the patch available in the artifact paths of this step:"
          echo "  patch -p1 < nix-expr.patch"
          echo "Aborting."
          exit 1
        else
          set -e
          echo "Nix expressions up-to-date!"
          exit 0
        fi
      '';
      retry.automatic = false;
      artifactPaths = [
        "nix-expr.patch"
      ];
    };

    scalafmt = commonAttrs // {
      label = "scalafmtCheck";
      command = ''
        nix-shell --run '$SBT scalafmtCheck'
      '';
    };

    compile = commonAttrs // {
      label = "compile everything";
      dependsOn = [ scalafmt ];
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

    test-rlp = commonAttrs // {
      dependsOn = [ compile ];
      label = "RLP tests";
      command = ''
        nix-shell --run '$SBT coverage rlp/test'
      '';
      artifactPaths = [
        "rlp/target/test-reports/**/*"
        "rlp/target/scala/2.12/scoverage-report/**/*"
        "rlp/target/scala/2.12/coverage-report/**/*"
      ];
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
