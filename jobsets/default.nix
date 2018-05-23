{ nixpkgs ? <nixpkgs>
, declInput ? {}
}:
let pkgs = import nixpkgs {};
    mkGitSrc = { repo, branch ? "refs/heads/master" }: {
      type = "git";
      value = repo + " " + branch;
      emailresponsible = false;
    };
    mkDefinition = { description, nixexprinput, nixexprpath, inputs }: {
        inherit description nixexprinput nixexprpath;
        enabled = 1;
        hidden = false;
        checkinterval = 60;
        schedulingshares = 100;
        emailoverride = "";
        enableemail = false;
        keepnr = 3;
        inputs = {
          src = mkGitSrc {
            repo = "https://github.com/input-output-hk/mantis.git";
            branch = "refs/heads/hydra-config";
          };
          nixpkgs = mkGitSrc {
            repo = "https://github.com/NixOS/nixpkgs.git";
            branch = "06c576b0525da85f2de86b3c13bb796d6a0c20f6";
          };
        } // inputs;
    };
    jobsetDefinition = {
      mantis = mkDefinition {
        description = "IELE Testnet - Mantis";
        nixexprinput = "src";
        nixexprpath = "jobsets/release-mantis.nix";
        inputs = {
          sbtVerifySrc = mkGitSrc {
            repo = "https://github.com/input-output-hk/sbt-verify.git";
            branch = "refs/tags/v0.4.1";
          };
          mantisSrc = mkGitSrc {
            repo = "https://github.com/input-output-hk/mantis.git";
            branch = "refs/heads/phase/iele_testnet";
          };
        };
      };
      kevm = mkDefinition {
        description = "IELE Testnet - KEVM";
        nixexprinput = "src";
        nixexprpath = "jobsets/release-kevm.nix";
        inputs = {
          kevmSrc = mkGitSrc {
            repo = "https://github.com/kframework/evm-semantics.git";
          };
        };
      };
      "eth-explorer" = mkDefinition {
        description = "IELE Testnet - Web Frontend";
        nixexprinput = "src";
        nixexprpath = "jobsets/release-eth-explorer.nix";
        inputs = {
          ethExplorerSrc = mkGitSrc {
            repo = "https://github.com/AlanVerbner/lightweight-eth-explorer.git";
          };
        };
      };
    };
in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    ${builtins.toJSON jobsetDefinition}
    EOF
  '';
}
