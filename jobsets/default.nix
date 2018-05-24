{ nixpkgs ? <nixpkgs>
, declInput ? {}
}:
let pkgs = import nixpkgs {};

    mkGitSrc = { repo, branch ? "refs/heads/master" }: {
      type = "git";
      value = repo + " " + branch;
      emailresponsible = false;
    };

    jobsetDefinition = {
      mantis = {
        description = "IELE Testnet - Mantis";
        nixexprinput = "src";
        nixexprpath = "jobsets/release.nix";

        inputs = {
          src = mkGitSrc {
            repo = "https://github.com/input-output-hk/mantis.git";
            branch = "refs/heads/hydra-config";
          };
          nixpkgs = mkGitSrc {
            repo = "https://github.com/NixOS/nixpkgs.git";
            branch = "06c576b0525da85f2de86b3c13bb796d6a0c20f6";
          };
          sbtVerifySrc = mkGitSrc {
            repo = "https://github.com/input-output-hk/sbt-verify.git";
            branch = "refs/tags/v0.4.1";
          };
          mantisSrc = mkGitSrc {
            repo = "https://github.com/input-output-hk/mantis.git";
            branch = "refs/heads/phase/iele_testnet";
          };
          kevmSrc = mkGitSrc {
            repo = "https://github.com/kframework/evm-semantics.git";
          };
          ethExplorerSrc = mkGitSrc {
            # This isn't public-access, so cannot be accessed by HTTP.
            # Access with git@ and ensure the hydra host's keys will be accepted.
            repo = "git@github.com:input-output-hk/ethereum-explorer.git";
          };
        };

        enabled = 1;
        hidden = false;
        checkinterval = 60;
        schedulingshares = 100;
        emailoverride = "";
        enableemail = false;
        keepnr = 3;
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
