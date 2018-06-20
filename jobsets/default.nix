{ nixpkgs ? <nixpkgs>
, declInput ? {}
, prsJSON ? ./simple-pr-dummy.json
}:
let pkgs = import nixpkgs {};

    prs = builtins.fromJSON (builtins.readFile prsJSON );

    mkGitSrc = { repo, branch ? "refs/heads/master" }: {
      type = "git";
      value = repo + " " + branch;
      emailresponsible = false;
    };

    mkJob = { name, description, mantisBranch }: {
      inherit name;
      value = {
        inherit description;
        nixexprinput = "jobsetSrc";
        nixexprpath = "jobsets/release.nix";

        inputs = rec {
          # Which repo provides our main nix build config?
          # It's the current mantis branch. This alias is just for clarity.
          jobsetSrc = mantisSrc;

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
            branch = mantisBranch;
          };
          kevmSrc = mkGitSrc {
            repo = "https://github.com/kframework/evm-semantics.git";
            branch = "ca59159cdb747073207bf05b384b2d0b43c2541b";
          };
          secp256k1Src = mkGitSrc {
            repo = "https://github.com/bitcoin-core/secp256k1";
          };
          ieleSrc = mkGitSrc {
            repo = "https://github.com/runtimeverification/iele-semantics.git";
          };
        };

        enabled = 1;
        hidden = false;
        checkinterval = 300;
        schedulingshares = 100;
        emailoverride = "";
        enableemail = false;
        keepnr = 3;
      };
    };

    jobsetDefinition = pkgs.lib.listToAttrs (
      [
        (mkJob {
          name = "iele_testnet";
          description = "IELE Testnet";
          mantisBranch =  "refs/heads/phase/iele_testnet";
        })
      ]
      ++
      (pkgs.lib.mapAttrsToList
        (
          num:
          info:
            mkJob {
              name = "mantis-PR-${num}";
              description = info.title;
              mantisBranch = info.head.sha;
            }
        )
        prs
      )
    );
in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toJSON declInput}
    EOF

    cat <<EOF
    ${builtins.toJSON jobsetDefinition}
    EOF

    cat > $out <<EOF
    ${builtins.toJSON jobsetDefinition}
    EOF
  '';
}
