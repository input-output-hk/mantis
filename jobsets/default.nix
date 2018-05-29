{ nixpkgs ? <nixpkgs>
, declInput ? {}
, mantisPrsJSON ? ./simple-pr-dummy.json
}:
let pkgs = import nixpkgs {};

    mantisPrs = builtins.fromJSON (builtins.readFile mantisPrsJSON );

    mkGitSrc = { repo, branch ? "refs/heads/master" }: {
      type = "git";
      value = repo + " " + branch;
      emailresponsible = false;
    };

    mkMantisBuild = { name, description, mantisBranch }: {
      inherit name;
      value = {
        description = "Mantis - ${description}";
        nixexprinput = "jobsetSrc";
        nixexprpath = "jobsets/release.nix";

        inputs = {
          # Which repo provides our main nix build config?
          # It's the current mantis branch. This alias is just for clarity.
          jobsetSrc = mantisBranch;

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
          };
          ethExplorerSrc = mkGitSrc {
            # This isn't public-access, so cannot be accessed over HTTP(S).
            # Access with git@ and ensure the hydra host's keys will be accepted.
            repo = "git@github.com:input-output-hk/ethereum-explorer.git";
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
        (mkMantisBuild {
          name = "master";
          description = "Master";
          mantisBranch =  "refs/heads/master";
        })
        (mkMantisBuild {
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
            mkMantisBuild {
              name = "mantis-PR-${num}";
              description = info.title;
              mantisBranch = info.head.sha;
            }
        )
        mantisPrs
      )
    );
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
