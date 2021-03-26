rev: final: prev: {
  jre = prev.jdk8.jre;

  mantis = final.callPackage ./mantis.nix {
    src = builtins.fetchGit {
      url = "https://github.com/input-output-hk/mantis";
      ref = "develop";
      rev = rev;
      submodules = true;
    };
  };

  mantis-hash = { ref, rev }:
    (final.callPackage ./mantis.nix {
      src = builtins.fetchGit {
        url = "https://github.com/input-output-hk/mantis";
        inherit rev ref;
        submodules = true;
      };
    }).overrideAttrs (_: {
      outputHash = "sha256-0000000000000000000000000000000000000000000=";
      outputHashMode = "recursive";
    });

  writeBashChecked = final.writers.makeScriptWriter {
    interpreter = "${final.bashInteractive}/bin/bash";
    check = final.writers.writeBash "shellcheck-check" ''
      ${final.shellcheck}/bin/shellcheck -x "$1"
    '';
  };

  writeBashBinChecked = name: final.writeBashChecked "/bin/${name}";

  mantis-entrypoint-script = final.writeBashBinChecked "mantis-entrypoint" ''
    export PATH=${
      final.lib.makeBinPath
      (with final; [ coreutils restic gnugrep awscli diffutils mantis procps ])
    }

    ${builtins.readFile ./entrypoint.sh}
  '';

  mantis-entrypoint = final.symlinkJoin {
    name = "mantis";
    paths = with final; [ mantis mantis-entrypoint-script ];
  };
}
