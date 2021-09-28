rev: final: prev: {
  jre = prev.jdk8.jre;

  mantis = final.callPackage ./mantis.nix {
    src = ../.;
    depsSha256 = "sha256-4DTSCv7491nG4+jF2VptULubFkVTW0IXRpGqNzuXU90=";
  };

  mantis-hash = final.mantis.override {
    depsSha256 = "sha256-0000000000000000000000000000000000000000000=";
  };

  # Last change to this was in 2018, so to avoid submodules we just clone
  # ourselves instead.
  mantis-extvm-pb = builtins.fetchGit {
    url = "https://github.com/input-output-hk/mantis-extvm-pb";
    rev = "8f52caba70afc95ce669e9d61f773468db54557e";
  };

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

  retesteth = final.callPackage ./retesteth.nix { };
  lllc = final.callPackage ./lllc.nix { };
  solc = final.callPackage ./solc.nix { };
}
