final: prev: {
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
    rev = "ae19e1fd9d3c0deba63c894be128d67e9519fe1f";
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
      (with final; [ coreutils restic gnugrep awscli diffutils mantis procps kevm ])
    }

    export LD_LIBRARY_PATH=${final.lib.makeLibraryPath [ final.glibc ]}

    ${builtins.readFile ./entrypoint.sh}
  '';

  mantis-entrypoint = final.symlinkJoin {
    name = "mantis";
    paths = with final; [ mantis mantis-entrypoint-script ];
  };

  retesteth = final.callPackage ./retesteth.nix { };
  lllc = final.callPackage ./lllc.nix { };
}
