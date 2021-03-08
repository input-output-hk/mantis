rev: final: prev: {
  jre = prev.jdk8.jre;

  mantis = final.callPackage ./mantis.nix {
    src = builtins.fetchGit {
      url = "https://github.com/input-output-hk/mantis";
      ref = "flake";
      rev = rev;
      submodules = true;
    };
  };

  writeBashChecked = final.writers.makeScriptWriter {
    interpreter = "${final.bashInteractive}/bin/bash";
    check = final.writers.writeBash "shellcheck-check" ''
      ${final.shellcheck}/bin/shellcheck -x "$1"
    '';
  };

  writeBashBinChecked = name: final.writeBashChecked "/bin/${name}";

  mantis-entrypoint = final.writeBashBinChecked "mantis-entrypoint" ''
    export PATH=${final.lib.makeBinPath [ final.coreutils final.restic ]}

    ${builtins.readFile ./entrypoint.sh}
  '';
}
