inputs: final: prev: {
  # Little convenience function helping us to containing the bash
  # madness: forcing our bash scripts to be shellChecked.
  writeBashChecked = final.writers.makeScriptWriter {
    interpreter = "${final.bash}/bin/bash";
    check = final.writers.writeBash "shellcheck-check" ''
      ${final.shellcheck}/bin/shellcheck -x "$1"
    '';
  };
  writeBashBinChecked = name: final.writeBashChecked "/bin/${name}";

  mantisPkgs = final.callPackage ./pkgs/mantis {
    src = builtins.fetchGit {
      url = "https://github.com/input-output-hk/mantis";
      rev = inputs.self.rev or "482340d5e6ab635e5a5047e9b670d59b4ad366c2";
      ref = "3.1.0-flake";
      submodules = true;
    };
  };

  inherit (final.mantisPkgs) mantis;

  inherit (import inputs.nixpkgs-sbt { inherit (final) system; }) sbt;

  sbtix = final.callPackage ../sbtix.nix { };

  jdk = prev.openjdk8_headless;
  jre = prev.openjdk8_headless.jre;
  kevm = final.callPackage ./pkgs/kevm.nix { };
  iele = final.callPackage ./pkgs/iele.nix { };
  mantis-entrypoint = final.callPackage ./entrypoint.nix { };

  makeFaucet = name:
    (final.callPackage ./pkgs/nginx.nix {
      package =
        inputs.mantis-faucet-web.defaultPackage.${final.system}.overrideAttrs
        (old: {
          MANTIS_VM = prev.lib.toUpper name;
          FAUCET_NODE_URL =
            "https://faucet-${prev.lib.toLower name}.portal.dev.cardano.org";
        });
      target = "/mantis-faucet";
    });

  makeExplorer = MANTIS_VM:
    (prev.callPackage ./pkgs/nginx.nix {
      package =
        inputs.mantis-explorer.defaultPackage.${final.system}.overrideAttrs
        (old: { inherit MANTIS_VM; });
      target = "/mantis-explorer";
    });

  mantis-explorer-evm = final.makeExplorer "EVM";
  mantis-explorer-iele = final.makeExplorer "IELE";
  mantis-explorer-kevm = final.makeExplorer "KEVM";

  mantis-faucet-web-evm = final.makeFaucet "EVM";
  mantis-faucet-web-iele = final.makeFaucet "IELE";
  mantis-faucet-web-kevm = final.makeFaucet "KEVM";
}
