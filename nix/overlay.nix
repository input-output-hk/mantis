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

  mantis-faucet-source = builtins.fetchGit {
    url = "https://github.com/input-output-hk/mantis";
    rev = "07e617cdd1bfc76ad1a8472305f0e5e60e2801e1";
    ref = "develop";
    submodules = true;
  };

  # Last change to this was in 2018, so to avoid submodules we just clone
  # ourselves instead.
  mantis-extvm-pb = builtins.fetchGit {
    url = "https://github.com/input-output-hk/mantis-extvm-pb";
    rev = "6b3039be92882df6ef6c15887c8d0b5f10c86d6f";
  };

  mantisPkgs = final.callPackage ./pkgs/mantis { src = ../.; };

  inherit (final.mantisPkgs) mantis;

  mantis-faucet = import final.mantis-faucet-source { inherit (final) system; };
  mantis-faucet-entrypoint =
    final.callPackage ./entrypoint.nix { mantis = final.mantis-faucet; };

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

  mantis-image =
    inputs.nixpkgs-unstable.legacyPackages.${final.system}.dockerTools.buildLayeredImage {
      name = "inputoutput/mantis";
      tag = "2021";

      contents = with final; [
        iele
        kevm
        mantis

        bashInteractive
        coreutils
        curl
        diffutils
        dnsutils
        fd
        gawk
        gnugrep
        gnused
        iproute
        jq
        less
        lsof
        netcat
        nettools
        procps
        ripgrep
        tree
        vim
      ];

      config.Entrypoint = [ "${final.bashInteractive}/bin/bash" ];
    };
}
