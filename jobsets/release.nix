{ nixpkgs
, sbtVerifySrc
, mantisSrc
, kevmSrc
, ethExplorerSrc
}:
with import nixpkgs {};
let sbtVerify = callPackage ./sbt-verify.nix {
      inherit sbtVerifySrc;
    };
in rec {
  mantis = callPackage ./mantis.nix {
    inherit mantisSrc;
    inherit sbtVerify;
  };

  kevm = pkgs.callPackage ./kevm.nix {
    inherit kevmSrc;
  };

  ethExplorer = pkgs.callPackage ./eth-explorer.nix {
    inherit ethExplorerSrc;
  };

  mantis-docker = stdenv.mkDerivation {
    name = "mantis-docker";
    requiredSystemFeatures = [ "kvm" ];

    installPhase = ''
      mkdir $out
      cp $src $out/
    '';

    src = dockerTools.buildImage {
      name = "mantis";
      tag = "latest";
      fromImageName = "ubuntu";
      fromImageTag = "16.04";
      contents = [
        coreutils
        gnused
        gawk
        openjdk8
        mantis
        kevm
        ethExplorer
      ];
      config = {
        ExposedPorts = {
          "9076/tcp" = {}; # Ethereum protocol connections
          "30303/tcp" = {}; # Discovery protocol
          "8546/tcp" = {}; # JSON-RPC http endpoint
          "5679/tcp" = {}; # Raft consensus protocol
          "8099/tcp" = {}; # Faucet.
        };
      };
    };
  };
}
