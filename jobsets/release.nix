{ nixpkgs
, sbtVerifySrc
, mantisSrc
, kevmSrc
}:
with import nixpkgs {};
rec {
  sbtVerify = callPackage ./sbt-verify.nix {
    inherit sbtVerifySrc;
  };
  mantis = callPackage ./mantis.nix {
    inherit mantisSrc;
    inherit sbtVerify;
  };

  kevm = pkgs.callPackage ./kevm.nix {
    inherit kevmSrc;
  };

  mantisDocker = stdenv.mkDerivation {
    name = "mantis-docker";
    requiredSystemFeatures = [ "kvm" ];

    installPhase = ''
      mkdir $out

      cp $src $out/image.tar.gz
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
        bashInteractive
        vim
        openjdk8
        mantis
        kevm
      ];
      config = {
        Env = [
          "_JAVA_OPTIONS=-Duser.home=/home/mantis"
        ];
        ExposedPorts = {
          "9076/tcp" = {}; # Ethereum protocol connections
          "30303/tcp" = {}; # Discovery protocol
          "8546/tcp" = {}; # JSON-RPC http endpoint
          "5679/tcp" = {}; # Raft consensus protocol
          "8099/tcp" = {}; # Faucet.
          "8888/tcp" = {}; # KEVM.
        };
      };
    };
  };
}
