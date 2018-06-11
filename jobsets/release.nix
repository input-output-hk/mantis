{ nixpkgs
, sbtVerifySrc
, mantisSrc
, kevmSrc
, secp256k1Src
, ieleSrc
, ...
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

  mantisZip = mantis.zip;

  kevm = pkgs.callPackage ./kevm.nix {
    inherit kevmSrc;
  };

  secp256k1 = callPackage ./secp256k1.nix {
    inherit secp256k1Src;
  };

  iele = callPackage ./iele.nix {
    inherit ieleSrc secp256k1;
  };

  mantisDocker = stdenv.mkDerivation {
    name = "mantis-docker";

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
        mantis.out
        kevm
        iele
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
