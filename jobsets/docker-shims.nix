{ nixpkgs ? <nixpkgs>
, sbtVerifySrc
, mantisSrc
}:

with import nixpkgs {};
let foo = import ./release-mantis.nix {
  inherit sbtVerifySrc mantisSrc nixpkgs;
};
in {
  base = dockerTools.buildImage {
    name = "iohk-base";
    tag = "latest";
    fromImageName = "ubuntu";
    fromImageTag = "16.04";

    contents = foo.mantis;
  };
}
