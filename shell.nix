{ sources ? import nix/sources.nix, pkgs ? import ./nix { } }:
with pkgs;

mkShell {
nativeBuildInputs = [ protobuf sbt ];
inputsFrom = [ mantis ];
}
