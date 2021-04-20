{ system ? builtins.currentSystem
, src ? ./.
, pkgs ? (import ./nix { inherit system src; }).pkgs
}:
pkgs.mantis
