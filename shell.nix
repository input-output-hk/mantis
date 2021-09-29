let
  system = builtins.currentSystem;
  compat = import ./nix/compat.nix { src = ./.; };

  pkgs = compat.defaultNix.pkgs.${system};
in
if __getEnv "BUILDKITE" == "true" then
  import .buildkite/shell.nix { inherit pkgs; }
else
  compat.shellNix
