{ src ? builtins.fetchGit ./.
, supportedSystems ? [ builtins.currentSystem ]
}:
let
  sources = import nix/sources.nix;
  lib = import (sources.nixpkgs + "/lib");
in
{
  mantis = lib.genAttrs supportedSystems (system: import src {
    inherit src system;
  });
}
