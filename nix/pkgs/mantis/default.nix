{ src, lib, callPackage, jre }: rec {

  mantis-unwrapped = callPackage ./unwrapped.nix { inherit src; };

  mantis = callPackage ./wrapped.nix {
    mantis = mantis-unwrapped;
    inherit jre;
  };
}
