{ src, mkSrc, lib, gitignoreSource, callPackage, jre }: rec {

  mantis-source = mkSrc src;

  mantis-unwrapped = callPackage ./unwrapped.nix { src = mantis-source; };

  mantis = callPackage ./wrapped.nix {
    mantis = mantis-unwrapped;
    inherit jre;
  };
}
