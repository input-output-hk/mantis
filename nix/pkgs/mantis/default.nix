{ src, lib, gitignoreSource, callPackage, jre }: rec {

  mkSrc = src:
    let
      isGit = builtins.pathExists (src + "/.git");
      repo = builtins.fetchGit { url = src; submodules = true; };
      dirty = repo.revCount == 0;
      filterSrc = src:
        lib.cleanSourceWith {
          inherit src;
          filter = path: _: !lib.hasSuffix "nix" path;
        };
    in if isGit then
      if dirty then filterSrc (gitignoreSource src) else repo
    else
      src;

  mantis-source = mkSrc src;

  mantis-unwrapped = callPackage ./unwrapped.nix { src = mantis-source; };

  mantis = callPackage ./wrapped.nix {
    mantis = mantis-unwrapped;
    inherit jre;
  };
}
