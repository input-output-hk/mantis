rev: final: prev: {
  jre = prev.jdk8.jre;
  mantis = final.callPackage ./mantis.nix {
    src = builtins.fetchGit {
      url = https://github.com/input-output-hk/mantis;
      ref = "flake";
      rev = rev;
      submodules = true;
    };
  };
}
