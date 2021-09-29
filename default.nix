let system = builtins.currentSystem;
in
(import ./nix/compat.nix { src = ./.; inherit system; }).defaultNix.defaultPackage.${system}
