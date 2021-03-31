let flake = builtins.getFlake (toString ./.);
in flake.pkgs.${builtins.currentSystem}.defaultPackage
