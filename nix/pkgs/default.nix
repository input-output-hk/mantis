{ pkgs, lib, src, impure ? false }:

lib.makeScope pkgs.newScope (self: with self; {
  inherit src impure;
  mantis = callPackage ./mantis.nix { };
  jdk = openjdk8_headless;
})
