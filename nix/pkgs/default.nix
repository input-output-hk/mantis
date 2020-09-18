{ pkgs, lib, newScope, sources, src, impure ? false }:

lib.makeScope newScope (self:
  with self;
  let callPackages = lib.callPackagesWith (pkgs // self);
  in {
    inherit sources src impure;
    jdk = prev.openjdk8_headless;
  })
