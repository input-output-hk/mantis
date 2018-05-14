#!/usr/bin/env bash

set -euxo pipefail

NIX_PROFILE=~/.nix-profile/etc/profile.d/nix.sh

export MANPATH=
. $NIX_PROFILE

export NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.03.tar.gz

function nix-install() {
  nix-env -f '<nixpkgs>' -iA "$@"
}
