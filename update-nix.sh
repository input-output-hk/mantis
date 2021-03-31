#!/usr/bin/env bash

set -euo pipefail

name=$(basename "$0")

usage() {
  echo "$name - Tool used to validate and update the sbt nix build"
  echo ""
  echo "USAGE:"
  echo "    $name [--check]"
  echo ""
  echo "OPTIONS:"
  echo -e "    --check\t Check whether ./nix/mantis.nix is up-to-date"
}

if [ "${1:-}" == "-h" ] || [ "${1:-}" == "--help" ]; then
  usage
  exit 1
fi

echo "Determining new sha for sbt build, this can take several minutes to do a 'sbt compile'"

current_sha=$(nix eval --raw '.#mantis.deps.outputHash')

output="$(
nix build \
  --impure \
  --expr "(builtins.getFlake (toString ./.)).legacyPackages.x86_64-linux.mantis-hash" \
  2>&1 || true
)"

new_sha="$(echo "$output" | awk '/^\s*got: / { print $2 }')"
current_sha=$(nix eval --raw '.#mantis.deps.outputHash')

if [ -z "$new_sha" ]; then
  echo "$output"
  echo "calculating hash failed!"
  exit 1
fi

echo "Calculated sha: $new_sha"

update_sha() {
  echo "Updating sha in ./nix/overlay.nix"
  sed -i "s|depsSha256 = \"$current_sha\";|depsSha256 = \"$new_sha\";|" nix/overlay.nix
  echo "./nix/overlay.nix has been updated"
}

if [ $# == 1 ] || [ "${1:-}" == "--check" ]; then
  current_sha=$(nix eval --raw '.#mantis.deps.outputHash')
  if [ "$current_sha" == "$new_sha" ]; then
    echo "./nix/overlay.nix is up-to-date"
    exit 0
  else
    echo "wanted: $new_sha"
    echo "   got: $current_sha"
    update_sha
    exit 1
  fi
fi

update_sha
