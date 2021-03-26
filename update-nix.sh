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

branch="$(git rev-parse --abbrev-ref HEAD)"
revision="$(git rev-parse HEAD)"

output="$(
nix build \
  --impure \
  --expr "
    (builtins.getFlake (toString ./.)).legacyPackages.x86_64-linux.mantis-hash {
      ref = \"${branch}\";
      rev = \"${revision}\";
    }
  " 2>&1 || true
)"

NEW_SHA=$(
echo "$output" \
  | awk '/^\s+got: / { print $2 }'
)

if [ -z "$NEW_SHA" ]; then
  echo "$output"
  echo "calculating hash failed!"
  exit 1
fi

echo "Calculated sha: $NEW_SHA"

update_sha() {
  echo "Updating sha in ./nix/mantis.nix"
  sed -r -i -e "s|depsSha256 = \"[^\"]+\";|depsSha256 = \"${NEW_SHA}\";|" ./nix/mantis.nix
  echo "./nix/mantis.nix has been updated"
}

if [ $# == 1 ] || [ "${1:-}" == "--check" ]; then
  current_sha=$(grep depsSha256 ./nix/mantis.nix | sed 's/\s*depsSha256\s*=\s*//g' | sed -e 's/"//g' -e 's/;//g' | xargs nix-hash --to-base32 --type sha256 )
  if [ "$current_sha" == "$NEW_SHA" ]; then
    echo "./nix/mantis.nix is up-to-date"
    exit 0
  else
    echo "wanted: $NEW_SHA"
    echo "   got: $current_sha"
    update_sha
    exit 1
  fi
fi

update_sha
