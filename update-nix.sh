#!/usr/bin/env nix-shell
#!nix-shell -p nixUnstable -i bash

set -e

# need to overwrite buildkite's version
PATH=$(nix-build '<nixpkgs>' -A nixUnstable)/bin:$PATH

name=$(basename $0)

usage() {
        echo "$name - Tool used to validate and update the sbt nix build"
        echo ""
        echo "USAGE:"
        echo "    $name [--check]"
        echo ""
        echo "OPTIONS:"
        echo -e "    --check\t Check whether ./nix/pkgs/mantis.nix is up-to-date"
}

if [ "$1" == "-h" -o "$1" == "--help" ]; then
        usage
        exit 1
fi

echo "Determining new sha for sbt build, this can take several minutes to do a 'sbt compile'"

NEW_SHA=$(nix-build -E 'with import ./. {}; deps.overrideAttrs( _: { outputHash = "0000000000000000000000000000000000000000000000000000"; })' 2>&1 | grep "  got: " | sed -r 's/\s+got:\s+//' | xargs nix hash to-sri --type sha256 )

echo "New sha: $NEW_SHA"

update_sha() {
        echo "Updating sha in ./nix/pkgs/mantis.nix"
        sed -r -i -e "s|depsSha256 = \"[^\"]+\";|depsSha256 = \"${NEW_SHA}\";|" ./nix/pkgs/mantis.nix
        echo "./nix/pkgs/mantis.nix has been updated"
}

if [ $# == 1 -o "$1" == "--check" ]; then
        if grep -q $NEW_SHA ./nix/pkgs/mantis.nix; then
                echo "./nix/pkgs/mantis.nix is up-to-date"
                exit 0
        else
                current_sha=$(cat ./nix/pkgs/mantis.nix | grep depsSha256 | sed 's/\s+depsSha256 =\s+//g' | sed 's/";//')
                echo "wanted: $NEW_SHA"
                echo "   got: $current_sha"
                update_sha
                exit 1
        fi
fi

update_sha



