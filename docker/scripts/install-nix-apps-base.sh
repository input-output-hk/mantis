#!/usr/bin/env bash

set -euxo pipefail

HERE=$(readlink -m $(dirname ${BASH_SOURCE[0]}))
. $HERE/install-nix-common.sh

nix-install openjdk
nix-collect-garbage -d