#!/usr/bin/env bash

set -euxo pipefail

HERE=$(readlink -m $(dirname ${BASH_SOURCE[0]}))
. $HERE/install-nix-common.sh

nix-install sbt gitMinimal unzip
nix-collect-garbage -d