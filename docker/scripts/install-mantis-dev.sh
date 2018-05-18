#!/usr/bin/env bash

set -euxo pipefail

SBT_VERIFY_TAG=$1
MANTIS_TAG=$2

HERE=$(readlink -m $(dirname ${BASH_SOURCE[0]}))
. $HERE/install-nix-common.sh

mkdir ~/repos

cd ~/repos
git clone https://github.com/input-output-hk/sbt-verify.git
cd sbt-verify
git checkout $SBT_VERIFY_TAG
# This is needed, since the library is not published in binary form. 
sbt publishLocal

cd ~/repos
git clone https://github.com/input-output-hk/mantis.git
cd mantis
git checkout $MANTIS_TAG
git submodule update --init

# Trigger compilation, so that we get some dependencies from the internetz.
sbt 'set test in Test := {}' compile