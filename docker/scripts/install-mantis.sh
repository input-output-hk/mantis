#!/usr/bin/env bash

set -euxo pipefail

MANTIS_TAG=$1
MANTIS_DIST_ZIP_NAME=$2

HERE=$(readlink -m $(dirname ${BASH_SOURCE[0]}))
. $HERE/install-nix-common.sh

cd ~/repos/mantis

git checkout $MANTIS_TAG
git submodule update --init

sbt 'set test in Test := {}' dist
mkdir -p ~/mantis-dist/app
unzip -d ~/mantis-dist/app target/universal/${MANTIS_DIST_ZIP_NAME}.zip
mv ~/mantis-dist/app/*/* ~/mantis-dist/app
rmdir ~/mantis-dist/app/$MANTIS_DIST_ZIP_NAME
rm -rf ~/repos ~/.ivy2 ~/.sbt