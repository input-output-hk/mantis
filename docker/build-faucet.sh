#!/bin/sh

set -eux

HERE=$(readlink -m $(dirname ${BASH_SOURCE[0]}))

exec $HERE/buildhelper.sh mantis-faucet Dockerfile-faucet
