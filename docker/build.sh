#!/bin/sh

set -eux

HERE=$(readlink -m $(dirname ${BASH_SOURCE[0]}))

$HERE/buildhelper.sh mantis Dockerfile
