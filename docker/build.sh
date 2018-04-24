#!/usr/bin/env bash

set -eux

HERE=$(readlink -m $(dirname ${BASH_SOURCE[0]}))
DATE_TAG=$(date --iso-8601)
TAG=${1:-$DATE_TAG}

docker build -t mantis:$TAG $HERE