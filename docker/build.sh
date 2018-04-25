#!/usr/bin/env bash

set -eux

HERE=$(readlink -m $(dirname ${BASH_SOURCE[0]}))

IMAGE_NAME=mantis
DOCKERFILE=Dockerfile

# If IMAGE_TAG is not given in the command line, then compute one in the format
#
#   YYYY-MM-DD '.' SHORT_HASH
#
# where the variable parts refer to the latest commit
IMAGE_TAG=${1:-$(git log -1 --format=%cd.%h --date=short)}

# This is the commit that the image will be based on
COMMIT=$(git log -1 --format=%H)

docker build --build-arg MANTIS_TAG=${COMMIT} -t $IMAGE_NAME:$IMAGE_TAG -f $HERE/$DOCKERFILE $HERE