#!/bin/sh

set -eux

HERE=$(readlink -m $(dirname ${BASH_SOURCE[0]}))

IMAGE_NAME=$1
DOCKERFILE=$2

# If IMAGE_TAG is not given in the command line, then compute one in the format
#
#   YYYY-MM-DD '.' SHORT_HASH
#
# where the variable parts refer to the latest commit
IMAGE_TAG=${3:-$(git log -1 --format=%cd.%h --date=short)}

# This is the commit that the image will be based on
GIT_HASH=$(git log -1 --format=%H)

docker build --build-arg MANTIS_TAG=$GIT_HASH -t $IMAGE_NAME:$IMAGE_TAG -f $HERE/$DOCKERFILE $HERE
docker tag $IMAGE_NAME:$IMAGE_TAG $IMAGE_NAME:latest
