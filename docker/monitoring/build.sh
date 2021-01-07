#!/usr/bin/env bash

HERE=$(dirname $0)

cd $HERE/../../
sbt docker:publishLocal

docker-compose -f docker/monitoring/docker-compose.yml up -d
