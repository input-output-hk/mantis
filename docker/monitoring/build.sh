#!/usr/bin/env bash

HERE=$(dirname $0)

cd $HERE/../../
sbt docker:publishLocal

cd $HERE
docker build -t prometheus -f ./prometheus/Dockerfile ./prometheus/
docker build -t grafana -f ./grafana/Dockerfile ./grafana/
docker-compose -f docker-compose.yml up -d
