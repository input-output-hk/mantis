#!/usr/bin/env bash

HERE=$(dirname $0)

cd $HERE/../../
sbt docker:publishLocal

cd $HERE
docker build -t prometheus -f ./prometheus/Dockerfile ./$HERE/prometheus/
docker build -t grafana -f ./grafana/Dockerfile ./$HERE/grafana/
docker-compose -f docker-compose.yml up -d
