#!/usr/bin/env bash

HERE=$(dirname $0)

docker build -t prometheus -f ./$HERE/prometheus/Dockerfile ./$HERE/prometheus/
docker build -t grafana -f ./$HERE/grafana/Dockerfile ./$HERE/grafana/
docker-compose -f $HERE/monitoring-docker-compose.yml up -d
