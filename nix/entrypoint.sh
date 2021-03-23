#!/usr/bin/env bash

set -exuo pipefail

mkdir -p /tmp "$NOMAD_TASK_DIR/mantis"
cd "$NOMAD_TASK_DIR"

if [ -n "${DAG_NAME:-}" ]; then
    if [ -f "ethash/$DAG_NAME" ]; then
        echo "found existing DAG"
        sha256sum "ethash/$DAG_NAME"
    else
        mkdir -p ethash
        aws \
            --endpoint-url "$MONITORING_ADDR" \
            s3 cp \
            "s3://mantis-kevm-dag/$DAG_NAME" \
            "ethash/$DAG_NAME" \
            || echo "Unable to download DAG, skipping."
    fi
fi

set +x
echo "Waiting for $REQUIRED_PEER_COUNT peers to show up in the config"
until [ "$(grep -c enode mantis.conf)" -ge "$REQUIRED_PEER_COUNT" ]; do
    sleep 1
done
set -x

ulimit -c unlimited
cp mantis.conf running.conf

exec mantis "-Duser.home=$NOMAD_TASK_DIR" "$@"
