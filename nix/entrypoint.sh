#!/usr/bin/env bash

set -exuo pipefail

trap 'trap - SIGTERM && kill -- -$$' SIGINT SIGTERM EXIT

mkdir -p /tmp
mkdir -p "$NOMAD_TASK_DIR/mantis"
cd "$NOMAD_TASK_DIR"
name="java"

if [ -n "${DAG_NAME:-}" ]; then
  if [ -f "ethash/$DAG_NAME" ]; then
    echo "found existing DAG"
    sha256sum "ethash/$DAG_NAME"
  else
    mkdir -p ethash
    aws \
      --endpoint-url "$MONITORING_ADDR" \
      s3 cp \
      "s3://$DAG_BUCKET/$DAG_NAME" \
      "ethash/$DAG_NAME" \
      || echo "Unable to download DAG, skipping."
  fi
fi

if [ -d "$STORAGE_DIR" ]; then
  echo "$STORAGE_DIR found, not restoring from backup..."
else
  echo "$STORAGE_DIR not found, restoring backup..."
  restic restore latest \
    --tag "$NAMESPACE" \
    --target / \
    || echo "couldn't restore backup, continue startup procedure..."
      mkdir -p "$NOMAD_TASK_DIR/mantis"
      rm -rf "$NOMAD_TASK_DIR/mantis/{keystore,node.key}"
      rm -rf "$NOMAD_TASK_DIR/mantis/logs"
fi

until [ "$(grep -c enode mantis.conf)" -ge "$REQUIRED_PEER_COUNT" ]; do
  sleep 1
done

ulimit -c unlimited
cp mantis.conf running.conf

(
trap 'trap - SIGTERM && kill -- -$$' SIGINT SIGTERM EXIT

while true; do
  set +x
  while diff -u running.conf mantis.conf > /dev/stderr; do
    sleep 900
  done
  set -x

  if ! diff -u running.conf mantis.conf > /dev/stderr; then
    echo "Found updated config file, restarting Mantis"
    cp mantis.conf running.conf
    cat running.conf > /dev/stderr
    pkill "$name" || true
  fi
done
) &

starts=0
while true; do
  starts="$((starts+1))"
  echo "Start Number $starts" > /dev/stderr
  rm -f "$NOMAD_TASK_DIR/mantis/rocksdb/LOCK"
  mantis "-Duser.home=$NOMAD_TASK_DIR" "$@" || true
  sleep 10
done
