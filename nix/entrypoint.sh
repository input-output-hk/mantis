#!/usr/bin/env bash

set -exuo pipefail


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

until [ "$(grep -c enode mantis.conf)" -ge "$REQUIRED_PEER_COUNT" ]; do
  sleep 1
done

ulimit -c unlimited
cp mantis.conf running.conf

(
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
