#!/usr/bin/env bash

git submodule init
git submodule update

echo "booting Mantis and waiting for RPC API to be up"
$SBT -Dconfig.file=./src/main/resources/conf/testmode.conf run &> mantis-log.txt &

while ! nc -z localhost 8546; do   
  sleep 0.1
done

final_exit_code=0

function run {
  echo "running retesteth $1"
  ets/retesteth -t "$1" &> "retesteth-$1-log.txt"
  exit_code=$?
  echo "retesteth $1 exit code: $exit_code"
}

run "GeneralStateTests"
run "BlockchainTests"

echo "shutting down mantis"
kill %1

exit $final_exit_code
