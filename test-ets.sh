#!/usr/bin/env bash

if [ -z "$IN_NIX_SHELL" ]; then
    export SBT_NIX="-Dnix=true"
fi

git submodule init;
git submodule update;

echo "booting Mantis and waiting for RPC API to be up"
$SBT -Dconfig.file=./src/main/resources/conf/testmode.conf run > mantis-stdout.txt 2> mantis-stderr.txt &

while ! nc -z localhost 8546; do   
  sleep 0.1
done

echo "running retesteth"
retesteth -- --testpath src/ets/resources/ets --datadir src/ets/resources/config --clients mantis > retesteth-stdout.txt 2> retesteth-stderr.txt
code=$?

kill %1

exit $code
