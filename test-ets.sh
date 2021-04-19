#!/usr/bin/env bash

if [ -z "$IN_NIX_SHELL" ]; then
    export SBT_NIX="-Dnix=true"
fi

git submodule init;
git submodule update;

echo "booting Mantis and waiting for RPC API to be up"
$SBT -Dconfig.file=./src/main/resources/conf/testmode.conf run &> mantis-log.txt &

while ! nc -z localhost 8546; do   
  sleep 0.1
done

echo "running retesteth GeneralStateTests"
retesteth -t GeneralStateTests -- --testpath src/ets/resources/ets --datadir src/ets/resources/config --clients mantis > retesteth-log.txt
code=$?
summary=$(sed -n '/Total Tests Run/,$p' retesteth-log.txt)

echo "retesteth exited with code $code"

cat <<EOF | buildkite-agent annotate --context "retesteth" --style "info"
  <details>
    <summary>retesteth: GeneralStateTests</summary>
    ```term
      $summary
    ```
  </details>
EOF

echo "shutting down mantis"
kill %1

exit $code
