#!/usr/bin/env bash

if [ -z "$IN_NIX_SHELL" ]; then
    export SBT_NIX="-Dnix=true"
fi

git submodule init
git submodule update

echo "booting Mantis and waiting for RPC API to be up"
$SBT -Dconfig.file=./src/main/resources/conf/testmode.conf run &> mantis-log.txt &

while ! nc -z localhost 8546; do   
  sleep 0.1
done

final_exit_code=0

function run_and_annotate {
  echo "running retesteth $1"
  retesteth -t "$1" -- --testpath src/ets/resources/ets --datadir src/ets/resources/config --clients mantis &> "retesteth-$1-log.txt"
  exit_code=$?
  echo "retesteth $1 exit code: $exit_code"

  style="info"
  if [[ "$exit_code" -gt "0" ]]; then
    final_exit_code="$exit_code"
    style="error"
  fi

  summary=$(sed -n '/Total Tests Run/,$p' "retesteth-$1-log.txt")
  if [[ -z "$summary" ]]; then
    summary="retesteth crashed; check the artifacts"
  fi

  cat <<EOF | buildkite-agent annotate --context "retesteth-$1" --style "$style"
<details>
<summary>retesteth: $1</summary>
<pre class="term"><code>
$summary
</code></pre>
</details>
EOF
}

run_and_annotate "GeneralStateTests"
run_and_annotate "BlockchainTests"

echo "shutting down mantis"
kill %1

exit $final_exit_code
