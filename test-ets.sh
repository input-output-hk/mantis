#!/usr/bin/env bash

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
  ets/retesteth -t "$1" 2>&1 | tee "retesteth-$1-log.txt"
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
  passed=$(grep -oP 'Total Tests Run: \d+' "retesteth-$1-log.txt")
  failed=$(grep -oP 'TOTAL ERRORS DETECTED: \d+' "retesteth-$1-log.txt")

  cat <<EOF | buildkite-agent annotate --context "retesteth-$1" --style "$style"
<details>
<summary>retesteth: $1 -- $passed -- $failed</summary>
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
