# this script is meant to run the Ethereum Test Suite (https://github.com/ethereum/tests) on Buildkite
# to skip these tests, set SKIP_ETS_TEST variable
# to differentiate by branch, use BUILDKITE_BRANCH variable in the conditional below

echo "current branch: $BUILDKITE_BRANCH"

echo "memory:"
free -m

if [ -z "$IN_NIX_SHELL" ]; then
    export SBT_NIX="-Dnix=true"
fi

# at least 1.5G of heap space and 10M of stack size is required for ETS.
export JAVA_OPTS="-Xmx2g -Xss16m -XX:MaxMetaspaceSize=512m"
export SBT = "sbt -v -mem 2048 $SBT_NIX";

if [ -z $SKIP_ETS_TESTS ]; then
    git submodule init;
    git submodule update;
    if [ "$BUILDKITE_BRANCH" == "feature/ETCM-532-run-full-ets" -o -n "$RUN_FULL_ETS" ]; then
        export JAVA_OPTS="-Xmx3g -Xss16m -XX:MaxMetaspaceSize=512m"
        echo "running full ETS"
        $SBT "ets:testOnly * -- -DuseLocalVM=true -Dexg=vmPerf*";
    else
        echo "running a subset of ETS"
        $SBT "ets:testOnly *VMSuite -- -Dexg=vmPerf*" &&
        $SBT "ets:testOnly *BlockchainSuite -- -DuseLocalVM=true -Ding=bcForkStress*,bcMulti*,bcState*,bcTotalDiff*,bcValidBlock*,Transition*";
    fi
else
    echo "SKIP_ETS_TESTS variable is set - skipping the tests";
fi
