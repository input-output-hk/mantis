# this script is meant to run the Ethereum Test Suite (https://github.com/ethereum/tests) on CircleCI
# to skip these tests, set SKIP_ETS_TEST variable CircleCI UI
# to differentiate by branch, use CIRCLE_BRANCH variable in the conditional below

echo "current branch: $CIRCLE_BRANCH"

echo "memory:"
free -m

# at least 1.5G of heap space and 10M of stack size is required for ETS.
# however setting these JVM params globally for the whole build causes failures
# due to CircleCI memory limit exceeded.
export JAVA_OPTS="-Xmx2g -Xss16m -XX:MaxMetaspaceSize=512m"

if [ -z $SKIP_ETS_TESTS ]; then
    git submodule init;
    git submodule update;
    if [ "$CIRCLE_BRANCH" == "master" -o -n "$RUN_FULL_ETS" ]; then
        export JAVA_OPTS="-Xmx3g -Xss16m -XX:MaxMetaspaceSize=512m"
        echo "running full ETS"
        sbt -v "ets:testOnly * -- -DuseLocalVM=true -Dexg=vmPerf*";
    else
        echo "running a subset of ETS"
        sbt -v "ets:testOnly *VMSuite -- -Dexg=vmPerf*" &&
        sbt -v "ets:testOnly *BlockchainSuite -- -DuseLocalVM=true -Ding=bcForkStress*,bcMulti*,bcState*,bcTotalDiff*,bcValidBlock*,Transition*";
    fi
else
    echo "SKIP_ETS_TESTS variable is set - skipping the tests";
fi
