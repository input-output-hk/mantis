# this script is meant to run the Ethereum Test Suite (https://github.com/ethereum/tests) on CircleCI
# to skip these tests, set SKIP_ETS_TEST variable CircleCI UI
# to differentiate by branch, use CIRCLE_BRANCH variable in the conditional below

echo "current branch: $CIRCLE_BRANCH"
echo "memory:"
free -m
if [ -z $SKIP_ETS_TESTS ]; then
    git submodule init;
    git submodule update;
    if [ "$CIRCLE_BRANCH" == "master" ]; then
        sbt "ets:testOnly * -- -Dexg=vmPerf*";
    elif [ "$CIRCLE_BRANCH" == "fix/etsOnCircle" ]; then
        sbt "ets:testOnly * -- -Ding=*JUMPDEST_AttackwithJump_d0g0v0"
    else
        sbt "ets:testOnly *VMSuite -- -Dexg=vmPerf*" &&
        sbt "ets:testOnly *BlockchainSuite -- -Ding=bcForkStress*,bcMulti*,bcState*,bcTotalDiff*,bcValidBlock*,Transition*";
    fi
else
    echo "SKIP_ETS_TESTS variable is set - skipping the tests";
fi
