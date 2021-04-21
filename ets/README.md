# Ethereum Test Suite

This folder contains a git submodule pointing at the Ethereum Consensus Tests,
also known as the Ethereum Test Suite (ETS), config files for retesteth (the
tool for running these tests) and a wrapper script to set the its command line
options. Oh, and this readme file is in there too, of course.

* ETS: https://github.com/ethereum/tests
* retesteth: https://github.com/ethereum/retesteth

## Continous integration

The tests are run on CI. For more details look at `.buildkite/pipeline.nix` and
`test-ets.sh`. Output is stored as artifacts and a summary is added as
annotation.

Two test suites are run; GeneralStateTests and BlockchainTests. These seem to
be the only ones maintained and recommended at the moment.

## Running ETS in a Nix environment

Start Mantis in test mode:

    sbt -Dconfig.file=./src/main/resources/conf/testmode.conf -Dlogging.logs-level=WARN run

NB. raising the log level is a good idea as there will be a lot of output,
depending on how many tests you run.

Once the RPC API is up, run retesteth:

    ets/retesteth -t GeneralStateTests

You can also run parts of the suite; refer to `ets/retesteth --help` for details.

## Running retesteth in Docker (eg. macOS)

You should run Mantis outside Nix as that is probably more convenient for your
tooling (eg. attaching a debugger.)

    sbt -Dconfig.file=./src/main/resources/conf/testmode.conf -Dlogging.logs-level=WARN run

Retesteth will need to be able to connect to Mantis, running on the host
system. First, find the IP it should use:

    nix-in-docker/run --command "getent hosts host.docker.internal"

Edit `ets/config/mantis` and replace `0.0.0.0` with this IP.

Finally, run retesteth in Nix in Docker:

    nix-in-docker/run --command "ets/retesteth -t GeneralStateTests"
