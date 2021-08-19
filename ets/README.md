# Ethereum Test Suite

This folder contains a git submodule pointing at the Ethereum Consensus Tests,
also known as the Ethereum Test Suite (ETS), config files for retesteth (the
tool for running these tests) and a wrapper script to set the its command line
options. Oh, and this readme file is in there too, of course.

* ETS: https://github.com/ethereum/tests
* retesteth: https://github.com/ethereum/retesteth

## Running locally

Use the `ets/run` wrapper script to boot Mantis and run retesteth against it.
On a Mac you will want to do this using Docker:

    nix-in-docker/run --command "ets/run"

Read on for more fine-grained control over running Mantis and retesteth, by
running them separately.

## Continous integration

The tests are run on CI. For more details look at `./ci.cue` and
`test-ets.sh`. Output is stored as artifacts.

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

Finally, run retesteth in Nix in Docker:

    nix-in-docker/run --command "ets/retesteth -t GeneralStateTests -- --nodes <ip>:8546"

## Useful options:

You can run one test by selecting one suite and using `--singletest`, for instance: 

    nix-in-docker/run -t BlockchainTests/ValidBlocks/VMTests/vmArithmeticTest -- --nodes <ip>:8546 --singletest add0"

However it's not always clear in wich subfolder the suite is when looking at the output of retesteth.

To get more insight about what is happening, you can use `--verbosity 6`. It will print every RPC call 
made by retesteth and also print out the state by using our `debug_*` endpoints. Note however that 
`debug_accountRange` and `debug_storageRangeAt` implementations are not complete at the moment :

 - `debug_accountRange` will only list accounts known at the genesis state. 
 - `debug_storageRangeAt` is not able to show the state after an arbitrary transaction inside a block.
It will just return the state after all transaction in the block have run.
