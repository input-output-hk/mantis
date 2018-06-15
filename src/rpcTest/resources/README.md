# Instruction to semi-automatic json-rpc testing

General Note:
There are 3 types of tests in current test suit which requires 3 different configurations of mantis:
MainNet, PrivNet, PrivNetNoMining. Different tests types are marked by different scala test tags.
Correct configurations for private net is provided in `mantis/src/rpcTest/resources/privateNetConfig/conf/PrivNetConf.zip`
It includes custom genesis block which specifies 3 different pre-funded accounts needed for transaction tests.
Private keys for pre-funded accounts are located in `mantis/src/rpcTest/resources/privateNetConfig/keystore`.


1. build `mantis` client via `sbt dist`.
2. unzip built client to some directory i.e `~/mantis`
3. change options in `~/mantis/conf` :
    `sync.fast-sync` to 'false'
    `network.rpc.http.mode` to 'http'
    `storage.datadir` to some empty or non-existent directory
3. run `mantis` client on mainnet by `~/mantis/bin/mantis` up to `150000` blocks.
4. go to `mantis` source dir and run `sbt`, next run mainnet portion of tests via `rpcTest:testOnly -- -n MainNet`
5. turn off mantis client in `~/mantis`
5. unzip private net config files from `mantis/src/rpcTest/resources/privateNetConfig/conf/PrivNetConf.zip`
   to `~/mantis/conf/`
6. create `~/.privateNetTest/keystore` dir and copy there keys from
   `mantis/src/rpcTest/resources/privateNetConfig/keystore` dir. These are prefunded accounts needed for transactions tests
7. run mantis client in `~/mantis/bin/mantis` (mantis will be run with miner so you need to wait till DAG is loaded)
8. go to `mantis` source dir and run `sbt`, next run privnet portion of tests via `rpcTest:testOnly -- -n PrivNet`
9. turn off mantis client
10. turn off mining in `~/mantis/conf/consensus.conf`
11. run mantis client
12. go to `mantis` source dir and run `sbt`, next run privet net portion of tests without mining
    via `rpcTest:testOnly -- -n PrivNetNoMining`
