# Instruction to semi-automatic json-rpc testing

General Note:
There are 3 types of tests in current test suite which requires 3 different configurations of mantis:
MainNet, PrivNet, PrivNetNoMining. Different tests types are marked by different scala test tags.
Correct configurations for private net is provided in `mantis/src/rpcTest/resources/privateNetConfig/conf`
It includes custom genesis block which specifies 3 different pre-funded accounts needed for transaction tests.
Private keys for pre-funded accounts are located in `mantis/src/rpcTest/resources/privateNetConfig/keystore`.

1. Build `mantis` client via `sbt dist`.
2. Unzip built client to some directory i.e `~/mantis_build`
3. Run script `patch-mantis` (it's in resources dir) with path to your mantis instance. Example invocation assuming that mantis is in `~/mantis_build` looks as follows:
    
        ./resources/patch-mantis ~/mantis_build

4. Go to `~/mantis_build` directory and run mantis on ETC mainnet with command:

        ./bin/mantis-launcher etc -Dmantis.sync.do-fast-sync=false -Dmantis.network.discovery.discovery-enabled=true -Dmantis.network.rpc.http.mode=http
        
5. Ensure it has at least `150000` blocks.
6. Go to `mantis` source dir and run 

        sbt "rpcTest:testOnly -- -n MainNet"
        
7. Turn off Mantis client in `~/mantis_build`
8. Go to `~/mantis_build` directory and run mantis using command below (mantis will be run with miner so you need to wait till DAG is loaded):

        ./bin/mantis -Dmantis.consensus.mining-enabled=true
9. Go to `mantis` source dir and run 

        sbt "rpcTest:testOnly -- -n PrivNet"
        
10. Turn off Mantis client
11. Go to `~/mantis_build` directory and run Mantis with mining disabled using command

        ./bin/mantis
        
12. Go to `mantis` source dir and run 

        sbt "rpcTest:testOnly -- -n PrivNetNoMining"
        
13. Turn off Mantis client.


__TODO__: It seems that simple bash script should be able to run all these tests now.
