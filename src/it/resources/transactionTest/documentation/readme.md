command for running parity and ethminer

```
parity -jw --jsonrpc-interface 127.0.0.1 --jsonrpc-port 8545 --author 004A44Da31Bb735cB20171AfF69E60E3e50d26De --identity parity_node --geth --chain path_to_private_chain_conf/prv.json --base-path /tmp/etc_prv  --pruning archive
ethminer -C -F 127.0.0.1:8545
```
example private network configuration is in `prv.json`

address for private net:

private key is included in keys folder and pass phrase is `rHmAnRF/u+K28QFv6#3Qs8t]J,QW`

main
0x004A44Da31Bb735cB20171AfF69E60E3e50d26De

seler
0xB2D985FFa6999E8D1Dd4c87290c316D0d5414275

buyer
0x71583ed3034345DD8224c491e2b4584F4D271ab8

deploy contracts with `Develop > Open Remix IDE` 


to start with empty chain:
`rm -rf chains dapps network` in /tmp/etc_prv

`rm -rf ~/.ethash` on every new chain start

you can use `DumpChainApp` to dump part of block chain with mpt tree, you can configure it with `chainDump.conf`