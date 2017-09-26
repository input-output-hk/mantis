`Ledger`/`EVM` regression test based on a DB snapshot from an actual syncing client. Goes through all the blocks, replaying the transactions and comparing results.

To run:

```
sbt -Dconfig.file=path/to/config snappy:test
```

An example configuration file can be found at: `src/snappy/resources/example.conf`. Additionally a genesis JSON file may need to be provided (in `blockchain` config section).
