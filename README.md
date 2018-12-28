# Mantis - Scala Client for Ethereum Classic and Ethereum, The 2.0 Release

This release of *Mantis*, the new Ethereum Classic and Ethereum client produced by the [Grothendieck Team](https://iohk.io/projects/ethereum-classic/),
has been focused on improving general performance and introducing full Ethereum network support.

This version has been tested on recent versions of Windows and macOS

### Status - CLI 2.0 Release

Continuous Integration Build Status [![CircleCI](https://circleci.com/gh/input-output-hk/mantis/tree/phase%2Frelease2_0.svg?style=svg)](https://circleci.com/gh/input-output-hk/mantis/tree/phase%2Frelease2_0)

Unit Test Code Coverage Status [![Coverage Status](https://coveralls.io/repos/github/input-output-hk/mantis/badge.svg?branch=phase%2Frelease2_0)](https://coveralls.io/github/input-output-hk/mantis?branch=phase%2Frelease2_0)

This version of the code supports

  - CPU mining
  - peer discovery
  - fast sync (download a recent state trie snapshot and all blocks, this is the default behaviour)
  - regular sync (download and execute every transaction in every block in the chain)
  - JSON RPC API (useful for console and Mist integration)
  - Morden and Ropsten testnet and private network
  - Ethereum classic and Ethereum networks
  - `ethminer` miner integration (allows *mantis* to mine blocks with [ethminer](https://github.com/Genoil/cpp-ethereum))

For more details on configuration and functionality check out our [wiki](http://mantis.readthedocs.io) (also at [wiki](https://github.com/input-output-hk/mantis/wiki))

### Download the client

The latest release can be downloaded from [here](https://github.com/input-output-hk/mantis/releases)

### Command line version

Depending on network you want to join you can use appropriate launcher, all can be found in `bin` directory:
  - `mantis-etc` - for joining Ethereum Classic network
  - `mantis-eth` - for joining Ethereum

### Building the client

#### Prerequisites to build

- JDK 1.8 (download from [java.com](http://www.java.com))
- sbt ([download sbt](http://www.scala-sbt.org/download.html))

#### Build the client

As an alternative to downloading the client build the client from source.

First of all `sbt-verify` is used in order to check the validity of the downloaded libraries checksums.

`sbt-verify` can be downloaded from our read only repository by typing

 `git clone  https://github.com/input-output-hk/sbt-verify`

 Then in order to make `sbt-verify` available to our build type

```
cd sbt-verify
sbt publishLocal
```

 This installs the `sbt-verify` library to your local repository.

After installing the `sbt-verify` library to your local repository checkout this repository from github and then type

 `sbt dist`

 in the root of the project.

This creates a distribution zip.

### Feedback

Feedback gratefully received through the Ethereum Classic Forum (http://forum.ethereumclassic.org/)

### Known Issues

There is a list of known issues in the 'RELEASE' file located in the root of the installation.

