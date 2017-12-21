# Mantis - Scala Client for Ethereum Classic, The Daedalus Release

In this release *Mantis* the new Ethereum Classic client produced by the [Grothendieck Team](https://iohk.io/projects/ethereum-classic/)
has been integrated with the [Daedalus](https://daedaluswallet.io/) wallet. This integration puts the Daedalus wallet management
software into the hands of Ethereum Classic users, giving them a safe way to create, manage and backup their wallets.

This version has been tested on recent versions of Windows and macOS

The Daedalus bundle contains a JVM, so no pre installed jvm is required. This make it easier to install compared to the command line version.

### Status - Release Candidate 1

Continuous Integration Build Status [![CircleCI](https://circleci.com/gh/input-output-hk/mantis/tree/master.svg?style=svg)](https://circleci.com/gh/input-output-hk/mantis/tree/master)

Unit Test Code Coverage Status [![Coverage Status](https://coveralls.io/repos/github/input-output-hk/mantis/badge.svg?branch=master)](https://coveralls.io/github/input-output-hk/mantis?branch=master)

This version of the code supports

  - CPU mining
  - peer discovery
  - fast sync (download a recent state trie snapshot and all blocks, this is the default behaviour)
  - bootstrap sync (download a database for *mantis* preloaded with a recent version of the block chain, **highly recommended**)
  - regular sync (download and execute every transaction in every block in the chain, this can be very slow - not recommended)
  - JSON RPC API (useful for console and Mist integration)
  - Morden testnet and private network
  - `ethminer` miner integration (allows *mantis* to mine blocks with [ethminer](https://github.com/Genoil/cpp-ethereum))

### Installers for Windows and macOS

To make the installation process as accessible as possible, we have created fully automated installers for windows and macOS.

- the installer will install Daedalus wallet, install the Mantis client, set up an SSL connection between the two.
- it will then download a bootstrap database in order to synchronise the Ethereum Classic blockchain.
- it will check the finger print of the downloaded database in order to prevent MITM attacks.
- then it will start up both the wallet and the mantis node and begin syncing.
- until the node is synced no transactions can be made.
- when Daedalus is closed down it will also stop the mantis node.
- uninstall using the OS 'Add/Remove' feature

*Note that the download and extract process could take up to 60 minutes depending on available network and disk resources!*
 
For more details on configuration and functionality check out our [wiki](http://mantis.readthedocs.io) (also at [wiki](https://github.com/input-output-hk/mantis/wiki))

### Download the client and bootstrap files

The latest release can be downloaded from [here](https://github.com/input-output-hk/mantis/releases)

The bootstrap database files can be downloaded from [here](https://github.com/input-output-hk/mantis/wiki/Bootstrap-Database-Download-Links)

### Command line version

To access the command line version of this release go to [daedalus-cli](https://github.com/input-output-hk/mantis/tree/phase/daedalus-cli)

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

