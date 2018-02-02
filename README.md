# mantis - Scala client for Ethereum Classic, The Daedalus Release

The Daedalus release consists of two parts, the integration with the Daedalus Wallet and the release of the command line version
This branch refers to 1.0 version of the of the command line version.
 
The differences are very minor and relate only to how the RPC apis are enabled. In the command line version they are enabled over HTTP
and in the Daedalus integration version they are forced to use HTTPS and client SSL certificate. The Daedalus version comes with a bundled JVM 
but the command line version requires a JVM to be installed and available on the target machine.   
    
This version has been tested on Windows 10, MacOS and Linux. 
 
 
### Status - CLI 1.0 Release

Continuous Integration Build Status [![CircleCI](https://circleci.com/gh/input-output-hk/mantis/tree/master.svg?style=svg)](https://circleci.com/gh/input-output-hk/mantis/tree/master)

Unit Test Code Coverage Status [![Coverage Status](https://coveralls.io/repos/github/input-output-hk/mantis/badge.svg?branch=master)](https://coveralls.io/github/input-output-hk/mantis?branch=master)

This version of the code supports 

  - CPU mining 
  - peer discovery 
  - fast sync (download a recent state trie snapshot and all blocks, this is the default behaviour)  
  - bootstrap sync (download a database for *mantis* preloaded with a recent version of the blockchain, **highly recommended**)
  - regular sync (download and execute every transaction in every block in the chain, this is very slow - not recommended) 
  - JSON RPC API (useful for console and Mist integration)
  - Morden testnet and private network
  - `ethminer` miner integration (allows *mantis* to mine blocks with [ethminer](https://github.com/Genoil/cpp-ethereum))

For more details on configuration and functionality check out our [wiki](http://mantis.readthedocs.io) (also at [wiki](https://github.com/input-output-hk/mantis/wiki))

### Download the client and bootstrap files

The latest release can be downloaded from [here](https://github.com/input-output-hk/mantis/releases)

The bootstrap database files can be downloaded from [here](https://github.com/input-output-hk/mantis/wiki/Bootstrap-Database-Download-Links)
    
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
 
### Install and run the client

#### Prerequisites to run the client
 
 - JVM 1.8 (download from [java.com](http://www.java.com))
 
Note that on windows a 64 bit version of the JVM is required. This can be checked by using `java -version`. Check for `64-Bit`
 
```
 java version "1.8.0_131"
 Java(TM) SE Runtime Environment (build 1.8.0_131-b11)
 Java HotSpot(TM) 64-Bit Server VM (build 25.131-b11, mixed mode)
```
 
 - the download of the Ethereum Classic chain will take up around 15G of disk space
 
Note that a slow disk will slow the chain download, an SSD with 25G free space is recommended  

#### Install the client 

Unzip that file to create a folder structure starting with

 ```
 mantis-1.0-cli
 ```

### Run the client on Linux and MacOS

In a terminal from the root of the installation run  
  
 ```
 ./bin/mantis
 ```
The client runs in the foreground, it is recommended to use a session manager like `tmux` to prevent the client exiting when the terminal is shut down.

Within a minute the chain begins downloading to a folder in the `$HOME` folder called `.mantis`
  
### Run the client on Windows 

Open a command terminal and from the root of the installation run

  
```
bin\mantis.bat
```
Within a minute the chain begins downloading to a folder in the `%HOME%` folder called `.mantis`

#### Configuration

Using the 'out of the box' settings the client attempts to contact the remote peers configured in the configuration file and begin a 
`fast-sync` download. Time taken to complete `fast-sync` varies significantly depending on the quality of the network connection and the quality of the hardware. 
  
A faster option for creating a node that is synced with the network is to use the bootstrap download.   

All options can be configured in the  files located in the `conf` folder. 

For example to change the default datadir to `/mydata` edit the `storage.conf` file and find the lines   

```
# Base directory where all the data used by the node is stored, including blockchain data and private keys
# datadir = ${user.home}"/.mantis"
```
Uncomment and edit `datadir` to read 
```
# Base directory where all the data used by the node is stored, including blockchain data and private keys
datadir = "/mydata"
```

For more details on configuration and functionality check out our [wiki](http://mantis.readthedocs.io) (also at [wiki](https://github.com/input-output-hk/mantis/wiki)) 

### Uninstall 

To remove the mantis client simple delete the folder the distribution (zip) file was unzipped to and delete the `datadir`. The default datadir folder is `$HOME/.mantis`  

### Feedback

Feedback gratefully received through the Ethereum Classic Slack (#mantis_client_bugs) 

### Known Issues
 
There is a list of known issues in the 'RELEASE' file located in the root of the installation.


