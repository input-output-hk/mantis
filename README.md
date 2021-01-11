# Mantis

Ethereum-like Blockchain Scala client built by IOHK's Team Grothendieck.

### Status - Release

For continuous integration we're using:
- [Buildkite](https://buildkite.com/input-output-hk/mantis) to run all the Scala build steps [![Build status](https://badge.buildkite.com/8a284e6f0af90afa544e06c8136b519f9f287f005ca581d8ed.svg?branch=master&theme=github)](https://buildkite.com/input-output-hk/mantis)
- [Hydra](https://hydra.project42.iohkdev.io/project/mantis) to test that the Nix build steps work and run each of the Ops test suites.

You can check the latest build results of the current branch by clicking the status icon in the header of the Github file browser.

Unit Test Code Coverage Status - TBD

### Docs - FIXME: Update docs!

For more details on configuration and functionality check out our [wiki](http://mantis.readthedocs.io) (also at [wiki](https://github.com/input-output-hk/mantis/wiki))

### Download the client

The latest release can be downloaded from [here](https://github.com/input-output-hk/mantis/releases)

### Command line version

You can use generic launcher with appropriate parameter to connect with pre-configured network, it can be found in `bin` directory.

Example:
  - `./bin/mantis-launcher etc` - for joining Ethereum Classic network

Possible networks: `etc`, `eth`, `mordor`, `testnet-internal`

### Command Line Interface

`cli` is a tool that can be used to:
 
 - generate a new private key
 ```
./bin/mantis cli generate-private-key
```
 - derive an address from private key
```
./bin/mantis cli derive-address 00b11c32957057651d56cd83085ef3b259319057e0e887bd0fdaee657e6f75d0
```
 - generate genesis allocs (using private keys and/or addresses)
```
`./bin/mantis cli generate-alloc --balance=42 --address=8b196738d90cf3d9fc299e0ec28e15ebdcbb0bdcb281d9d5084182c9c66d5d12 --key=00b11c32957057651d56cd83085ef3b259319057e0e887bd0fdaee657e6f75d1`
```
 - generate multiple key-pairs (following example generate 5 key-pairs)
 ```
./bin/mantis cli generate-key-pairs 5
```

- encrypt private key (default passphrase is empty string)
 ```
./bin/mantis cli encrypt-key --passphrase=pass 00b11c32957057651d56cd83085ef3b259319057e0e887bd0fdaee657e6f75d0
```

Command output uses the same format as keystore so it could be used ex. to setup private faucet

ex.
```
{
  "id":"3038d914-c4cd-43b7-9e91-3391ea443f95",
  "address":"c28e15ebdcbb0bdcb281d9d5084182c9c66d5d12",
  "version":3,
  "crypto":{
    "cipher":"aes-128-ctr",
    "ciphertext":"6ecdb74b2a33dc3c016b460dccc96843d9d050aea3df27a3ae5348e85b3adc3e",
    "cipherparams":{
      "iv":"096b6490fe29e42e68e2db902920cad6"
    },
    "kdf":"scrypt",
    "kdfparams":{
      "salt":"cdcc875e116e2824ab02f387210c2f4ad7fd6fa1a4fc791cc92b981e3062a23e",
      "n":262144,
      "r":8,
      "p":1,
      "dklen":32
    },
    "mac":"8388ae431198d31d57e4c17f44335c2f15959b0d08d1145234d82f0d253fa593"
  }
}
```

### Building the client

#### SBT

##### Prerequisites to build

- JDK 1.8 (download from [java.com](http://www.java.com))
- sbt ([download sbt](http://www.scala-sbt.org/download.html))
- python 2.7.15 (download from [python.org](https://www.python.org/downloads/))

##### Build the client

As an alternative to downloading the client build the client from source.


```
git submodule update --recursive --init
sbt dist
```

in the root of the project.

This updates all submodules and creates a distribution zip in `~/target/universal/`.

#### Nix

In the root of the project:

##### Build the client

```
nix-build
```

##### Update sbt+nix dependencies

When updating project dependencies, the nix fixed-output-derivation
will need to be updated so that it includes the new dependency state.

To do so, please run:
```
./update-nix.sh
git add ./nix/pkgs/mantis.nix
git commit -m "Update nix-sbt sha"
```

*NOTE:* This should only be necessary when updating dependencies
(For example, edits to build.sbt or project/plugins.sbt will likely need to be regenerated)

### Monitoring

#### Locally build & run monitoring client

Setup a dashboard using Prometheus and Grafana, popular choice of monitoring stack.
Before that you need enable the metrics in the file “metrics.conf”, setting mantis.metrics.enabled=true.

You can start Docker Compose initializing Prometheus and Grafana with a preconfigured dashboard.
For build the monitoring, you need to run the  following script: `./docker/monitoring/build.sh`
This script prepares a docker image of mantis. And as a precondition you need to have installed docker-compose and sbt.

We can see the dashboard called "Mantis" at URL: http://localhost:3000 using user and password: admin and admin


### TLS setup

Both the JSON RPC (on the node and faucet) can be additionally protected using TLS.
On the development environment it's already properly configured with a development certificate.

#### Generating a new certificate

If a new certificate is required, create a new keystore with a certificate by running `./tls/gen-cert.sh`

#### Configuring the node

1. Configure the certificate and password file to be used at `mantis.network.rpc.http.certificate` key on the `application.conf` file:

    keystore-path: path to the keystore storing the certificates (if generated through our script they are by default located in "./tls/mantisCA.p12")
    keystore-type: type of certificate keystore being used (if generated through our script use "pkcs12")
    password-file: path to the file with the password used for accessing the certificate keystore (if generated through our script they are by default located in "./tls/password")
2. Enable TLS in specific config:
    - For JSON RPC: `mantis.network.rpc.http.mode=https`

#### Configuring the faucet

1. Configure the certificate and password file to be used at `mantis.network.rpc.http.certificate` key on the `faucet.conf` file:

    keystore-path: path to the keystore storing the certificates (if generated through our script they are by default located in "./tls/mantisCA.p12")
    keystore-type: type of certificate keystore being used (if generated through our script use "pkcs12")
    password-file: path to the file with the password used for accessing the certificate keystore (if generated through our script they are by default located in "./tls/password")
2. Enable TLS in specific config:
    - For JSON RPC: `mantis.network.rpc.http.mode=https`
3. Configure the certificate used from RpcClient to connect with the node. Necessary if the node uses http secure. 
   This certificate and password file to be used at `faucet.rpc-client.certificate` key on the `faucet.conf` file:

    keystore-path: path to the keystore storing the certificates
    keystore-type: type of certificate keystore being used (if generated through our script use "pkcs12")
    password-file: path to the file with the password used for accessing the certificate keystore


### Feedback

Feedback gratefully received through the Ethereum Classic Forum (http://forum.ethereumclassic.org/)

### Known Issues

There is a list of known issues in the 'RELEASE' file located in the root of the installation.

