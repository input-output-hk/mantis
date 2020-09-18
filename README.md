# Mantis

Ethereum-like Blockchain Scala client built by IOHK's Team Grothendieck.

### Status

Continuous Integration Build Status [FIXME]

Unit Test Code Coverage Status [FIXME]

// FIXME: Should we continue using this? or should we migrate to atlassian wiki?
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
- python 2.7.15 (download from [python.org](https://www.python.org/downloads/))

#### Build the client

As an alternative to downloading the client build the client from source.


```
git submodule update --recursive --init
sbt dist
```

in the root of the project.

This updates all submodules and creates a distribution zip in `~/target/universal/`.

### Monitoring

#### Locally build & run monitoring client

```
# Build monitoring client docker image
projectRoot $ docker build -f ./docker/monitoring-client.Dockerfile -t mantis-monitoring-client ./docker/
# Run monitoring client in http://localhost:9090
projectRoot $ docker run --network=host mantis-monitoring-client
```

### Feedback

Feedback gratefully received through the Ethereum Classic Forum (http://forum.ethereumclassic.org/)

### Known Issues

There is a list of known issues in the 'RELEASE' file located in the root of the installation.

