# Mantis

![Mantis Logo](https://github.com/input-output-hk/mantis/blob/docs/readme-update/src/assets/logo.png)

Ethereum Classic client written in Scala by the IOHK team.

## About

Mantis is a fully featured client, developed from the ground up and written in Scala for the Ethereum Classic network (ETC). Created by IOHK to add robustness and variety to the ETC's client share, it includes optimizations and network upgrades that improve network security, sustainability, and performance in the long term.

## Useful Links

* [Mantis User Documentation](https://docs.mantisclient.io/)
* [Mantis Releases](https://github.com/input-output-hk/mantis/releases)
* [Mantis issues](https://github.com/input-output-hk/mantis/issues)
* [Mantis Discord](https://discord.gg/5BEpX2xV)

## Getting Started

### Requirements

* JVM version 1.8.x  
* 4G of memory (RAM)  
  Additional RAM is needed for the DAG file if mining is enabled.
* 100GB of disk space to ensure fast sync of the node. 500GB is recommended for maintainability.


### Installation

Detailed install instructions [can be found in the documentation](https://docs.mantisclient.io/install/install-client/).

## Usage

To check and see if you installed correctly, run `mantis` or `mantis-launcher` from the bin directory. Remember to 

```
./mantis mordor # change argument to desired network name
```

## Roadmap

Many of the current Mantis Development efforts are being concentrated on experimental ECIP implementations that could benefit the network as a whole by adding security, robustness and longevity to the Ethereum Classic chain.

Some of these efforts include:

* Checkpointing
* ETC Proto-Treasury 
* SHA3 consensus algorithm

More details on the roadmap can be [found in the documentation](https://docs.mantisclient.io/learn/-roadmap).


## Contributing

Mantis is open source and licensed under the Apache 2.0 License. We welcome all contributions from community members- PRs, issues and comments are encouraged.  
When contributing to Mantis, please discuss the change you are planning on making via github issue or discord with the maintainers before making a change.

### Pull Request Process

1. Create a personal fork of the project on Github.
2. Clone the fork on your local machine. Your remote repo on Github is called origin.
3. Add the original repository as a remote called upstream.
4. If you created your fork a while ago be sure to pull upstream changes into your local repository.
5. Create a new branch to work on! Branch from develop if it exists, else from master. We recommend using gitflow branch naming (ie. feature/feature-name)
6. Implement/fix your feature, comment your code.
7. Squash your commits into a single commit with git's interactive rebase.
8. From your fork open a pull request in the `develop` branch.
9. Follow along the comments and possible issue the PR could have if brought up by the maintainer team.


## License




Update the README.md with details of changes to the interface, this includes new environment variables, exposed ports, useful file locations and container parameters.
Increase the version numbers in any examples files and the README.md to the new version that this Pull Request would represent. The versioning scheme we use is SemVer.
You may merge the Pull Request in once you have the sign-off of two other developers, or if you do not have permission to do that, you may request the second reviewer to merge it for you.
