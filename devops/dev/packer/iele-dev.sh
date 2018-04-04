#!/usr/bin/env bash

set -eu
set -o pipefail

export DEBIAN_FRONTEND=noninteractive

# Add extra repos
sudo add-apt-repository -y ppa:ethereum/ethereum
sudo add-apt-repository -y ppa:webupd8team/java
sudo apt-add-repository -y ppa:fish-shell/release-2

# Add sbt repo
echo "deb https://dl.bintray.com/sbt/debian /" \
  |  sudo tee -a /etc/apt/sources.list.d/sbt.list \
  && sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823

# Add docker (yes docker) repo
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"

sudo apt-get update && sudo apt-get upgrade -y

# Install basic stuff
sudo apt-get install -y locales \
  && locale-gen en_US.UTF-8 \
  && update-locale LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8

sudo apt-get install -y \
  software-properties-common \
  fish \
  docker-ce \
  openjdk-8-jdk \
  solc \
  sbt

# Add user to docker group
sudo adduser $USER docker
  
# Fetch and build Mantis dependencies
mkdir ~/repos
cd ~/repos
git clone --branch v0.4.1 --depth=1 https://github.com/input-output-hk/sbt-verify.git
cd sbt-verify
sbt publishLocal

# Fetch and build Mantis
cd ~/repos
git clone --recurse-submodules --branch 'phase/iele_testnet' https://github.com/input-output-hk/mantis.git
cd mantis
sbt 'set test in Test := {}' dist
mv target/universal/mantis-*.zip ..

