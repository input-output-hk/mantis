FROM ubuntu:xenial

RUN \
  apt-get update &&\
  apt-get install -y locales &&\
  locale-gen en_US.UTF-8 &&\
  update-locale LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8

RUN \
  apt-get install -y openjdk-8-jdk

RUN \
  echo "deb http://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list &&\
  apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 &&\
  apt-get update &&\
  apt-get install -y sbt

RUN \
  apt-get install -y software-properties-common &&\
  add-apt-repository -y ppa:ethereum/ethereum &&\
  apt-get update &&\
  apt-get install -y solc

RUN useradd --create-home -s /bin/bash circleci

WORKDIR /home/circleci

USER circleci

RUN \
  git clone https://github.com/input-output-hk/sbt-verify.git &&\
  cd sbt-verify &&\
  sbt publishLocal
