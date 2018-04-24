FROM ubuntu:xenial

ENV DEBIAN_FRONTEND noninteractive

ARG SBT_VERIFY_TAG
ENV SBT_VERIFY_TAG ${SBT_VERIFY_TAG:-v0.4.1}

ARG MANTIS_TAG
ENV MANTIS_TAG ${MANTIS_TAG:-phase/iele_testnet}

ARG MANTIS_DIST_ZIP_NAME
ENV MANTIS_DIST_ZIP_NAME ${MANTIS_DIST_ZIP_NAME:-mantis-*.zip}

# Just install enough to get nix up and running
RUN apt-get update \
    && apt-get install -y curl bzip2 locales \
    && locale-gen en_US.UTF-8 \
    && update-locale LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8 \
    && adduser --disabled-password --gecos '' mantis \
    && mkdir /nix \
    && chown mantis:mantis /nix \
    && su mantis -c 'curl https://nixos.org/nix/install | sh && . ~/.nix-profile/etc/profile.d/nix.sh && nix-channel --update && tail -n 1 ~/.profile >> ~/.bashrc' \
    && ln -s /home/mantis/mantis-dist/app /app \
    && apt-get purge -y curl bzip2 \
    && apt-get clean autoremove

USER mantis
WORKDIR /home/mantis
ENV USER mantis

# install java, sbt, git, unzip
RUN . ~/.nix-profile/etc/profile.d/nix.sh \
    && nix-env -i openjdk-8u172b02 sbt-1.1.4 git unzip

# TODO: Could just use a nix-shell and garbage collect (e.g. git, unzip, sbt) after the installations ...
RUN . ~/.nix-profile/etc/profile.d/nix.sh \
    && mkdir repos && cd repos \
    && git clone --branch $SBT_VERIFY_TAG --depth=1 https://github.com/input-output-hk/sbt-verify.git \
    && cd sbt-verify \
    && sbt publishLocal \
    && cd - \
    && git clone --branch $MANTIS_TAG --depth=1 https://github.com/input-output-hk/mantis.git \
    && cd mantis \
    && git submodule update --init \
    && sbt 'set test in Test := {}' dist \
    && mkdir -p ~/mantis-dist/app \
    && unzip -d ~/mantis-dist/app target/universal/$MANTIS_DIST_ZIP_NAME \
    && mv ~/mantis-dist/app/*/* ~/mantis-dist/app \
    && rmdir ~/mantis-dist/app/$MANTIS_DIST_ZIP_NAME \
    && cd ~ \
    && rm -rf ~/repos ~/.ivy2 ~/.sbt

# Now mantis is in /home/mantis/mantis-dist/app
# or just /app
