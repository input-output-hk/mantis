#!/usr/bin/env bash

set -euxo pipefail

apt-get update
apt-get dist-upgrade -y
apt-get install -y curl bzip2 locales
locale-gen en_US.UTF-8
update-locale LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8

adduser --disabled-password --gecos '' mantis

mkdir /nix
chown mantis:mantis /nix
su mantis -c 'curl https://nixos.org/nix/install | sh \
              && tail -n 1 ~/.profile >> ~/.bashrc'
ln -s /home/mantis/mantis-dist/app /app

apt-get purge -y curl bzip2
apt-get clean -y
rm -rf /var/cache/debconf/* /var/lib/apt/lists/* /var/log/* /tmp/* /var/tmp/*