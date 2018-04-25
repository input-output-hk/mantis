FROM mantis-dev:2018-04-25.3e3a3eb

ARG MANTIS_TAG
ENV MANTIS_TAG ${MANTIS_TAG:-phase/iele_testnet}

# The command `sbt dist` generates a zip file in the `target/universal` directory.
# The value of `MANTIS_DIST_ZIP_NAME` must be the name of the generated zip,
# excluding the extension.
# So, for example, currently (commit `35e06611`) the `sbt dist` command
# produces `target/universal/mantis-1.0-daedalus-rc1.zip`, so we can set
# `MANTIS_DIST_ZIP_NAME` to be `mantis-1.0-daedalus-rc1`.
# A glob like `mantis-*` also works and is more convenient, since it is invariant
# with respect to the other part, which dependens on the software version.
ARG MANTIS_DIST_ZIP_NAME
ENV MANTIS_DIST_ZIP_NAME ${MANTIS_DIST_ZIP_NAME:-mantis-*}

# TODO: Could just use a nix-shell and garbage collect (e.g. git, unzip, sbt) after the installations ...
RUN . ~/.nix-profile/etc/profile.d/nix.sh \
    && cd repos \
    && git clone https://github.com/input-output-hk/mantis.git \
    && cd mantis \
    && git checkout $MANTIS_TAG \
    && git submodule update --init \
    && sbt 'set test in Test := {}' dist \
    && mkdir -p ~/mantis-dist/app \
    && unzip -d ~/mantis-dist/app target/universal/${MANTIS_DIST_ZIP_NAME}.zip \
    && mv ~/mantis-dist/app/*/* ~/mantis-dist/app \
    && rmdir ~/mantis-dist/app/$MANTIS_DIST_ZIP_NAME \
    && cd ~ \
    && rm -rf ~/repos ~/.ivy2 ~/.sbt

# Now mantis is in /home/mantis/mantis-dist/app
# or just /app
