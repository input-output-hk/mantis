
FROM mantis-dev:latest as CURRENTBUILD

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

# Grab latest mantis, build the distribution and install it
RUN ~/install-mantis.sh $MANTIS_TAG $MANTIS_DIST_ZIP_NAME
# Now mantis is in /home/mantis/mantis-dist/app
# or just /app

# Start over and keep what is needed.
# Now the size optimization comes from `mantis-base`:
# smaller `mantis-base` means smaller `mantis` image (this image).  
FROM mantis-base:latest

USER root
COPY --from=CURRENTBUILD /home/mantis/mantis-dist /home/mantis/mantis-dist
RUN chown -R mantis:mantis /home/mantis/mantis-dist

USER mantis
WORKDIR /app
VOLUME /app/conf
