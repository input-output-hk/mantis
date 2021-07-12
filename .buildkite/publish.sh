#!/usr/bin/env bash

set -euv

# The build agents have gpg 2.0.22, which doesn't have the `--pinentry-mode` option, but
# the sbt-pgp plugin assumes that 2.x has it, and generates an invalid command.
# We can either create a wrapper script that removes that option, or update gpg
# somewhere in pipeline.nix

# Force a restart of the agent becuase it may be out of sync with what Nix installed.
gpgconf --kill all

gpg --version


# The build agent might have this key from before.
GPG_EXISTS=$(gpg --list-keys "$GPG_KEY_ID" >&2 && echo "yes" || echo "no")

if [[ "$GPG_EXISTS" == "no" ]]; then
	echo "$GPG_KEY" | base64 --decode | gpg --batch --import
    # Local testing showed that without this the SBT plugin got "Bad passphrase".
    gpg --passphrase $GPG_PASSPHRASE --batch --yes -a -b LICENSE
fi

# https://github.com/olafurpg/sbt-ci-release#secrets
export PGP_SECRET="$GPG_KEY"
export PGP_PASSPHRASE="$GPG_PASSPHRASE"
export SONATYPE_USERNAME="$OSS_USERNAME"
export SONATYPE_PASSWORD="$OSS_PASSWORD"

set +u

#https://github.com/sbt/sbt/issues/3570
export JAVA_OPTS="$JAVA_OPTS -Dsbt.gigahorse=false"

# ci-release cannot be called on individual modules, but it looks like
# with `publish / skip := true` in build.sbt for the default project,
# without any aggregation, by default it would publish nothing, so
# let's tell it here by using `sbt-ci-release` env vars.
# NOTE: +rlp/publishSigned with the `+` would cross publish,
# but it doesn't work because of scapegoat (see below).
export CI_SNAPSHOT_RELEASE="; bytes/publishSigned; rlp/publishSigned; crypto/publishSigned"
export CI_RELEASE=$CI_SNAPSHOT_RELEASE
export CI_SONATYPE_RELEASE=$"; bytes/sonatypeBundleRelease; rlp/sonatypeBundleRelease; crypto/sonatypeBundleRelease"

# Scala 2.12 has up to scapegoat 1.4.5, while Scala 2.13 starts with 1.4.7.
# I couldn't make build.sbt vary the scapegoat version by the current cross build,
# so as a workaround the combos are called here explicitly.
function release {
    SCALA_VERSION=$1

    sbt "++ $SCALA_VERSION ; ci-release"
}

function releaseAll {
    release 2.12.13
    release 2.13.6
}

if [[ "$BUILDKITE_BRANCH" == "develop" ]]; then

    # Publish the -SNAPSHOT version.
    releaseAll

elif [[ "$BUILDKITE_BRANCH" == "master" ]]; then

    # Remove the -SNAPSHOT from the version file, then publish and release.
    sed -i 's/-SNAPSHOT//' version.sbt

    # Whether ci-release does a release or a snapshot depends on whether it thinks the build is tagged; setting a dummy value.
    # Check https://github.com/olafurpg/sbt-ci-release/blob/main/plugin/src/main/scala/com/geirsson/CiReleasePlugin.scala for the rules.
	export CI_COMMIT_TAG=$(sbt -Dsbt.supershell=false -error "print version")

    releaseAll

else

  echo "Skipping the publish step."

fi
