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
export CI_SNAPSHOT_RELEASE="; rlp/publishSigned; crypto/publishSigned"
export CI_RELEASE=$CI_SNAPSHOT_RELEASE
export CI_SONATYPE_RELEASE=$"; rlp/sonatypeBundleRelease; crypto/sonatypeBundleRelease"

if [[ "$BUILDKITE_BRANCH" == "develop" ]]; then

    # Publish the -SNAPSHOT version.
    sbt ci-release

elif [[ "$BUILDKITE_BRANCH" == "master" ]]; then

    # Remove the -SNAPSHOT from the version file, then publish and release.
    sed -i 's/-SNAPSHOT//' version.sbt

    # Whether ci-release does a release or a snapshot depends on whether it thinks the build is tagged; setting a dummy value.
    # Check https://github.com/olafurpg/sbt-ci-release/blob/main/plugin/src/main/scala/com/geirsson/CiReleasePlugin.scala for the rules.
	export CI_COMMIT_TAG=$(sbt -Dsbt.supershell=false -error "print version")

    sbt ci-release

else

  echo "Skipping the publish step."

fi
