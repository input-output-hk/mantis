#!/usr/bin/env bash

set -euxo pipefail

tmpFile=$(mktemp -t aws_ecr_login.XXXXXX)

function cleanup() {
  if test -e $tmpFile; then
    rm $tmpFile
  fi
}
trap cleanup EXIT ERR


# We assume `awscli` is set up.
# Authentication token for the ECR registry expires, so we log into the registry again.
echo '#!/usr/bin/env bash' > $tmpFile
echo >> $tmpFile
aws ecr get-login --region eu-west-1 --no-include-email >> $tmpFile
chmod +x $tmpFile
$tmpFile



./docker/build-dev.sh
./docker/build.sh

# docker push 920648890259.dkr.ecr.eu-west-1.amazonaws.com/kevm:2018-04-20
