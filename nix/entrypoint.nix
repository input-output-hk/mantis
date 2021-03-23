{ lib, writeShellScript, awscli, mantis-kevm, coreutils, gnugrep }:
writeShellScript "mantis" ''
  set -exuo pipefail

  export PATH="${lib.makeBinPath [ coreutils mantis-kevm awscli gnugrep ]}"

  ${builtins.readFile ./entrypoint.sh}
''
