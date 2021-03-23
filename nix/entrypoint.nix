{ lib, writeShellScript, awscli, mantis, coreutils, gnugrep }:
writeShellScript "mantis" ''
  set -exuo pipefail

  export PATH="${lib.makeBinPath [ coreutils mantis awscli gnugrep ]}"

  ${builtins.readFile ./entrypoint.sh}
''
