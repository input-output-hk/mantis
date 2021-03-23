{ lib, writeShellScriptBin, awscli, mantis, coreutils, gnugrep }:
writeShellScriptBin "mantis-entrypoint" ''
  set -exuo pipefail

  export PATH="${lib.makeBinPath [ coreutils mantis awscli gnugrep ]}"

  ${builtins.readFile ./entrypoint.sh}
''
