{ lib, writeBashBinChecked, awscli, mantis, coreutils, gnugrep }:
writeBashBinChecked "mantis-entrypoint" ''
  set -exuo pipefail

  export PATH="${lib.makeBinPath [ coreutils mantis awscli gnugrep ]}"

  ${builtins.readFile ./entrypoint.sh}
''
