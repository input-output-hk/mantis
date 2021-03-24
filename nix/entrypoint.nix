{ lib, writeBashBinChecked, awscli, mantis, coreutils, gnugrep, gnused }:
writeBashBinChecked "mantis-entrypoint" ''
  set -exuo pipefail

  export PATH="${lib.makeBinPath [ coreutils mantis awscli gnugrep gnused ]}"

  ${builtins.readFile ./entrypoint.sh}
''
