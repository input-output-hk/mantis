{ lib, writeBashBinChecked, awscli, mantis, coreutils, gnugrep, gnused, kevm
, iele }:
writeBashBinChecked "mantis-entrypoint" ''
  set -exuo pipefail

  export PATH="${
    lib.makeBinPath [ coreutils mantis awscli gnugrep gnused kevm iele ]
  }"

  ${builtins.readFile ./entrypoint.sh}
''
