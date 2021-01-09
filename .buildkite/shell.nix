{ sources,  pkgs }:

let

  # TODO, share this code with mantis build in this project
  # sbt-protoc puts the scala plugin in /tmp/protobridge<some-random-number>.
  # it is in fact a shell script with a standard `#!/usr/bin/env sh` shebang
  # that makes the Nix sandbox ANGRY and breaks all the things in a cryptic,
  # hairpull-inducing way. So we gotta sed it out. Not the prettiest thing
  # but it works.
  protoc-wrapper = pkgs.writeShellScriptBin "protoc" ''
    set -e

    for f in "$@"; do
      echo ''${f##*=}
    done | grep protocbridge | xargs sed -i "1s|.*|#!${pkgs.bash}/bin/bash|"

    exec ${pkgs.protobuf}/bin/protoc "$@"
  '';
in

  with pkgs;

  mkShell {
    nativeBuildInputs = [ sbt solc jdk8 protoc-wrapper ];
    # SBT = "sbt -v -mem 2048 -J-Xmx4g -Dsbt.ivy.home=/cache/ivy2 -Dsbt.boot.directory=/cache/sbt -Dmaven.repo.local=/cache/maven -Dnix=true";
    SBT = "sbt -v -mem 2048 -J-Xmx4g -Dnix=true";
  }
