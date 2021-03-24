{ lib, writeBashBinChecked, nginx, coreutils, package, target }:
writeBashBinChecked "entrypoint" ''
  export PATH="${lib.makeBinPath [ nginx coreutils ]}"
  mkdir -p /var/cache/nginx
  ln -fs ${package} ${target}

  config="$1"
  echo "waiting for valid nginx config..."
  until nginx -t -c "$config"; do
    sleep 1
  done

  exec nginx -g 'error_log stderr;' -c "$@"
''
