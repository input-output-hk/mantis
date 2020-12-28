chain="$1"
config_file="${app_home}/../conf/${chain}.conf"

if [ -f "$config_file" ]; then
  addJava "-Dconfig.file=${config_file}"
  shift
elif [ $# -eq 0 ]; then
  echo "You need to choose a chain"
  exit 1
fi

addJava "-Dlogback.configurationFile=${app_home}/../conf/logback.xml"
