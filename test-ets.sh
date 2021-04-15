if [ -z "$IN_NIX_SHELL" ]; then
    export SBT_NIX="-Dnix=true"
fi

git submodule init;
git submodule update;

echo "running ETS"
$SBT -Dconfig.file=./src/main/resources/conf/testmode.conf run | tee mantis-output.txt &

while ! nc -z localhost 8546; do   
  sleep 0.1
done

retesteth -- --testpath src/ets/resources/ets --datadir src/ets/resources/config --clients mantis | tee retesteth-output.txt

kill %1
