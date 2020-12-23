set "CHAIN=%~1"
set "CONFIG_FILE=%APP_HOME\..\conf\%CHAIN%.conf"

if exists %CONFIG_FILE% (
    call :addJava "-Dconfig.file=%CONFIG_FILE%"
    shift
) else if "%1"=="" (
    echo "You need to choose a chain"
    exit /B 1
)
