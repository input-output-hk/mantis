set "CHAIN=%~1"
set "CONFIG_FILE=%APP_HOME%\conf\%CHAIN%.conf"

if exist %CONFIG_FILE% (
    call :add_java "-Dconfig.file=%CONFIG_FILE%"
    shift
) else if "%1"=="" (
    echo "You need to choose a chain"
    exit /B 1
)

call :add_java "-Dlogback.configurationFile=%APP_HOME%\conf\logback.xml"
