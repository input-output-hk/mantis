set "CHAIN=%1"
set "CONFIG_FILE=%APP_HOME%\conf\%CHAIN%.conf"

if not exist %CONFIG_FILE% goto :skip
call :add_java "-Dconfig.file=%CONFIG_FILE%"
shift

:skip
call :add_java "-Dlogback.configurationFile=%APP_HOME%\conf\logback.xml"
