@echo off

cd "%~dp0\.."

set "CONFIG_FILE=conf\%1.conf"
set "RESTVAR=%*"

if not exist %CONFIG_FILE% goto :skip
    set "CHAIN_PARAM=-Dconfig.file=%CONFIG_FILE%"
    set RESTVAR=
    shift
    :loop
    if "%1"=="" goto skip
        set RESTVAR=%RESTVAR% %1
        shift
        goto loop

:skip
call bin\mantis.bat %CHAIN_PARAM% %RESTVAR%
