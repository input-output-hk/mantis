@echo off

cd "%~dp0\.."

set "CONFIG_FILE=conf\%1.conf"
set "RESTVAR=%*"

if exist %CONFIG_FILE% goto :set_chain_param

if "%1"=="" set "CHAIN_PARAM=-Dconfig.file=conf\etc.conf"
goto :launch

:set_chain_param
set "CHAIN_PARAM=-Dconfig.file=%CONFIG_FILE%"
set RESTVAR=
shift
:loop
if "%1"=="" goto :launch
    set RESTVAR=%RESTVAR% %1
    shift
    goto :loop

:launch
call bin\mantis.bat %CHAIN_PARAM% %RESTVAR%
