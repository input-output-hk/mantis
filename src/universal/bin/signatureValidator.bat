@echo off

cd "%~dp0\.."

call bin\mantis.bat signature-validator %*
