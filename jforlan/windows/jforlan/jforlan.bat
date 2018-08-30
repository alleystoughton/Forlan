@echo off

REM **********
REM edit the following line to set jardir to the directory where JForlan.jar
REM is located
REM **********

set jardir=C:\

java -jar %jardir%\JForlan.jar %1 %2 %3 %4
