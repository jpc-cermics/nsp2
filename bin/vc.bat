@echo off
rem This file is used by findmsvccompiler
rem to run vcvarsall.bat and obtain the env variables
cd /D "%1"
call "vcvarsall.bat" %2
set
