@echo off
REM Makesubdirs.bat all
if "%1" == "src" goto src
if "%1" == "src-clean" goto src-clean 
if "%1" == "src-distclean" goto src-distclean 
if "%1" == "macros" goto macros
if "%1" == "macros-clean" goto macros-clean 
if "%1" == "macros-distclean" goto macros-distclean 
if "%1" == "wless" goto wless
if "%1" == "imp" goto imp
if "%1" == "intersci" goto intersci
if "%1" == "dumpexts" goto dumpexts
if "%1" == "pvm" goto pvm
if "%1" == "def" goto def

echo Unknown target %1 
goto end

:dumpexts
cd Win95-util\Nm
 nmake /C /f Makefile.mak 
cd ..\..
goto end 

:pvm 
cd pvm3
 nmake /C /f Makefile.mak 
cd ..
goto end 

:def 
cd libs
 nmake /C /f Makefile.mak 
cd ..
goto end 

:src 
cd src 
 nmake /C /f Makefile.mak 
cd ..
goto end 

:src-distclean 
cd src 
 nmake /C /f Makefile.mak distclean 
cd ..
goto end 

:src-clean 
cd src 
 nmake /C /f Makefile.mak clean 
cd ..
goto end 

:macros 
cd macros 
 nmake /C /f Makefile.mak 
cd ..
goto end

:macros-clean 
cd macros 
 nmake /C /f Makefile.mak clean
cd ..
goto end

:macros-distclean 
cd macros 
 nmake /C /f Makefile.mak distclean
cd ..
goto end

:wless
cd wless
 echo making all in wless
 nmake /C /f Makefile.mak 
cd ..
goto end

:imp
cd imp
 echo making all in imp
 nmake /C /f Makefile.mak 
cd ..
goto end

:intersci
cd intersci
 echo making all in intersci
 nmake /C /f Makefile.mak 
cd ..
goto end

:end 
