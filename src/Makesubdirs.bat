@echo off
REM Makesubdirs.bat all
if "%1" == "all" goto all
if "%1" == "clean" goto all
if "%1" == "distclean" goto all
if "%1" == "tksci" goto tksci 
if "%1" == "pvm" goto pvm 

echo Unknown target %1 
goto end

 Functions Newstack Parse

:all
cd f2c\src 
echo Making %1 in directory  f2c\src 
 nmake /C /f Makefile.mak %1
cd ..\..
cd f2c\libf2c
echo Making %1 in directory  f2c\libf2c
 nmake /C /f Makefile.mak %1
cd ..\..
cd xdr
echo Making %1 in directory  xdr
 nmake /C /f Makefile.mak %1
cd ..
cd zcalelm
echo Making %1 in directory  zcalelm
 nmake /C /f Makefile.mak %1
cd ..
cd zblas
echo Making %1 in directory  zblas
 nmake /C /f Makefile.mak %1
cd ..
cd System 
echo Making %1 in directory  System
 nmake /C /f Makefile.mak %1
cd ..
cd System1
echo Making %1 in directory  System
 nmake /C /f Makefile.mak %1
cd ..
cd mexlib
echo Making %1 in directory  mexlib 
 nmake /C /f Makefile.mak %1
cd ..
cd Parse 
echo Making %1 in directory  Parse 
 nmake /C /f Makefile.mak %1
cd ..
cd Newstack
echo Making %1 in directory  Newstack
 nmake /C /f Makefile.mak %1
cd ..
cd Functions
echo Making %1 in directory  Functions
 nmake /C /f Makefile.mak %1
cd ..
echo on
goto end

:tksci 
cd tksci 
echo Making %1 in directory  tksci 
 nmake /C /f Makefile.mak all
cd ..
goto end 

:pvm 
cd pvm 
echo Making %1 in directory  pvm 
 nmake /C /f Makefile.mak all
cd ..
goto end

:end 


