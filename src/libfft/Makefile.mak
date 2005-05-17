#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh

include ../../Makefile.incl.mak
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib

CFLAGS = $(CC_OPTIONS) $(FFTW3_CFLAGS)
FFLAGS = $(FC_OPTIONS)
OBJSC=fft-IN.obj

NOTHING=

FFTPACK=zfftb.obj cfftb1.obj zfftf.obj cfftf1.obj zffti.obj cffti1.obj dcosqb.obj cosqb1.obj dcosqf.obj\
	cosqf1.obj dcosqi.obj dcost.obj dcosti.obj ezfft1.obj dzfftb.obj dzfftf.obj dzffti.obj passb.obj\
	passb2.obj passb3.obj passb4.obj passb5.obj passf.obj passf2.obj passf3.obj passf4.obj passf5.obj\
	radb2.obj radb3.obj radb4.obj radb5.obj radbg.obj radf2.obj radf3.obj radf4.obj radf5.obj radfg.obj\
	dfftb.obj rfftb1.obj dfftf.obj rfftf1.obj dffti.obj rffti1.obj dsinqb.obj dsinqf.obj dsinqi.obj\
	dsint.obj sint1.obj dsinti.obj
 
OBJSF=$(FFTPACK)
 
include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile



