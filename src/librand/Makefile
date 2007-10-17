SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = rand.a

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC=  Random.o Random-IN.o rand_mt.o rand_kiss.o rand_clcg2.o rand_clcg4.o rand_fsultra.o rand_well1024a.o \
        normal.o exponential.o discrete.o poisson.o binomial.o other_distributions.o \
	genbet.o genchi.o genexp.o genf.o gengam.o genmn.o genmul.o gennch.o gennf.o gennor.o \
	igngeom.o ignbin.o ignnbn.o ignpoi.o \
	phrtsd.o sdot.o setgmn.o sexpo.o sgamma.o snorm.o

OBJSF= 


include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile



