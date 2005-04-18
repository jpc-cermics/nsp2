SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = lapack.a

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC =

# ATLAS symbol must remain empty 
ATLAS= 
# LOCAL symbol must contain a list of local routines which are 
# also present in ATLAS  
# the following LOCAL variable is valid for ATLAS-3.2.1 

LOCAL= dgesv.o dgetrf.o dgetrs.o dpotrs.o dpotrf.o  zgetrf.o zgetrs.o zpotrf.o

OBJSF = dbdsdc.o dbdsqr.o ddisna.o dgbbrd.o dgbcon.o dgbequ.o dgbrfs.o dgbsv.o \
	 dgbsvx.o dgbtf2.o dgbtrf.o dgbtrs.o dgebak.o dgebal.o dgebd2.o dgebrd.o \
	dgecon.o dgeequ.o dgees.o dgeesx.o dgeev.o dgeevx.o dgegs.o dgegv.o dgehd2.o \
	dgehrd.o dgelq2.o dgelqf.o dgelsd.o dgels.o dgelss.o dgelsx.o dgelsy.o dgeql2.o \
	dgeqlf.o dgeqp3.o dgeqpf.o dgeqr2.o dgeqrf.o dgerfs.o dgerq2.o dgerqf.o dgesc2.o \
	dgesdd.o dgesvd.o dgesv.o dgesvx.o dgetc2.o dgetf2.o dgetrf.o dgetri.o dgetrs.o \
	dggbak.o dggbal.o dgges.o dggesx.o dggev.o dggevx.o dggglm.o dgghrd.o dgglse.o \
	dggqrf.o dggrqf.o dggsvd.o dggsvp.o dgtcon.o dgtrfs.o dgtsv.o dgtsvx.o dgttrf.o \
	dgttrs.o dgtts2.o dhgeqz.o dhsein.o dhseqr.o dlabad.o dlabrd.o dlacon.o dlacpy.o \
	dladiv.o dlae2.o dlaebz.o dlaed0.o dlaed1.o dlaed2.o dlaed3.o dlaed4.o dlaed5.o \
	dlaed6.o dlaed7.o dlaed8.o dlaed9.o dlaeda.o dlaein.o dlaev2.o dlaexc.o dlag2.o \
	dlags2.o dlagtf.o dlagtm.o dlagts.o dlagv2.o dlahqr.o dlahrd.o dlaic1.o dlaln2.o \
	dlals0.o dlalsa.o dlalsd.o dlamch.o dlamrg.o dlangb.o dlange.o dlangt.o dlanhs.o \
	dlansb.o dlansp.o dlanst.o dlansy.o dlantb.o dlantp.o dlantr.o dlanv2.o dlapll.o \
	dlapmt.o dlapy2.o dlapy3.o dlaqgb.o dlaqge.o dlaqp2.o dlaqps.o dlaqsb.o dlaqsp.o \
	dlaqsy.o dlaqtr.o dlar1v.o dlar2v.o dlarfb.o dlarf.o dlarfg.o dlarft.o dlarfx.o \
	dlargv.o dlarnv.o dlarrb.o dlarre.o dlarrf.o dlarrv.o dlartg.o dlartv.o dlaruv.o \
	dlarzb.o dlarz.o dlarzt.o dlas2.o dlascl.o dlasd0.o dlasd1.o dlasd2.o dlasd3.o \
	dlasd4.o dlasd5.o dlasd6.o dlasd7.o dlasd8.o dlasd9.o dlasda.o dlasdq.o dlasdt.o \
	dlaset.o dlasq1.o dlasq2.o dlasq3.o dlasq4.o dlasq5.o dlasq6.o dlasr.o dlasrt.o \
	dlassq.o dlasv2.o dlaswp.o dlasy2.o dlasyf.o dlatbs.o dlatdf.o dlatps.o dlatrd.o \
	dlatrs.o dlatrz.o dlatzm.o dlauu2.o dlauum.o dopgtr.o dopmtr.o dorg2l.o dorg2r.o \
	dorgbr.o dorghr.o dorgl2.o dorglq.o dorgql.o dorgqr.o dorgr2.o dorgrq.o dorgtr.o \
	dorm2l.o dorm2r.o dormbr.o dormhr.o dorml2.o dormlq.o dormql.o dormqr.o dormr2.o \
	dormr3.o dormrq.o dormrz.o dormtr.o dpbcon.o dpbequ.o dpbrfs.o dpbstf.o dpbsv.o \
	dpbsvx.o dpbtf2.o dpbtrf.o dpbtrs.o dpocon.o dpoequ.o dporfs.o dposv.o dposvx.o \
	dpotf2.o dpotrf.o dpotri.o dpotrs.o dppcon.o dppequ.o dpprfs.o dppsv.o dppsvx.o \
	dpptrf.o dpptri.o dpptrs.o dptcon.o dpteqr.o dptrfs.o dptsv.o dptsvx.o dpttrf.o dpttrs.o \
	dptts2.o drscl.o dsbevd.o dsbev.o dsbevx.o dsbgst.o dsbgvd.o dsbgv.o dsbgvx.o dsbtrd.o \
	dsecnd.o dspcon.o dspevd.o dspev.o dspevx.o dspgst.o dspgvd.o dspgv.o dspgvx.o dsprfs.o \
	dspsv.o dspsvx.o dsptrd.o dsptrf.o dsptri.o dsptrs.o dstebz.o dstedc.o dstegr.o dstein.o \
	dsteqr.o dsterf.o dstevd.o dstev.o dstevr.o dstevx.o dsycon.o dsyevd.o dsyev.o dsyevr.o \
	dsyevx.o dsygs2.o dsygst.o dsygvd.o dsygv.o dsygvx.o dsyrfs.o dsysv.o dsysvx.o dsytd2.o \
	dsytf2.o dsytrd.o dsytrf.o dsytri.o dsytrs.o dtbcon.o dtbrfs.o dtbtrs.o dtgevc.o dtgex2.o \
	dtgexc.o dtgsen.o dtgsja.o dtgsna.o dtgsy2.o dtgsyl.o dtpcon.o dtprfs.o dtptri.o dtptrs.o \
	dtrcon.o dtrevc.o dtrexc.o dtrrfs.o dtrsen.o dtrsna.o dtrsyl.o dtrti2.o dtrtri.o dtrtrs.o \
	dtzrqf.o dtzrzf.o dzsum1.o icmax1.o ieeeck.o ilaenv.o izmax1.o lsame.o lsamen.o slamch.o \
	xerbla.o zbdsqr.o zdrot.o zdrscl.o zgbbrd.o zgbcon.o zgbequ.o zgbrfs.o zgbsv.o zgbsvx.o \
	zgbtf2.o zgbtrf.o zgbtrs.o zgebak.o zgebal.o zgebd2.o zgebrd.o zgecon.o zgeequ.o zgees.o \
	zgeesx.o zgeev.o zgeevx.o zgegs.o zgegv.o zgehd2.o zgehrd.o zgelq2.o zgelqf.o zgelsd.o \
	zgels.o zgelss.o zgelsx.o zgelsy.o zgeql2.o zgeqlf.o zgeqp3.o zgeqpf.o zgeqr2.o zgeqrf.o \
	zgerfs.o zgerq2.o zgerqf.o zgesc2.o zgesdd.o zgesvd.o zgesv.o zgesvx.o zgetc2.o zgetf2.o \
	zgetrf.o zgetri.o zgetrs.o zggbak.o zggbal.o zgges.o zggesx.o zggev.o zggevx.o zggglm.o \
	zgghrd.o zgglse.o zggqrf.o zggrqf.o zggsvd.o zggsvp.o zgtcon.o zgtrfs.o zgtsv.o zgtsvx.o \
	zgttrf.o zgttrs.o zgtts2.o zhbevd.o zhbev.o zhbevx.o zhbgst.o zhbgvd.o zhbgv.o zhbgvx.o \
	zhbtrd.o zhecon.o zheevd.o zheev.o zheevr.o zheevx.o zhegs2.o zhegst.o zhegvd.o zhegv.o \
	zhegvx.o zherfs.o zhesv.o zhesvx.o zhetd2.o zhetf2.o zhetrd.o zhetrf.o zhetri.o zhetrs.o \
	zhgeqz.o zhpcon.o zhpevd.o zhpev.o zhpevx.o zhpgst.o zhpgvd.o zhpgv.o zhpgvx.o zhprfs.o \
	zhpsv.o zhpsvx.o zhptrd.o zhptrf.o zhptri.o zhptrs.o zhsein.o zhseqr.o zlabrd.o zlacgv.o \
	zlacon.o zlacp2.o zlacpy.o zlacrm.o zlacrt.o zladiv.o zlaed0.o zlaed7.o zlaed8.o zlaein.o \
	zlaesy.o zlaev2.o zlags2.o zlagtm.o zlahef.o zlahqr.o zlahrd.o zlaic1.o zlals0.o zlalsa.o \
	zlalsd.o zlangb.o zlange.o zlangt.o zlanhb.o zlanhe.o zlanhp.o zlanhs.o zlanht.o zlansb.o \
	zlansp.o zlansy.o zlantb.o zlantp.o zlantr.o zlapll.o zlapmt.o zlaqgb.o zlaqge.o zlaqhb.o \
	zlaqhe.o zlaqhp.o zlaqp2.o zlaqps.o zlaqsb.o zlaqsp.o zlaqsy.o zlar1v.o zlar2v.o zlarcm.o \
	zlarfb.o zlarf.o zlarfg.o zlarft.o zlarfx.o zlargv.o zlarnv.o zlarrv.o zlartg.o zlartv.o \
	zlarzb.o zlarz.o zlarzt.o zlascl.o zlaset.o zlasr.o zlassq.o zlaswp.o zlasyf.o zlatbs.o \
	zlatdf.o zlatps.o zlatrd.o zlatrs.o zlatrz.o zlatzm.o zlauu2.o zlauum.o zpbcon.o zpbequ.o \
	zpbrfs.o zpbstf.o zpbsv.o zpbsvx.o zpbtf2.o zpbtrf.o zpbtrs.o zpocon.o zpoequ.o zporfs.o \
	zposv.o zposvx.o zpotf2.o zpotrf.o zpotri.o zpotrs.o zppcon.o zppequ.o zpprfs.o zppsv.o \
	zppsvx.o zpptrf.o zpptri.o zpptrs.o zptcon.o zpteqr.o zptrfs.o zptsv.o zptsvx.o zpttrf.o \
	zpttrs.o zptts2.o zrot.o zspcon.o zspmv.o zspr.o zsprfs.o zspsv.o zspsvx.o zsptrf.o zsptri.o \
	zsptrs.o zstedc.o zstegr.o zstein.o zsteqr.o zsycon.o zsymv.o zsyr.o zsyrfs.o zsysv.o zsysvx.o \
	zsytf2.o zsytrf.o zsytri.o zsytrs.o ztbcon.o ztbrfs.o ztbtrs.o ztgevc.o ztgex2.o ztgexc.o \
	ztgsen.o ztgsja.o ztgsna.o ztgsy2.o ztgsyl.o ztpcon.o ztprfs.o ztptri.o ztptrs.o ztrcon.o \
	ztrevc.o ztrexc.o ztrrfs.o ztrsen.o ztrsna.o ztrsyl.o ztrti2.o ztrtri.o ztrtrs.o ztzrqf.o \
	ztzrzf.o zung2l.o zung2r.o zungbr.o zunghr.o zungl2.o zunglq.o zungql.o zungqr.o zungr2.o \
	zungrq.o zungtr.o zunm2l.o zunm2r.o zunmbr.o zunmhr.o zunml2.o zunmlq.o zunmql.o zunmqr.o \
	zunmr2.o zunmr3.o zunmrq.o zunmrz.o zunmtr.o zupgtr.o zupmtr.o 

include ../Make.lib

all:: Makefile.mak

dlamch.o: dlamch.f
	$(FC) -c dlamch.f -o dlamch.o

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile
