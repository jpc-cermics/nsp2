#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh

include ../../Makefile.incl.mak
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = lapack.lib

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC =

# ATLAS symbol must remain empty 
ATLAS= 
# LOCAL symbol must contain a list of local routines which are 
# also present in ATLAS  
# the following LOCAL variable is valid for ATLAS-3.2.1 

LOCAL= dgesv.obj dgetrf.obj dgetrs.obj dpotrs.obj dpotrf.obj  zgetrf.obj zgetrs.obj zpotrf.obj

UNUSED= dlamch.obj xerbla.obj dhgeqz.obj

OBJSF = dbdsdc.obj dbdsqr.obj ddisna.obj dgbbrd.obj dgbcon.obj dgbequ.obj dgbrfs.obj dgbsv.obj \
	 dgbsvx.obj dgbtf2.obj dgbtrf.obj dgbtrs.obj dgebak.obj dgebal.obj dgebd2.obj dgebrd.obj \
	dgecon.obj dgeequ.obj dgees.obj dgeesx.obj dgeev.obj dgeevx.obj dgegs.obj dgegv.obj dgehd2.obj \
	dgehrd.obj dgelq2.obj dgelqf.obj dgelsd.obj dgels.obj dgelss.obj dgelsx.obj dgelsy.obj dgeql2.obj \
	dgeqlf.obj dgeqp3.obj dgeqpf.obj dgeqr2.obj dgeqrf.obj dgerfs.obj dgerq2.obj dgerqf.obj dgesc2.obj \
	dgesdd.obj dgesvd.obj dgesv.obj dgesvx.obj dgetc2.obj dgetf2.obj dgetrf.obj dgetri.obj dgetrs.obj \
	dggbak.obj dggbal.obj dgges.obj dggesx.obj dggev.obj dggevx.obj dggglm.obj dgghrd.obj dgglse.obj \
	dggqrf.obj dggrqf.obj dggsvd.obj dggsvp.obj dgtcon.obj dgtrfs.obj dgtsv.obj dgtsvx.obj dgttrf.obj \
	dgttrs.obj dgtts2.obj  dhsein.obj dhseqr.obj dlabad.obj dlabrd.obj dlacon.obj dlacpy.obj \
	dladiv.obj dlae2.obj dlaebz.obj dlaed0.obj dlaed1.obj dlaed2.obj dlaed3.obj dlaed4.obj dlaed5.obj \
	dlaed6.obj dlaed7.obj dlaed8.obj dlaed9.obj dlaeda.obj dlaein.obj dlaev2.obj dlaexc.obj dlag2.obj \
	dlags2.obj dlagtf.obj dlagtm.obj dlagts.obj dlagv2.obj dlahqr.obj dlahrd.obj dlaic1.obj dlaln2.obj \
	dlals0.obj dlalsa.obj dlalsd.obj dlamrg.obj dlangb.obj dlange.obj dlangt.obj dlanhs.obj \
	dlansb.obj dlansp.obj dlanst.obj dlansy.obj dlantb.obj dlantp.obj dlantr.obj dlanv2.obj dlapll.obj \
	dlapmt.obj dlapy2.obj dlapy3.obj dlaqgb.obj dlaqge.obj dlaqp2.obj dlaqps.obj dlaqsb.obj dlaqsp.obj \
	dlaqsy.obj dlaqtr.obj dlar1v.obj dlar2v.obj dlarfb.obj dlarf.obj dlarfg.obj dlarft.obj dlarfx.obj \
	dlargv.obj dlarnv.obj dlarrb.obj dlarre.obj dlarrf.obj dlarrv.obj dlartg.obj dlartv.obj dlaruv.obj \
	dlarzb.obj dlarz.obj dlarzt.obj dlas2.obj dlascl.obj dlasd0.obj dlasd1.obj dlasd2.obj dlasd3.obj \
	dlasd4.obj dlasd5.obj dlasd6.obj dlasd7.obj dlasd8.obj dlasd9.obj dlasda.obj dlasdq.obj dlasdt.obj \
	dlaset.obj dlasq1.obj dlasq2.obj dlasq3.obj dlasq4.obj dlasq5.obj dlasq6.obj dlasr.obj dlasrt.obj \
	dlassq.obj dlasv2.obj dlaswp.obj dlasy2.obj dlasyf.obj dlatbs.obj dlatdf.obj dlatps.obj dlatrd.obj \
	dlatrs.obj dlatrz.obj dlatzm.obj dlauu2.obj dlauum.obj dopgtr.obj dopmtr.obj dorg2l.obj dorg2r.obj \
	dorgbr.obj dorghr.obj dorgl2.obj dorglq.obj dorgql.obj dorgqr.obj dorgr2.obj dorgrq.obj dorgtr.obj \
	dorm2l.obj dorm2r.obj dormbr.obj dormhr.obj dorml2.obj dormlq.obj dormql.obj dormqr.obj dormr2.obj \
	dormr3.obj dormrq.obj dormrz.obj dormtr.obj dpbcon.obj dpbequ.obj dpbrfs.obj dpbstf.obj dpbsv.obj \
	dpbsvx.obj dpbtf2.obj dpbtrf.obj dpbtrs.obj dpocon.obj dpoequ.obj dporfs.obj dposv.obj dposvx.obj \
	dpotf2.obj dpotrf.obj dpotri.obj dpotrs.obj dppcon.obj dppequ.obj dpprfs.obj dppsv.obj dppsvx.obj \
	dpptrf.obj dpptri.obj dpptrs.obj dptcon.obj dpteqr.obj dptrfs.obj dptsv.obj dptsvx.obj dpttrf.obj dpttrs.obj \
	dptts2.obj drscl.obj dsbevd.obj dsbev.obj dsbevx.obj dsbgst.obj dsbgvd.obj dsbgv.obj dsbgvx.obj dsbtrd.obj \
	dsecnd.obj dspcon.obj dspevd.obj dspev.obj dspevx.obj dspgst.obj dspgvd.obj dspgv.obj dspgvx.obj dsprfs.obj \
	dspsv.obj dspsvx.obj dsptrd.obj dsptrf.obj dsptri.obj dsptrs.obj dstebz.obj dstedc.obj dstegr.obj dstein.obj \
	dsteqr.obj dsterf.obj dstevd.obj dstev.obj dstevr.obj dstevx.obj dsycon.obj dsyevd.obj dsyev.obj dsyevr.obj \
	dsyevx.obj dsygs2.obj dsygst.obj dsygvd.obj dsygv.obj dsygvx.obj dsyrfs.obj dsysv.obj dsysvx.obj dsytd2.obj \
	dsytf2.obj dsytrd.obj dsytrf.obj dsytri.obj dsytrs.obj dtbcon.obj dtbrfs.obj dtbtrs.obj dtgevc.obj dtgex2.obj \
	dtgexc.obj dtgsen.obj dtgsja.obj dtgsna.obj dtgsy2.obj dtgsyl.obj dtpcon.obj dtprfs.obj dtptri.obj dtptrs.obj \
	dtrcon.obj dtrevc.obj dtrexc.obj dtrrfs.obj dtrsen.obj dtrsna.obj dtrsyl.obj dtrti2.obj dtrtri.obj dtrtrs.obj \
	dtzrqf.obj dtzrzf.obj dzsum1.obj icmax1.obj ieeeck.obj ilaenv.obj izmax1.obj lsame.obj lsamen.obj slamch.obj \
	zbdsqr.obj zdrot.obj zdrscl.obj zgbbrd.obj zgbcon.obj zgbequ.obj zgbrfs.obj zgbsv.obj zgbsvx.obj \
	zgbtf2.obj zgbtrf.obj zgbtrs.obj zgebak.obj zgebal.obj zgebd2.obj zgebrd.obj zgecon.obj zgeequ.obj zgees.obj \
	zgeesx.obj zgeev.obj zgeevx.obj zgegs.obj zgegv.obj zgehd2.obj zgehrd.obj zgelq2.obj zgelqf.obj zgelsd.obj \
	zgels.obj zgelss.obj zgelsx.obj zgelsy.obj zgeql2.obj zgeqlf.obj zgeqp3.obj zgeqpf.obj zgeqr2.obj zgeqrf.obj \
	zgerfs.obj zgerq2.obj zgerqf.obj zgesc2.obj zgesdd.obj zgesvd.obj zgesv.obj zgesvx.obj zgetc2.obj zgetf2.obj \
	zgetrf.obj zgetri.obj zgetrs.obj zggbak.obj zggbal.obj zgges.obj zggesx.obj zggev.obj zggevx.obj zggglm.obj \
	zgghrd.obj zgglse.obj zggqrf.obj zggrqf.obj zggsvd.obj zggsvp.obj zgtcon.obj zgtrfs.obj zgtsv.obj zgtsvx.obj \
	zgttrf.obj zgttrs.obj zgtts2.obj zhbevd.obj zhbev.obj zhbevx.obj zhbgst.obj zhbgvd.obj zhbgv.obj zhbgvx.obj \
	zhbtrd.obj zhecon.obj zheevd.obj zheev.obj zheevr.obj zheevx.obj zhegs2.obj zhegst.obj zhegvd.obj zhegv.obj \
	zhegvx.obj zherfs.obj zhesv.obj zhesvx.obj zhetd2.obj zhetf2.obj zhetrd.obj zhetrf.obj zhetri.obj zhetrs.obj \
	zhgeqz.obj zhpcon.obj zhpevd.obj zhpev.obj zhpevx.obj zhpgst.obj zhpgvd.obj zhpgv.obj zhpgvx.obj zhprfs.obj \
	zhpsv.obj zhpsvx.obj zhptrd.obj zhptrf.obj zhptri.obj zhptrs.obj zhsein.obj zhseqr.obj zlabrd.obj zlacgv.obj \
	zlacon.obj zlacp2.obj zlacpy.obj zlacrm.obj zlacrt.obj zladiv.obj zlaed0.obj zlaed7.obj zlaed8.obj zlaein.obj \
	zlaesy.obj zlaev2.obj zlags2.obj zlagtm.obj zlahef.obj zlahqr.obj zlahrd.obj zlaic1.obj zlals0.obj zlalsa.obj \
	zlalsd.obj zlangb.obj zlange.obj zlangt.obj zlanhb.obj zlanhe.obj zlanhp.obj zlanhs.obj zlanht.obj zlansb.obj \
	zlansp.obj zlansy.obj zlantb.obj zlantp.obj zlantr.obj zlapll.obj zlapmt.obj zlaqgb.obj zlaqge.obj zlaqhb.obj \
	zlaqhe.obj zlaqhp.obj zlaqp2.obj zlaqps.obj zlaqsb.obj zlaqsp.obj zlaqsy.obj zlar1v.obj zlar2v.obj zlarcm.obj \
	zlarfb.obj zlarf.obj zlarfg.obj zlarft.obj zlarfx.obj zlargv.obj zlarnv.obj zlarrv.obj zlartg.obj zlartv.obj \
	zlarzb.obj zlarz.obj zlarzt.obj zlascl.obj zlaset.obj zlasr.obj zlassq.obj zlaswp.obj zlasyf.obj zlatbs.obj \
	zlatdf.obj zlatps.obj zlatrd.obj zlatrs.obj zlatrz.obj zlatzm.obj zlauu2.obj zlauum.obj zpbcon.obj zpbequ.obj \
	zpbrfs.obj zpbstf.obj zpbsv.obj zpbsvx.obj zpbtf2.obj zpbtrf.obj zpbtrs.obj zpocon.obj zpoequ.obj zporfs.obj \
	zposv.obj zposvx.obj zpotf2.obj zpotrf.obj zpotri.obj zpotrs.obj zppcon.obj zppequ.obj zpprfs.obj zppsv.obj \
	zppsvx.obj zpptrf.obj zpptri.obj zpptrs.obj zptcon.obj zpteqr.obj zptrfs.obj zptsv.obj zptsvx.obj zpttrf.obj \
	zpttrs.obj zptts2.obj zrot.obj zspcon.obj zspmv.obj zspr.obj zsprfs.obj zspsv.obj zspsvx.obj zsptrf.obj zsptri.obj \
	zsptrs.obj zstedc.obj zstegr.obj zstein.obj zsteqr.obj zsycon.obj zsymv.obj zsyr.obj zsyrfs.obj zsysv.obj zsysvx.obj \
	zsytf2.obj zsytrf.obj zsytri.obj zsytrs.obj ztbcon.obj ztbrfs.obj ztbtrs.obj ztgevc.obj ztgex2.obj ztgexc.obj \
	ztgsen.obj ztgsja.obj ztgsna.obj ztgsy2.obj ztgsyl.obj ztpcon.obj ztprfs.obj ztptri.obj ztptrs.obj ztrcon.obj \
	ztrevc.obj ztrexc.obj ztrrfs.obj ztrsen.obj ztrsna.obj ztrsyl.obj ztrti2.obj ztrtri.obj ztrtrs.obj ztzrqf.obj \
	ztzrzf.obj zung2l.obj zung2r.obj zungbr.obj zunghr.obj zungl2.obj zunglq.obj zungql.obj zungqr.obj zungr2.obj \
	zungrq.obj zungtr.obj zunm2l.obj zunm2r.obj zunmbr.obj zunmhr.obj zunml2.obj zunmlq.obj zunmql.obj zunmqr.obj \
	zunmr2.obj zunmr3.obj zunmrq.obj zunmrz.obj zunmtr.obj zupgtr.obj zupmtr.obj 

include ../Make.lib.mak



dlamch.obj: dlamch.f
	$(FC) -c dlamch.f -o dlamch.obj

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile
