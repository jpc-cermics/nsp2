#include <stdlib.h> 
#include <string.h>
#include "nsp/machine.h"
#include "../system/link.h"
#include "scicos.h"
#include "import.h"
#include "blocks.h"

#define abs(x) ((x) >= 0 ? (x) : -(x))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#define min(a,b) ((a) <= (b) ? (a) : (b))



#define freeall \
	      free(rhot);\
	      free(ihot);\
	      free(jroot);\
	      free(W);\
	      free(zcros);

#define freeallx \
	      free(rhot);\
	      free(ihot);\
	      free(jroot);\
	      free(W);\
	      free(zcros);\
	      free(scicos_xproperty);\
              free(Mode_save);

void cosini(double *);
void idoit(double *);
void cosend(double *);
void cdoit(double *);
void doit(double *);
void ddoit(double *);
void edoit(double *,int *);
void odoit(double *,double *,double *,double *);
void ozdoit(double *,double *,double *,int *);
void zdoit(double *,double *,double *,double *);
void reinitdoit(double *,int *);
void cossimdaskr(double *);
void cossim(double *);
void callf(double *, double *, double *, double *,double *,int *);
int C2F(simblk)(int *, double *, double *, double *);
int C2F(simblkdaskr)(double *, double *, double *, int *, double *, int *, double *, int *);
int C2F(grblk)(int *, double *, double *, int *, double *);
int C2F(grblkdaskr)(int *, double *, double *, double *, int *, double *, double *, int *);
void addevs(double ,int *,int *);
void putevs(double *,int *,int *);
void free_blocks(void);
int setmode(double *,double *,double *,int *,double);


extern void  F2C(sciblk)();
extern void  sciblk2();
extern void  sciblk4();
extern void  GetDynFunc(int, void (**) (/* ??? */));
extern void  sciprint();
extern void  C2F(iislink)();
extern  int C2F(cvstr)();
extern  int C2F(dset)();
extern  int C2F(dcopy)();
extern  int C2F(iset)();
extern  int C2F(realtime)();
extern  int C2F(realtimeinit)();
extern  int C2F(sxevents)();
extern  int C2F(stimer)();
extern  int C2F(xscion)();
extern  int C2F(ddaskr)();
extern  int C2F(lsodar2)();

ScicosImport  scicos_imp;

static int nblk, nordptr, nout, ng, ncord, noord, nzord,niord,
  nclock,nordclk,niord,nmod;

static int *neq;

static  double Atol, rtol, ttol, deltat,hmax;
static int hot;

extern struct {
  int iero;
} C2F(ierode);

extern  struct {
  int kfun;
} C2F(curblk);

struct {
  double scale;
}  C2F(rtfactor);

extern struct {
  int halt;
}  C2F(coshlt);


struct {
  int cosd;
} C2F(cosdebug);

struct {
  int solver;
} C2F(cmsolver);

/* Table of constant values */

static int c__90 = 90;
static int c__0 = 0;
static int c__91 = 91;
static double c_b14 = 0.;
static int c__1 = 1;
static int panj=5;

static int *iwa;

static int *xptr,*modptr, *evtspt;
static int  *funtyp, *inpptr, *outptr, *inplnk, *outlnk, *lnkptr;
static int *clkptr, *ordptr, *ordclk, *cord,   *iord, *oord,  *zord,  *critev,  *zcptr;
static int *pointi;
static int *ierr;

static double *x,*xd,*tevts,*outtb;
static int *mod;
static double *t0,*tf,scicos_time;

static scicos_block *Blocks; 

static int phase;

int *pointer_xproperty;

int n_pointer_xproperty;

static int *block_error;

void call_debug_scicos(double *, double *, double *, double *,double *,int *,int,int,int);

static int debug_block;

/* Subroutine */ 
int C2F(scicos)
     (x_in, xptr_in, z__, work,zptr,modptr_in, iz, izptr, t0_in, tf_in, tevts_in, 
      evtspt_in, nevts, pointi_in, outtb_in, nout1, funptr, funtyp_in, inpptr_in, outptr_in, 
      inplnk_in, outlnk_in, lnkptr_in, nlnkptr, rpar, rpptr, ipar, ipptr, clkptr_in, 
      ordptr_in, nordptr1, ordclk_in, cord_in, ncord1, iord_in, niord1, oord_in, noord1, 
      zord_in, nzord1, critev_in, nblk1, ztyp, zcptr_in, subscr, nsubs, simpar, 
      flag__, ierr_out)
     double *x_in,*z__;
     void **work;
     int *modptr_in;
     int *xptr_in;
     int *zptr, *iz, *izptr;
     double *t0_in, *tf_in, *tevts_in;
     int *evtspt_in, *nevts, *pointi_in;
     double *outtb_in;
     int *nout1, *funptr, *funtyp_in, *inpptr_in, *outptr_in;
     int *inplnk_in, *outlnk_in, *lnkptr_in,*nlnkptr;
     double *rpar;
     int *rpptr, *ipar, *ipptr, *clkptr_in, *ordptr_in, *nordptr1;
     int *ordclk_in, *cord_in, *ncord1, *iord_in, *niord1, *oord_in;
     int *noord1, *zord_in, *nzord1, *critev_in, *nblk1, *ztyp, *zcptr_in;
     int *subscr, *nsubs;
     double *simpar;
     int *flag__, *ierr_out;

{
  int i1,kf,lprt,in,out,job=1;

  extern /* Subroutine */ int C2F(msgs)();
  static int mxtb, ierr0, kfun0, i, j, k;
  extern /* Subroutine */ int C2F(makescicosimport)();
  extern /* Subroutine */ int C2F(getscsmax)();
  static int ni, no;
  extern /* Subroutine */ int C2F(clearscicosimport)();
  static int nx, nz;
  double *W;

  /*     Copyright INRIA */
  /* iz,izptr are used to pass block labels */

  t0=t0_in;
  tf=tf_in;
  ierr=ierr_out;

  /* Parameter adjustments */
  pointi=pointi_in;
  x=x_in;
  xptr=xptr_in-1;
  modptr=modptr_in-1;
  --zptr;
  --izptr;
  evtspt=evtspt_in-1;
  tevts=tevts_in-1;
  outtb=outtb_in;
  funtyp=funtyp_in-1;
  inpptr=inpptr_in-1;
  outptr=outptr_in-1;
  inplnk=inplnk_in-1;
  outlnk=outlnk_in-1;
  lnkptr=lnkptr_in-1;
  --rpptr;
  --ipptr;
  clkptr=clkptr_in-1;
  ordptr=ordptr_in-1;
  ordclk=ordclk_in-1;
  cord=cord_in-1;
  iord=iord_in-1;
  oord=oord_in-1;
  zord=zord_in-1;

  critev=critev_in-1;
  --ztyp;
  zcptr=zcptr_in-1;
  --simpar;

  /* Function Body */
  Atol = simpar[1];
  rtol = simpar[2];
  ttol = simpar[3];
  deltat = simpar[4];
  C2F(rtfactor).scale = simpar[5];
  C2F(cmsolver).solver = (int) simpar[6];
  hmax=simpar[7];

  nordptr = *nordptr1;
  nblk = *nblk1;
  ncord = *ncord1;
  noord = *noord1;
  nzord = *nzord1;
  niord = *niord1;
  nout = *nout1;

  *ierr = 0;

  xd=&x[xptr[nblk+1]-1];

  nordclk=ordptr[nordptr]-1;
  /*     computes number of zero crossing surfaces */
  ng = zcptr[nblk + 1] - 1;
  nmod = modptr[nblk + 1] - 1;

  /*     number of  discrete real states */
  nz = zptr[nblk + 1] - 1;
  /*     number of continuous states */
  nx = xptr[nblk +1] - 1;
  neq=&nx;
  /*        add an error message please */
  /*     number of rows in ordclk is ordptr(nclkp1)-1 */

  for (i = 1; i <= nblk; ++i) {
    if (funtyp[i] < 10000) {
      funtyp[i] %= 1000;
    } else {
      funtyp[i] = funtyp[i] % 1000 + 10000;
    }
    ni = inpptr[i + 1] - inpptr[i];
    no = outptr[i + 1] - outptr[i];
    if (funtyp[i] == 1) {
      if (ni + no > 11) {
	/*     hard coded maxsize in callf.c */
	C2F(msgs)(&c__90, &c__0);
	C2F(curblk).kfun = i;
	*ierr = i + 1005;
	return 0;
      }
    } else if (funtyp[i] == 2 || funtyp[i] == 3) {
      /*     hard coded maxsize in scicos.h */
      if (ni + no > SZ_SIZE) {
	C2F(msgs)(&c__90, &c__0);
	C2F(curblk).kfun = i;
	*ierr = i + 1005;
	return 0;
      }
    }
    mxtb = 0;
    if (funtyp[i] == 0) {
      if (ni > 1) {
	for (j = 1; j <= ni; ++j) {
	  k = inplnk[inpptr[i] - 1 + j];
	  mxtb = mxtb + lnkptr[k + 1] - lnkptr[k];
	}
      }
      if (no > 1) {
	for (j = 1; j <= no; ++j) {
	  k = outlnk[outptr[i] - 1 + j];
	  mxtb = mxtb + lnkptr[k + 1] - lnkptr[k];
	}
      }
      if (mxtb > TB_SIZE) {
	C2F(msgs)(&c__91, &c__0);
	C2F(curblk).kfun = i;
	*ierr = i + 1005;
	return 0;
      }
    }
  }

  if((Blocks=malloc(sizeof(scicos_block)*nblk))== NULL ){
    *ierr =5;
    return 0;
  }
  if(nmod>0){
    if((mod=malloc(sizeof(int)*nmod))== NULL ){
      *ierr =5;
      return 0;
    }
  }
  debug_block=-1; /* no debug block for start */

  for (kf = 0; kf < nblk; ++kf) {
    C2F(curblk).kfun = kf+1;
    i=funptr[kf];
    Blocks[kf].type=funtyp[kf+1];
    if (i<0) {
      switch (funtyp[kf+1]) {
      case 0:
	Blocks[kf].funpt=F2C(sciblk);
	break;
      case 1:
	sciprint("type 1 function not allowed for scilab blocks\r\n");
	*ierr =1000+kf+1;
	free_blocks();
	return 0;
      case 2:
	sciprint("type 2 function not allowed for scilab blocks\r\n");
	*ierr =1000+kf+1;
	free_blocks();
	return 0;
      case 3:
	Blocks[kf].funpt=sciblk2;
	Blocks[kf].type=2;
	break;
      case 5:
	Blocks[kf].funpt=sciblk4;
	Blocks[kf].type=4;
	break;
      case 99: /* debugging block */
       Blocks[kf].funpt=sciblk4;
       Blocks[kf].type=4;
       debug_block=kf;
       break;

      case 10005:
	Blocks[kf].funpt=sciblk4;
	Blocks[kf].type=10004;
	break;
      default :
	sciprint("Undefined Function type\r\n");
	*ierr =1000+kf+1;
	free_blocks();
	return 0;
      }
      Blocks[kf].scsptr=-i; /* set scilab function adress for sciblk */
    }
    else if (i<=ntabsim)
      Blocks[kf].funpt=*(tabsim[i-1].fonc);
    else {
      i -= (ntabsim+1);
      GetDynFunc(i,&Blocks[kf].funpt);
      if ( Blocks[kf].funpt == (voidf) 0) {
	sciprint("Function not found\r\n");
	*ierr =1000+kf+1;
	free_blocks();
	return 0;
      }
    }
    Blocks[kf].ztyp=ztyp[kf+1];
    Blocks[kf].nx=xptr[kf+2]-xptr[kf+1];
    Blocks[kf].ng=zcptr[kf+2]-zcptr[kf+1];
    Blocks[kf].nz=zptr[kf+2]-zptr[kf+1];
    Blocks[kf].nrpar=rpptr[kf+2]-rpptr[kf+1];
    Blocks[kf].nipar=ipptr[kf+2]-ipptr[kf+1];
    Blocks[kf].nin=inpptr[kf+2]-inpptr[kf+1]; /* number of input ports */
    Blocks[kf].nout=outptr[kf+2]-outptr[kf+1];/* number of output ports */
    if ((Blocks[kf].insz=malloc(sizeof(int)*Blocks[kf].nin))== NULL ){
      free_blocks();
      *ierr =5;
      return 0;
    }
    if ((Blocks[kf].inptr=malloc(sizeof(double*)*Blocks[kf].nin))== NULL ){
      free_blocks();
      *ierr =5;
      return 0;
    }
    for(in=0;in<Blocks[kf].nin;in++) {
      lprt=inplnk[inpptr[kf+1]+in];
      Blocks[kf].inptr[in]=&(outtb[lnkptr[lprt]-1]);
      Blocks[kf].insz[in]=lnkptr[lprt+1]-lnkptr[lprt];
    }
    if ((Blocks[kf].outsz=malloc(sizeof(int)*Blocks[kf].nout))== NULL ){
      free_blocks();
      *ierr =5;
      return 0;
    }
    if ((Blocks[kf].outptr=malloc(sizeof(double*)*Blocks[kf].nout))== NULL ){
      free_blocks();
      *ierr =5;
      return 0;
    }
    for(out=0;out<Blocks[kf].nout;out++) {
      lprt=outlnk[outptr[kf+1]+out];
      Blocks[kf].outptr[out]=&(outtb[lnkptr[lprt]-1]);
      Blocks[kf].outsz[out]=lnkptr[lprt+1]-lnkptr[lprt];
    }
    Blocks[kf].nevout=clkptr[kf+2] - clkptr[kf+1];
    if ((Blocks[kf].evout=calloc(Blocks[kf].nevout,sizeof(double)))== NULL ){
      free_blocks();
      *ierr =5;
      return 0;
    }

    Blocks[kf].z=&(z__[zptr[kf+1]-1]);
    Blocks[kf].rpar=&(rpar[rpptr[kf+1]-1]);
    Blocks[kf].ipar=&(ipar[ipptr[kf+1]-1]);

    if ((Blocks[kf].res=malloc(sizeof(double)*Blocks[kf].nx))== NULL ){
      free_blocks();
      *ierr =5;
      return 0;
    }
    
    i1=izptr[kf+2]-izptr[kf+1];
    if ((Blocks[kf].label=malloc(sizeof(char)*(i1+1)))== NULL ){
      free_blocks();
      *ierr =5;
      return 0;
    }
    Blocks[kf].label[i1]='\0';
    C2F(cvstr)(&i1,&(iz[izptr[kf+1]-1]),Blocks[kf].label,&job,i1);    
    if ((Blocks[kf].jroot=calloc(Blocks[kf].ng,sizeof(int)))== NULL ){
      free_blocks();
      *ierr =5;
      return 0;
    }
    
    Blocks[kf].work=(void **)(((double *)work)+kf);
    Blocks[kf].nmode=modptr[kf+2]-modptr[kf+1]; 
    if ( Blocks[kf].nmode!=0){
      Blocks[kf].mode=&(mod[modptr[kf+1]-1]);
    }
  }


  if((iwa=malloc(sizeof(int)*(*nevts)))== NULL ){
    free_blocks();
    *ierr =5;
    return 0;
  }


  C2F(makescicosimport)(x, &xptr[1], &zcptr[1], z__, &zptr[1],mod,&modptr[1], iz, &izptr[1], 
			&inpptr[1], &inplnk[1], &outptr[1], &outlnk[1], &lnkptr[1], 
			nlnkptr, rpar, &rpptr[1], ipar, &ipptr[1], &nblk, outtb, 
			&nout, subscr, nsubs, &tevts[1], &evtspt[1], nevts, pointi, 
			&oord[1], &zord[1], funptr, &funtyp[1], &ztyp[1], &cord[1],
			&ordclk[1], &clkptr[1], &ordptr[1], &critev[1], iwa);
  if (*flag__ == 1) {
    /*     initialisation des blocks */
    for (kf = 0; kf < nblk; ++kf) {
      *(Blocks[kf].work)=NULL;
    }
    cosini(t0);
    if (*ierr != 0) {
      ierr0=*ierr;
      kfun0 = C2F(curblk).kfun;
      cosend(t0);
      *ierr=ierr0;
      C2F(curblk).kfun = kfun0;
    }
  } else if (*flag__ == 2) {
    /*     integration */
    if (C2F(cmsolver).solver == 0) {
      cossim(t0);
    } else if (C2F(cmsolver).solver == 100) {
      cossimdaskr(t0);
    } else {
      /*     add a warning message please */
    }
    if (*ierr != 0) {
      ierr0=*ierr;
      kfun0 = C2F(curblk).kfun;
      cosend(t0);
      *ierr=ierr0;
      C2F(curblk).kfun = kfun0;
    }

  } else if (*flag__ == 3) {
    /*     fermeture des blocks */
    cosend(t0);
  } else if (*flag__ == 4) {
    idoit(t0);
    if (*ierr == 0) {
      if((W=malloc(sizeof(double)*nx))== NULL ){
	free(iwa);
	free_blocks();
	*ierr =5;
	return 0;
      }
    
      C2F(simblk)(&nx, t0, x, W);
      for (i = 0; i < nx; ++i) {
	x[i] = W[i];
      }
      free(W);
    }
  }
  free(iwa);
  free_blocks(); 

  C2F(clearscicosimport)();
  return 0;
} 



void cosini(double *told)
{
  static int flag__;
  static int i;

  static int kfune;
  static int jj;

  double *W;
  jj=max(ng,nout);
  if((W=malloc(sizeof(double)*(jj)))== NULL ){
    *ierr =10000;
    return;
  }

  /* Function Body */
  *ierr = 0;
  /*     initialization (flag 4) */
  /*     loop on blocks */

  C2F(dset)(&jj, &c_b14, W, &c__1);
  nclock = 0;
  for (C2F(curblk).kfun = 1; C2F(curblk).kfun <= nblk; ++C2F(curblk).kfun) {
    if (funtyp[C2F(curblk).kfun] >= 0) {
      flag__ = 4;
      callf(told, xd, x, x,W,&flag__);
      if (flag__ < 0 && *ierr == 0) {
	*ierr = 5 - flag__;
	kfune = C2F(curblk).kfun;
      }
    }
  }
  if (*ierr != 0) {
    C2F(curblk).kfun = kfune;
    free(W);
    return;
  }
  /*     initialization (flag 6) */
  nclock = 0;

  for (jj = 1; jj <= ncord; ++jj) {
    C2F(curblk).kfun = cord[jj];
    flag__ = 6;
    if (funtyp[C2F(curblk).kfun] >= 0) {
      callf(told, xd, x, x,W,&flag__);
      if (flag__ < 0) {
	*ierr = 5 - flag__;
	free(W);
	return;
      }
    }
  }
  /*     point-fix iterations */
  nclock =0;
  for (i = 1; i <= nblk + 1; ++i) {
    /*     loop on blocks */
    for (C2F(curblk).kfun = 1; C2F(curblk).kfun <= nblk; ++C2F(curblk).kfun) {
      flag__ = 6;
      if (funtyp[C2F(curblk).kfun] >= 0) {
	callf(told, xd, x, x,W,&flag__);
	if (flag__ < 0) {
	  *ierr = 5 - flag__;
	  free(W);
	  return;
	}
      }
    }

    nclock = 0;

    for (jj = 1; jj <= ncord; ++jj) {
      C2F(curblk).kfun = cord[jj];
      flag__ = 6;
      if (funtyp[C2F(curblk).kfun] >= 0) {
	callf(told, xd, x, x,W,&flag__);
	if (flag__ < 0) {
	  *ierr = 5 - flag__;
	  free(W);
	  return;
	}
      }
    }
    for (jj = 0; jj <= nout-1; ++jj) {
      if (outtb[jj] != W[jj]) {
	goto L30;
      }
    }
    free(W);
    return;
  L30:
    C2F(dcopy)(&nout, outtb, &c__1, W, &c__1);

  }
  *ierr = 20;
  free(W);
} 


void idoit(double *told)
{
  static int flag__;
  static int i,jj;
  static int ierr1;
  static int i2;
  /*     Copyright INRIA */
  /* ..   Parameters .. */
  /*     maximum number of clock output for one block */
  /*     neq must contain after #states all int data for simblk and grblk */
  /*     X must contain after state values all real data for simblk and grblk */
  /* Parameter adjustments */
  /* Function Body */
  /*     initialisation (propagation of constant blocks outputs) */

  for (jj = 1; jj <= niord; ++jj) {
    C2F(curblk).kfun = iord[jj];
    if (outptr[C2F(curblk).kfun + 1] - outptr[C2F(curblk).kfun] > 0) {
      nclock = iord[jj + niord];
      flag__ = 1;
      callf(told, xd, x, x,x,&flag__);
	
      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }
    if (Blocks[C2F(curblk).kfun - 1].nevout > 0) {
      if (funtyp[C2F(curblk).kfun] < 0) {
	
	if (funtyp[C2F(curblk).kfun] == -1) {
	  if (outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]] <= 0.) {
	    i=2;
	  } else {
	    i=1;
	  }
	} else if (funtyp[C2F(curblk).kfun] == -2) {
	  i=max(min((int) outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]],
		    Blocks[C2F(curblk).kfun - 1].nevout),1);
	}
	i2 =i+ clkptr[C2F(curblk).kfun] - 1;
	putevs(told, &i2, &ierr1);
	if (ierr1 != 0) {
	  /*     !                 event conflict */
	  *ierr = 3;
	  return;
	}
	doit(told);
	if (*ierr != 0) {
	  return;
	}
      }
    }
  }  
} 



void cossim(double *told)
{
  /* Initialized data */
  static int otimer = 0;
  /* System generated locals */
  int i3;
  
  /* Local variables */
  static int flag__, jdum;
  static int iopt;
  
  static int ierr1;
  static int j, k;
  static double t;
  static int itask;
  static int jj, jt;
  static int istate, ntimer;

  static double rhotmp;
  static int inxsci;

  static int kpo, kev;
  
  double *rhot;
  int *ihot,niwp,nrwp;
  int *jroot,*zcros;

  double *W;

  nrwp = (*neq) * max(16,*neq + 9) + 22 + ng * 3;
  /* +1 below is so that rhot starts from 1; one wasted location */
  if((rhot=malloc(sizeof(double)*(nrwp+1)))== NULL ){
    *ierr =10000;
    return;
  }
  niwp = *neq + 20 + ng;/* + ng is for change in lsodar2 to
				       handle masking */

  /* +1 below is so that ihot starts from 1; one wasted location */
  if((ihot=malloc(sizeof(int)*(niwp+1)))== NULL ){
    *ierr =10000;
    free(rhot);
    return;
  }
  if((jroot=malloc(sizeof(int)*ng))== NULL ){
    *ierr =10000;
    free(rhot);
    free(ihot);
    return;
  }
  if((zcros=malloc(sizeof(int)*ng))== NULL ){
    *ierr =10000;
    free(rhot);
    free(ihot);
    free(jroot);
    return;
  }
  if((W=malloc(sizeof(double)*ng))== NULL ){
    *ierr =10000;
    free(rhot);
    free(ihot);
    free(jroot);
    free(zcros);
    return;
  }
  
  
  /* Function Body */
  
  C2F(coshlt).halt = 0;
  *ierr = 0;
  
  C2F(xscion)(&inxsci);
  /*     initialization */
  C2F(iset)(&niwp, &c__0, &ihot[1], &c__1);
  C2F(dset)(&nrwp, &c_b14, &rhot[1], &c__1);
  C2F(realtimeinit)(told, &C2F(rtfactor).scale);
  
  phase=1;
  hot = 0;
  itask = 4;

  jt = 2;

  jj = 0;
  for (C2F(curblk).kfun = 1; C2F(curblk).kfun <= nblk; ++C2F(curblk).kfun) {
    if (Blocks[C2F(curblk).kfun-1].ng >= 1) {
      zcros[jj] = C2F(curblk).kfun;
      ++jj;
    }
  }
  /*     . Il fault:  ng >= jj */
  if (jj != ng) {
    zcros[jj] = -1;
  }
  /*     initialisation (propagation of constant blocks outputs) */
  idoit(told);
  if (*ierr != 0) {
    freeall;
    return;
  }
  /*
    }
    }
  */
  
  /*     main loop on time */
  
  while(*told < *tf) {
    
    if (inxsci == 1) {
      ntimer = C2F(stimer)();
      if (ntimer != otimer) {
	C2F(sxevents)();
	otimer = ntimer;
	/*     .     sxevents can modify halt */
      }
    }
    if (C2F(coshlt).halt != 0) {
      C2F(coshlt).halt = 0;
      freeall;
      return;
    }
    if (*pointi == 0) {
      t = *tf;
    } else {
      t = tevts[*pointi];
    }
    if (abs(t - *told) < ttol) {
      t = *told;
      /*     update output part */
    }
    if (*told > t) {
      /*     !  scheduling problem */
      *ierr = 1;
      freeall;
      return;
    }
    if (*told != t) {
      if (xptr[nblk+1] == 1) {
	/*     .     no continuous state */
	if (*told + deltat + ttol > t) {
	  *told = t;
	} else {
	  *told += deltat;
	}
	/*     .     update outputs of 'c' type blocks with no continuous state */
	if (*told >= *tf) {
	  /*     .     we are at the end, update continuous part before leaving */
	  if (ncord > 0) {
	    cdoit(told);
	    freeall;
	    return;
	  }
	}
      } else {
	/*     integrate */
	rhotmp = *tf + ttol;
	kpo = *pointi;
      L20:
	if (critev[kpo] == 1) {
	  rhotmp = tevts[kpo];
	  goto L30;
	}
	kpo = evtspt[kpo];
	if (kpo != 0) {
	  goto L20;
	}
      L30:
	if (rhotmp < rhot[1]) {
	  hot = 0;
	}
	rhot[1] = rhotmp;
	t = min(*told + deltat,min(t,*tf + ttol));
	
	if (ng>0 &&  hot == 0 && nmod>0) {
	  zdoit(W,x,x,told);
	  if (*ierr != 0){
	    freeall;
	    return;
	  }
	}
	
	
	if (hot){
	  istate=2;
	}else{
	  istate = 1;
	}
	if (C2F(cosdebug).cosd >= 1) {
	  sciprint("****lsodar from: %f to %f hot= %d  \r\n", *told,t,hot);
	}

	if(hmax==0){
	  iopt = 0;
	}else{
	  iopt=1;
	  C2F(iset)(&panj, &c__0, &ihot[5], &c__1);
	  C2F(dset)(&panj, &c_b14, &rhot[5], &c__1);
	  rhot[6]=hmax;
	}
	phase=2;
	C2F(lsodar2)(C2F(simblk), neq, x, told, &t, &c__1, &rtol, 
		     &Atol, &itask, &istate, &iopt, &rhot[1], &
		     nrwp, &ihot[1], &niwp, &jdum, &jt, 
		     C2F(grblk), &ng, jroot);
	phase=1;
	if (*ierr > 5) {
	  /*     !           singularity in block */
	  freeall;
	  return;
	}
	if (istate <= 0) {
	  /* integration problem */
	  *ierr = 100 - istate;
	  freeall;
	  return;
	} else {
	  if (C2F(cosdebug).cosd >= 1) {
	    sciprint("****lsodar reached: %f\r\n",*told);
	  }
	  hot = 1;
	}
	
	/*     .     update outputs of 'c' type  blocks if we are at the end*/
	if (*told >= *tf) {
	  if (ncord > 0) {
	    cdoit(told);
	    freeall;
	    return;
	  }
	}
	if (istate == 4) hot=0; /* new feature of lsodar, detects unmasking */
	if (istate == 3) {
	  /*     .        at a least one root has been found */
	  hot = 0;
	  if (C2F(cosdebug).cosd >= 1) {
	    sciprint("root found at t=: %f\r\n",*told);
	  }
	  /*     .        update outputs affecting ztyp blocks ONLY FOR OLD BLOCKS */
	  zdoit(W, xd, x,told);
	  if (*ierr != 0) {
	    freeall;
	    return;
	  }
	  for (jj = 0; jj < ng; ++jj) {
	    C2F(curblk).kfun = zcros[ jj];
	    if (C2F(curblk).kfun == -1) {
	      break; 
	    }
	    kev = 0;
	    
	    for (j = zcptr[C2F(curblk).kfun]-1 ; 
		 j <zcptr[C2F(curblk).kfun+1]-1 ; ++j) {
	      if(jroot[j]!=0){
		kev=1;
		break;
	      }
	    }
	    /*   */
	    if (kev != 0) {
	      Blocks[C2F(curblk).kfun-1].jroot=&jroot[zcptr[C2F(curblk).kfun]-1];
	      if (funtyp[C2F(curblk).kfun] > 0) {
		
		if (Blocks[C2F(curblk).kfun-1].nevout > 0) {
		  flag__ = 3;
		  /* call corresponding block to determine output event (kev) */
		  nclock = -kev;
		  callf(told, xd, x, x,W,&flag__);
		  if (flag__ < 0) {
		    *ierr = 5 - flag__;
		    freeall;
		    return;
		  }
		  /*     .              update event agenda */
		  for (k = 0; k < Blocks[C2F(curblk).kfun-1].nevout; ++k) {
		    if (Blocks[C2F(curblk).kfun-1].evout[k] >= 0.) {
		      i3 = k + clkptr[C2F(curblk).kfun] ;
		      addevs(Blocks[C2F(curblk).kfun-1].evout[k]+(*told), &i3, &ierr1);
		      if (ierr1 != 0) {
			/*     .                       nevts too small */
			*ierr = 3;
			freeall;
			return;
		      }
		    } 
		  }
		}
		/*     .              update state */
		if (Blocks[C2F(curblk).kfun-1].nx > 0) {
		  /*     .              call corresponding block to update state */
		  flag__ = 2;
		  nclock = -kev;
		  callf(told, xd, x, x,W,&flag__);
		  
		  if (flag__ < 0) {
		    *ierr = 5 - flag__;
		    freeall;
		    return;
		  }
		}
	      }
	    }
	  }
	}
      }
      C2F(realtime)(told);
    } else {
      /*     .  t==told */   
      if (C2F(cosdebug).cosd >= 1) {
	sciprint("Event: %d activated at t=%f\r\n",*pointi,*told);
	for(kev=0;kev<nblk;kev++){
	  if (Blocks[kev].nmode>0){
	    sciprint("mode of block %d=%d, ",kev,Blocks[kev].mode[0]);
	  }
	}
	sciprint("**mod**\r\n");
      }

      ddoit(told);
      if (C2F(cosdebug).cosd >= 1) {
	sciprint("End of activation\r\n");
      }
      if (*ierr != 0) {
	freeall;
	return;
      }

    }
    /*     end of main loop on time */
  }
  freeall;
} 





void cossimdaskr(double *told)
{
  /* Initialized data */
  static int otimer = 0;
  /* System generated locals */
  int i3,*ipardummy=NULL;
  double /*d__1,*/*rpardummy=NULL;

  /* Local variables */
  static int flag__, jdum;
  static int info[20];

  static int ierr1;
  static int j, k;
  static double t;
  static int jj, jt;
  static int istate, ntimer;
  static double rhotmp;
  static int inxsci;
  static int kpo, kev;

  double *rhot;
  int *ihot,niwp,nrwp;
  int *jroot,*zcros;
  int maxord;
  int *scicos_xproperty;

  double *W;
  int *Mode_save;
  int Mode_change;

  maxord = 5;
  nrwp = max(maxord + 4,7) * (*neq) + 60 + (*neq)*(*neq) + ng * 3;
  niwp = (*neq) + 40 + (*neq) +ng ; /* + ng is for change in ddaskr to
				       handle masking */

   /* +1 below is so that rhot starts from 1; one wasted location */
  if((rhot=malloc(sizeof(double)*(nrwp+1)))== NULL ){
    *ierr =10000;
    return;
  }
  /* +1 below is so that ihot starts from 1; one wasted location */
  if((ihot=malloc(sizeof(int)*(niwp+1)))== NULL ){
    free(rhot);
    *ierr =10000;
    return;
  }
  if((jroot=malloc(sizeof(int)*ng))== NULL ){
    *ierr =10000;
    free(rhot);
    free(ihot);
    return;
  }
  if((scicos_xproperty=malloc(sizeof(int)*(*neq)))== NULL ){
    *ierr =10000;
    free(rhot);
    free(ihot);
    free(jroot);
    return;
  }
  C2F(iset)(neq, &c__1, scicos_xproperty, &c__1);
  if((zcros=malloc(sizeof(int)*ng))== NULL ){
    *ierr =10000;
    free(rhot);
    free(ihot);
    free(jroot);
    free(scicos_xproperty);
    return;
  }
  if((W=malloc(sizeof(double)*ng))== NULL ){
    *ierr =10000;
    free(rhot);
    free(ihot);
    free(jroot);
    free(scicos_xproperty);
    free(zcros);
    return;
  }
  if((Mode_save=malloc(sizeof(double)*nmod))== NULL ){
    *ierr =10000;
    free(rhot);
    free(ihot);
    free(jroot);
    free(scicos_xproperty);
    free(zcros);
    free(W);
    return;
  }
  /* Function Body */
  C2F(coshlt).halt = 0;
  *ierr = 0;
  /*     hot = .false. */
  phase=1;
  hot = 0;

  jt = 2;

  /*      stuck=.false. */
  C2F(xscion)(&inxsci);
  /*     initialization */
  C2F(iset)(&niwp, &c__0, &ihot[1], &c__1);
  C2F(dset)(&nrwp, &c_b14, &rhot[1], &c__1);
  C2F(realtimeinit)(told, &C2F(rtfactor).scale);
  /*     ATOL and RTOL are scalars */
  info[1] = 0;
  info[2] = 0;
  info[3] = 1;
  /*don't go beyond stopping point TSTOP defined by RWORK(1)*/

  /*     derivatives automatically computed by numerical differences */
  info[4] = 0;
  /*     full jac matrx */
  info[5] = 0;


  /*     code determines initial step size */
  info[7] = 0;
  /*     MAXORD=5 */
  info[8] = 0;
  /*     no info on solution sign available */
  info[9] = 0;
  /*     direc method instead of Dcrylof method */
  info[11] = 0;
  info[12] = 0;
  info[13] = 0;
  info[14] = 0;
  info[15] = 0;
  info[16] = 0;
  info[17] = 0;

  jj = 0;
  for (C2F(curblk).kfun = 1; C2F(curblk).kfun <= nblk; ++C2F(curblk).kfun) {
    if (Blocks[C2F(curblk).kfun-1].ng >= 1) {
      zcros[jj] = C2F(curblk).kfun;
      ++jj;
    }
  }
  /*     . Il fault:  ng >= jj */
  if (jj != ng) {
    zcros[jj] = -1;
  }
  /*     initialisation (propagation of constant blocks outputs) */
  idoit(told);
  if (*ierr != 0) {
    freeallx;
    return;
  }
  /*     main loop on time */
  while (*told < *tf) {
    if (inxsci == 1) {
      ntimer = C2F(stimer)();
      if (ntimer != otimer) {
	C2F(sxevents)();
	otimer = ntimer;
	/*     .     sxevents can modify halt */
      }
    }
    if (C2F(coshlt).halt != 0) {
      C2F(coshlt).halt = 0;
      freeallx;
      return;
    }
    if (*pointi == 0) {
      t = *tf;
    } else {
      t = tevts[*pointi];
    }
    if (abs(t - *told) < ttol) {
      t = *told;
      /*     update output part */
    }
    if (*told > t) {
      /*     !  scheduling problem */
      *ierr = 1;
      freeallx;
      return;
    }
    if (*told != t) {
      if (xptr[nblk+1] == 1) {
	/*     .     no continuous state */
	if (*told + deltat + ttol > t) {
	  *told = t;
	} else {
	  *told += deltat;
	}
	/*     .     update outputs of 'c' type blocks with no continuous state */
	if (*told >= *tf) {
	  /*     .     we are at the end, update continuous part before leaving */
	  cdoit(told);
	  freeallx;
	  return;
	}
      } else {
	if (hot == 0) {
	  reinitdoit(told,scicos_xproperty);
	  if(*ierr >0){
	    freeallx;
	    return;
	  }
	}      
	rhotmp = *tf + ttol;
	kpo = *pointi;
      L20:
	if (critev[kpo] == 1) {
	  rhotmp = tevts[kpo];
	  goto L30;
	}
	kpo = evtspt[kpo];
	if (kpo != 0) {
	  goto L20;
	}
      L30:
	if (rhotmp < rhot[1]) {
	  hot = 0;/* Do cold-restat the solver:if the new TSTOP isn't beyong the previous one*/ 
	}
	rhot[1] = rhotmp;
	t = min(*told + deltat,min(t,*tf + ttol));

	if (hot == 0){ /* CIC calculation when hot==0 */

	  if (ng>0&&nmod>0){
	    phase=1;
	    zdoit(W, xd, x,told);
	    if (*ierr != 0) {
	      freeallx;
	      return;
	    }
	  }


	  for(j=0;j<=nmod;j++){/* counter to reevaluate the 
				  modes in  mode->CIC->mode->CIC-> loop 
				  do it once in the absence of mode (nmod=0)*/
	    /* updating the modes through Flag==9, Phase==1 */
	    
	    info[0]=0;  /* cold start */
	    info[10]=1; /* inconsistent IC */
	    info[13]=1; /* return after CIC calculation */
	    reinitdoit(told,scicos_xproperty);/* filling up the scicos_xproperties */
 	    if(*ierr >0){
	      freeallx;
	      return; 
	    }
	    for (jj = 1; jj <= *neq; ++jj) {
	      ihot[jj + 40] = scicos_xproperty[jj-1];
	    }
	    phase=2;

	    C2F(ddaskr)(C2F(simblkdaskr), neq, told, x, xd, &t, info,  &rtol, 
			 &Atol, &istate, &rhot[1],&nrwp, &ihot[1], &niwp,
			rpardummy, ipardummy, &jdum, rpardummy, 
			C2F(grblkdaskr), &ng, jroot);

	    if (*ierr > 5) {
	      freeallx;
	      return;
	    }
	    if (C2F(cosdebug).cosd >= 1) {
	      if (istate==4) {
		sciprint("**** daskr succesfully initialized *****/r/n" );
	      }
	      else{
		sciprint("**** daskr failed to initialize ->try again *****/r/n" );
	      }
	    }
	    /*-------------------------------------*/
	    /* saving the previous modes*/
	    for (jj = 0; jj < nmod; ++jj) {
	      Mode_save[jj] = mod[jj];
	    }
	    if (ng>0&&nmod>0){	 
	      phase=1;
	      zdoit(W, xd, x,told);
	      if (*ierr != 0) {
		freeallx;
		return; 
	      }
	    }
	    /*------------------------------------*/
	    Mode_change=0;
	    for (jj = 0; jj < nmod; ++jj) {
	      if(Mode_save[jj] != mod[jj])
		{Mode_change=1;
 		break;
		}
	    }
	    if ( Mode_change==0)      break;
	  }/* mode-CIC  counter*/
	  if(Mode_change==1){
	    *ierr = 100 - (-17);
	    freeallx;
	    return;
	  }
	  info[0]=0;  /* cold restart */
	  info[10]=1; /* to reevaluate CIC when info[0]==0*/
	  info[13]=0; /* continue after CIC calculation */
	} /* CIC calculation when hot==0 */

	info[0]=hot;  
	/*     Warning rpar and ipar are used here as dummy pointers */
	phase=2;
	if (C2F(cosdebug).cosd >= 1) {
	  sciprint("****daskr from: %f to %f hot= %d  \r\n", *told,t,hot);
	}
	C2F(ddaskr)(C2F(simblkdaskr), neq, told, x, xd, &t, 
		    info, &rtol, &Atol, &istate, &rhot[1], &
		    nrwp, &ihot[1], &niwp, rpardummy, ipardummy
		    , &jdum, rpardummy, C2F(grblkdaskr), &ng, jroot);


	phase=1;
	if (*ierr > 5) {
	  freeallx; /* singularity in block */
	  return;
	}
	
	if (istate <= 0) {
	  *ierr = 100 - istate;
	  freeallx;/* singularity in block */
	  return;
	} else {
	  if (C2F(cosdebug).cosd >= 1) {
	    sciprint("****daskr reached: %f\r\n",*told);
	  }
	  hot = 1;/* successful return from DDASKR => hot restart*/
	}
	
	/*     update outputs of 'c' type  blocks if we are at the end*/
	if (*told >= *tf) {
	  cdoit(told);
	  freeallx;
	  return;
	}
	if (istate == 6) hot=0; /* new feature of daskr, detects unmasking */
	if (istate == 5) {
	  /*     .        at a least one root has been found */
	  hot = 0;
	  if (C2F(cosdebug).cosd >= 1) {
	    sciprint("root found at t=: %f\r\n",*told);
	  }
	  /*     .        update outputs affecting ztyp blocks  ONLY FOR OLD BLOCKS*/
	  zdoit(W, xd, x,told);
	  if (*ierr != 0) {
	    freeallx;
	    return;
	  }
	  for (jj = 0; jj < ng; ++jj) {
	    C2F(curblk).kfun = zcros[jj];
	    if (C2F(curblk).kfun == -1) {
	      break; 
	    }
	    kev = 0;
	    for (j = zcptr[C2F(curblk).kfun]-1 ; 
		 j <zcptr[C2F(curblk).kfun+1]-1 ; ++j) {
	      if(jroot[j]!=0){
		kev=1;
		break;
	      }
	    }
	    if (kev != 0) {
	      Blocks[C2F(curblk).kfun-1].jroot=&jroot[zcptr[C2F(curblk).kfun]-1];
	      if (funtyp[C2F(curblk).kfun] > 0) {
		if (Blocks[C2F(curblk).kfun-1].nevout > 0) {
		  flag__ = 3;
		  /*     call corresponding block to determine output event (kev) */
		  nclock = -kev;
		  callf(told, xd, x, x,W,&flag__);
		  if (flag__ < 0) {
		    *ierr = 5 - flag__;
		    freeallx;
		    return;
		  }
		  /*     update event agenda */
		  for (k = 0; k < Blocks[C2F(curblk).kfun-1].nevout; ++k) {
		    if (Blocks[C2F(curblk).kfun-1].evout[k] >= 0) {
		      i3 = k + clkptr[C2F(curblk).kfun] ;
		      addevs(Blocks[C2F(curblk).kfun-1].evout[k]+(*told), &i3, &ierr1);
		      if (ierr1 != 0) {
			/*     .                       nevts too small */
			*ierr = 3;
			freeallx;
			return;
		      }
		    }
		  }
		}
		/*     .              update state */
		if (Blocks[C2F(curblk).kfun-1].nx > 0) {
		  /*     .call corresponding block to update state */
		  flag__ = 2;
		  nclock = -kev;
		  pointer_xproperty=
		    &scicos_xproperty[-1+xptr[C2F(curblk).kfun]];
		  n_pointer_xproperty=Blocks[C2F(curblk).kfun-1].nx;
		  callf(told, xd, x, x,W,&flag__);
		  if (flag__ < 0) {
		    *ierr = 5 - flag__;
		    freeallx;
		    return;
		  }
		}
	      }
	    }
	  }
	}
	if (inxsci == 1) {
	  ntimer = C2F(stimer)();
	  if (ntimer != otimer) {
	    C2F(sxevents)();
	    otimer = ntimer;
	    /*     .     sxevents can modify halt */
	  }
	}
	if (C2F(coshlt).halt != 0) {
	  C2F(coshlt).halt = 0;
	  freeallx;
	  return;
	}
	  /* if(*pointi!=0){
	    t=tevts[*pointi];
	    if(*told<t-ttol){
	      cdoit(told);
	      goto L15;
	    }
	  }else{
	    if(*told<*tf){
	      cdoit(told);
	      goto L15;
	    }
	    }*/
      }
      C2F(realtime)(told);
    } else {
      /*     .  t==told */
      if (C2F(cosdebug).cosd >= 1) {
	sciprint("Event: %d activated at t=%f\r\n",*pointi,*told);
      }
      
      ddoit(told);
      if (C2F(cosdebug).cosd >= 1) {
	sciprint("End of activation");
      }
      if (*ierr != 0) {
	freeallx;
	return;
      }
    }
    /*     end of main loop on time */
  }
  freeallx;
}



void cosend(double *told)
{
  /* Local variables */
  static int flag__;
  static int kfune;
  /* Function Body */
  *ierr = 0;
  /*     loop on blocks */
  nclock=0;

  for (C2F(curblk).kfun = 1; C2F(curblk).kfun <= nblk; ++C2F(curblk).kfun) {
    flag__ = 5;
    if (funtyp[C2F(curblk).kfun] >= 0) {
      callf(told, xd, x, x,x,&flag__);
      if (flag__ < 0 && *ierr == 0) {
	*ierr = 5 - flag__;
	kfune = C2F(curblk).kfun;
      }
    }
  }
  if (*ierr != 0) {
    C2F(curblk).kfun = kfune;
    return;
  }
} 


void doit(double *told)
{
  int i,i2;
  static int flag__, nord;
  static int ierr1;
  int ii, kever;
  /* Function Body */
  kever = *pointi;
  *pointi = evtspt[kever];
  evtspt[kever] = -1;

  nord = ordptr[kever + 1] - ordptr[kever];
  if (nord == 0) {
    return;
  }

  for (ii = ordptr[kever]; ii <=ordptr[kever + 1] - 1 ; ++ii) {
    C2F(curblk).kfun = ordclk[ii];
    if (outptr[C2F(curblk).kfun + 1] - outptr[C2F(curblk).kfun] > 0) {
      nclock = abs(ordclk[ii + nordclk]);
      flag__ = 1;
      callf(told, xd, x, x,x,&flag__);

      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }
  
    /*     .     Initialize tvec */
    if (Blocks[C2F(curblk).kfun - 1].nevout > 0) {
      if (funtyp[C2F(curblk).kfun] < 0) {
	if (funtyp[C2F(curblk).kfun] == -1) {
	  if (outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]] <= 0.) {
	    i= 2;
	  } else {
	    i= 1;
	  }
	} else if (funtyp[C2F(curblk).kfun] == -2) {

	  i=max(min((int) outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]],
		    Blocks[C2F(curblk).kfun - 1].nevout),1);
	}
	i2 = i + clkptr[C2F(curblk).kfun] - 1;
	putevs(told, &i2, &ierr1);
	if (ierr1 != 0) {
	  /*     !                 event conflict */
	  *ierr = 3;
	  return;
	}
	doit(told);
	if (*ierr != 0) {
	  return;
	}
      }
    }
  }
} 



void cdoit(double *told)
{
  int i2;
  static int flag__;
  static int ierr1;
  static int i,jj;
  /* Function Body */
  for (jj = 1; jj <= ncord; ++jj) {
    C2F(curblk).kfun = cord[jj];
    nclock = cord[jj + ncord];
    if (outptr[C2F(curblk).kfun + 1] - outptr[C2F(curblk).kfun] > 0) {
      flag__ = 1;
      callf(told, xd, x, x,x,&flag__);
	    
      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }

    if (Blocks[C2F(curblk).kfun - 1].nevout > 0) {
      if (funtyp[C2F(curblk).kfun] < 0) {

	if (funtyp[C2F(curblk).kfun] == -1) {
	  if (outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]] <= 0.) {
	    i = 2;
	  } else {
	    i = 1;
	  }
	} else if (funtyp[C2F(curblk).kfun] == -2) {
	  i= max(min((int) outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]],
		    Blocks[C2F(curblk).kfun - 1].nevout),1);
	}
	i2 = i + clkptr[C2F(curblk).kfun] - 1;
	putevs(told, &i2, &ierr1);
	if (ierr1 != 0) {
	  /*     !                 event conflict */
	  *ierr = 3;
	  return;
	}
	doit(told);
	if (*ierr != 0) {
	  return;
	}
      }
    }
  }
} 



void ddoit(double *told)
{
  int i2;
  static int flag__, kiwa;
  static int i;
  static int  ii, keve;
  kiwa = 0;
  edoit(told,&kiwa);
  if (*ierr != 0) {
    return;
  }
  /*     .  update continuous and discrete states on event */
  if (kiwa == 0) {
    return;
  }
  for (i = 0; i < kiwa; ++i) {
    keve = iwa[i];
    if(critev[keve]!= 0){
      hot = 0;
    }
    i2 = ordptr[keve + 1] - 1;
    for (ii = ordptr[keve]; ii <= i2; ++ii) {
      C2F(curblk).kfun = ordclk[ii];
      nclock=ordclk[ii + nordclk];
      if(nclock> 0) {
	if (Blocks[C2F(curblk).kfun-1].nx+Blocks[C2F(curblk).kfun-1].nz > 0||
	    *Blocks[C2F(curblk).kfun-1].work !=NULL) {
	  /*  if a hidden state exists, must also call (for new scope eg)  */
	  /*  to avoid calling non-real activations */
	  flag__ = 2;
	  callf(told, xd, x, x,x,&flag__);
	  if (flag__ < 0) {
	    *ierr = 5 - flag__;
	    return;
	  }
	}
      }else{
	if (*Blocks[C2F(curblk).kfun-1].work !=NULL) {
	  flag__ = 2;
	  nclock=0;  /* in case some hidden continuous blocks need updating */
	  callf(told, xd, x, x,x,&flag__);
	  if (flag__ < 0) {
	    *ierr = 5 - flag__;
	    return;
	  }
	}
      }
    }
  }
} 

void edoit(double *told, int *kiwa)
{
  int i2, i3;
  double d__1;
  static int flag__;
  static int nord;
  static int ierr1, i;
  int kever, ii;
  kever = *pointi;
  *pointi = evtspt[kever];
  evtspt[kever] = -1;

  nord = ordptr[kever + 1] - ordptr[kever];
  if (nord == 0) {
    return;
  }
  iwa[*kiwa] = kever;
  ++(*kiwa);
  for (ii = ordptr[kever]; ii <= ordptr[kever + 1] - 1; ++ii) {
    C2F(curblk).kfun = ordclk[ii];

    if (outptr[C2F(curblk).kfun + 1] - outptr[C2F(curblk).kfun] > 0) {
      nclock = abs(ordclk[ii + nordclk]);
      flag__ = 1;
      callf(told, xd, x, x,x,&flag__);
	    
      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }
 
    /*     .     Initialize tvec */
    if (Blocks[C2F(curblk).kfun - 1].nevout > 0) {
      if (funtyp[C2F(curblk).kfun] >= 0) {
	d__1 =  - 1.;
	C2F(dset)(&Blocks[C2F(curblk).kfun - 1].nevout, 
		  &d__1, Blocks[C2F(curblk).kfun-1].evout, &c__1);

	flag__ = 3;
	nclock=ordclk[ii + nordclk];
	if(nclock>0){ /* if event has continuous origin don't call*/
	  callf(told, xd, x, x ,x,&flag__);
	  if (flag__ < 0) {
	    *ierr = 5 - flag__;
	    return;
	  }
	}

	if (Blocks[C2F(curblk).kfun - 1].nevout >= 1) {
	  for (i = 0; i < Blocks[C2F(curblk).kfun - 1].nevout; ++i) {
	    if (Blocks[C2F(curblk).kfun-1].evout[i] >= 0.) {
	      i3 = i + clkptr[C2F(curblk).kfun] ;
	      addevs(Blocks[C2F(curblk).kfun-1].evout[i]+(*told), &i3, &ierr1);
	      if (ierr1 != 0) {
		/*     !                 event conflict */
		*ierr = 3;
		return;
	      }
	    }
	  }
	}
      } else {
	if (funtyp[C2F(curblk).kfun] == -1) {
	  if (outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]] <= 0.) {
	    i = 2;
	  } else {
	    i = 1;
	  }
	} else if (funtyp[C2F(curblk).kfun] == -2) {
	  i= max(min((int) outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]],
		    Blocks[C2F(curblk).kfun - 1].nevout),1);
	}
	i2 = i + clkptr[C2F(curblk).kfun] - 1;
	putevs(told, &i2, &ierr1);
	if (ierr1 != 0) {
	  /*     !                 event conflict */
	  *ierr = 3;
	  return;
	}
	edoit(told,kiwa);
      }
    }
  }
} 



void odoit(double *residual, double *xt, double *xtd, double *told)
{
  int i2;
  static int flag__, keve, kiwa;
  static int ierr1, i;
  static int ii, jj;
  /* Function Body */
  kiwa = 0;

  for (jj = 1; jj <= noord; ++jj) {
    C2F(curblk).kfun = oord[jj];
    nclock = oord[jj + noord];
    if (outptr[C2F(curblk).kfun + 1] - outptr[C2F(curblk).kfun] > 0) {
      flag__ = 1;
      callf(told, xtd, xt, residual,x,&flag__);

      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }

    if (Blocks[C2F(curblk).kfun - 1].nevout > 0) {
      if (funtyp[C2F(curblk).kfun] < 0) {
	if(Blocks[C2F(curblk).kfun - 1].nmode > 0){
	  i2 = Blocks[C2F(curblk).kfun - 1].mode[0] + 
	    clkptr[C2F(curblk).kfun] - 1;
	} else{
	  if (funtyp[C2F(curblk).kfun] == -1) {
	    if (outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]] <= 0.) {
	      i=2;
	    } else {
	      i=1;
	    }
	  } else if (funtyp[C2F(curblk).kfun] == -2) {
	    i=max(min((int) outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]],
		      Blocks[C2F(curblk).kfun - 1].nevout),1);
	  }
	  i2 =i+ clkptr[C2F(curblk).kfun] - 1;
	}
	putevs(told, &i2, &ierr1);
	if (ierr1 != 0) {
	  /*     !                 event conflict */
	  *ierr = 3;
	  return;
	}
	ozdoit(xtd, xt,told, &kiwa);
      }
    }
  }
  
  /*     .  update states derivatives */
  for (ii = 1; ii <= noord; ++ii) {
    C2F(curblk).kfun = oord[ii];
    if (Blocks[C2F(curblk).kfun-1].nx > 0||
	*Blocks[C2F(curblk).kfun-1].work !=NULL) {
      /* work tests if a hidden state exists, used for delay block */
      flag__ = 0;
      nclock = oord[ii + noord];
      callf(told, xtd, xt, residual,xt,&flag__);

      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }
  }

  for (i = 0; i < kiwa; ++i) {
    keve = iwa[i];
    for (ii = ordptr[keve]; ii <= ordptr[keve + 1] - 1; ++ii) {
      C2F(curblk).kfun = ordclk[ii ];
      if (Blocks[C2F(curblk).kfun-1].nx > 0||
	*Blocks[C2F(curblk).kfun-1].work !=NULL) {
	/* work tests if a hidden state exists */
	flag__ = 0;
	nclock = abs(ordclk[ii + nordclk]);
	callf(told, xtd, xt, residual,xt,&flag__);

	if (flag__ < 0) {
	  *ierr = 5 - flag__;
	  return;
	}
      }
    }
  }
}


void reinitdoit(double *told, int *scicos_xproperty)
{
  int i2;
  static int flag__, keve, kiwa;
  static int ierr1, i;
  static int ii, jj;
  /* Function Body */
  kiwa = 0;
  for (jj = 1; jj <= noord; ++jj) {
    C2F(curblk).kfun = oord[jj];
    nclock = oord[jj + noord];
    if (outptr[C2F(curblk).kfun + 1] - outptr[C2F(curblk).kfun] > 0) {
      flag__ = 1;
      callf(told, xd, x, x,x,&flag__);
      
      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }
    
    if (Blocks[C2F(curblk).kfun - 1].nevout > 0) {
      if (funtyp[C2F(curblk).kfun] == -1) {
	if (outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]] <= 0.) {
	  i=2;
	} else {
	  i=1;
	}
      } else if (funtyp[C2F(curblk).kfun] == -2) {
	i= max(min((int) outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]],
		  Blocks[C2F(curblk).kfun - 1].nevout),1);
      }
      if(Blocks[C2F(curblk).kfun - 1].nmode>0){
	Blocks[C2F(curblk).kfun - 1].mode[0]=i;
      }
      i2 =i+ clkptr[C2F(curblk).kfun] - 1;
      putevs(told, &i2, &ierr1);
      if (ierr1 != 0) {
	/*     !                 event conflict */
	*ierr = 3;
	return;
      }
      doit(told);
      if (*ierr != 0) {
	return;
      }
    }
  }
  
  /*     .  re-initialize */
  for (ii = 1; ii <= noord; ++ii) {
    C2F(curblk).kfun = oord[ii];
    if (Blocks[C2F(curblk).kfun-1].nx > 0) {
      flag__ = 7;
      nclock = oord[ii + noord];
      pointer_xproperty=&scicos_xproperty[-1+xptr[C2F(curblk).kfun]];
      n_pointer_xproperty=Blocks[C2F(curblk).kfun-1].nx;
      callf(told, xd, x, xd,x,&flag__);

      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }
  }

  for (i = 0; i < kiwa; ++i) {
    keve = iwa[i];
    for (ii = ordptr[keve]; ii <= ordptr[keve + 1] - 1; ++ii) {
      C2F(curblk).kfun = ordclk[ii ];
      if (Blocks[C2F(curblk).kfun-1].nx > 0) {
	flag__ = 7;
	nclock = abs(ordclk[ii + nordclk]);
	n_pointer_xproperty=Blocks[C2F(curblk).kfun-1].nx;
	pointer_xproperty=&scicos_xproperty[-1+xptr[C2F(curblk).kfun]];
	callf(told, xd, x, xd,x,&flag__);

	if (flag__ < 0) {
	  *ierr = 5 - flag__;
	  return;
	}
      }
    }
  }
} 



void ozdoit(double *xtd, double *xt, double *told, int *kiwa)
{
  int i2;
  static int flag__, nord;
  static int ierr1, i;
  int ii, kever; 
  /* Function Body */
  kever = *pointi;
  *pointi = evtspt[kever];
  evtspt[kever] = -1;

  nord = ordptr[kever + 1] - ordptr[kever];
  if (nord == 0) {
    return;
  }
  iwa[*kiwa] = kever;
  ++(*kiwa);

  for (ii = ordptr[kever]; ii <= ordptr[kever + 1] - 1; ++ii) {
    C2F(curblk).kfun = ordclk[ii];
    if (outptr[C2F(curblk).kfun + 1] - outptr[C2F(curblk).kfun] > 0) {
      nclock = abs(ordclk[ii + nordclk]);
      flag__ = 1;
      callf(told, xtd, xt, xt,x,&flag__);

      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }
    /*     .     Initialize tvec */
    
    if (Blocks[C2F(curblk).kfun - 1].nevout > 0) {

      if (funtyp[C2F(curblk).kfun] < 0) {

	if (funtyp[C2F(curblk).kfun] == -1) {
	  if (phase==1 || Blocks[C2F(curblk).kfun - 1].nmode==0){
	    if (outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]] <= 0.) {
	      i=2;
	    } else {
	      i=1;
	    }
	  }else{
	    i=Blocks[C2F(curblk).kfun - 1].mode[0];
	  }
	} else if (funtyp[C2F(curblk).kfun] == -2) {
	  if (phase==1 || Blocks[C2F(curblk).kfun - 1].nmode==0){
	    i= max(min((int) 
		       outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]],
		       Blocks[C2F(curblk).kfun - 1].nevout),1);
	  }else{
	    i=Blocks[C2F(curblk).kfun - 1].mode[0];
	    
	  }
	}
	i2 =i+clkptr[C2F(curblk).kfun] - 1;
	putevs(told, &i2, &ierr1);
	if (ierr1 != 0) {
	  /*     !                 event conflict */
	  *ierr = 3;
	  return;
	}
	ozdoit(xtd, xt,told, kiwa);
      }
    }
  }
} 


void zdoit(double *g, double *xtd, double *xt, double *told)
{
  int i2;
  static int flag__, keve, kiwa;
  static int ierr1, i,j;
  static int ii, jj;
  /* Function Body */
  C2F(dset)(&ng, &c_b14,g , &c__1);

  kiwa = 0;
  for (jj = 1; jj <= nzord; ++jj) {
    C2F(curblk).kfun = zord[jj];
    nclock = zord[jj + nzord];
    if (outptr[C2F(curblk).kfun + 1] - outptr[C2F(curblk).kfun] > 0) {
      flag__ = 1;
      callf(told, xtd, xt, xt,xt,&flag__);

      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }

    if (Blocks[C2F(curblk).kfun - 1].nevout > 0) {
      if (funtyp[C2F(curblk).kfun] < 0) {


	if (funtyp[C2F(curblk).kfun] == -1) {
	  if (phase==1|| Blocks[C2F(curblk).kfun - 1].nmode==0){
	    if (outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]] <= 0.) {
	      i=2;
	    } else {
	      i=1;
	    }
	  }else{
	    i=Blocks[C2F(curblk).kfun - 1].mode[0];
	  }
	} else if (funtyp[C2F(curblk).kfun] == -2) {
	  if (phase==1|| Blocks[C2F(curblk).kfun - 1].nmode==0){
	    i=max(min((int) 
		      outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]],
		      Blocks[C2F(curblk).kfun - 1].nevout),1);
	  }else{
	    i=Blocks[C2F(curblk).kfun - 1].mode[0];
	  }
	}
	i2 =i+clkptr[C2F(curblk).kfun] - 1;
	putevs(told, &i2, &ierr1);
	if (ierr1 != 0) {
	  /*     !                 event conflict */
	  *ierr = 3;
	  return;
	}
	ozdoit(xtd, xt,told, &kiwa);
      }
    }
  }
    
  /*     .  update zero crossing surfaces */
  for (ii = 1; ii <= nzord; ++ii) {
    C2F(curblk).kfun = zord[ii];
    if (Blocks[C2F(curblk).kfun-1].ng > 0) {
      if (funtyp[C2F(curblk).kfun] > 0) {
	flag__ = 9;
	nclock = zord[ii +nzord];
	callf(told, xtd, xt, xtd,g,&flag__);
	if (flag__ < 0) {
	  *ierr = 5 - flag__;
	  return;
	}
      }else{
	if (funtyp[C2F(curblk).kfun] == -1) {
	  g[zcptr[C2F(curblk).kfun]-1]=outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]];
	  if(phase==1&&Blocks[C2F(curblk).kfun - 1].nmode>0){
	    if (g[zcptr[C2F(curblk).kfun]-1] <= 0.) {
	      Blocks[C2F(curblk).kfun - 1].mode[0] = 2;
	    }
	    else {
	      Blocks[C2F(curblk).kfun - 1].mode[0] = 1;
	    }
	  }
	} else if (funtyp[C2F(curblk).kfun] == -2) {
	  for (jj=0;jj<Blocks[C2F(curblk).kfun - 1].nevout-1;++jj) {
	    g[zcptr[C2F(curblk).kfun]-1+jj]=
	      outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]]
	      -(double)(jj+2);
	  }
	  if(phase==1&&Blocks[C2F(curblk).kfun - 1].nmode>0){
	    j=max(min((int) 
		      outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]],
		      Blocks[C2F(curblk).kfun - 1].nevout),1);
	    Blocks[C2F(curblk).kfun - 1].mode[0]= j;
	  }
	}
      }
    }
  }
  for (i = 0; i < kiwa; ++i) {
    keve = iwa[i];
    for (ii = ordptr[keve]; ii <= ordptr[keve + 1] - 1; ++ii) {
      C2F(curblk).kfun = ordclk[ii ];
      if (Blocks[C2F(curblk).kfun-1].ng > 0) {
	if (funtyp[C2F(curblk).kfun] > 0) {
	  flag__ = 9;
	  nclock = abs(ordclk[ii + nordclk]);
	  callf(told, xtd, xt, xtd,g,&flag__);
	  
	  if (flag__ < 0) {
	    *ierr = 5 - flag__;
	    return;
	  }
	}else{
	  if (funtyp[C2F(curblk).kfun] == -1) {
	    g[zcptr[C2F(curblk).kfun]-1]=
	      outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]];
	    if(phase==1&&Blocks[C2F(curblk).kfun - 1].nmode>0){
	      if (g[zcptr[C2F(curblk).kfun]-1] <= 0.) {
		Blocks[C2F(curblk).kfun - 1].mode[0] = 2;
	      } else {
		Blocks[C2F(curblk).kfun - 1].mode[0] = 1;
	      }
	    }
	  } else if (funtyp[C2F(curblk).kfun] == -2) {
	    for (jj=0;jj<Blocks[C2F(curblk).kfun - 1].nevout-1;++jj) {
	      g[zcptr[C2F(curblk).kfun]-1+jj]=
		outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]]
		-(double)(jj+2);
	    }
	    if(phase==1&&Blocks[C2F(curblk).kfun - 1].nmode>0){
	      j=max(min((int) 
			outtb[-1+lnkptr[inplnk[inpptr[C2F(curblk).kfun]]]],
			Blocks[C2F(curblk).kfun - 1].nevout),1);
	      Blocks[C2F(curblk).kfun - 1].mode[0]= j;
	    }
	  }
	}
      }
    }
  }
}


void  callf(double *t, double *xtd, double *xt, double *residual, double *g, int *flag)
{
  voidf loc ; 
  double* args[SZ_SIZE];
  int sz[SZ_SIZE];
  double intabl[TB_SIZE],outabl[TB_SIZE];
  int ii,kf,in,out,ki,ko,ni,no,k,j;
  int lprt,szi,flagi;
  int solver=C2F(cmsolver).solver;
  int cosd=C2F(cosdebug).cosd;
  ScicosF0 loc0;
  ScicosF loc1;
  /*  ScicosFm1 loc3;*/
  ScicosF2 loc2;
  ScicosF2z loc2z;
  ScicosFi loci1;
  ScicosFi2 loci2;
  ScicosFi2z loci2z;
  ScicosF4 loc4;
  
  kf=C2F(curblk).kfun;
  if(debug_block==kf-1) return;  /* a debugging block */

  flagi=*flag; /* flag 7 implicit initialization */
  if(flagi==7 && Blocks[kf-1].type<10000) *flag=0;

  if ( cosd > 1){
    sciprint("block %d is called ",kf);
    sciprint("with flag %d ",*flag);
    sciprint("at time %f \r\n",*t);
    if(debug_block>-1){
      sciprint("Entering the block \r\n");
      call_debug_scicos(t,xtd,xt,residual,g,flag,kf,flagi,debug_block);
    }
  }

  
  C2F(scsptr).ptr=Blocks[kf-1].scsptr; /* set scilab function adress for sciblk */

  block_error=flag;
  loc=Blocks[kf-1].funpt;
  if (Blocks[kf-1].type==4||Blocks[kf-1].type==10004) {
    scicos_time=*t;
    Blocks[kf-1].nevprt=nclock;
    loc4 = (ScicosF4) loc;
    if(Blocks[kf-1].ng>0){
	Blocks[kf-1].g=&g[zcptr[kf]-1];
      }
    if(Blocks[kf-1].nx==0){
      (*loc4)(&Blocks[kf-1],*flag);
    } 
    else {
      Blocks[kf-1].x=&xt[xptr[kf]-1];
      if(Blocks[kf-1].type==4) {
	if(*flag==0 && solver==100) {
	  Blocks[kf-1].res=&residual[xptr[kf]-1];
	  Blocks[kf-1].xd=&residual[xptr[kf]-1];
	  (*loc4)(&Blocks[kf-1],*flag);
	  Blocks[kf-1].xd=&xtd[xptr[kf]-1];
	  if(flagi!=7) {
	    for (k=0;k<Blocks[kf-1].nx;k++) {
	      Blocks[kf-1].res[k]=Blocks[kf-1].res[k]-Blocks[kf-1].xd[k];
	    }
	  }
	  else {
	    for (k=0;k<Blocks[kf-1].nx;k++) {
	      Blocks[kf-1].xd[k]=Blocks[kf-1].res[k];
	    } 
	  }
	}
	else {
	  Blocks[kf-1].xd=&xtd[xptr[kf]-1];
	  (*loc4)(&Blocks[kf-1],*flag);
	}
      }
      else {
	Blocks[kf-1].xd=&xtd[xptr[kf]-1];
	Blocks[kf-1].res=&residual[xptr[kf]-1];
	(*loc4)(&Blocks[kf-1],*flag);
      }
    }
    if ( cosd > 1){
      sciprint("block %d is called ",kf);
      sciprint("with flag %d ",*flag);
      sciprint("at time %f \r\n",*t);
      if(debug_block>-1){
	sciprint("Leaving the block \r\n");
	call_debug_scicos(t,xtd,xt,residual,g,flag,kf,flagi,debug_block);
      }
    }
    return;
  }
  
  /*This is for compatibility*/
  if(nclock<0){
      for (j =0;j<Blocks[kf-1].ng;++j){
	Blocks[kf-1].g[j]=(double)Blocks[kf-1].jroot[j];
    }
  }

  if(Blocks[kf-1].ztyp>0){
    Blocks[kf-1].g=&g[zcptr[kf]-1];
  }
  if(Blocks[kf-1].nx>0){
    Blocks[kf-1].x=&xt[xptr[kf]-1];
    Blocks[kf-1].xd=&xtd[xptr[kf]-1];
    if(solver==100) {
      Blocks[kf-1].res=&residual[xptr[kf]-1];
    }
  }

  switch (Blocks[kf-1].type) {

  case 1 :			
    /* one entry for each input or output */
    for (in = 0 ; in < Blocks[kf-1].nin ; in++) 
      {
	args[in]=Blocks[kf-1].inptr[in];
	sz[in]=Blocks[kf-1].insz[in];
      }
    for (out=0;out<Blocks[kf-1].nout;out++) {
      args[in+out]=Blocks[kf-1].outptr[out];
      sz[in+out]=Blocks[kf-1].outsz[out];
    }
    if(Blocks[kf-1].ztyp>0){
      Blocks[kf-1].g=&g[zcptr[kf]-1];
      args[Blocks[kf-1].nin+Blocks[kf-1].nout]=Blocks[kf-1].g;
      sz[Blocks[kf-1].nin+Blocks[kf-1].nout]=Blocks[kf-1].ng;
    }
    loc1 = (ScicosF) loc;
    if (solver==100) {
      (*loc1)(flag,&nclock,t,Blocks[kf-1].res,Blocks[kf-1].x,&Blocks[kf-1].nx,
	      Blocks[kf-1].z,&Blocks[kf-1].nz,
	      Blocks[kf-1].evout,&Blocks[kf-1].nevout,Blocks[kf-1].rpar,&Blocks[kf-1].nrpar,
	      Blocks[kf-1].ipar,&Blocks[kf-1].nipar,
	      (double *)args[0],&sz[0],
	      (double *)args[1],&sz[1],(double *)args[2],&sz[2],
	      (double *)args[3],&sz[3],(double *)args[4],&sz[4],
	      (double *)args[5],&sz[5],(double *)args[6],&sz[6],
	      (double *)args[7],&sz[7],(double *)args[8],&sz[8],
	      (double *)args[9],&sz[9],(double *)args[10],&sz[10],
	      (double *)args[11],&sz[11],(double *)args[12],&sz[12],
	      (double *)args[13],&sz[13],(double *)args[14],&sz[14],
	      (double *)args[15],&sz[15],(double *)args[16],&sz[16],
	      (double *)args[17],&sz[17]); 
    }
    else {
      (*loc1)(flag,&nclock,t,Blocks[kf-1].xd,Blocks[kf-1].x,&Blocks[kf-1].nx,
	      Blocks[kf-1].z,&Blocks[kf-1].nz,
	      Blocks[kf-1].evout,&Blocks[kf-1].nevout,Blocks[kf-1].rpar,&Blocks[kf-1].nrpar,
	      Blocks[kf-1].ipar,&Blocks[kf-1].nipar,
	      (double *)args[0],&sz[0],
	      (double *)args[1],&sz[1],(double *)args[2],&sz[2],
	      (double *)args[3],&sz[3],(double *)args[4],&sz[4],
	      (double *)args[5],&sz[5],(double *)args[6],&sz[6],
	      (double *)args[7],&sz[7],(double *)args[8],&sz[8],
	      (double *)args[9],&sz[9],(double *)args[10],&sz[10],
	      (double *)args[11],&sz[11],(double *)args[12],&sz[12],
	      (double *)args[13],&sz[13],(double *)args[14],&sz[14],
	      (double *)args[15],&sz[15],(double *)args[16],&sz[16],
	      (double *)args[17],&sz[17]);
    }
    break;   
  case 0 :			
    /* concatenated entries and concatened outputs */
    ni=0;
    /* catenate inputs if necessary */

    if (Blocks[kf-1].nin>1) {
      ki=0;
      for (in=0;in<Blocks[kf-1].nin;in++) {
	lprt=inplnk[inpptr[kf]+in];
	szi=lnkptr[lprt+1]-lnkptr[lprt];
	for (ii=0;ii<szi;ii++) 
	  intabl[ki++]=outtb[lnkptr[lprt]-1+ii];
	ni=ni+szi;
      }
      args[0]=&(intabl[0]);
    }
    else {
      if (Blocks[kf-1].nin==0) {
	ni=0;
	args[0]=&(outtb[0]);
      }
      else {
	lprt=inplnk[inpptr[kf]];
	args[0]=&(outtb[lnkptr[lprt]-1]);
	ni=lnkptr[lprt+1]-lnkptr[lprt];
      }
    }
    in=Blocks[kf-1].nin;
    
    /* catenate outputs if necessary */
	no=0;
    if (Blocks[kf-1].nout>1) {
      ko=0;
      for (out=0;out<Blocks[kf-1].nout;out++) {
	lprt=outlnk[outptr[kf]+out];
	szi=lnkptr[lprt+1]-lnkptr[lprt];
	
	for (ii=0;ii<szi;ii++)  
	  outabl[ko++]=outtb[lnkptr[lprt]-1+ii];
	no=no+szi;
      }
      args[1]=&(outabl[0]);
    }
    else {
      if (Blocks[kf-1].nout==0) {
	no=0;
	args[1]=&(outtb[0]);
      }
      else {
	lprt=outlnk[outptr[kf]];
	args[1]=&(outtb[lnkptr[lprt]-1]);
	no=lnkptr[lprt+1]-lnkptr[lprt];
      }
    }

    loc0 = (ScicosF0) loc;
    if (solver==100) {
      (*loc0)(flag,&nclock,t,Blocks[kf-1].res,Blocks[kf-1].x,&Blocks[kf-1].nx,
	      Blocks[kf-1].z,&Blocks[kf-1].nz,
	      Blocks[kf-1].evout,&Blocks[kf-1].nevout,Blocks[kf-1].rpar,&Blocks[kf-1].nrpar,
	      Blocks[kf-1].ipar,&Blocks[kf-1].nipar,(double *)args[0],&ni,
	      (double *)args[1],&no);
    }
    else {
      (*loc0)(flag,&nclock,t,Blocks[kf-1].xd,Blocks[kf-1].x,&Blocks[kf-1].nx,
	      Blocks[kf-1].z,&Blocks[kf-1].nz,
	      Blocks[kf-1].evout,&Blocks[kf-1].nevout,Blocks[kf-1].rpar,&Blocks[kf-1].nrpar,
	      Blocks[kf-1].ipar,&Blocks[kf-1].nipar,(double *)args[0],&ni,
	      (double *)args[1],&no);
    }
    
    /* split output vector on each port if necessary */
    if (Blocks[kf-1].nout>1) {
      ko=0;
      for (out=0;out<Blocks[kf-1].nout;out++) {
	lprt=outlnk[outptr[kf]+out];
	szi=lnkptr[lprt+1]-lnkptr[lprt];
	for (ii=0;ii<szi;ii++)  
	  outtb[lnkptr[lprt]-1+ii]=outabl[ko++];
      }
    }
    break;
  case 2 :			

    
    if (solver==100) {
      if (Blocks[kf-1].ztyp==0){
	loc2 = (ScicosF2) loc;
	(*loc2)(flag,&nclock,t,Blocks[kf-1].res,Blocks[kf-1].x,&Blocks[kf-1].nx,
		Blocks[kf-1].z,&Blocks[kf-1].nz,
		Blocks[kf-1].evout,&Blocks[kf-1].nevout,Blocks[kf-1].rpar,&Blocks[kf-1].nrpar,
		Blocks[kf-1].ipar,&Blocks[kf-1].nipar,Blocks[kf-1].inptr,
		Blocks[kf-1].insz,&Blocks[kf-1].nin,
		Blocks[kf-1].outptr,Blocks[kf-1].outsz,&Blocks[kf-1].nout);
      }
      else{
	loc2z = (ScicosF2z) loc;
	(*loc2z)(flag,&nclock,t,Blocks[kf-1].res,Blocks[kf-1].x,&Blocks[kf-1].nx,
		 Blocks[kf-1].z,&Blocks[kf-1].nz,
		 Blocks[kf-1].evout,&Blocks[kf-1].nevout,Blocks[kf-1].rpar,&Blocks[kf-1].nrpar,
		 Blocks[kf-1].ipar,&Blocks[kf-1].nipar,Blocks[kf-1].inptr,Blocks[kf-1].insz,&Blocks[kf-1].nin,
		 Blocks[kf-1].outptr,Blocks[kf-1].outsz,&Blocks[kf-1].nout,
		 Blocks[kf-1].g,&Blocks[kf-1].ng);
      }
    }
    else {
      if (Blocks[kf-1].ztyp==0){
	loc2 = (ScicosF2) loc;
	(*loc2)(flag,&nclock,t,Blocks[kf-1].xd,Blocks[kf-1].x,&Blocks[kf-1].nx,
		Blocks[kf-1].z,&Blocks[kf-1].nz,
		Blocks[kf-1].evout,&Blocks[kf-1].nevout,Blocks[kf-1].rpar,&Blocks[kf-1].nrpar,
		Blocks[kf-1].ipar,&Blocks[kf-1].nipar,Blocks[kf-1].inptr,
		Blocks[kf-1].insz,&Blocks[kf-1].nin,
		Blocks[kf-1].outptr,Blocks[kf-1].outsz,&Blocks[kf-1].nout);
      }
      else{
	loc2z = (ScicosF2z) loc;
	(*loc2z)(flag,&nclock,t,Blocks[kf-1].xd,Blocks[kf-1].x,&Blocks[kf-1].nx,
		 Blocks[kf-1].z,&Blocks[kf-1].nz,
		 Blocks[kf-1].evout,&Blocks[kf-1].nevout,Blocks[kf-1].rpar,&Blocks[kf-1].nrpar,
		 Blocks[kf-1].ipar,&Blocks[kf-1].nipar,Blocks[kf-1].inptr,
		 Blocks[kf-1].insz,&Blocks[kf-1].nin,
		 Blocks[kf-1].outptr,Blocks[kf-1].outsz,&Blocks[kf-1].nout,
		 Blocks[kf-1].g,&Blocks[kf-1].ng);
      }
    }
    break;
  case 10001 :			
    /* implicit block one entry for each input or output */
      for (in = 0 ; in < Blocks[kf-1].nin ; in++) 
	{
	  args[in]=Blocks[kf-1].inptr[in];
	  sz[in]=Blocks[kf-1].insz[in];
	}
    for (out=0;out<Blocks[kf-1].nout;out++) {
      args[in+out]=Blocks[kf-1].outptr[out];
      sz[in+out]=Blocks[kf-1].outsz[out];
    }
    if(Blocks[kf-1].ztyp>0){
      Blocks[kf-1].g=&g[zcptr[kf]-1];
      args[Blocks[kf-1].nin+Blocks[kf-1].nout]=Blocks[kf-1].g;
      sz[Blocks[kf-1].nin+Blocks[kf-1].nout]=Blocks[kf-1].ng;
    }
    loci1 = (ScicosFi) loc;

    (*loci1)(flag,&nclock,t,Blocks[kf-1].res,Blocks[kf-1].xd,Blocks[kf-1].x,
	     &Blocks[kf-1].nx,Blocks[kf-1].z,&Blocks[kf-1].nz,
	     Blocks[kf-1].evout,&Blocks[kf-1].nevout,Blocks[kf-1].rpar,&Blocks[kf-1].nrpar,
	     Blocks[kf-1].ipar,&Blocks[kf-1].nipar,
	     (double *)args[0],&sz[0],
	     (double *)args[1],&sz[1],(double *)args[2],&sz[2],
	     (double *)args[3],&sz[3],(double *)args[4],&sz[4],
	     (double *)args[5],&sz[5],(double *)args[6],&sz[6],
	     (double *)args[7],&sz[7],(double *)args[8],&sz[8],
	     (double *)args[9],&sz[9],(double *)args[10],&sz[10],
	     (double *)args[11],&sz[11],(double *)args[12],&sz[12],
	     (double *)args[13],&sz[13],(double *)args[14],&sz[14],
	     (double *)args[15],&sz[15],(double *)args[16],&sz[16],
	     (double *)args[17],&sz[17]); 
    break; 
  case 10002 :			
    /* implicit block, inputs and outputs given by a table of pointers */
   
    if(Blocks[kf-1].ztyp==0) {
      loci2 = (ScicosFi2) loc;
      
      (*loci2)(flag,&nclock,t,Blocks[kf-1].res,
	       Blocks[kf-1].xd,Blocks[kf-1].x,&Blocks[kf-1].nx,
	       Blocks[kf-1].z,&Blocks[kf-1].nz,
	       Blocks[kf-1].evout,&Blocks[kf-1].nevout,Blocks[kf-1].rpar,&Blocks[kf-1].nrpar,
	       Blocks[kf-1].ipar,&Blocks[kf-1].nipar,Blocks[kf-1].inptr,
	       Blocks[kf-1].insz,&Blocks[kf-1].nin,
	       Blocks[kf-1].outptr,Blocks[kf-1].outsz,&Blocks[kf-1].nout);
    }
    else {
      loci2z = (ScicosFi2z) loc;
      
      (*loci2z)(flag,&nclock,t,Blocks[kf-1].res,
		Blocks[kf-1].xd,Blocks[kf-1].x,&Blocks[kf-1].nx,
		Blocks[kf-1].z,&Blocks[kf-1].nz,
		Blocks[kf-1].evout,&Blocks[kf-1].nevout,Blocks[kf-1].rpar,&Blocks[kf-1].nrpar,
		Blocks[kf-1].ipar,&Blocks[kf-1].nipar,Blocks[kf-1].inptr,Blocks[kf-1].insz,&Blocks[kf-1].nin,
		Blocks[kf-1].outptr,Blocks[kf-1].outsz,&Blocks[kf-1].nout,
		Blocks[kf-1].g,&Blocks[kf-1].ng);
    }
    break;  
  default:
    sciprint("Undefined Function type\r\n");
    *flag=-1000;
    return;
  }
  if(solver==100 && Blocks[kf-1].type<10000 && *flag==0) { /* Implicit Solver */

    if(flagi!=7) {
      for (k=0;k<Blocks[kf-1].nx;k++) {
	Blocks[kf-1].res[k]=Blocks[kf-1].res[k]-Blocks[kf-1].xd[k];
      }
    }
    else {
      for (k=0;k<Blocks[kf-1].nx;k++) {
	Blocks[kf-1].xd[k]=Blocks[kf-1].res[k];
      } 
    }
  }
  for(in=0;in<Blocks[kf-1].nevout;++in){
    Blocks[kf-1].evout[in]=Blocks[kf-1].evout[in]-*t;
  }
  if ( cosd > 1){
    sciprint("block %d is called ",kf);
    sciprint("with flag %d ",*flag);
    sciprint("at time %f \r\n",*t);
    if(debug_block>-1){
      sciprint("Leaving the block \r\n");
      call_debug_scicos(t,xtd,xt,residual,g,flag,kf,flagi,debug_block);
    }
  }
}



void call_debug_scicos(double *t, double *xtd, double *xt, double *residual, double *g, int *flag, int kf, int flagi, int deb_blk)
{
  voidf loc ; 
  int solver=C2F(cmsolver).solver,k;
  ScicosF4 loc4;

  C2F(scsptr).ptr=Blocks[deb_blk].scsptr;
  loc=Blocks[deb_blk].funpt;
  scicos_time=*t;
  Blocks[kf-1].nevprt=nclock;
  loc4 = (ScicosF4) loc;
  if(Blocks[kf-1].ng>0){
    Blocks[kf-1].g=&g[zcptr[kf]-1];
  }
  if(Blocks[kf-1].nx==0){
    (*loc4)(&Blocks[kf-1],*flag);
  } 
  else {
    Blocks[kf-1].x=&xt[xptr[kf]-1];
    if(*flag==0 && solver==100) {
      Blocks[kf-1].res=&residual[xptr[kf]-1];
      Blocks[kf-1].xd=&residual[xptr[kf]-1];
      (*loc4)(&Blocks[kf-1],*flag);
      Blocks[kf-1].xd=&xtd[xptr[kf]-1];
      if(flagi!=7) {
	for (k=0;k<Blocks[kf-1].nx;k++) {
	  Blocks[kf-1].res[k]=Blocks[kf-1].res[k]-Blocks[kf-1].xd[k];
	}
      }
      else {
	for (k=0;k<Blocks[kf-1].nx;k++) {
	  Blocks[kf-1].xd[k]=Blocks[kf-1].res[k];
	} 
      }
    }
    else {
      Blocks[kf-1].xd=&xtd[xptr[kf]-1];
      (*loc4)(&Blocks[kf-1],*flag);
    }
  }
}
  


int C2F(funnum)(fname)
     char * fname;
{
  int i=0,ln;
  int loc=-1;
  while ( tabsim[i].name != (char *) NULL) {
    if ( strcmp(fname,tabsim[i].name) == 0 ) return(i+1);
    i++;
  }
  ln=strlen(fname);
  C2F(iislink)(fname,&loc);C2F(iislink)(fname,&loc);
  if (loc >= 0) return(ntabsim+(int)loc+1);
  return(0);
}


/* 
   !purpose 
   compute state derivative of the continuous part
   !calling sequence 
   neq   : int the size of the  continuous state
   t     : current time 
   xc    : double precision vector whose  contains the continuous state. 
   xcdot : double precision vector, contain the computed derivative 
   of the state 
*/

int C2F(simblk)(neq1, t, xc, xcdot)
     int *neq1;
     double *t, *xc, *xcdot;
{ 
  C2F(dset)(neq, &c_b14,xcdot , &c__1);
  C2F(ierode).iero = 0;
  *ierr= 0;
  odoit(xcdot, xc,xcdot,t);
  C2F(ierode).iero = *ierr;
  return 0;
}

     
/* 
   !purpose 
   compute residual  of the continuous part
   !calling sequence 
   t     : current time 
   xc    : double precision vector whose  contains the continuous state. 
   xcdot : double precision vector, contain the computed derivative 
   of the state 
*/
 
int C2F(simblkdaskr)(t,xc,xcdot,cj,residual,ires,rpar1,ipar1)
     int *ires,*ipar1;
     double *t, *xc, *xcdot, *rpar1, *residual;
     int *cj;
{ 
  C2F(dcopy)(neq, xcdot, &c__1, residual, &c__1);
  *ires=0;
  *ierr= 0;
  C2F(ierode).iero = 0;
  odoit(residual, xc, xcdot,t);
  C2F(ierode).iero = *ierr;
  if(C2F(ierode).iero != 0) *ires=-1;
  return 0;
}
 

int C2F(grblkdaskr)(neq1, t, xc, xtd,ng1, g,rpar1,ipar1)
     int *neq1;
     double *t, *xc, *xtd;
     int *ng1,*ipar1;
     double *g,*rpar1;
{
  *ierr= 0;
  C2F(ierode).iero = 0;
  zdoit(g, xtd, xc,t);
  C2F(ierode).iero = *ierr;
  return 0;
}

     
     
/*
  !purpose 
  interface to grbl1 at the lsodar format 
  !calling sequence 
  neq   : int  the size of the continuous state
  t     : current time 
  xc    : double precision vector contains the continuous state
  g     : computed zero crossing surface (see lsodar) 
  !
*/

int C2F(grblk)(neq1, t, xc, ng1, g)
     int *neq1;
     double *t, *xc;
     int *ng1;
     double *g;
{ 
 C2F(ierode).iero = 0;
 *ierr= 0;
 zdoit(g,xc, xc,t);
 C2F(ierode).iero = *ierr;
 return 0;
}


void addevs(double t, int *evtnb, int *ierr1)
{
  static int i, j;
  *ierr1 = 0;
  if (evtspt[*evtnb] != -1) {
    *ierr1 = 1;
    return;
  } else {
    evtspt[*evtnb] = 0;
    tevts[*evtnb] = t;
  }
  if (*pointi == 0) {
    *pointi = *evtnb;
    return;
  }
  if (t < tevts[*pointi]) {
    evtspt[*evtnb] = *pointi;
    *pointi = *evtnb;
    return;
  }
  i = *pointi;

 L100:
  if (evtspt[i] == 0) {
    evtspt[i] = *evtnb;
    return;
  }
  if (t >= tevts[evtspt[i]]) {
    j = evtspt[i];
    if (evtspt[j] == 0) {
      evtspt[j] = *evtnb;
      return;
    }
    i = j;
    goto L100;
  } else {
    evtspt[*evtnb] = evtspt[i];
    evtspt[i] = *evtnb;
  }
} 


void putevs(double *t, int *evtnb, int *ierr1)
{
  *ierr1 = 0;
  if (evtspt[*evtnb] != -1) {
    *ierr1 = 1;
    return;
  } else {
    evtspt[*evtnb] = 0;
    tevts[*evtnb] = *t;
  }
  if (*pointi == 0) {
    *pointi = *evtnb;
    return;
  }
  evtspt[*evtnb] = *pointi;
  *pointi = *evtnb;
} 



void free_blocks(void)
{
  int kf;
  for (kf = 0; kf < nblk; ++kf) {
    if (Blocks[kf].insz!=NULL) {
      free(Blocks[kf].insz);
    }else {
      break;
    }
    if (Blocks[kf].inptr!=NULL){
      free(Blocks[kf].inptr);
    }else {
      break;
    }
    if (Blocks[kf].outsz!=NULL){
      free(Blocks[kf].outsz);
    }else {
      break;
    }
    if (Blocks[kf].outptr!=NULL){
      free(Blocks[kf].outptr);
    }else {
      break;
    }
    if (Blocks[kf].label!=NULL){
      free(Blocks[kf].label);
    }else {
      break;
    }
    if (Blocks[kf].evout!=NULL){
      free(Blocks[kf].evout);
    }else {
      break;
    }
  }
  free(Blocks);
  if(nmod>0){
    free(mod);
  }
  return;
}
  
/* work space W needs to be ng+*neq*2 */

int setmode(double *W, double *x, double *told, int *jroot, double ttol)
{
  int k,j,jj,diff;
  double ttmp;

  ttmp=*told+ttol;
  zdoit(W,x,x,told);  /*fix the mode*/
  if (*ierr != 0) return 1;
  for(jj=0;jj<*neq;++jj){
    W[jj]=x[jj];
  } 
  diff=1;
  k=0;
  while (diff!=0){
    /*save modes */
    for(jj=0;jj<nmod;++jj){ 
      jroot[jj]=mod[jj];
    }
    for(j=0;j<=*neq;++j){
      C2F(simblk)(neq, &ttmp, W, &W[*neq]);  
      if (*ierr != 0) return 1;
      for(jj=0;jj<*neq;++jj){
	W[jj]=x[jj]+ttol*W[jj+(*neq)];
      } 
    }
    /*recompute modes*/
    zdoit(&W[2*(*neq)],W,W,&ttmp);
    if (*ierr != 0) return 1;
    /*test against saved modes*/
    diff=0;
    for(jj=0;jj<nmod;++jj){ 
      if (jroot[jj]!=mod[jj]) {
	if(k>*neq) {
	  *ierr=22;
	  return 1;
	}
	k=k+1;
	diff=1;
	break;
      }
    }
  }  
  return 0;
}

int get_phase_simulation(void)
{
  return phase;
}

void do_cold_restart(void)
{
  hot=0;
  return;
}

double get_scicos_time(void)
{
  return scicos_time;
}

int get_block_number(void)
{
  return C2F(curblk).kfun;
}

void set_block_error(int err)

{
  *block_error=err;
  return;
}

void set_pointer_xproperty(int* pointer)
{
  int i;
  for (i=0;i<n_pointer_xproperty;i++){
    pointer_xproperty[i]=pointer[i];
  }
  return;
}

