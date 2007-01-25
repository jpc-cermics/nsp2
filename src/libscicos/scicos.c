#include <stdlib.h> 
#include <string.h>

#include "nsp/machine.h"
#include "../functions/link.h"
#include "nsp/graphics/Graphics.h" 
#include "nsp/object.h" 
#include "nsp/blas.h" 
#include "nsp/matutil.h" 
#include "nsp/system.h" 


#include "scicos/scicos.h"
#define TABSIM /* to force include of tabsim definition */
#include "scicos/blocks.h"


/* to be moved elsewhere */
void Set_Jacobian_flag(int flag);
double Get_Jacobian_parameter(void);
double Get_Scicos_SQUR(void);
void call_debug_scicos(double *, double *, double *, double *,double *,int *,int,int,int);

extern  int C2F(ddaskr)();
extern  int C2F(lsodar2)();

/*
 * used to share errors with ODE solvers. 
 */

extern struct {
  int iero;
} C2F(ierode);


static void cosini(double *);
static void idoit(double *);
static void cosend(double *);
static void cdoit(double *);
static void doit(double *);
static void ddoit(double *);
static void edoit(double *,int *);
static void odoit(double *,double *,double *,double *);
static void ozdoit(double *,double *,double *,int *);
static void zdoit(double *,double *,double *,double *);
static void reinitdoit(double *,int *);
static void cossimdaskr(double *);
static void cossim(double *);
static void callf(double *, double *, double *, double *,double *,int *);
static int simblk(int *, double *, double *, double *);
static int simblkdaskr(double *, double *, double *, int *, double *, int *, double *, int *);
static int grblk(int *, double *, double *, int *, double *);
static int grblkdaskr(int *, double *, double *, double *, int *, double *, double *, int *);
static void addevs(double ,int *,int *);
static int putevs(double ,int );
/* static int setmode(double *,double *,double *,int *,double); */
/* Jacobian*/
static void Jdoit(double *,double *,double *,double *,int *);
static int Jacobian(double *, double *, double *, double *, double *, double *, int *);
static void Multp(double *, double *,double *, int, int, int ,int);

/* Table of constant values */

static int *ierr;
static double *xd;
static double *t0,*tf,scicos_time;
static int phase;
int *pointer_xproperty;
int n_pointer_xproperty;
static int *block_error=NULL;

/* Jacobian*/
static int Jacobian_Flag;
static double  CJJ;
static double SQuround;
/* Jacobian*/

/* main object */

scicos_run *Scicos = NULL;
int scicos_debug_level=-1;

int scicos_main( scicos_run *sr, double *t0_in, double *tf_in, double *simpar, int *flag__, int *ierr_out)
{
  int kf, mxtb, ierr0, kfun0, i, j, k, ni, no;
  double *W;

  Scicos =sr;
  Scicos->params.debug = scicos_debug_level;
  t0=t0_in;
  tf=tf_in;
  ierr=ierr_out;
  /* Function Body */
  Scicos->params.Atol = simpar[0];
  Scicos->params.rtol = simpar[1];
  Scicos->params.ttol = simpar[2];
  Scicos->params.deltat = simpar[3];
  Scicos->params.scale = simpar[4];
  Scicos->params.solver = (int) simpar[5];
  Scicos->params.hmax=simpar[6];
  Scicos->params.debug_counter = 0;
  *ierr = 0;

  xd=&Scicos->state.x[Scicos->sim.xptr[Scicos->sim.nblk]-1];
  Scicos->params.neq=&Scicos->sim.nx;

  for (i = 1; i <= Scicos->sim.nblk; ++i) {
    if (Scicos->sim.funtyp[-1+i] < 10000) {
      Scicos->sim.funtyp[-1+i] %= 1000;
    } else {
      Scicos->sim.funtyp[-1+i] = Scicos->sim.funtyp[-1+i] % 1000 + 10000;
    }
    ni = Scicos->sim.inpptr[i] - Scicos->sim.inpptr[-1+i];
    no = Scicos->sim.outptr[i] - Scicos->sim.outptr[-1+i];
    if (Scicos->sim.funtyp[-1+i] == 1) {
      if (ni + no > 11) {
	/*     hard coded maxsize in callf.c */
	Scierror("Too many input/output ports for hilited block\n");
	Scicos->params.curblk = i;
	*ierr = i + 1005;
	return 0;
      }
    } else if (Scicos->sim.funtyp[-1+i] == 2 || Scicos->sim.funtyp[-1+i] == 3) {
      /*     hard coded maxsize in scicos.h */
      if (ni + no > SZ_SIZE) {
	Scierror("Too many input/output ports for hilited block\n");
	Scicos->params.curblk = i;
	*ierr = i + 1005;
	return 0;
      }
    }
    mxtb = 0;
    if (Scicos->sim.funtyp[-1+i] == 0) {
      if (ni > 1) {
	for (j = 1; j <= ni; ++j) {
	  k = Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+i] - 1 + j];
	  mxtb = mxtb + Scicos->sim.lnkptr[k] - Scicos->sim.lnkptr[-1+k];
	}
      }
      if (no > 1) {
	for (j = 1; j <= no; ++j) {
	  k = Scicos->sim.outlnk[-1+Scicos->sim.outptr[-1+i] - 1 + j];
	  mxtb = mxtb + Scicos->sim.lnkptr[k] - Scicos->sim.lnkptr[-1+k];
	}
      }
      if (mxtb > TB_SIZE) {
	Scierror("Too many input/output entries for hilited block\n");

	Scicos->params.curblk = i;
	*ierr = i + 1005;
	return 0;
      }
    }
  }

  if (*flag__ == 1) {
    /*     initialisation des blocks */
    for (kf = 0; kf < Scicos->sim.nblk; ++kf) {
      *(Scicos->Blocks[kf].work)=NULL;
    }
    cosini(t0);
    if (*ierr != 0) {
      ierr0=*ierr;
      kfun0 = Scicos->params.curblk;
      cosend(t0);
      *ierr=ierr0;
      Scicos->params.curblk = kfun0;
    }
  } else if (*flag__ == 2) {
    /*     integration */
    if (Scicos->params.solver == 0) {
      cossim(t0);
    } else if (Scicos->params.solver == 100) {
      cossimdaskr(t0);
    } else {
      /*     add a warning message please */
    }
    if (*ierr != 0) {
      ierr0=*ierr;
      kfun0 = Scicos->params.curblk;
      cosend(t0);
      *ierr=ierr0;
      Scicos->params.curblk = kfun0;
    }

  } else if (*flag__ == 3) {
    /*     fermeture des blocks */
    cosend(t0);
  } else if (*flag__ == 4) {
    idoit(t0);
    if (*ierr == 0) {
      if((W=malloc(sizeof(double)*Scicos->sim.nx))== NULL ){
	*ierr =5;
	return 0;
      }
    
      simblk(&Scicos->sim.nx, t0, Scicos->state.x, W);
      for (i = 0; i < Scicos->sim.nx; ++i) {
	Scicos->state.x[i] = W[i];
      }
      FREE(W);
    }
  }
  return 0;
} 



static void cosini(double *told)
{
  double c_b14 = 0.0;
  int c__1 = 1;
  static int flag__;
  static int i;

  static int kfune;
  static int jj;

  double *W;
  jj=Max(Scicos->sim.ng,Scicos->state.nout);
  if((W=malloc(sizeof(double)*(jj)))== NULL ){
    *ierr =10000;
    return;
  }
  /* Jacobian*/
  Jacobian_Flag=0;
  /* Jacobian*/

  /* Function Body */
  *ierr = 0;
  /*     initialization (flag 4) */
  /*     loop on blocks */

  nsp_dset(&jj, &c_b14, W, &c__1);
  Scicos->params.nclock = 0;
  for (Scicos->params.curblk = 1; Scicos->params.curblk <= Scicos->sim.nblk; ++Scicos->params.curblk) {
    if (Scicos->sim.funtyp[-1+Scicos->params.curblk] >= 0) {
      flag__ = 4;
      callf(told, xd, Scicos->state.x, Scicos->state.x,W,&flag__);
      if (flag__ < 0 && *ierr == 0) {
	*ierr = 5 - flag__;
	kfune = Scicos->params.curblk;
      }
    }
  }
  if (*ierr != 0) {
    Scicos->params.curblk = kfune;
    FREE(W);
    return;
  }
  /*     initialization (flag 6) */
  Scicos->params.nclock = 0;

  for (jj = 1; jj <= Scicos->sim.ncord; ++jj) {
    Scicos->params.curblk = Scicos->sim.cord[-1+jj];
    flag__ = 6;
    if (Scicos->sim.funtyp[-1+Scicos->params.curblk] >= 0) {
      callf(told, xd, Scicos->state.x, Scicos->state.x,W,&flag__);
      if (flag__ < 0) {
	*ierr = 5 - flag__;
	FREE(W);
	return;
      }
    }
  }
  /*     point-fix iterations */
  Scicos->params.nclock =0;
  for (i = 1; i <= Scicos->sim.nblk + 1; ++i) {
    /*     loop on blocks */
    for (Scicos->params.curblk = 1; Scicos->params.curblk <= Scicos->sim.nblk; ++Scicos->params.curblk) {
      flag__ = 6;
      if (Scicos->sim.funtyp[-1+Scicos->params.curblk] >= 0) {
	callf(told, xd, Scicos->state.x, Scicos->state.x,W,&flag__);
	if (flag__ < 0) {
	  *ierr = 5 - flag__;
	  FREE(W);
	  return;
	}
      }
    }

    Scicos->params.nclock = 0;

    for (jj = 1; jj <= Scicos->sim.ncord; ++jj) {
      Scicos->params.curblk = Scicos->sim.cord[-1+jj];
      flag__ = 6;
      if (Scicos->sim.funtyp[-1+Scicos->params.curblk] >= 0) {
	callf(told, xd, Scicos->state.x, Scicos->state.x,W,&flag__);
	if (flag__ < 0) {
	  *ierr = 5 - flag__;
	  FREE(W);
	  return;
	}
      }
    }
    for (jj = 0; jj <= Scicos->state.nout-1; ++jj) {
      if (Scicos->state.outtb[jj] != W[jj]) {
	goto L30;
      }
    }
    FREE(W);
    return;
  L30:
    C2F(dcopy)(&Scicos->state.nout, Scicos->state.outtb, &c__1, W, &c__1);
  }
  *ierr = 20;
  FREE(W);
} 



static void idoit(double *told)
{
  int flag,  i=0,jj, ierr1, i2;

  for (jj = 1; jj <= Scicos->sim.niord; ++jj) 
    {
      Scicos->params.curblk =  Scicos->sim.iord[-1+jj];
      if (Scicos->sim.outptr[-1+Scicos->params.curblk + 1] - Scicos->sim.outptr[-1+Scicos->params.curblk] > 0) 
	{
	  Scicos->params.nclock = Scicos->sim.iord[-1+jj + Scicos->sim.niord];
	  flag = 1;
	  callf(told, xd, Scicos->state.x, Scicos->state.x,Scicos->state.x,&flag);
	  if (flag < 0) {
	    *ierr = 5 - flag;
	    return;
	  }
	}
      if (Scicos->Blocks[Scicos->params.curblk - 1].nevout > 0) 
	{
	  if (Scicos->sim.funtyp[-1+Scicos->params.curblk] < 0) 
	    {
	      if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -1) 
		{
		  if (Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]] <= 0.)
		    {
		      i=2;
		    } 
		  else 
		    {
		      i=1;
		    }
		} 
	      else if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -2) 
		{
		  i=Max(Min((int) Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]],
			    Scicos->Blocks[Scicos->params.curblk - 1].nevout),1);
		}
	      i2 =i+ Scicos->sim.clkptr[-1+Scicos->params.curblk] - 1;
	      ierr1= putevs(*told, i2);
	      if (ierr1 != 0) {	/* event conflict */
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



static void cossim(double *told)
{
  static int otimer = 0;
  int c__0 = 0, c__1 = 1, i3, flag__, jdum=0,  iopt, ierr1, j, k;
  int itask, jj, jt,  istate, ntimer;
  int inxsci, kpo, kev, *ihot,niwp,nrwp, *jroot,*zcros;
  double *W=NULL, *rhot, c_b14 = 0.0,  t, rhotmp;

  nrwp = (*Scicos->params.neq) * Max(16,*Scicos->params.neq + 9) + 22 + Scicos->sim.ng * 3;
  /* +1 below is so that rhot starts from 1; one wasted location */
  if((rhot=malloc(sizeof(double)*(nrwp+1)))== NULL ){
    *ierr =10000;
    return;
  }
  niwp = *Scicos->params.neq + 20 + Scicos->sim.ng;/* + ng is for change in lsodar2 to
				       handle masking */

  /* +1 below is so that ihot starts from 1; one wasted location */
  if((ihot=malloc(sizeof(int)*(niwp+1)))== NULL ){
    *ierr =10000;
    FREE(rhot);
    return;
  }
  if((jroot=malloc(sizeof(int)*Scicos->sim.ng))== NULL ){
    *ierr =10000;
    FREE(rhot);
    FREE(ihot);
    return;
  }
  if((zcros=malloc(sizeof(int)*Scicos->sim.ng))== NULL ){
    *ierr =10000;
    FREE(rhot);
    FREE(ihot);
    FREE(jroot);
    return;
  }
  if((W=malloc(sizeof(double)*Scicos->sim.ng))== NULL ){
    *ierr =10000;
    FREE(rhot);
    FREE(ihot);
    FREE(jroot);
    FREE(zcros);
    return;
  }
  
  /* Function Body */
  
  Scicos->params.halt = 0;
  *ierr = 0;
  
  inxsci = nsp_check_events_activated();

  /*     initialization */
  nsp_iset(&niwp, &c__0, &ihot[1], &c__1);
  nsp_dset(&nrwp, &c_b14, &rhot[1], &c__1);
  nsp_realtime_init(told, &Scicos->params.scale);
  
  phase=1;
  Scicos->params.hot = 0;
  itask = 4;

  jt = 2;

  jj = 0;
  for (Scicos->params.curblk = 1; Scicos->params.curblk <= Scicos->sim.nblk; ++Scicos->params.curblk) {
    if (Scicos->Blocks[Scicos->params.curblk-1].ng >= 1) {
      zcros[jj] = Scicos->params.curblk;
      ++jj;
    }
  }
  /*     . Il fault:  ng >= jj */
  if (jj != Scicos->sim.ng) {
    zcros[jj] = -1;
  }
  /*     initialisation (propagation of constant blocks outputs) */
  idoit(told);
  if (*ierr != 0) {
    goto freeall;
    return;
  }
  
  /*     main loop on time */
  
  while(*told < *tf) {
    if (inxsci == TRUE)
      {
	ntimer = nsp_stimer();
	if (ntimer != otimer) {
	  nsp_check_gtk_events();
	  otimer = ntimer;
	}
      }
    if (Scicos->params.halt != 0) {
      Scicos->params.halt = 0;
      goto freeall;
      return;
    }
    if (*Scicos->state.pointi == 0) {
      t = *tf;
    } else {
      t = Scicos->state.tevts[-1+*Scicos->state.pointi];
    }
    if (Abs(t - *told) < Scicos->params.ttol) {
      t = *told;
      /*     update output part */
    }
    if (*told > t) {
      /*     !  scheduling problem */
      *ierr = 1;
      goto freeall;
      return;
    }
    if (*told != t) {
      if (Scicos->sim.xptr[Scicos->sim.nblk] == 1) {
	/*     .     no continuous state */
	if (*told + Scicos->params.deltat + Scicos->params.ttol > t) {
	  *told = t;
	} else {
	  *told += Scicos->params.deltat;
	}
	/*     .     update outputs of 'c' type blocks with no continuous state */
	if (*told >= *tf) {
	  /*     .     we are at the end, update continuous part before leaving */
	  if ( Scicos->sim.ncord > 0) {
	    cdoit(told);
	    goto freeall;
	    return;
	  }
	}
      } else {
	/*     integrate */
	rhotmp = *tf + Scicos->params.ttol;
	kpo = *Scicos->state.pointi;
      L20:
	if (Scicos->sim.critev[-1+kpo] == 1) {
	  rhotmp = Scicos->state.tevts[-1+kpo];
	  goto L30;
	}
	kpo = Scicos->state.evtspt[kpo-1];
	if (kpo != 0) {
	  goto L20;
	}
      L30:
	if (rhotmp < rhot[1]) {
	  Scicos->params.hot = 0;
	}
	rhot[1] = rhotmp;
	t = Min(*told + Scicos->params.deltat,Min(t,*tf + Scicos->params.ttol));
	
	if (Scicos->sim.ng>0 &&  Scicos->params.hot == 0 && Scicos->sim.nmod>0) {
	  zdoit(W,Scicos->state.x,Scicos->state.x,told);
	  if (*ierr != 0){
	    goto freeall;
	    return;
	  }
	}
	
	istate = (Scicos->params.hot) ? 2 : 1 ;
	if (Scicos->params.debug >= 1) {
	  sciprint("****lsodar from: %f to %f hot= %d  \n", *told,t,Scicos->params.hot);
	}

	if(Scicos->params.hmax==0){
	  iopt = 0;
	}else{
	  int panj=5;
	  iopt=1;
	  nsp_iset(&panj, &c__0, &ihot[5], &c__1);
	  nsp_dset(&panj, &c_b14, &rhot[5], &c__1);
	  rhot[6]=Scicos->params.hmax;
	}
	phase=2;
	C2F(lsodar2)(simblk, Scicos->params.neq, Scicos->state.x, told, &t, &c__1, &Scicos->params.rtol, 
		     &Scicos->params.Atol, &itask, &istate, &iopt, &rhot[1], &
		     nrwp, &ihot[1], &niwp, &jdum, &jt, 
		     grblk, &Scicos->sim.ng, jroot);
	phase=1;
	if (*ierr > 5) {
	  /*     !           singularity in block */
	  goto freeall;
	  return;
	}
	if (istate <= 0) {
	  /* integration problem */
	  *ierr = 100 - istate;
	  goto freeall;
	  return;
	} else {
	  if (Scicos->params.debug >= 1) {
	    sciprint("****lsodar reached: %f\n",*told);
	  }
	  Scicos->params.hot = 1;
	}
	
	/*     .     update outputs of 'c' type  blocks if we are at the end*/
	if (*told >= *tf) {
	  if ( Scicos->sim.ncord > 0) {
	    cdoit(told);
	    goto freeall;
	    return;
	  }
	}
	if (istate == 4) Scicos->params.hot=0; /* new feature of lsodar, detects unmasking */
	if (istate == 3) {
	  /*     .        at a least one root has been found */
	  Scicos->params.hot = 0;
	  if (Scicos->params.debug >= 1) {
	    sciprint("root found at t=: %f\n",*told);
	  }
	  /*     .        update outputs affecting ztyp blocks ONLY FOR OLD BLOCKS */
	  zdoit(W, xd, Scicos->state.x,told);
	  if (*ierr != 0) {
	    goto freeall;
	    return;
	  }
	  for (jj = 0; jj < Scicos->sim.ng; ++jj) {
	    Scicos->params.curblk = zcros[ jj];
	    if (Scicos->params.curblk == -1) {
	      break; 
	    }
	    kev = 0;
	    
	    for (j = Scicos->sim.zcptr[-1+Scicos->params.curblk]-1 ; 
		 j <Scicos->sim.zcptr[-1+Scicos->params.curblk+1]-1 ; ++j) {
	      if(jroot[j]!=0){
		kev=1;
		break;
	      }
	    }
	    /*   */
	    if (kev != 0) {
	      Scicos->Blocks[Scicos->params.curblk-1].jroot=&jroot[Scicos->sim.zcptr[-1+Scicos->params.curblk]-1];
	      if (Scicos->sim.funtyp[-1+Scicos->params.curblk] > 0) {
		
		if (Scicos->Blocks[Scicos->params.curblk-1].nevout > 0) {
		  flag__ = 3;
		  /* call corresponding block to determine output event (kev) */
		  Scicos->params.nclock = -kev;
		  callf(told, xd, Scicos->state.x, Scicos->state.x,W,&flag__);
		  if (flag__ < 0) {
		    *ierr = 5 - flag__;
		    goto freeall;
		    return;
		  }
		  /*     .              update event agenda */
		  for (k = 0; k < Scicos->Blocks[Scicos->params.curblk-1].nevout; ++k) {
		    if (Scicos->Blocks[Scicos->params.curblk-1].evout[k] >= 0.) {
		      i3 = k + Scicos->sim.clkptr[-1+Scicos->params.curblk] ;
		      addevs(Scicos->Blocks[Scicos->params.curblk-1].evout[k]+(*told), &i3, &ierr1);
		      if (ierr1 != 0) {
			/*     .                       nevts too small */
			*ierr = 3;
			goto freeall;
			return;
		      }
		    } 
		  }
		}
		/*     .              update state */
		if (Scicos->Blocks[Scicos->params.curblk-1].nx > 0) {
		  /*     .              call corresponding block to update state */
		  flag__ = 2;
		  Scicos->params.nclock = -kev;
		  callf(told, xd, Scicos->state.x, Scicos->state.x,W,&flag__);
		  
		  if (flag__ < 0) {
		    *ierr = 5 - flag__;
		    goto freeall;
		    return;
		  }
		}
	      }
	    }
	  }
	}
      }
      nsp_realtime(told);
    } else {
      /*     .  t==told */   
      if (Scicos->params.debug >= 1) {
	sciprint("Event: %d activated at t=%f\n",*Scicos->state.pointi,*told);
	for(kev=0;kev<Scicos->sim.nblk;kev++){
	  if (Scicos->Blocks[kev].nmode>0){
	    sciprint("mode of block %d=%d, ",kev,Scicos->Blocks[kev].mode[0]);
	  }
	}
	sciprint("**mod**\n");
      }

      ddoit(told);
      if (Scicos->params.debug >= 1) {
	sciprint("End of activation\n");
      }
      if (*ierr != 0) {
	goto freeall;
	return;
      }

    }
    /*     end of main loop on time */
  }
 freeall: 
  FREE(rhot);	
  FREE(ihot);	
  FREE(jroot);	
  FREE(W);	
  FREE(zcros);
} 


static void cossimdaskr(double *told)
{
  static int otimer = 0;
  double c_b14 = 0.0, *rpardummy=NULL, t, rhotmp, *rhot, *W;
  int c__0 = 0, c__1=1;
  int i3,*ipardummy=NULL;
  int flag__, info[20], ierr1,  j, k,  jj, jt; 
  int istate, ntimer, inxsci;
  int kpo, kev;
  int *ihot,niwp,nrwp;
  int *jroot,*zcros;
  int maxord;
  int *scicos_xproperty;
  int *Mode_save;
  int Mode_change=0;
  /*-------------------- Analytical Jacobian memory allocation ----------*/
  int  Jn, Jnx, Jno, Jni, Jactaille;
  double uround;
  Jactaille=0;   
  if(Jacobian_Flag==1){
    Jn=*Scicos->params.neq;
    Jnx=Scicos->Blocks[Scicos->sim.nblk-1].nx;
    Jno=Scicos->Blocks[Scicos->sim.nblk-1].nout;
    Jni=Scicos->Blocks[Scicos->sim.nblk-1].nin;
    Jactaille= 2+3*Jn+(Jn+Jni)*(Jn+Jno)+Jnx*(Jni+2*Jn+Jno)+(Jn-Jnx)*(2*(Jn-Jnx)+Jno+Jni)+2*Jni*Jno;}
  /*----------------------------Jacobian----------------------------------*/
  maxord = 5;
  nrwp = Max(maxord + 4,7) * (*Scicos->params.neq) + 60 + (*Scicos->params.neq)*(*Scicos->params.neq) + Scicos->sim.ng * 3 + Jactaille;
  niwp = (*Scicos->params.neq) + 40 + (*Scicos->params.neq) +Scicos->sim.ng ; /* + ng is for change in ddaskr to handle masking */ 

   /* +1 below is so that rhot starts from 1; one wasted location */
  if((rhot=malloc(sizeof(double)*(nrwp+1)))== NULL ){
    *ierr =10000;
    return;
  }
  /* +1 below is so that ihot starts from 1; one wasted location */
  if((ihot=malloc(sizeof(int)*(niwp+1)))== NULL ){
    FREE(rhot);
    *ierr =10000;
    return;
  }
  if((jroot=malloc(sizeof(int)*Scicos->sim.ng))== NULL ){
    *ierr =10000;
    FREE(rhot);
    FREE(ihot);
    return;
  }
  if((scicos_xproperty=malloc(sizeof(int)*(*Scicos->params.neq)))== NULL ){
    *ierr =10000;
    FREE(rhot);
    FREE(ihot);
    FREE(jroot);
    return;
  }
  nsp_iset(Scicos->params.neq, &c__1, scicos_xproperty, &c__1);
  if((zcros=malloc(sizeof(int)*Scicos->sim.ng))== NULL ){
    *ierr =10000;
    FREE(rhot);
    FREE(ihot);
    FREE(jroot);
    FREE(scicos_xproperty);
    return;
  }
  if((W=malloc(sizeof(double)*Scicos->sim.ng))== NULL ){
    *ierr =10000;
    FREE(rhot);
    FREE(ihot);
    FREE(jroot);
    FREE(scicos_xproperty);
    FREE(zcros);
    return;
  }
  if((Mode_save=malloc(sizeof(double)*Scicos->sim.nmod))== NULL ){
    *ierr =10000;
    FREE(rhot);
    FREE(ihot);
    FREE(jroot);
    FREE(scicos_xproperty);
    FREE(zcros);
    FREE(W);
    return;
  }

  uround = 1.0;
  do{
    uround = uround*0.5;
  }while ( 1.0 + uround != 1.0);
  uround = uround*2.0;
  SQuround=sqrt(uround);

  /* Function Body */
  Scicos->params.halt = 0;
  *ierr = 0;
  /*     hot = .false. */
  phase=1;
  Scicos->params.hot = 0;

  jt = 2;

  /*      stuck=.false. */
  inxsci = nsp_check_events_activated();

  /*     initialization */
  nsp_iset(&niwp, &c__0, &ihot[1], &c__1);
  nsp_dset(&nrwp, &c_b14, &rhot[1], &c__1);
  nsp_realtime_init(told, &Scicos->params.scale);
  /*     ATOL and RTOL are scalars */
  info[1] = 0;
  info[2] = 0;
  info[3] = 1;
  /*don't go beyond stopping point TSTOP defined by RWORK(1)*/

  /*     derivatives automatically computed by numerical differences */
  info[4] = 0;
  /*     full jac matrx */
  info[5] = 0;

  if(Scicos->params.hmax==0){
    info[6] = 0;    /*  code determines maximaum step-size */
  }else{
    info[6] = 1;
    rhot[2]=Scicos->params.hmax;  /*  user defined maximaum step-size */
  } 


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
  for (Scicos->params.curblk = 1; Scicos->params.curblk <= Scicos->sim.nblk; ++Scicos->params.curblk) {
    if (Scicos->Blocks[Scicos->params.curblk-1].ng >= 1) {
      zcros[jj] = Scicos->params.curblk;
      ++jj;
    }
  }
  /*     . Il fault:  ng >= jj */
  if (jj != Scicos->sim.ng) {
    zcros[jj] = -1;
  }
  /*     initialisation (propagation of constant blocks outputs) */
  idoit(told);
  if (*ierr != 0) {
    goto freeallx;
    return;
  }
  /*     main loop on time */
  while (*told < *tf) {
    if (inxsci == TRUE) {
      ntimer = nsp_stimer();
      if (ntimer != otimer) {
	nsp_check_gtk_events();
	otimer = ntimer;
	/*     .     sxevents can modify halt */
      }
    }
    if (Scicos->params.halt != 0) {
      Scicos->params.halt = 0;
      goto freeallx;
      return;
    }
    if (*Scicos->state.pointi == 0) {
      t = *tf;
    } else {
      t = Scicos->state.tevts[-1+*Scicos->state.pointi];
    }
    if (Abs(t - *told) < Scicos->params.ttol) {
      t = *told;
      /*     update output part */
    }
    if (*told > t) {
      /*     !  scheduling problem */
      *ierr = 1;
      goto freeallx;
      return;
    }
    if (*told != t) {
      if (Scicos->sim.xptr[Scicos->sim.nblk] == 1) {
	/*     .     no continuous state */
	if (*told + Scicos->params.deltat + Scicos->params.ttol > t) {
	  *told = t;
	} else {
	  *told += Scicos->params.deltat;
	}
	/*     .     update outputs of 'c' type blocks with no continuous state */
	if (*told >= *tf) {
	  /*     .     we are at the end, update continuous part before leaving */
	  cdoit(told);
	  goto freeallx;
	  return;
	}
      } else {
	if (Scicos->params.hot == 0) {
	  reinitdoit(told,scicos_xproperty);
	  if(*ierr >0){
	    goto freeallx;
	    return;
	  }
	}      
	rhotmp = *tf + Scicos->params.ttol;
	kpo = *Scicos->state.pointi;
      L20:
	if (Scicos->sim.critev[-1+kpo] == 1) {
	  rhotmp = Scicos->state.tevts[-1+kpo];
	  goto L30;
	}
	kpo = Scicos->state.evtspt[kpo-1];
	if (kpo != 0) {
	  goto L20;
	}
      L30:
	if (rhotmp < rhot[1]) {
	  Scicos->params.hot = 0;/* Do cold-restat the solver:if the new TSTOP isn't beyong the previous one*/ 
	}
	rhot[1] = rhotmp;
	t = Min(*told + Scicos->params.deltat,Min(t,*tf + Scicos->params.ttol));

	if (Scicos->params.hot == 0){ /* CIC calculation when hot==0 */

	  if (Scicos->sim.ng>0&&Scicos->sim.nmod>0){
	    phase=1;
	    zdoit(W, xd, Scicos->state.x,told);
	    if (*ierr != 0) {
	      goto freeallx;
	      return;
	    }
	  }


	  for(j=0;j<=Scicos->sim.nmod;j++){/* counter to reevaluate the 
				  modes in  mode->CIC->mode->CIC-> loop 
				  do it once in the absence of mode (nmod=0)*/
	    /* updating the modes through Flag==9, Phase==1 */
	    
	    info[0]=0;  /* cold start */
	    info[10]=1; /* inconsistent IC */
	    info[13]=1; /* return after CIC calculation */
	    reinitdoit(told,scicos_xproperty);/* filling up the scicos_xproperties */
 	    if(*ierr >0){
	      goto freeallx;
	      return; 
	    }
	    for (jj = 1; jj <= *Scicos->params.neq; ++jj) {
	      ihot[jj + 40] = scicos_xproperty[jj-1];
	    }
	    phase=2;
	    /* Jacobian*/
	    info[4] =Jacobian_Flag; 
	    /*	    info[4] =0; */ /* numerical Jacobian */
	    C2F(ddaskr)(simblkdaskr, Scicos->params.neq, told, Scicos->state.x, xd, &t, info,
			&Scicos->params.rtol, 
			&Scicos->params.Atol, &istate, &rhot[1],&nrwp, &ihot[1], &niwp,
			rpardummy, ipardummy, Jacobian, rpardummy, 
			grblkdaskr, &Scicos->sim.ng, jroot);

	    if (*ierr > 5) {
	      goto freeallx;
	      return;
	    }
	    if (Scicos->params.debug >= 1) {
	      if (istate==4) {
		sciprint("**** daskr succesfully initialized *****/r/n" );
	      }
	      else{
		sciprint("**** daskr failed to initialize ->try again *****/r/n" );
	      }
	    }
	    /*-------------------------------------*/
	    /* saving the previous modes*/
	    for (jj = 0; jj < Scicos->sim.nmod; ++jj) {
	      Mode_save[jj] = Scicos->sim.mod[jj];
	    }
	    if (Scicos->sim.ng>0&&Scicos->sim.nmod>0){	 
	      phase=1;
	      zdoit(W, xd, Scicos->state.x,told);
	      if (*ierr != 0) {
		goto freeallx;
		return; 
	      }
	    }
	    /*------------------------------------*/
	    Mode_change=0;
	    for (jj = 0; jj < Scicos->sim.nmod; ++jj) {
	      if(Mode_save[jj] != Scicos->sim.mod[jj])
		{Mode_change=1;
 		break;
		}
	    }
	    if ( Mode_change==0)      break;
	  }/* mode-CIC  counter*/
	  if(Mode_change==1){
	    *ierr = 100 - (-17);
	    goto freeallx;
	    return;
	  }
	  info[0]=0;  /* cold restart */
	  info[10]=1; /* to reevaluate CIC when info[0]==0*/
	  info[13]=0; /* continue after CIC calculation */
	} /* CIC calculation when hot==0 */

	info[0]=Scicos->params.hot;  
	/*     Warning rpar and ipar are used here as dummy pointers */
	phase=2;
	if (Scicos->params.debug >= 1) {
	  sciprint("****daskr from: %f to %f hot= %d  \n", *told,t,Scicos->params.hot);
	}
	C2F(ddaskr)(simblkdaskr, Scicos->params.neq, told, Scicos->state.x, xd, &t, 
		    info, &Scicos->params.rtol, &Scicos->params.Atol, &istate, &rhot[1], &
		    nrwp, &ihot[1], &niwp, rpardummy, ipardummy
		    ,Jacobian, rpardummy, grblkdaskr, &Scicos->sim.ng, jroot);

	if (istate == -1)
	  sciprint("**** Stiffness at: %26.18f %d\n",*told,istate);

	phase=1;
	if (*ierr > 5) {
	  goto freeallx; /* singularity in block */
	  return;
	}

	if (istate <= -2) { /* in case istate==-1 : continue the integration*/
	  *ierr = 100 - istate;
	  goto freeallx;/* singularity in block */
	  return;
	} else {
	  if (Scicos->params.debug >= 1) {
	    sciprint("****daskr reached: %f\n",*told);
	  }
	  Scicos->params.hot = 1;/* successful return from DDASKR => hot restart*/
	}
	
	/*     update outputs of 'c' type  blocks if we are at the end*/
	if (*told >= *tf) {
	  cdoit(told);
	  goto freeallx;
	  return;
	}
	if (istate == 6) Scicos->params.hot=0; /* new feature of daskr, detects unmasking */
	if (istate == 5) {
	  /*     .        at a least one root has been found */
	  Scicos->params.hot = 0;
	  if (Scicos->params.debug >= 1) {
	    sciprint("root found at t=: %f\n",*told);
	  }
	  /*     .        update outputs affecting ztyp blocks  ONLY FOR OLD BLOCKS*/
	  zdoit(W, xd, Scicos->state.x,told);
	  if (*ierr != 0) {
	    goto freeallx;
	    return;
	  }
	  for (jj = 0; jj < Scicos->sim.ng; ++jj) {
	    Scicos->params.curblk = zcros[jj];
	    if (Scicos->params.curblk == -1) {
	      break; 
	    }
	    kev = 0;
	    for (j = Scicos->sim.zcptr[-1+Scicos->params.curblk]-1 ; 
		 j <Scicos->sim.zcptr[-1+Scicos->params.curblk+1]-1 ; ++j) {
	      if(jroot[j]!=0){
		kev=1;
		break;
	      }
	    }
	    if (kev != 0) {
	      Scicos->Blocks[Scicos->params.curblk-1].jroot=&jroot[Scicos->sim.zcptr[-1+Scicos->params.curblk]-1];
	      if (Scicos->sim.funtyp[-1+Scicos->params.curblk] > 0) {
		if (Scicos->Blocks[Scicos->params.curblk-1].nevout > 0) {
		  flag__ = 3;
		  /*     call corresponding block to determine output event (kev) */
		  Scicos->params.nclock = -kev;
		  callf(told, xd, Scicos->state.x, Scicos->state.x,W,&flag__);
		  if (flag__ < 0) {
		    *ierr = 5 - flag__;
		    goto freeallx;
		    return;
		  }
		  /*     update event agenda */
		  for (k = 0; k < Scicos->Blocks[Scicos->params.curblk-1].nevout; ++k) {
		    if (Scicos->Blocks[Scicos->params.curblk-1].evout[k] >= 0) {
		      i3 = k + Scicos->sim.clkptr[-1+Scicos->params.curblk] ;
		      addevs(Scicos->Blocks[Scicos->params.curblk-1].evout[k]+(*told), &i3, &ierr1);
		      if (ierr1 != 0) {
			/*     .                       nevts too small */
			*ierr = 3;
			goto freeallx;
			return;
		      }
		    }
		  }
		}
		/*     .              update state */
		if (Scicos->Blocks[Scicos->params.curblk-1].nx > 0) {
		  /*     .call corresponding block to update state */
		  flag__ = 2;
		  Scicos->params.nclock = -kev;
		  pointer_xproperty=
		    &scicos_xproperty[-1+Scicos->sim.xptr[Scicos->params.curblk-1]];
		  n_pointer_xproperty=Scicos->Blocks[Scicos->params.curblk-1].nx;
		  callf(told, xd, Scicos->state.x, Scicos->state.x,W,&flag__);
		  if (flag__ < 0) {
		    *ierr = 5 - flag__;
		    goto freeallx;
		    return;
		  }
		}
	      }
	    }
	  }
	}
	if (inxsci == 1) {
	  ntimer = nsp_stimer();
	  if (ntimer != otimer) {
	    nsp_check_gtk_events();
	    otimer = ntimer;
	    /*     .     sxevents can modify halt */
	  }
	}
	if (Scicos->params.halt != 0) {
	  Scicos->params.halt = 0;
	  goto freeallx;
	  return;
	}
	  /* if(*Scicos->state.pointi!=0){
	    t=Scicos->state.tevts[-1+*Scicos->state.pointi];
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
      nsp_realtime(told);
    } else {
      /*     .  t==told */
      if (Scicos->params.debug >= 1) {
	sciprint("Event: %d activated at t=%f\n",*Scicos->state.pointi,*told);
      }
      
      ddoit(told);
      if (Scicos->params.debug >= 1) {
	sciprint("End of activation");
      }
      if (*ierr != 0) {
	goto freeallx;
	return;
      }
    }
    /*     end of main loop on time */
  }

 freeallx: 
  FREE(rhot);
  FREE(ihot);
  FREE(jroot);
  FREE(W);	
  FREE(zcros);	
  FREE(scicos_xproperty);
  FREE(Mode_save);
}



static void cosend(double *told)
{
  int flag, kfune=0;
  *ierr = 0;
  Scicos->params.nclock=0;
  for (Scicos->params.curblk = 1; Scicos->params.curblk <= Scicos->sim.nblk; ++Scicos->params.curblk) {
    flag= 5;
    if (Scicos->sim.funtyp[-1+Scicos->params.curblk] >= 0) {
      callf(told, xd, Scicos->state.x,  Scicos->state.x,Scicos->state.x,&flag);
      if (flag < 0 && *ierr == 0) {
	*ierr = 5 - flag;
	kfune = Scicos->params.curblk;
      }
    }
  }
  if (*ierr != 0) {
    Scicos->params.curblk = kfune;
    return;
  }
} 


static void doit(double *told)
{
  int i=0,i2, flag__, nord, ierr1, ii, kever;

  kever = *Scicos->state.pointi;
  *Scicos->state.pointi = Scicos->state.evtspt[kever-1];
  Scicos->state.evtspt[kever-1] = -1;

  nord = Scicos->sim.ordptr[kever] - Scicos->sim.ordptr[-1+kever];
  if (nord == 0) return;

  for (ii = Scicos->sim.ordptr[-1+kever]; ii <=Scicos->sim.ordptr[kever] - 1 ; ++ii) 
    {
      Scicos->params.curblk = Scicos->sim.ordclk[-1+ii];
      if (Scicos->sim.outptr[-1+Scicos->params.curblk + 1] - Scicos->sim.outptr[-1+Scicos->params.curblk] > 0) 
	{
	  Scicos->params.nclock = Abs(Scicos->sim.ordclk[-1+ii + Scicos->sim.nordclk]);
	  flag__ = 1;
	  callf(told, xd, Scicos->state.x, Scicos->state.x,Scicos->state.x,&flag__);

	  if (flag__ < 0) 
	    {
	      *ierr = 5 - flag__;
	      return;
	    }
	}
      /*  Initialize tvec */
      if (Scicos->Blocks[Scicos->params.curblk - 1].nevout > 0) 
	{
	  if (Scicos->sim.funtyp[-1+Scicos->params.curblk] < 0) 
	    {
	      if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -1) 
		{
		  if (Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]] <= 0.) 
		    {
		      i= 2;
		    } 
		  else 
		    {
		      i= 1;
		    }
		} 
	      else if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -2) 
		{
		  i=Max(Min((int) Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]],
			    Scicos->Blocks[Scicos->params.curblk - 1].nevout),1);
		}
	      i2 = i + Scicos->sim.clkptr[-1+Scicos->params.curblk] - 1;
	      ierr1= putevs(*told, i2);
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



static void cdoit(double *told)
{
  int i2;
  static int flag__;
  static int ierr1;
  static int i,jj;
  /* Function Body */
  for (jj = 1; jj <= Scicos->sim.ncord; ++jj) {
    Scicos->params.curblk = Scicos->sim.cord[-1+jj];
    Scicos->params.nclock = Scicos->sim.cord[-1+jj + Scicos->sim.ncord];
    if (Scicos->sim.outptr[-1+Scicos->params.curblk + 1] - Scicos->sim.outptr[-1+Scicos->params.curblk] > 0) {
      flag__ = 1;
      callf(told, xd, Scicos->state.x, Scicos->state.x,Scicos->state.x,&flag__);
	    
      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }

    if (Scicos->Blocks[Scicos->params.curblk - 1].nevout > 0) {
      if (Scicos->sim.funtyp[-1+Scicos->params.curblk] < 0) {

	if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -1) {
	  if (Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]] <= 0.) {
	    i = 2;
	  } else {
	    i = 1;
	  }
	} else if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -2) {
	  i= Max(Min((int) Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]],
		    Scicos->Blocks[Scicos->params.curblk - 1].nevout),1);
	}
	i2 = i + Scicos->sim.clkptr[-1+Scicos->params.curblk] - 1;
	ierr1= putevs(*told, i2);
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



static void ddoit(double *told)
{
  int c__1 = 1;
  int i2,j;
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
    keve = Scicos->state.iwa[i];
    if(Scicos->sim.critev[-1+keve]!= 0){
      Scicos->params.hot = 0;
    }
    i2 = Scicos->sim.ordptr[keve] - 1;
    for (ii = Scicos->sim.ordptr[-1+keve]; ii <= i2; ++ii) {
      Scicos->params.curblk = Scicos->sim.ordclk[-1+ii];
      Scicos->params.nclock=Scicos->sim.ordclk[-1+ii + Scicos->sim.nordclk];

      if (Scicos->Blocks[Scicos->params.curblk - 1].nevout > 0) {
	if (Scicos->sim.funtyp[-1+Scicos->params.curblk] >= 0) {
	  double d__1 =  - 1.;
	  nsp_dset(&Scicos->Blocks[Scicos->params.curblk - 1].nevout, 
		   &d__1, Scicos->Blocks[Scicos->params.curblk-1].evout, &c__1);
	  
	  flag__ = 3;

	  if(Scicos->params.nclock>0){ /* if event has continuous origin don't call*/
	    callf(told, xd, Scicos->state.x, Scicos->state.x ,Scicos->state.x,&flag__);
	    if (flag__ < 0) {
	      *ierr = 5 - flag__;
	      return;
	    }
	  }
	  
	  for (j = 0; j < Scicos->Blocks[Scicos->params.curblk - 1].nevout; ++j) {
	    int i3;
	    if (Scicos->Blocks[Scicos->params.curblk-1].evout[j] >= 0.) {
	      int ierr1=0;
	      i3 = j + Scicos->sim.clkptr[-1+Scicos->params.curblk] ;
	      addevs(Scicos->Blocks[Scicos->params.curblk-1].evout[j]+(*told), &i3, &ierr1);
	      if (ierr1 != 0) {
		/*     !                 event conflict */
		*ierr = 3;
		return;
	      }
	    }
	  }
	}
      }

      if(Scicos->params.nclock> 0) {
	if (Scicos->Blocks[Scicos->params.curblk-1].nx+Scicos->Blocks[Scicos->params.curblk-1].nz > 0||
	    *Scicos->Blocks[Scicos->params.curblk-1].work !=NULL) {
	  /*  if a hidden state exists, must also call (for new scope eg)  */
	  /*  to avoid calling non-real activations */
	  flag__ = 2;
	  callf(told, xd, Scicos->state.x, Scicos->state.x,Scicos->state.x,&flag__);
	  if (flag__ < 0) {
	    *ierr = 5 - flag__;
	    return;
	  }
	}
      }else{
	if (*Scicos->Blocks[Scicos->params.curblk-1].work !=NULL) {
	  flag__ = 2;
	  Scicos->params.nclock=0;  /* in case some hidden continuous blocks need updating */
	  callf(told, xd, Scicos->state.x, Scicos->state.x,Scicos->state.x,&flag__);
	  if (flag__ < 0) {
	    *ierr = 5 - flag__;
	    return;
	  }
	}
      }
    }
  }
} 

static void edoit(double *told, int *kiwa)
{
  int i2;
  static int flag__;
  static int nord;
  static int ierr1, i;
  int kever, ii;
  kever = *Scicos->state.pointi;
  *Scicos->state.pointi = Scicos->state.evtspt[kever-1];
  Scicos->state.evtspt[kever-1] = -1;

  nord = Scicos->sim.ordptr[kever] - Scicos->sim.ordptr[-1+kever];
  if (nord == 0) {
    return;
  }
  Scicos->state.iwa[*kiwa] = kever;
  ++(*kiwa);
  for (ii = Scicos->sim.ordptr[-1+kever]; ii <= Scicos->sim.ordptr[kever] - 1; ++ii) {
    Scicos->params.curblk = Scicos->sim.ordclk[-1+ii];

    if (Scicos->sim.outptr[-1+Scicos->params.curblk + 1] - Scicos->sim.outptr[-1+Scicos->params.curblk] > 0) {
      Scicos->params.nclock = Abs(Scicos->sim.ordclk[-1+ii + Scicos->sim.nordclk]);
      flag__ = 1;
      callf(told, xd, Scicos->state.x, Scicos->state.x,Scicos->state.x,&flag__);
	    
      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }
 
    /*     .     Initialize tvec */
    if (Scicos->Blocks[Scicos->params.curblk - 1].nevout > 0) {
      if (Scicos->sim.funtyp[-1+Scicos->params.curblk] < 0) {
	if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -1) {
	  if (Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]] <= 0.) {
	    i = 2;
	  } else {
	    i = 1;
	  }
	} else if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -2) {
	  i= Max(Min((int) Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]],
		    Scicos->Blocks[Scicos->params.curblk - 1].nevout),1);
	}
	i2 = i + Scicos->sim.clkptr[-1+Scicos->params.curblk] - 1;
	ierr1 = putevs(*told, i2);
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





static void odoit(double *residual, double *xt, double *xtd, double *told)
{
  int i2;
  static int flag__, keve, kiwa;
  static int ierr1, i;
  static int ii, jj;
  kiwa = 0;
  for (jj = 1; jj <= Scicos->sim.noord; ++jj) {
    Scicos->params.curblk = Scicos->sim.oord[-1+jj];
    Scicos->params.nclock = Scicos->sim.oord[-1+jj + Scicos->sim.noord];
    if (Scicos->sim.outptr[-1+Scicos->params.curblk + 1] - Scicos->sim.outptr[-1+Scicos->params.curblk] > 0) {
      flag__ = 1;
      callf(told, xtd, xt, residual,Scicos->state.x,&flag__);

      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }

    if (Scicos->Blocks[Scicos->params.curblk - 1].nevout > 0) {
      if (Scicos->sim.funtyp[-1+Scicos->params.curblk] < 0) {
	if(Scicos->Blocks[Scicos->params.curblk - 1].nmode > 0){
	  i2 = Scicos->Blocks[Scicos->params.curblk - 1].mode[0] + 
	    Scicos->sim.clkptr[-1+Scicos->params.curblk] - 1;
	} else{
	  if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -1) {
	    if (Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]] <= 0.) {
	      i=2;
	    } else {
	      i=1;
	    }
	  } else if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -2) {
	    i=Max(Min((int) Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]],
		      Scicos->Blocks[Scicos->params.curblk - 1].nevout),1);
	  }
	  i2 =i+ Scicos->sim.clkptr[-1+Scicos->params.curblk] - 1;
	}
	ierr1 = putevs(*told, i2);
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
  for (ii = 1; ii <= Scicos->sim.noord; ++ii) {
    Scicos->params.curblk = Scicos->sim.oord[-1+ii];
    if (Scicos->Blocks[Scicos->params.curblk-1].nx > 0||
	*Scicos->Blocks[Scicos->params.curblk-1].work !=NULL) {
      /* work tests if a hidden state exists, used for delay block */
      flag__ = 0;
      Scicos->params.nclock = Scicos->sim.oord[-1+ii + Scicos->sim.noord];
      callf(told, xtd, xt, residual,xt,&flag__);

      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }
  }

  for (i = 0; i < kiwa; ++i) {
    keve = Scicos->state.iwa[i];
    for (ii = Scicos->sim.ordptr[-1+keve]; ii <= Scicos->sim.ordptr[keve] - 1; ++ii) {
      Scicos->params.curblk = Scicos->sim.ordclk[-1+ii ];
      if (Scicos->Blocks[Scicos->params.curblk-1].nx > 0||
	*Scicos->Blocks[Scicos->params.curblk-1].work !=NULL) {
	/* work tests if a hidden state exists */
	flag__ = 0;
	Scicos->params.nclock = Abs(Scicos->sim.ordclk[-1+ii + Scicos->sim.nordclk]);
	callf(told, xtd, xt, residual,xt,&flag__);

	if (flag__ < 0) {
	  *ierr = 5 - flag__;
	  return;
	}
      }
    }
  }
}


static void reinitdoit(double *told, int *scicos_xproperty)
{
  int i2;
  static int flag__, keve, kiwa;
  static int ierr1, i;
  static int ii, jj;
  kiwa = 0;
  for (jj = 1; jj <= Scicos->sim.noord; ++jj) {
    Scicos->params.curblk = Scicos->sim.oord[-1+jj];
    Scicos->params.nclock = Scicos->sim.oord[-1+jj +Scicos->sim.noord];
    if (Scicos->sim.outptr[-1+Scicos->params.curblk + 1] - Scicos->sim.outptr[-1+Scicos->params.curblk] > 0) {
      flag__ = 1;
      callf(told, xd, Scicos->state.x, Scicos->state.x,Scicos->state.x,&flag__);
      
      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }
    
    if (Scicos->Blocks[Scicos->params.curblk - 1].nevout > 0) {
      if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -1) {
	if (Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]] <= 0.) {
	  i=2;
	} else {
	  i=1;
	}
      } else if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -2) {
	i= Max(Min((int) Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]],
		  Scicos->Blocks[Scicos->params.curblk - 1].nevout),1);
      }
      if(Scicos->Blocks[Scicos->params.curblk - 1].nmode>0){
	Scicos->Blocks[Scicos->params.curblk - 1].mode[0]=i;
      }
      i2 =i+ Scicos->sim.clkptr[-1+Scicos->params.curblk] - 1;
      ierr1 = putevs(*told, i2);
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
  for (ii = 1; ii <= Scicos->sim.noord; ++ii) {
    Scicos->params.curblk = Scicos->sim.oord[-1+ii];
    if (Scicos->Blocks[Scicos->params.curblk-1].nx > 0) {
      flag__ = 7;
      Scicos->params.nclock = Scicos->sim.oord[-1+ii + Scicos->sim.noord];
      pointer_xproperty=&scicos_xproperty[-1+Scicos->sim.xptr[Scicos->params.curblk-1]];
      n_pointer_xproperty=Scicos->Blocks[Scicos->params.curblk-1].nx;
      callf(told, xd, Scicos->state.x, xd,Scicos->state.x,&flag__);

      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }
  }

  for (i = 0; i < kiwa; ++i) {
    keve = Scicos->state.iwa[i];
    for (ii = Scicos->sim.ordptr[-1+keve]; ii <= Scicos->sim.ordptr[keve] - 1; ++ii) {
      Scicos->params.curblk = Scicos->sim.ordclk[-1+ii ];
      if (Scicos->Blocks[Scicos->params.curblk-1].nx > 0) {
	flag__ = 7;
	Scicos->params.nclock = Abs(Scicos->sim.ordclk[-1+ii + Scicos->sim.nordclk]);
	n_pointer_xproperty=Scicos->Blocks[Scicos->params.curblk-1].nx;
	pointer_xproperty=&scicos_xproperty[-1+Scicos->sim.xptr[Scicos->params.curblk-1]];
	callf(told, xd, Scicos->state.x, xd,Scicos->state.x,&flag__);

	if (flag__ < 0) {
	  *ierr = 5 - flag__;
	  return;
	}
      }
    }
  }
} 



static void ozdoit(double *xtd, double *xt, double *told, int *kiwa)
{
  int i2;
  static int flag__, nord;
  static int ierr1, i;
  int ii, kever; 

  kever = *Scicos->state.pointi;
  *Scicos->state.pointi = Scicos->state.evtspt[kever-1];
  Scicos->state.evtspt[kever-1] = -1;

  nord = Scicos->sim.ordptr[kever] - Scicos->sim.ordptr[-1+kever];
  if (nord == 0) {
    return;
  }
  Scicos->state.iwa[*kiwa] = kever;
  ++(*kiwa);

  for (ii = Scicos->sim.ordptr[-1+kever]; ii <= Scicos->sim.ordptr[kever] - 1; ++ii) {
    Scicos->params.curblk = Scicos->sim.ordclk[-1+ii];
    if (Scicos->sim.outptr[-1+Scicos->params.curblk + 1] - Scicos->sim.outptr[-1+Scicos->params.curblk] > 0) {
      Scicos->params.nclock = Abs(Scicos->sim.ordclk[-1+ii +Scicos->sim.nordclk]);
      flag__ = 1;
      callf(told, xtd, xt, xt,Scicos->state.x,&flag__);

      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }
    /*     .     Initialize tvec */
    
    if (Scicos->Blocks[Scicos->params.curblk - 1].nevout > 0) {

      if (Scicos->sim.funtyp[-1+Scicos->params.curblk] < 0) {

	if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -1) {
	  if (phase==1 || Scicos->Blocks[Scicos->params.curblk - 1].nmode==0){
	    if (Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]] <= 0.) {
	      i=2;
	    } else {
	      i=1;
	    }
	  }else{
	    i=Scicos->Blocks[Scicos->params.curblk - 1].mode[0];
	  }
	} else if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -2) {
	  if (phase==1 || Scicos->Blocks[Scicos->params.curblk - 1].nmode==0){
	    i= Max(Min((int) 
		       Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]],
		       Scicos->Blocks[Scicos->params.curblk - 1].nevout),1);
	  }else{
	    i=Scicos->Blocks[Scicos->params.curblk - 1].mode[0];
	    
	  }
	}
	i2 =i+Scicos->sim.clkptr[-1+Scicos->params.curblk] - 1;
	ierr1=putevs(*told, i2);
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


static void zdoit(double *g, double *xtd, double *xt, double *told)
{
  double c_b14 = 0.0;
  int i2, c__1=1;;
  static int flag__, keve, kiwa;
  static int ierr1, i,j;
  static int ii, jj;

  nsp_dset(&Scicos->sim.ng, &c_b14,g , &c__1);

  kiwa = 0;
  for (jj = 1; jj <= Scicos->sim.nzord; ++jj) {
    Scicos->params.curblk = Scicos->sim.zord[-1+jj];
    Scicos->params.nclock = Scicos->sim.zord[-1+jj + Scicos->sim.nzord];
    if (Scicos->sim.outptr[-1+Scicos->params.curblk + 1] - Scicos->sim.outptr[-1+Scicos->params.curblk] > 0) {
      flag__ = 1;
      callf(told, xtd, xt, xt,xt,&flag__);

      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }

    if (Scicos->Blocks[Scicos->params.curblk - 1].nevout > 0) {
      if (Scicos->sim.funtyp[-1+Scicos->params.curblk] < 0) {


	if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -1) {
	  if (phase==1|| Scicos->Blocks[Scicos->params.curblk - 1].nmode==0){
	    if (Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]] <= 0.) {
	      i=2;
	    } else {
	      i=1;
	    }
	  }else{
	    i=Scicos->Blocks[Scicos->params.curblk - 1].mode[0];
	  }
	} else if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -2) {
	  if (phase==1|| Scicos->Blocks[Scicos->params.curblk - 1].nmode==0){
	    i=Max(Min((int) 
		      Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]],
		      Scicos->Blocks[Scicos->params.curblk - 1].nevout),1);
	  }else{
	    i=Scicos->Blocks[Scicos->params.curblk - 1].mode[0];
	  }
	}
	i2 =i+Scicos->sim.clkptr[-1+Scicos->params.curblk] - 1;
	ierr1=putevs(*told, i2);
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
  for (ii = 1; ii <= Scicos->sim.nzord; ++ii) {
    Scicos->params.curblk = Scicos->sim.zord[-1+ii];
    if (Scicos->Blocks[Scicos->params.curblk-1].ng > 0) {
      if (Scicos->sim.funtyp[-1+Scicos->params.curblk] > 0) {
	flag__ = 9;
	Scicos->params.nclock = Scicos->sim.zord[-1+ii +Scicos->sim.nzord];
	callf(told, xtd, xt, xtd,g,&flag__);
	if (flag__ < 0) {
	  *ierr = 5 - flag__;
	  return;
	}
      }else{
	if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -1) {
	  g[Scicos->sim.zcptr[-1+Scicos->params.curblk]-1]=Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]];
	  if(phase==1&&Scicos->Blocks[Scicos->params.curblk - 1].nmode>0){
	    if (g[Scicos->sim.zcptr[-1+Scicos->params.curblk]-1] <= 0.) {
	      Scicos->Blocks[Scicos->params.curblk - 1].mode[0] = 2;
	    }
	    else {
	      Scicos->Blocks[Scicos->params.curblk - 1].mode[0] = 1;
	    }
	  }
	} else if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -2) {
	  for (jj=0;jj<Scicos->Blocks[Scicos->params.curblk - 1].nevout-1;++jj) {
	    g[Scicos->sim.zcptr[-1+Scicos->params.curblk]-1+jj]=
	      Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]]
	      -(double)(jj+2);
	  }
	  if(phase==1&&Scicos->Blocks[Scicos->params.curblk - 1].nmode>0){
	    j=Max(Min((int) 
		      Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]],
		      Scicos->Blocks[Scicos->params.curblk - 1].nevout),1);
	    Scicos->Blocks[Scicos->params.curblk - 1].mode[0]= j;
	  }
	}
      }
    }
  }
  for (i = 0; i < kiwa; ++i) {
    keve = Scicos->state.iwa[i];
    for (ii = Scicos->sim.ordptr[-1+keve]; ii <= Scicos->sim.ordptr[keve] - 1; ++ii) {
      Scicos->params.curblk = Scicos->sim.ordclk[-1+ii ];
      if (Scicos->Blocks[Scicos->params.curblk-1].ng > 0) {
	if (Scicos->sim.funtyp[-1+Scicos->params.curblk] > 0) {
	  flag__ = 9;
	  Scicos->params.nclock = Abs(Scicos->sim.ordclk[-1+ii + Scicos->sim.nordclk]);
	  callf(told, xtd, xt, xtd,g,&flag__);
	  
	  if (flag__ < 0) {
	    *ierr = 5 - flag__;
	    return;
	  }
	}else{
	  if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -1) {
	    g[Scicos->sim.zcptr[-1+Scicos->params.curblk]-1]=
	      Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]];
	    if(phase==1&&Scicos->Blocks[Scicos->params.curblk - 1].nmode>0){
	      if (g[Scicos->sim.zcptr[-1+Scicos->params.curblk]-1] <= 0.) {
		Scicos->Blocks[Scicos->params.curblk - 1].mode[0] = 2;
	      } else {
		Scicos->Blocks[Scicos->params.curblk - 1].mode[0] = 1;
	      }
	    }
	  } else if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -2) {
	    for (jj=0;jj<Scicos->Blocks[Scicos->params.curblk - 1].nevout-1;++jj) {
	      g[Scicos->sim.zcptr[-1+Scicos->params.curblk]-1+jj]=
		Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]]
		-(double)(jj+2);
	    }
	    if(phase==1&&Scicos->Blocks[Scicos->params.curblk - 1].nmode>0){
	      j=Max(Min((int) 
			Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]],
			Scicos->Blocks[Scicos->params.curblk - 1].nevout),1);
	      Scicos->Blocks[Scicos->params.curblk - 1].mode[0]= j;
	    }
	  }
	}
      }
    }
  }
}


static void  callf(double *t, double *xtd, double *xt, double *residual, double *g, int *flag)
{
  voidf loc ; 
  double* args[SZ_SIZE];
  int sz[SZ_SIZE];
  double intabl[TB_SIZE],outabl[TB_SIZE];
  int ii,kf,in,out,ki,ko,ni,no,k,j;
  int lprt,szi,flagi;
  int solver=Scicos->params.solver;
  int cosd=Scicos->params.debug;
  ScicosF0 loc0;
  ScicosF loc1;
  /*  ScicosFm1 loc3;*/
  ScicosF2 loc2;
  ScicosF2z loc2z;
  ScicosFi loci1;
  ScicosFi2 loci2;
  ScicosFi2z loci2z;
  ScicosF4 loc4;

  kf=Scicos->params.curblk;
  
  if (kf==(Scicos->sim.debug_block+1)) return; /* debug block is never called */

  block_error=flag;  /* to return error from blocks of type 4 */

  flagi=*flag; /* flag 7 implicit initialization */
  if(flagi==7 && Scicos->Blocks[kf-1].type<10000) *flag=0;

  if ( cosd > 1){
    sciprint("block %d is called ",kf);
    sciprint("with flag %d ",*flag);
    sciprint("at time %f \n",*t);
    if(Scicos->sim.debug_block>-1){
      sciprint("Entering the block \n");
      call_debug_scicos(t,xtd,xt,residual,g,flag,kf,flagi,Scicos->sim.debug_block);
      if (*flag<0) return;  /* error in debug block */
    }
  }
  
  Scicos->params.scsptr=Scicos->Blocks[kf-1].scsptr; /* set scilab function adress for sciblk */
  Scicos->params.scsptr_flag=Scicos->Blocks[kf-1].scsptr_flag; 

  loc=Scicos->Blocks[kf-1].funpt;
  if (Scicos->Blocks[kf-1].type==4||Scicos->Blocks[kf-1].type==10004) {
    scicos_time=*t;
    Scicos->Blocks[kf-1].nevprt=Scicos->params.nclock;
    loc4 = (ScicosF4) loc;
    if(Scicos->Blocks[kf-1].ng>0){
	Scicos->Blocks[kf-1].g=&g[Scicos->sim.zcptr[-1+kf]-1];
      }
    if(Scicos->Blocks[kf-1].nx==0){
      (*loc4)(&Scicos->Blocks[kf-1],*flag);
    } 
    else {
      Scicos->Blocks[kf-1].x=&xt[Scicos->sim.xptr[kf-1]-1];
      if(Scicos->Blocks[kf-1].type==4) {
	if(*flag==0 && solver==100) {
	  Scicos->Blocks[kf-1].res=&residual[Scicos->sim.xptr[kf-1]-1];
	  Scicos->Blocks[kf-1].xd=&residual[Scicos->sim.xptr[kf-1]-1];
	  (*loc4)(&Scicos->Blocks[kf-1],*flag);
	  Scicos->Blocks[kf-1].xd=&xtd[Scicos->sim.xptr[kf-1]-1];
	  if(flagi!=7) {
	    for (k=0;k<Scicos->Blocks[kf-1].nx;k++) {
	      Scicos->Blocks[kf-1].res[k]=Scicos->Blocks[kf-1].res[k]-Scicos->Blocks[kf-1].xd[k];
	    }
	  }
	  else {
	    for (k=0;k<Scicos->Blocks[kf-1].nx;k++) {
	      Scicos->Blocks[kf-1].xd[k]=Scicos->Blocks[kf-1].res[k];
	    } 
	  }
	}
	else {
	  Scicos->Blocks[kf-1].xd=&xtd[Scicos->sim.xptr[kf-1]-1];
	  (*loc4)(&Scicos->Blocks[kf-1],*flag);
	}
      }
      else {
	Scicos->Blocks[kf-1].xd=&xtd[Scicos->sim.xptr[kf-1]-1];
	Scicos->Blocks[kf-1].res=&residual[Scicos->sim.xptr[kf-1]-1];
	(*loc4)(&Scicos->Blocks[kf-1],*flag);
      }
    }
    if ( cosd > 1){
      if(Scicos->sim.debug_block>-1){
	if (*flag<0) return;  /* error in block */
	sciprint("Leaving block %d \n",kf);
	call_debug_scicos(t,xtd,xt,residual,g,flag,kf,flagi,Scicos->sim.debug_block);
      }
    }
    return;
  }
  
  /*This is for compatibility*/
  if(Scicos->params.nclock<0){
      for (j =0;j<Scicos->Blocks[kf-1].ng;++j){
	Scicos->Blocks[kf-1].g[j]=(double)Scicos->Blocks[kf-1].jroot[j];
    }
  }
  
  if(Scicos->Blocks[kf-1].ztyp>0){
    Scicos->Blocks[kf-1].g=&g[Scicos->sim.zcptr[-1+kf]-1];
  }
  if(Scicos->Blocks[kf-1].nx>0){
    Scicos->Blocks[kf-1].x=&xt[Scicos->sim.xptr[kf-1]-1];
    Scicos->Blocks[kf-1].xd=&xtd[Scicos->sim.xptr[kf-1]-1];
    if(solver==100) {
      Scicos->Blocks[kf-1].res=&residual[Scicos->sim.xptr[kf-1]-1];
    }
  }

  switch (Scicos->Blocks[kf-1].type) {

  case 1 :			
    /* one entry for each input or output */
    for (in = 0 ; in < Scicos->Blocks[kf-1].nin ; in++) 
      {
	args[in]=Scicos->Blocks[kf-1].inptr[in];
	sz[in]=Scicos->Blocks[kf-1].insz[in];
      }
    for (out=0;out<Scicos->Blocks[kf-1].nout;out++) {
      args[in+out]=Scicos->Blocks[kf-1].outptr[out];
      sz[in+out]=Scicos->Blocks[kf-1].outsz[out];
    }
    if(Scicos->Blocks[kf-1].ztyp>0){
      Scicos->Blocks[kf-1].g=&g[Scicos->sim.zcptr[-1+kf]-1];
      args[Scicos->Blocks[kf-1].nin+Scicos->Blocks[kf-1].nout]=Scicos->Blocks[kf-1].g;
      sz[Scicos->Blocks[kf-1].nin+Scicos->Blocks[kf-1].nout]=Scicos->Blocks[kf-1].ng;
    }
    loc1 = (ScicosF) loc;
    if (solver==100) {
      (*loc1)(flag,&Scicos->params.nclock,t,Scicos->Blocks[kf-1].res,Scicos->Blocks[kf-1].x,&Scicos->Blocks[kf-1].nx,
	      Scicos->Blocks[kf-1].z,&Scicos->Blocks[kf-1].nz,
	      Scicos->Blocks[kf-1].evout,&Scicos->Blocks[kf-1].nevout,Scicos->Blocks[kf-1].rpar,&Scicos->Blocks[kf-1].nrpar,
	      Scicos->Blocks[kf-1].ipar,&Scicos->Blocks[kf-1].nipar,
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
      (*loc1)(flag,&Scicos->params.nclock,t,Scicos->Blocks[kf-1].xd,Scicos->Blocks[kf-1].x,&Scicos->Blocks[kf-1].nx,
	      Scicos->Blocks[kf-1].z,&Scicos->Blocks[kf-1].nz,
	      Scicos->Blocks[kf-1].evout,&Scicos->Blocks[kf-1].nevout,Scicos->Blocks[kf-1].rpar,&Scicos->Blocks[kf-1].nrpar,
	      Scicos->Blocks[kf-1].ipar,&Scicos->Blocks[kf-1].nipar,
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

    if (Scicos->Blocks[kf-1].nin>1) {
      ki=0;
      for (in=0;in<Scicos->Blocks[kf-1].nin;in++) {
	lprt=Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+kf]+in];
	szi=Scicos->sim.lnkptr[lprt]-Scicos->sim.lnkptr[-1+lprt];
	for (ii=0;ii<szi;ii++) 
	  intabl[ki++]=Scicos->state.outtb[Scicos->sim.lnkptr[-1+lprt]-1+ii];
	ni=ni+szi;
      }
      args[0]=&(intabl[0]);
    }
    else {
      if (Scicos->Blocks[kf-1].nin==0) {
	ni=0;
	args[0]=&(Scicos->state.outtb[0]);
      }
      else {
	lprt=Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+kf]];
	args[0]=&(Scicos->state.outtb[Scicos->sim.lnkptr[-1+lprt]-1]);
	ni=Scicos->sim.lnkptr[lprt]-Scicos->sim.lnkptr[-1+lprt];
      }
    }
    in=Scicos->Blocks[kf-1].nin;
    
    /* catenate outputs if necessary */
	no=0;
    if (Scicos->Blocks[kf-1].nout>1) {
      ko=0;
      for (out=0;out<Scicos->Blocks[kf-1].nout;out++) {
	lprt=Scicos->sim.outlnk[-1+Scicos->sim.outptr[-1+kf]+out];
	szi=Scicos->sim.lnkptr[lprt]-Scicos->sim.lnkptr[-1+lprt];
	
	for (ii=0;ii<szi;ii++)  
	  outabl[ko++]=Scicos->state.outtb[Scicos->sim.lnkptr[-1+lprt]-1+ii];
	no=no+szi;
      }
      args[1]=&(outabl[0]);
    }
    else {
      if (Scicos->Blocks[kf-1].nout==0) {
	no=0;
	args[1]=&(Scicos->state.outtb[0]);
      }
      else {
	lprt=Scicos->sim.outlnk[-1+Scicos->sim.outptr[-1+kf]];
	args[1]=&(Scicos->state.outtb[Scicos->sim.lnkptr[-1+lprt]-1]);
	no=Scicos->sim.lnkptr[lprt]-Scicos->sim.lnkptr[-1+lprt];
      }
    }

    loc0 = (ScicosF0) loc;
    if (solver==100) {
      (*loc0)(flag,&Scicos->params.nclock,t,Scicos->Blocks[kf-1].res,Scicos->Blocks[kf-1].x,&Scicos->Blocks[kf-1].nx,
	      Scicos->Blocks[kf-1].z,&Scicos->Blocks[kf-1].nz,
	      Scicos->Blocks[kf-1].evout,&Scicos->Blocks[kf-1].nevout,Scicos->Blocks[kf-1].rpar,&Scicos->Blocks[kf-1].nrpar,
	      Scicos->Blocks[kf-1].ipar,&Scicos->Blocks[kf-1].nipar,(double *)args[0],&ni,
	      (double *)args[1],&no);
    }
    else {
      (*loc0)(flag,&Scicos->params.nclock,t,Scicos->Blocks[kf-1].xd,Scicos->Blocks[kf-1].x,&Scicos->Blocks[kf-1].nx,
	      Scicos->Blocks[kf-1].z,&Scicos->Blocks[kf-1].nz,
	      Scicos->Blocks[kf-1].evout,&Scicos->Blocks[kf-1].nevout,Scicos->Blocks[kf-1].rpar,&Scicos->Blocks[kf-1].nrpar,
	      Scicos->Blocks[kf-1].ipar,&Scicos->Blocks[kf-1].nipar,(double *)args[0],&ni,
	      (double *)args[1],&no);
    }
    
    /* split output vector on each port if necessary */
    if (Scicos->Blocks[kf-1].nout>1) {
      ko=0;
      for (out=0;out<Scicos->Blocks[kf-1].nout;out++) {
	lprt=Scicos->sim.outlnk[-1+Scicos->sim.outptr[-1+kf]+out];
	szi=Scicos->sim.lnkptr[lprt]-Scicos->sim.lnkptr[-1+lprt];
	for (ii=0;ii<szi;ii++)  
	  Scicos->state.outtb[Scicos->sim.lnkptr[-1+lprt]-1+ii]=outabl[ko++];
      }
    }
    break;
  case 2 :			

    
    if (solver==100) {
      if (Scicos->Blocks[kf-1].ztyp==0){
	loc2 = (ScicosF2) loc;
	(*loc2)(flag,&Scicos->params.nclock,t,Scicos->Blocks[kf-1].res,Scicos->Blocks[kf-1].x,&Scicos->Blocks[kf-1].nx,
		Scicos->Blocks[kf-1].z,&Scicos->Blocks[kf-1].nz,
		Scicos->Blocks[kf-1].evout,&Scicos->Blocks[kf-1].nevout,Scicos->Blocks[kf-1].rpar,&Scicos->Blocks[kf-1].nrpar,
		Scicos->Blocks[kf-1].ipar,&Scicos->Blocks[kf-1].nipar,Scicos->Blocks[kf-1].inptr,
		Scicos->Blocks[kf-1].insz,&Scicos->Blocks[kf-1].nin,
		Scicos->Blocks[kf-1].outptr,Scicos->Blocks[kf-1].outsz,&Scicos->Blocks[kf-1].nout);
      }
      else{
	loc2z = (ScicosF2z) loc;
	(*loc2z)(flag,&Scicos->params.nclock,t,Scicos->Blocks[kf-1].res,Scicos->Blocks[kf-1].x,&Scicos->Blocks[kf-1].nx,
		 Scicos->Blocks[kf-1].z,&Scicos->Blocks[kf-1].nz,
		 Scicos->Blocks[kf-1].evout,&Scicos->Blocks[kf-1].nevout,Scicos->Blocks[kf-1].rpar,&Scicos->Blocks[kf-1].nrpar,
		 Scicos->Blocks[kf-1].ipar,&Scicos->Blocks[kf-1].nipar,Scicos->Blocks[kf-1].inptr,Scicos->Blocks[kf-1].insz,&Scicos->Blocks[kf-1].nin,
		 Scicos->Blocks[kf-1].outptr,Scicos->Blocks[kf-1].outsz,&Scicos->Blocks[kf-1].nout,
		 Scicos->Blocks[kf-1].g,&Scicos->Blocks[kf-1].ng);
      }
    }
    else {
      if (Scicos->Blocks[kf-1].ztyp==0){
	loc2 = (ScicosF2) loc;
	(*loc2)(flag,&Scicos->params.nclock,t,Scicos->Blocks[kf-1].xd,Scicos->Blocks[kf-1].x,&Scicos->Blocks[kf-1].nx,
		Scicos->Blocks[kf-1].z,&Scicos->Blocks[kf-1].nz,
		Scicos->Blocks[kf-1].evout,&Scicos->Blocks[kf-1].nevout,Scicos->Blocks[kf-1].rpar,&Scicos->Blocks[kf-1].nrpar,
		Scicos->Blocks[kf-1].ipar,&Scicos->Blocks[kf-1].nipar,Scicos->Blocks[kf-1].inptr,
		Scicos->Blocks[kf-1].insz,&Scicos->Blocks[kf-1].nin,
		Scicos->Blocks[kf-1].outptr,Scicos->Blocks[kf-1].outsz,&Scicos->Blocks[kf-1].nout);
      }
      else{
	loc2z = (ScicosF2z) loc;
	(*loc2z)(flag,&Scicos->params.nclock,t,Scicos->Blocks[kf-1].xd,Scicos->Blocks[kf-1].x,&Scicos->Blocks[kf-1].nx,
		 Scicos->Blocks[kf-1].z,&Scicos->Blocks[kf-1].nz,
		 Scicos->Blocks[kf-1].evout,&Scicos->Blocks[kf-1].nevout,Scicos->Blocks[kf-1].rpar,&Scicos->Blocks[kf-1].nrpar,
		 Scicos->Blocks[kf-1].ipar,&Scicos->Blocks[kf-1].nipar,Scicos->Blocks[kf-1].inptr,
		 Scicos->Blocks[kf-1].insz,&Scicos->Blocks[kf-1].nin,
		 Scicos->Blocks[kf-1].outptr,Scicos->Blocks[kf-1].outsz,&Scicos->Blocks[kf-1].nout,
		 Scicos->Blocks[kf-1].g,&Scicos->Blocks[kf-1].ng);
      }
    }
    break;
  case 10001 :			
    /* implicit block one entry for each input or output */
      for (in = 0 ; in < Scicos->Blocks[kf-1].nin ; in++) 
	{
	  args[in]=Scicos->Blocks[kf-1].inptr[in];
	  sz[in]=Scicos->Blocks[kf-1].insz[in];
	}
    for (out=0;out<Scicos->Blocks[kf-1].nout;out++) {
      args[in+out]=Scicos->Blocks[kf-1].outptr[out];
      sz[in+out]=Scicos->Blocks[kf-1].outsz[out];
    }
    if(Scicos->Blocks[kf-1].ztyp>0){
      Scicos->Blocks[kf-1].g=&g[Scicos->sim.zcptr[-1+kf]-1];
      args[Scicos->Blocks[kf-1].nin+Scicos->Blocks[kf-1].nout]=Scicos->Blocks[kf-1].g;
      sz[Scicos->Blocks[kf-1].nin+Scicos->Blocks[kf-1].nout]=Scicos->Blocks[kf-1].ng;
    }
    loci1 = (ScicosFi) loc;

    (*loci1)(flag,&Scicos->params.nclock,t,Scicos->Blocks[kf-1].res,Scicos->Blocks[kf-1].xd,Scicos->Blocks[kf-1].x,
	     &Scicos->Blocks[kf-1].nx,Scicos->Blocks[kf-1].z,&Scicos->Blocks[kf-1].nz,
	     Scicos->Blocks[kf-1].evout,&Scicos->Blocks[kf-1].nevout,Scicos->Blocks[kf-1].rpar,&Scicos->Blocks[kf-1].nrpar,
	     Scicos->Blocks[kf-1].ipar,&Scicos->Blocks[kf-1].nipar,
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
   
    if(Scicos->Blocks[kf-1].ztyp==0) {
      loci2 = (ScicosFi2) loc;
      
      (*loci2)(flag,&Scicos->params.nclock,t,Scicos->Blocks[kf-1].res,
	       Scicos->Blocks[kf-1].xd,Scicos->Blocks[kf-1].x,&Scicos->Blocks[kf-1].nx,
	       Scicos->Blocks[kf-1].z,&Scicos->Blocks[kf-1].nz,
	       Scicos->Blocks[kf-1].evout,&Scicos->Blocks[kf-1].nevout,Scicos->Blocks[kf-1].rpar,&Scicos->Blocks[kf-1].nrpar,
	       Scicos->Blocks[kf-1].ipar,&Scicos->Blocks[kf-1].nipar,Scicos->Blocks[kf-1].inptr,
	       Scicos->Blocks[kf-1].insz,&Scicos->Blocks[kf-1].nin,
	       Scicos->Blocks[kf-1].outptr,Scicos->Blocks[kf-1].outsz,&Scicos->Blocks[kf-1].nout);
    }
    else {
      loci2z = (ScicosFi2z) loc;
      
      (*loci2z)(flag,&Scicos->params.nclock,t,Scicos->Blocks[kf-1].res,
		Scicos->Blocks[kf-1].xd,Scicos->Blocks[kf-1].x,&Scicos->Blocks[kf-1].nx,
		Scicos->Blocks[kf-1].z,&Scicos->Blocks[kf-1].nz,
		Scicos->Blocks[kf-1].evout,&Scicos->Blocks[kf-1].nevout,Scicos->Blocks[kf-1].rpar,&Scicos->Blocks[kf-1].nrpar,
		Scicos->Blocks[kf-1].ipar,&Scicos->Blocks[kf-1].nipar,Scicos->Blocks[kf-1].inptr,Scicos->Blocks[kf-1].insz,&Scicos->Blocks[kf-1].nin,
		Scicos->Blocks[kf-1].outptr,Scicos->Blocks[kf-1].outsz,&Scicos->Blocks[kf-1].nout,
		Scicos->Blocks[kf-1].g,&Scicos->Blocks[kf-1].ng);
    }
    break;  
  default:
    sciprint("Undefined Function type\n");
    *flag=-1000;
    return;
  }
  if(solver==100 && Scicos->Blocks[kf-1].type<10000 && *flag==0) { /* Implicit Solver */

    if(flagi!=7) {
      for (k=0;k<Scicos->Blocks[kf-1].nx;k++) {
	Scicos->Blocks[kf-1].res[k]=Scicos->Blocks[kf-1].res[k]-Scicos->Blocks[kf-1].xd[k];
      }
    }
    else {
      for (k=0;k<Scicos->Blocks[kf-1].nx;k++) {
	Scicos->Blocks[kf-1].xd[k]=Scicos->Blocks[kf-1].res[k];
      } 
    }
  }
  for(in=0;in<Scicos->Blocks[kf-1].nevout;++in){
    Scicos->Blocks[kf-1].evout[in]=Scicos->Blocks[kf-1].evout[in]-*t;
  }
  if ( cosd > 1){
    if(Scicos->sim.debug_block>-1){
      if (*flag<0) return;  /* error in block */
      sciprint("Leaving block %d \n",kf);
      call_debug_scicos(t,xtd,xt,residual,g,flag,kf,flagi,Scicos->sim.debug_block);
    }
  }
}



void call_debug_scicos(double *t, double *xtd, double *xt, double *residual, double *g, int *flag, int kf, int flagi, int deb_blk)
{
  voidf loc ; 
  int solver=Scicos->params.solver,k;
  ScicosF4 loc4;
  Scicos->params.debug_counter++;
  Scicos->params.scsptr=Scicos->Blocks[deb_blk].scsptr;
  Scicos->params.scsptr_flag=Scicos->Blocks[deb_blk].scsptr_flag;
  loc=Scicos->Blocks[deb_blk].funpt;
  scicos_time=*t;
  Scicos->Blocks[kf-1].nevprt=Scicos->params.nclock;
  loc4 = (ScicosF4) loc;
  if(Scicos->Blocks[kf-1].ng>0){
    Scicos->Blocks[kf-1].g=&g[Scicos->sim.zcptr[-1+kf]-1];
  }
  if(Scicos->Blocks[kf-1].nx==0){
    (*loc4)(&Scicos->Blocks[kf-1],*flag);
  } 
  else {
    Scicos->Blocks[kf-1].x=&xt[Scicos->sim.xptr[kf-1]-1];
    if(*flag==0 && solver==100) {
      Scicos->Blocks[kf-1].res=&residual[Scicos->sim.xptr[kf-1]-1];
      Scicos->Blocks[kf-1].xd=&residual[Scicos->sim.xptr[kf-1]-1];
      (*loc4)(&Scicos->Blocks[kf-1],*flag);
      Scicos->Blocks[kf-1].xd=&xtd[Scicos->sim.xptr[kf-1]-1];
      if(flagi!=7) {
	for (k=0;k<Scicos->Blocks[kf-1].nx;k++) {
	  Scicos->Blocks[kf-1].res[k]=Scicos->Blocks[kf-1].res[k]-Scicos->Blocks[kf-1].xd[k];
	}
      }
      else {
	for (k=0;k<Scicos->Blocks[kf-1].nx;k++) {
	  Scicos->Blocks[kf-1].xd[k]=Scicos->Blocks[kf-1].res[k];
	} 
      }
    }
    else {
      Scicos->Blocks[kf-1].xd=&xtd[Scicos->sim.xptr[kf-1]-1];
      (*loc4)(&Scicos->Blocks[kf-1],*flag);
    }
  }
  if (*flag<0) 
    {
      sciprint("Error in the Debug block \n");
    }
}
  

/*
 * get a function in blocks functions from its name 
 */

void *scicos_get_function(char * fname)
{
  int (*loc)();
  int i=0;
  while ( tabsim[i].name != (char *) NULL) {
    if ( strcmp(fname,tabsim[i].name) == 0 ) 
      return tabsim[i].fonc;
    i++;
  }
  if ( SearchInDynLinks(fname,&loc) != -1 ) 
    return loc ;
  return NULL;
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

static int simblk(int *neq1,double * t,double * xc,double * xcdot)
{ 
  double c_b14 = 0.0;
  int c__1=1;
  nsp_dset(Scicos->params.neq, &c_b14,xcdot , &c__1);
  C2F(ierode).iero = 0;
  *ierr= 0;
  odoit(xcdot, xc,xcdot,t);
  C2F(ierode).iero = *ierr;
  return 0;
}
     
/* 
 *    compute residual  of the continuous part
 *   t     : current time 
 *  xc    : double precision vector whose  contains the continuous state. 
 *  xcdot : double precision vector, contain the computed derivative 
 *  of the state 
 */

static int simblkdaskr(double *t,double *xc,double *xcdot,int *cj,double *residual,int *ires,double *rpar1,int *ipar1)
{ 
  int c__1 = 1;
  C2F(dcopy)(Scicos->params.neq, xcdot, &c__1, residual, &c__1);
  *ires=0;
  *ierr= 0;
  C2F(ierode).iero = 0;
  odoit(residual, xc, xcdot,t);
  C2F(ierode).iero = *ierr;
  if(C2F(ierode).iero != 0) *ires=-1;
  return 0;
}

static int grblkdaskr(int *neq1,double * t,double * xc,double * xtd,int *ng1,double * g,double *rpar1,int *ipar1)
{
  *ierr= 0;
  C2F(ierode).iero = 0;
  zdoit(g, xtd, xc,t);
  C2F(ierode).iero = *ierr;
  return 0;
}
     
/*
 *  interface to grbl1 at the lsodar format 
 *  neq   : int  the size of the continuous state
 * t     : current time 
 * xc    : double precision vector contains the continuous state
 * g     : computed zero crossing surface (see lsodar) 
 */

static int grblk(int *neq1,double * t,double * xc,int * ng1,double * g)
{ 
 C2F(ierode).iero = 0;
 *ierr= 0;
 zdoit(g,xc, xc,t);
 C2F(ierode).iero = *ierr;
 return 0;
}


static void addevs(double t, int *evtnb, int *ierr1)
{
  static int i, j;
  *ierr1 = 0;
  if (Scicos->state.evtspt[*evtnb-1] != -1) {
    *ierr1 = 1;
    return;
  } else {
    Scicos->state.evtspt[*evtnb-1] = 0;
    Scicos->state.tevts[-1+*evtnb] = t;
  }
  if (*Scicos->state.pointi == 0) {
    *Scicos->state.pointi = *evtnb;
    return;
  }
  if (t < Scicos->state.tevts[-1+*Scicos->state.pointi]) {
    Scicos->state.evtspt[*evtnb-1] = *Scicos->state.pointi;
    *Scicos->state.pointi = *evtnb;
    return;
  }
  i = *Scicos->state.pointi;

 L100:
  if (Scicos->state.evtspt[i-1] == 0) {
    Scicos->state.evtspt[i-1] = *evtnb;
    return;
  }
  if (t >= Scicos->state.tevts[-1+Scicos->state.evtspt[i-1]]) {
    j = Scicos->state.evtspt[i-1];
    if (Scicos->state.evtspt[j-1] == 0) {
      Scicos->state.evtspt[j-1] = *evtnb;
      return;
    }
    i = j;
    goto L100;
  } else {
    Scicos->state.evtspt[*evtnb-1] = Scicos->state.evtspt[i-1];
    Scicos->state.evtspt[i-1] = *evtnb;
  }
} 


static int putevs(double t, int evtnb)
{
  if (Scicos->state.evtspt[evtnb-1] != -1) 
    {
      return 1 ; 
    } 
  else 
    {
      Scicos->state.evtspt[evtnb-1] = 0;
      Scicos->state.tevts[-1+evtnb] = t;
    }
  if (*Scicos->state.pointi == 0) 
    {
      *Scicos->state.pointi = evtnb;
      return 0;
    }
  Scicos->state.evtspt[evtnb-1] = *Scicos->state.pointi;
  *Scicos->state.pointi = evtnb;
  return 0;
} 


  
/* work space W needs to be ng+*neq*2 */

/* int setmode(double *W, double *x, double *told, int *jroot, double ttol) */
/* { */
/*   int k,j,jj,diff; */
/*   double ttmp; */

/*   ttmp=*told+ttol; */
/*   zdoit(W,x,x,told);  */
/*   if (*ierr != 0) return 1; */
/*   for(jj=0;jj<*Scicos->params.neq;++jj){ */
/*     W[jj]=x[jj]; */
/*   }  */
/*   diff=1; */
/*   k=0; */
/*   while (diff!=0){ */
/*     /\*save modes *\/ */
/*     for(jj=0;jj<Scicos->sim.nmod;++jj){  */
/*       jroot[jj]=Scicos->sim.mod[jj]; */
/*     } */
/*     for(j=0;j<=*Scicos->params.neq;++j){ */
/*       simblk(Scicos->params.neq, &ttmp, W, &W[*Scicos->params.neq]);   */
/*       if (*ierr != 0) return 1; */
/*       for(jj=0;jj<*Scicos->params.neq;++jj){ */
/* 	W[jj]=x[jj]+ttol*W[jj+(*Scicos->params.neq)]; */
/*       }  */
/*     } */
/*     /\*recompute modes*\/ */
/*     zdoit(&W[2*(*Scicos->params.neq)],W,W,&ttmp); */
/*     if (*ierr != 0) return 1; */
/*     /\*test against saved modes*\/ */
/*     diff=0; */
/*     for(jj=0;jj<Scicos->sim.nmod;++jj){  */
/*       if (jroot[jj]!=Scicos->sim.mod[jj]) { */
/* 	if(k>*Scicos->params.neq) { */
/* 	  *ierr=22; */
/* 	  return 1; */
/* 	} */
/* 	k=k+1; */
/* 	diff=1; */
/* 	break; */
/*       } */
/*     } */
/*   }   */
/*   return 0; */
/* } */


int get_phase_simulation(void)
{
  return phase;
}

void do_cold_restart(void)
{
  Scicos->params.hot=0;
  return;
}

double get_scicos_time(void)
{
  return scicos_time;
}

int get_block_number(void)
{
  return Scicos->params.curblk;
}

void set_block_error(int err)

{
  if ( block_error != NULL)  *block_error=err;
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


/* Jacobian*/
void Set_Jacobian_flag(int flag)
{
  Jacobian_Flag=flag;
  return;
}

double Get_Jacobian_parameter(void)
{
  return CJJ;
}

double Get_Scicos_SQUR(void)
{
  return  SQuround;
}

static void Jdoit(double *residual, double *xt, double *xtd, double *told, int *job)
{ 
  /* System generated locals */
  int i2;

  /* Local variables */
  static int flag__, keve, kiwa;
  static int ierr1, i;
  static int ii, jj;
  /* Function Body */
  kiwa = 0;
  for (jj = 1; jj <= Scicos->sim.noord; ++jj) {
    Scicos->params.curblk = Scicos->sim.oord[-1+jj];
    Scicos->params.nclock = Scicos->sim.oord[-1+jj + Scicos->sim.noord];
    if (Scicos->sim.outptr[-1+Scicos->params.curblk + 1] - Scicos->sim.outptr[-1+Scicos->params.curblk] > 0) {
      flag__ = 1;

      if ((*job==2)&&(Scicos->sim.oord[-1+jj]==Scicos->sim.nblk)) {/* applying desired output */
      }else
	callf(told, xtd, xt, residual,Scicos->state.x,&flag__);      
      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }

    if (Scicos->Blocks[Scicos->params.curblk - 1].nevout > 0) {
      if (Scicos->sim.funtyp[-1+Scicos->params.curblk] < 0) {
	if(Scicos->Blocks[Scicos->params.curblk - 1].nmode > 0){
	  i2 = Scicos->Blocks[Scicos->params.curblk - 1].mode[0] + 
	    Scicos->sim.clkptr[-1+Scicos->params.curblk] - 1;
	} else{
	  if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -1) {
	    if (Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]] <= 0.) {
	      i=2;
	    } else {
	      i=1;
	    }
	  } else if (Scicos->sim.funtyp[-1+Scicos->params.curblk] == -2) {
	    i=Max(Min((int) Scicos->state.outtb[-1+Scicos->sim.lnkptr[-1+Scicos->sim.inplnk[-1+Scicos->sim.inpptr[-1+Scicos->params.curblk]]]],
		      Scicos->Blocks[Scicos->params.curblk - 1].nevout),1);
	  }
	  i2 =i+ Scicos->sim.clkptr[-1+Scicos->params.curblk] - 1;
	}
	ierr1=putevs(*told,i2);
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
  for (ii = 1; ii <= Scicos->sim.noord; ++ii) {
    Scicos->params.curblk = Scicos->sim.oord[-1+ii];
    if (Scicos->Blocks[Scicos->params.curblk-1].nx > 0||
	*Scicos->Blocks[Scicos->params.curblk-1].work !=NULL) {
      /* work tests if a hidden state exists, used for delay block */
      flag__ = 0;
      if (((*job==1)&&(Scicos->sim.oord[-1+ii]==Scicos->sim.nblk))||(*job!=1)){
	if (*job==1)  flag__ = 10;
	Scicos->params.nclock = Scicos->sim.oord[-1+ii + Scicos->sim.noord];
	callf(told, xtd, xt, residual,xt,&flag__);
      };
      if (flag__ < 0) {
	*ierr = 5 - flag__;
	return;
      }
    }
  }

  for (i = 0; i < kiwa; ++i) {
    keve = Scicos->state.iwa[i];
    for (ii = Scicos->sim.ordptr[-1+keve]; ii <= Scicos->sim.ordptr[keve] - 1; ++ii) {
      Scicos->params.curblk = Scicos->sim.ordclk[-1+ii ];
      if (Scicos->Blocks[Scicos->params.curblk-1].nx > 0||
	*Scicos->Blocks[Scicos->params.curblk-1].work !=NULL) {
	/* work tests if a hidden state exists */

	flag__ = 0;
	if (((*job==1)&&(Scicos->sim.oord[-1+ii]==Scicos->sim.nblk))||(*job!=1)){
	  if (*job==1)  flag__ = 10;
	  Scicos->params.nclock = Abs(Scicos->sim.ordclk[-1+ii + Scicos->sim.nordclk]);
	  callf(told, xtd, xt, residual,xt,&flag__);
	}
	if (flag__ < 0) {
	  *ierr = 5 - flag__;
	  return;
	}
      }
    }
  }
} 


static int Jacobian(double *t,double *xc,double * xcdot,double *residual,
	     double *cj,double *rpar1,int *ipar1)
{ 
  int i,j,n, nx,ni,no,nb,m;
  double *RX, *Fx, *Fu, *Gx, *Gu, *Ewt,*H,*SQUR,*ERR1,*ERR2;
  double *Hx, *Hu,*Kx,*Ku,*HuGx,*FuKx,*FuKuGx,*HuGuKx;
  double del,delinv,xsave,xdsave,ysave;
  double a,b;
  int job;
  double **y = Scicos->Blocks[Scicos->sim.nblk-1].outptr;
  double **u = Scicos->Blocks[Scicos->sim.nblk-1].inptr;
  /*  taill1= 2+3*n+(n+ni)*(n+no)+nx(2*nx+ni+2*m+no)+m*(2*m+no+ni)+2*ni*no*/
  *ierr= 0;
  CJJ=*cj;
  n=*Scicos->params.neq; 
  nb=Scicos->sim.nblk;
  nx=Scicos->Blocks[Scicos->sim.nblk-1].nx;m=n-nx;
  no=Scicos->Blocks[Scicos->sim.nblk-1].nout;
  ni=Scicos->Blocks[Scicos->sim.nblk-1].nin;
  H=residual+ n * n;
  SQUR=H+1;
  Ewt=SQUR+1;
  ERR1=Ewt+n;
  ERR2=ERR1+n;
  RX=ERR2+n;
  Fx=RX+(n+ni)*(n+no); /* car (nx+ni)*(nx+no) peut etre > `a n*n*/
  Fu=Fx+nx*nx;
  Gx=Fu+nx*ni;
  Gu=Gx+no*nx;
  Hx=Gu+no*ni;
  Hu=Hx+m*m; 
  Kx=Hu+m*no;
  Ku=Kx+ni*m;
  HuGx=Ku+ni*no;
  FuKx=HuGx+m*nx;
  FuKuGx=FuKx+nx*m;
  HuGuKx=FuKuGx+nx*nx;
  /* HuGuKx+m*m;=.  m*m=size of HuGuKx */
  /* ------------------ Numerical Jacobian--->> Hx,Kx */
  job=0;/* read residuals;*/
  Jdoit(ERR1, xc, xcdot,t,&job);
  if (*ierr < 0) return -1;

  for (i=0;i<m;i++)
    for (j=0;j<ni;j++)
      Kx[j+i*ni]=u[j][0];

  for(i=0;i<m;i++){
    a= Max(Abs(H[0]*xcdot[i]),Abs(1.0/Ewt[i]));
    b= Max(1.0,Abs(xc[i]));
    del=SQUR[0]*Max(a,b);
    if (H[0]*xcdot[i]<0) del*=-1;
    del=(xc[i]+del)-xc[i];
    xsave=xc[i];
    xdsave=xcdot[i];
    xc[i]+=del;
    xcdot[i]+=CJJ*del;
    job=0;/* read residuals */
    Jdoit(ERR2, xc, xcdot,t,&job);
    if (*ierr < 0) return -1;
    delinv=1.0/del;
    for(j=0;j<m;j++)
      Hx[m*i+j]=(ERR2[j]-ERR1[j])*delinv;
    for (j=0;j<ni;j++)
      Kx[j+i*ni]=(u[j][0]-Kx[j+i*ni])*delinv;
    xc[i]=xsave;
    xcdot[i]=xdsave;
  }
  /*----- Numerical Jacobian--->> Hu,Ku */
  job=0;
  Jdoit(ERR1, xc, xcdot,t,&job);
  for (i=0;i<no;i++)
    for (j=0;j<ni;j++)
      Ku[j+i*ni]=u[j][0];

  for(i=0;i<no;i++){
    del=SQUR[0]* Max(1.0,Abs(y[i][0]));
    del=(y[i][0]+del)-y[i][0];
    ysave=y[i][0];
    y[i][0]+=del;
    job=2;/* applying y[i][0] to the output of imp block*/
    Jdoit(ERR2, xc, xcdot,t,&job);
    if (*ierr < 0) return -1;
    delinv=1.0/del;
    for(j=0;j<m;j++)
      Hu[m*i+j]=(ERR2[j]-ERR1[j])*delinv;
    for (j=0;j<ni;j++)
      Ku[j+i*ni]=(u[j][0]-Ku[j+i*ni])*delinv;
    y[i][0]=ysave;
  }
  /*----------------------------------------------*/
  job=1;/* read jacobian through flag=10; */
  Jdoit(&Fx[-m], xc, xcdot,t,&job);/* Filling up the FX:Fu:Gx:Gu*/
  if (*block_error!=0) sciprint("\n\r error in Jacobian");
  /*-------------------------------------------------*/
  Multp(Fu,Ku,RX,nx,ni,ni,no);Multp(RX,Gx,FuKuGx,nx,no,no,nx);
  for (i=0;i<nx;i++)
    for (j=0;j<nx;j++){
      residual[i+m+(j+m)*n]=Fx[i+j*nx]+FuKuGx[i+j*nx];
    }

  Multp(Hu,Gx,HuGx,m, no, no,nx);
  for (i=0;i<nx;i++)
    for (j=0;j<m;j++){
      residual[j+(i+m)*n]=HuGx[j+i*m];
    }

  Multp(Fu,Kx,FuKx,nx,ni,ni,m);
  for (i=0;i<m;i++)
    for (j=0;j<nx;j++){
      residual[(j+m)+i*n]=FuKx[j+i*nx];
    }

  Multp(Hu,Gu,RX,m,no,no,ni); Multp(RX,Kx,HuGuKx,m,ni,ni,m);
  for (i=0;i<m;i++)
    for (j=0;j<m;j++){
      residual[i+(j)*n]=Hx[i+j*m]+HuGuKx[i+j*m]; 
    }

  /*  chr='R'; DISP(residual,n,n,&chr);*/
  C2F(ierode).iero = *ierr;
 return 0;

}

static void Multp(double *A, double *B, double *R, int ra, int ca, int rb, int cb)
{ 
  int i,j,k;
  if (ca!=rb) sciprint("\n\r Error in matrix multiplication");
  for (i = 0; i<ra; i++)
    for (j = 0; j<cb; j++){
      R[i+ra*j]=0.0;
      for (k = 0; k<ca; k++)
	R[i+ra*j]+=A[i+k*ra]*B[k+j*rb];
    }
  return;
}

/* void DISP(A,ra ,ca,name)
     double *A;
     int ra,ca,*name;
{
  int i,j;
  sciprint("\n\r");
  for (i=0;i<ca;i++)
    for (j=0;j<ra;j++){
      if (A[j+i*ra]!=0) 
       sciprint(" %s(%d,%d)=%g;",name,j+1,i+1,A[j+i*ra]);
    }; 
}*/
/* Jacobian*/


