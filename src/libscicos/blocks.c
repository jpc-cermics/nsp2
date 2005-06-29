#include <stdio.h>
#include <math.h>
#include "nsp/machine.h"
#include "nsp/math.h" 
#include "nsp/graphics/Graphics.h" 
#include "nsp/object.h" 
#include "nsp/blas.h" 
#include "nsp/matutil.h" 
#include "../librand/grand.h" /* rand_ranf() */
#include "../system/files.h" /*  FSIZE */

#include "scicos.h"

struct
{
  int halt;
} C2F(coshlt);


extern struct {
  int idb;
} C2F(dbcos);


struct
{
  double atol, rtol, ttol, deltat;
} C2F(costol);


struct
{
  int kfun;
} C2F(curblk);

/* 
 * most of the blocks defined here have the following calling sequence
 */

#define scicos_args_poo int *flag__, int *nevprt, double *t, double *xd, double *x, \
	       int *nx, double *z__, int *nz, double *tvec, int *ntvec,\
	       double *rpar, int *nrpar, int *ipar, int *nipar, double *u,\
	       int *nu, double *y, int *ny 

int scicos_csslti (scicos_args_poo);
int scicos_delay (scicos_args_poo) ;
int scicos_dlradp (scicos_args_poo);
int scicos_dollar (scicos_args_poo);
int scicos_dsslti (scicos_args_poo);
int scicos_evscpe (scicos_args_poo);
int scicos_evtdly (scicos_args_poo);
int scicos_expblk (scicos_args_poo);
int scicos_forblk (scicos_args_poo);
int scicos_fsv (scicos_args_poo) ;
int scicos_gensin (scicos_args_poo);
int scicos_gensqr (scicos_args_poo);
int scicos_hltblk (scicos_args_poo);
int scicos_integr (scicos_args_poo);
int scicos_intplt (scicos_args_poo);
int scicos_intpol (scicos_args_poo);
int scicos_intrpl (scicos_args_poo) ;
int scicos_invblk (scicos_args_poo);
int scicos_iocopy (scicos_args_poo);
int scicos_logblk (scicos_args_poo);
int scicos_lookup (scicos_args_poo);
int scicos_lsplit (scicos_args_poo);
int scicos_maxblk (scicos_args_poo);
int scicos_memo (scicos_args_poo) ;
int scicos_mfclck (scicos_args_poo);
int scicos_minblk (scicos_args_poo);
int scicos_mscope (scicos_args_poo);
int scicos_pload ( scicos_args_poo) ;
int scicos_powblk (scicos_args_poo);
int scicos_qzcel (scicos_args_poo) ;
int scicos_qzflr (scicos_args_poo) ;
int scicos_qzrnd (scicos_args_poo);
int scicos_qztrn (scicos_args_poo);
int scicos_rndblk (scicos_args_poo);
int scicos_samphold (scicos_args_poo) ;
int scicos_sawtth (scicos_args_poo);
int scicos_scopxy (scicos_args_poo);
int scicos_scoxy (scicos_args_poo);
int scicos_selblk (scicos_args_poo);
int scicos_sinblk (scicos_args_poo);
int scicos_sqrblk (scicos_args_poo);
int scicos_tanblk (scicos_args_poo);
int scicos_tcsltj (scicos_args_poo) ;
int scicos_timblk (scicos_args_poo);
int scicos_trash (scicos_args_poo);
int scicos_zcross (scicos_args_poo) ;
int scicos_bound (scicos_args_poo);
int scicos_affich (scicos_args_poo);



int scicos_csslti (scicos_args_poo)
{
  int la, lb, lc, ld;
  static int c__1 = 1;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     continuous state space linear system simulator */
  /*     rpar(1:nx*nx)=A */
  /*     rpar(nx*nx+1:nx*nx+nx*nu)=B */
  /*     rpar(nx*nx+nx*nu+1:nx*nx+nx*nu+nx*ny)=C */
  /*     rpar(nx*nx+nx*nu+nx*ny+1:nx*nx+nx*nu+nx*ny+ny*nu)=D */

  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  la = 1;
  lb = *nx * *nx + la;
  lc = lb + *nx * *nu;
  if (*flag__ == 1 || *flag__ == 6)
    {
      /*     y=c*x+d*u */
      ld = lc + *nx * *ny;
      dmmul_scicos (&rpar[lc], ny, &x[1], nx, &y[1], ny, ny, nx, &c__1);
      dmmul1_scicos(&rpar[ld], ny, &u[1], nu, &y[1], ny, ny, nu, &c__1);
      /*         if(t.gt.64.0) write(6,'(e15.8,10(e10.3,x))') t,x(1),x(2), */
      /*     $        u(1),y(1) */
    }
  else if (*flag__ == 0)
    {
      /*     xd=a*x+b*u */
      dmmul_scicos (&rpar[la], nx, &x[1], nx, &xd[1], nx, nx, nx, &c__1);
      dmmul1_scicos(&rpar[lb], nx, &u[1], nu, &xd[1], nx, nx, nu, &c__1);
      /*         if(t.gt.64.0) write(6,'(e15.8,10(e10.3,x))') t,x(1),x(2), */
      /*     $        xd(1),xd(2),u(1) */
    }
  return 0;
}


int scicos_delay (scicos_args_poo) 
{
  int i__1;
  int i__;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     Ouputs nx*dt delayed input */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 1 || *flag__ == 4 || *flag__ == 6)
    {
      y[1] = z__[1];
    }
  else if (*flag__ == 2)
    {
      /*     .  shift buffer */
      i__1 = *nz - 1;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  z__[i__] = z__[i__ + 1];
	  /* L10: */
	}
      /*     .  add new point to the buffer */
      z__[*nz] = u[1];
    }
  return 0;
}			



int
scicos_cstblk (int *flag__, int *nevprt, double *t, double *xd, double *x,
	       int *nx, double *z__, int *nz, double *tvec, int *ntvec,
	       double *rpar, int *nrpar, int *ipar, int *nipar, double *y,
	       int *ny)
{
  static int c__1 = 1;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     output a vector of constants out(i)=rpar(i) */
  /*     rpar(1:nrpar) : given constants */
  C2F(dcopy) (nrpar, rpar, &c__1, y, &c__1);
  return 0;
}			


int
scicos_constraint (int *flag__, int *nevprt, double *t, double *res,
		   double *xd, double *x, int *nx, double *z__, int *nz,
		   double *tvec, int *ntvec, double *rpar, int *nrpar,
		   int *ipar, int *nipar, double *u, int *nu, double *y,
		   int *ny)
{
  int i__1;
  int i__;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  --res;
  if (*flag__ == 0)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  res[i__] = xd[i__] - u[i__];
	  res[i__ + *nu] = xd[i__];
	  /* L12: */
	}
    }
  else if (*flag__ == 1)
    {
      i__1 = *ny;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  y[i__] = xd[i__ + *nu];
	  /* L14: */
	}
      /*      elseif(flag.eq.6.or.flag.eq.7) then */
      /*         do 12 i=1,nu */
      /* 12      continue */
    }
  return 0;
}






int
scicos_delayv (int *flag__, int *nevprt, double *t, double *xd, double *x,
	       int *nx, double *z__, int *nz, double *tvec, int *ntvec,
	       double *rpar, int *nrpar, int *ipar, int *nipar, double *u1,
	       int *nu1, double *u2, int *nu2, double *y, int *ny)
{

  int i__1, i__2;
  double dtat, a;
  int i__, j, k, ii, in;
  double u2r;

  /*     Copyright INRIA */
  /*     rpar(1)=dt */
  /*     delayv=u(nin) */

  --y;
  --u2;
  --u1;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  j = (*nz - 1) / *nu1;
  if (*flag__ == 3)
    {
      tvec[1] = *t + rpar[1];
      k = (int) (u2[1] / rpar[1]);
      if (k > j - 3)
	{
	  tvec[2] = *t;
	}
      if (k < 1)
	{
	  tvec[2] = *t;
	}
    }

  /*     .   shift buffer */
  if (*flag__ == 2)
    {
      i__1 = j;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  z__[i__] = z__[i__ + 1];
	  /* L10: */
	}
      i__1 = *nu1 - 1;
      for (in = 1; in <= i__1; ++in)
	{
	  i__2 = (in + 1) * j;
	  for (ii = in * j + 1; ii <= i__2; ++ii)
	    {
	      z__[ii] = z__[ii + 1];
	      /* L35: */
	    }
	  /* L30: */
	}
      z__[*nz] = *t;
      i__1 = *nu1;
      for (in = 1; in <= i__1; ++in)
	{
	  z__[j * in] = u1[in];
	  /* L20: */
	}
    }
  if (*flag__ == 1 || *flag__ == 6)
    {
      dtat = *t - z__[*nz];
      i__1 = *nu1;
      for (in = 1; in <= i__1; ++in)
	{
	  /*     extrapolate to find values at delta.t */
	  if (u2[1] <= dtat)
	    {
	      /*     initialisation start */
	      if (dtat < rpar[1] / 100.)
		{
		  a = u2[1] / (rpar[1] + dtat);
		  /*     delete negative delay */
		  if (a <= 0.)
		    {
		      a = 0.;
		    }
		  y[in] = (1 - a) * z__[j * in] + a * z__[j * in - 1];
		}
	      else
		{
		  a = u2[1] / dtat;
		  /*     delete negative delay */
		  if (a <= 0.)
		    {
		      a = 0.;
		    }
		  y[in] = (1 - a) * u1[in] + a * z__[j * in];
		}
	    }
	  else
	    {
	      u2r = u2[1] - dtat;
	      k = (int) (u2r / rpar[1]);
	      /*     limitation of size buffer */
	      if (k > j - 3)
		{
		  k = j - 3;
		  a = 1.;
		}
	      else
		{
		  a = (u2r - k * rpar[1]) / rpar[1];
		}
	      /*     interpolate to find values at t-delay */
	      y[in] = (1 - a) * z__[j * in - k] + a * z__[j * in - k - 1];
	    }
	  /* L8: */
	}
    }
  return 0;
}


int
scicos_demux (int *flag__, int *nevprt, double *t, double *xd, double *x,
	      int *nx, double *z__, int *nz, double *tvec, int *ntvec,
	      double *rpar, int *nrpar, int *ipar, int *nipar, double *uy1,
	      int *nuy1, double *uy2, int *nuy2, double *uy3, int *nuy3,
	      double *uy4, int *nuy4, double *uy5, int *nuy5, double *uy6,
	      int *nuy6, double *uy7, int *nuy7, double *uy8, int *nuy8,
	      double *uy9, int *nuy9)
{

  int i__1;
  int i__, k;
  /*     Copyright INRIA */
  /*     Scicos block simulator */

  --uy9;
  --uy8;
  --uy7;
  --uy6;
  --uy5;
  --uy4;
  --uy3;
  --uy2;
  --uy1;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  k = 0;
  switch (ipar[1] - 1)
    {
    case 1:
      goto L20;
    case 2:
      goto L30;
    case 3:
      goto L40;
    case 4:
      goto L50;
    case 5:
      goto L60;
    case 6:
      goto L70;
    case 7:
      goto L80;
    }

 L20:
  i__1 = *nuy2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy2[i__] = uy1[k];
      /* L25: */
    }
  i__1 = *nuy3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy3[i__] = uy1[k];
      /* L27: */
    }
  return 0;

 L30:
  i__1 = *nuy2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy2[i__] = uy1[k];
      /* L35: */
    }
  i__1 = *nuy3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy3[i__] = uy1[k];
      /* L37: */
    }
  i__1 = *nuy4;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy4[i__] = uy1[k];
      /* L38: */
    }
  return 0;

 L40:
  i__1 = *nuy2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy2[i__] = uy1[k];
      /* L41: */
    }
  i__1 = *nuy3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy3[i__] = uy1[k];
      /* L42: */
    }
  i__1 = *nuy4;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy4[i__] = uy1[k];
      /* L43: */
    }
  i__1 = *nuy5;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy5[i__] = uy1[k];
      /* L44: */
    }
  return 0;

 L50:
  i__1 = *nuy2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy2[i__] = uy1[k];
      /* L51: */
    }
  i__1 = *nuy3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy3[i__] = uy1[k];
      /* L52: */
    }
  i__1 = *nuy4;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy4[i__] = uy1[k];
      /* L53: */
    }
  i__1 = *nuy5;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy5[i__] = uy1[k];
      /* L54: */
    }
  i__1 = *nuy6;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy6[i__] = uy1[k];
      /* L55: */
    }
  return 0;

 L60:
  i__1 = *nuy2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy2[i__] = uy1[k];
      /* L61: */
    }
  i__1 = *nuy3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy3[i__] = uy1[k];
      /* L62: */
    }
  i__1 = *nuy4;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy4[i__] = uy1[k];
      /* L63: */
    }
  i__1 = *nuy5;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy5[i__] = uy1[k];
      /* L64: */
    }
  i__1 = *nuy6;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy6[i__] = uy1[k];
      /* L65: */
    }
  i__1 = *nuy7;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy7[i__] = uy1[k];
      /* L66: */
    }
  return 0;

 L70:
  i__1 = *nuy2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy2[i__] = uy1[k];
      /* L71: */
    }
  i__1 = *nuy3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy3[i__] = uy1[k];
      /* L72: */
    }
  i__1 = *nuy4;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy4[i__] = uy1[k];
      /* L73: */
    }
  i__1 = *nuy5;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy5[i__] = uy1[k];
      /* L74: */
    }
  i__1 = *nuy6;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy6[i__] = uy1[k];
      /* L75: */
    }
  i__1 = *nuy7;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy7[i__] = uy1[k];
      /* L76: */
    }
  i__1 = *nuy8;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy8[i__] = uy1[k];
      /* L77: */
    }
  return 0;

 L80:
  i__1 = *nuy2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy2[i__] = uy1[k];
      /* L81: */
    }
  i__1 = *nuy3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy3[i__] = uy1[k];
      /* L82: */
    }
  i__1 = *nuy4;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy4[i__] = uy1[k];
      /* L83: */
    }
  i__1 = *nuy5;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy5[i__] = uy1[k];
      /* L84: */
    }
  i__1 = *nuy6;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy6[i__] = uy1[k];
      /* L85: */
    }
  i__1 = *nuy7;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy7[i__] = uy1[k];
      /* L86: */
    }
  i__1 = *nuy8;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy8[i__] = uy1[k];
      /* L87: */
    }
  i__1 = *nuy9;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy9[i__] = uy1[k];
      /* L88: */
    }
  return 0;
}			





int
scicos_diffblk (int *flag__, int *nevprt, double *t, double *res, double *xd,
		double *x, int *nx, double *z__, int *nz, double *tvec,
		int *ntvec, double *rpar, int *nrpar, int *ipar, int *nipar,
		double *u, int *nu, double *y, int *ny)
{

  int i__1;
  int i__;

  /*     Copyright INRIA */
  /*     Scicos block simulator */



  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  --res;
  if (*flag__ == 0)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  res[i__] = x[i__] - u[i__];
	  /* L10: */
	}
    }
  else if (*flag__ == 1)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  y[i__] = xd[i__];
	  /* L11: */
	}
    }
  else if (*flag__ == 6 || *flag__ == 7)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  x[i__] = u[i__];
	  /* L12: */
	}
    }
  return 0;
}			



int scicos_dlradp (scicos_args_poo)
{
  static int c__1 = 1;
  /* static int c_n1 = -1; */

  int i__1;  int m, n, iflag;
  double  yy[201], den[51];
  int mpn;
  double num[51];
  int npt;
  double yyp;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     SISO, strictly proper adapted transfer function */

  /*     u(1)    : main input */
  /*     u(2)    : modes adaptation input */

  /*     m = ipar(1) : degree of numerator */
  /*     n = ipar(2) : degree of denominator n>m */
  /*     npt = ipar(3) : number of mesh points */
  /*     x = rpar(1:npt) : mesh points abscissae */
  /*     rnr = rpar(npt+1:npt+m*npt) : rnr(i,k) i=1:m  is the real part of */
  /*          the roots of the numerator at the kth mesh point */
  /*     rni = rpar(npt+m*npt+1:npt+2*m*npt) : rni(i,k) i=1:m  is the */
  /*          imaginary part of the roots of the numerator at the kth */
  /*          mesh point */
  /*     rdr = rpar(npt+2*m*np+1:npt+(2*m+n)*npt) : rdr(i,k) i=1:n */
  /*          is the real part of the roots of the denominator at the kth */
  /*          meshpoint */
  /*     rdi = rpar(npt+(2*m+n)*np+1:npt+2*(m+n)*npt) : rdi(i,k) i=1:n */
  /*          is the imaginary part of the roots of the denominator at */
  /*          the kth  meshpoint */
  /*     g   = rpar(npt+2*(m+n)*npt+1:npt+2*(m+n)*npt+npt) is the */
  /*           gain values at the mesh points. */
  /* ! */

  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  m = ipar[1];
  n = ipar[2];
  if (*flag__ == 2)
    {
      /*     state */
      m = ipar[1];
      n = ipar[2];
      mpn = m + n;
      npt = ipar[3];
      i__1 = (mpn << 1) + 1;
      Scierror("Error : scicos_intp to be done \n");
      /* scicos_intp (&u[2], &rpar[1], &rpar[npt + 1], &i__1, &npt, yy);
	 scicos_wprxc (&m, yy, &yy[m], num, ww);
	 scicos_wprxc (&n, &yy[m * 2], &yy[(m << 1) + 1 + n - 1], den, ww);
      */
      yyp = - C2F(ddot) (&n, den, &c__1, &z__[m + 1], &c__1) 
	+ (C2F(ddot) (&m, num, &c__1, &z__[1],&c__1) + u[1]) * yy[mpn * 2];
      if (m > 0)
	{
	  /* 
	     i__1 = m - 1;
	     scicos_unsfdcopy (&i__1, &z__[2], &c_n1, &z__[1], &c_n1);
	  */
	  memmove(&z__[1], &z__[2],(m-1)*sizeof(double));
	  z__[m] = u[1];
	}
      /*
	i__1 = n - 1;
	scicos_unsfdcopy (&i__1, &z__[m + 2], &c_n1, &z__[m + 1], &c_n1);
      */
      memmove(&z__[m+1], &z__[m+2],(n-1)*sizeof(double));
      z__[mpn] = yyp;
    }
  else if (*flag__ == 4)
    {
      /*     init */
      m = ipar[1];
      n = ipar[2];
      if (m > 50 || n > 50)
	{
	  iflag = -1;
	  return 0;
	}
    }
  /*     y */
  y[1] = z__[m + n];
  return 0;
}			


int scicos_dollar (scicos_args_poo)
{
  int j;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     Ouputs delayed input */
  if (*flag__ == 1 || *flag__ == 6 || *flag__ == 4)
    {
      for (j = 0 ; j < *nu ; ++j)  y[j] = z__[j];
    }
  else if (*flag__ == 2)
    {
      for (j = 0 ; j < *nu ; ++j)  z__[j] = u[j];
    }
  return 0;
}			



int scicos_dsslti (scicos_args_poo)
{
  int c__1 = 1;
  double w[100];
  int la, lb, lc, ld;

  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     discrete state space linear system simulator */
  /*     rpar(1:nx*nx)=A */
  /*     rpar(nx*nx+1:nx*nx+nx*nu)=B */
  /*     rpar(nx*nx+nx*nu+1:nx*nx+nx*nu+nx*ny)=C */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  la = 1;
  lb = *nz * *nz + la;
  lc = lb + *nz * *nu;
  if (*flag__ == 4)
    {
      if (*nz > 100)
	{
	  *flag__ = -1;
	  return 0;
	}
    }
  else if (*flag__ == 2)
    {
      /*     x+=a*x+b*u */
      C2F(dcopy) (nz, &z__[1], &c__1, w, &c__1);
      dmmul_scicos (&rpar[la], nz, w, nz, &z__[1], nz, nz, nz, &c__1);
      dmmul1_scicos(&rpar[lb], nz, &u[1], nu, &z__[1], nz, nz, nu, &c__1);
    }
  else if (*flag__ == 1 || *flag__ == 6)
    {
      /*     y=c*x+d*u */
      ld = lc + *nz * *ny;
      dmmul_scicos (&rpar[lc], ny, &z__[1], nz, &y[1], ny, ny, nz, &c__1);
      dmmul1_scicos(&rpar[ld], ny, &u[1], nu, &y[1], ny, ny, nu, &c__1);
    }
  return 0;
}			

int
scicos_eselect (int *flag__, int *nevprt, int *ntvec, double *rpar,
		int *nrpar, int *ipar, int *nipar, double *u, int *nu)
{
  int i__1, i__2;
  int iu;
  /*     Scicos block simulator */
  /*     if-then-else block */
  /*     if event input exits from then or else clock ouputs based */
  /*     on the sign of the unique input (if input>0 then  else ) */
  /*     Copyright INRIA */
  --u;
  --ipar;
  --rpar;

  if (C2F(dbcos).idb == 1)
    {
      sciprint("ifthel:  flag=%d\n",*flag__);
    }
  /* Computing MAX */
  /* Computing MIN */
  i__2 = (int) u[1];
  i__1 = Min (i__2, ipar[1]);
  iu = Max (i__1, 1);
  if (*flag__ == 3)
    {
      *ntvec = iu;
    }
  return 0;
}			



int scicos_evscpe (scicos_args_poo)
{
  BCG *Xgc;
  int record;
  static int c__0 = 0;
  static int c__1 = 1;
  static int c_n1 = -1;
  static int c__2 = 2;
  static int c__3 = 3;
  /* Initialized data */
  static double frect[4] = { 0., 0., 1., 1. };
  static int cur = 0;
  double rect[4];
  double ymin, ymax;
  int i__,  n1;
  double xx[2], yy[2];
  int wid, iwd;
  double per;
  int nax[4], iwp;

  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     Event scope */
  /*     ipar(1) = win_num */
  /*     ipar(2) = 0/1 color flag */
  /*     ipar(3:10) = color */
  /*     ipar(nipar-3:nipar-2) = window position */
  /*     ipar(nipar-1:nipar)= window position */

  /*     rpar(1)=periode */

  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  /*      data yy / 0.00d0,0.80d0/ */

  if (*flag__ == 2)
    {
      per = rpar[1];
      wid = ipar[1];
      Xgc = scicos_set_win(wid,&cur);
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,ipar[2]);
      record= Xgc->graphic_engine->xget_recording(Xgc);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      if (*t / per >= z__[1])
	{
	  z__[1] = (int) (*t / per) + 1.;
	  /*     clear window */
	  nax[0] = 2;
	  nax[1] = 10;
	  nax[2] = 2;
	  nax[3] = 10;
	  Xgc = scicos_set_win(wid,&cur);
	  Xgc->graphic_engine->clearwindow(Xgc);
	  Xgc->graphic_engine->scale->xset_usecolor(Xgc,ipar[2]);
	  Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
	  rect[0] = per * (z__[1] - 1.);
	  rect[1] = 0.;
	  rect[2] = per * z__[1];
	  rect[3] = 1.;
	  Xgc->graphic_engine->scale->xset_dash(Xgc,c__0);
	  nsp_plot2d(Xgc,rect, &rect[1],&c__1, &c__1, &c_n1, "011","t@ @input and output",
		     0, rect, nax);
	}

      xx[0] = *t;
      xx[1] = *t;
      yy[0] = 0.;
      yy[1] = .8;
      i__ = 1;
      *nx = 1;
    L10:
      if ((*nevprt & *nx) != 0)
	{
	  Xgc->graphic_engine->scale->drawpolylines(Xgc,xx, yy, &ipar[i__ + 2], c__1, c__2);
	  yy[0] = (yy[0] + yy[1]) / 2;
	}
      ++i__;
      *nx <<= 1;
      if (*nx <= *nevprt)
	{
	  goto L10;
	}
      yy[0] = 0.;
      Xgc->graphic_engine->xset_recording(Xgc,record);
    }
  else if (*flag__ == 4)
    {
      char *str;
      wid = ipar[1];
      ymin = 0.;
      ymax = 1.;
      per = rpar[1];
      nax[0] = 2;
      nax[1] = 10;
      nax[2] = 2;
      nax[3] = 10;
      n1 = (int) ((int) (*t) / per);
      if (*t <= 0.)
	{
	  --n1;
	}
      Xgc = scicos_set_win(wid,&cur);
      rect[0] = per * (n1 + 1);
      rect[1] = ymin;
      rect[2] = per * (n1 + 2);
      rect[3] = ymax;
      Nsetscale2d(Xgc,frect,NULL,rect,"nn");
      iwp = *nipar - 3;
      if (ipar[iwp] >= 0)
	{
	  Xgc->graphic_engine->xset_windowpos(Xgc,ipar[iwp], ipar[iwp + 1]);
	}
      iwd = *nipar - 1;
      if (ipar[iwd] >= 0)
	{
	  Xgc->graphic_engine->xset_windowdim(Xgc,ipar[iwd], ipar[iwd + 1]);
	}
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,ipar[2]);
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,c__3);
      Xgc->graphic_engine->clearwindow(Xgc);
      Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
      Xgc->graphic_engine->scale->xset_dash(Xgc,c__0);
      nsp_plot2d(Xgc,rect, &rect[1],&c__1, &c__1, &c_n1, "011","t@ @input and output", 0, rect, nax);
      str = scicos_getlabel (C2F(curblk).kfun);
      if ( str != NULL && strlen(str) != 0 && strcmp(str," ") != 0 ) 
	Xgc->graphic_engine->setpopupname(Xgc,str);
      nsp_check_gtk_events ();
      z__[1] = 0.;
    }
  return 0;
}			


int scicos_evtdly (scicos_args_poo)
{
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     event delay */
  /*     delay=rpar(1) */
  if (*flag__ == 3)
    {
      tvec[0] = *t + rpar[0];
    }
  return 0;
}			


int scicos_expblk (scicos_args_poo)
{
  int i__1;
  /* Builtin functions */
  int i__;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     Outputs a^u(i) */
  /*     a=rpar(1) */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 1)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  y[i__] = exp (log (rpar[1]) * u[i__]);
	  /* L15: */
	}
    }
  if (*flag__ >= 4)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  y[i__] = exp (log (rpar[1]) * u[i__]);
	  /* L20: */
	}
    }
  return 0;
}			


int scicos_forblk (scicos_args_poo)
{
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     For block */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 3)
    {
      if (*nevprt == 1)
	{
	  z__[2] = u[1];
	  z__[1] = 1.;

	  if (u[1] >= 1.)
	    {
	      tvec[1] = *t - 1.;
	      tvec[2] = *t + C2F(costol).ttol / 2.;
	    }
	  else
	    {
	      tvec[1] = *t - 1.;
	      tvec[2] = *t - 1.;
	    }
	}
      else
	{
	  z__[1] += 1.;

	  if (z__[1] >= z__[2])
	    {
	      tvec[1] = *t + C2F(costol).ttol / 2.;
	      tvec[2] = *t - 1.;
	    }
	  else
	    {
	      tvec[1] = *t - 1.;
	      tvec[2] = *t + C2F(costol).ttol / 2.;
	    }
	}
    }
  if (*flag__ == 1 || *flag__ == 3)
    {
      y[1] = z__[1];
    }
  return 0;
}			


int
scicos_fscope (int *flag__, int *nevprt, double *t, double *xd, double *x,
	       int *nx, double *z__, int *nz, double *tvec, int *ntvec,
	       double *rpar, int *nrpar, int *ipar, int *nipar)
{
  BCG *Xgc;
  static int c__0 = 0;
  static int c__1 = 1;
  static int c_n1 = -1;
  static int c__3 = 3;
  static double c_b88 = 0.;
  static double frect[4] = { 0., 0., 1., 1. };
  static int cur = 0;
  int i__1, i__2;
  double rect[4];
  double ymin, ymax;
  int i__, k, n;
  double u[8];
  double tsave;
  int n1, n2;
  double dt;
  int nu;
  int wid, iwd;
  double per;
  int nax[4], iwp;
  int record;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     ipar(1) = win_num */
  /*     ipar(2) = 0/1 color flag */
  /*     ipar(3) = buffer size */
  /*     ipar(4:11) = line type for ith curve */

  /*     ipar(12:13) : window position */
  /*     ipar(14:15) : window dimension */

  /*     rpar(1)=dt */
  /*     rpar(2)=ymin */
  /*     rpar(3)=ymax */
  /*     rpar(4)=periode */



  /*      character*(4) logf */
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;

  iwp = 12;
  iwd = 14;
  nu = ipar[16];
  if (*flag__ == 2)
    {
      scicos_getouttb (nu, &ipar[17], u);
      dt = rpar[1];
      ymin = rpar[2];
      ymax = rpar[3];
      per = rpar[4];
      wid = ipar[1];
      n = ipar[3];
      k = (int) z__[1];
      if (k > 0)
	{
	  n1 = (int) (z__[k + 1] / per);
	  if (z__[k + 1] < 0.)
	    {
	      --n1;
	    }
	}
      else
	{
	  n1 = 0;
	}

      tsave = *t;
      if (dt > 0.)
	{
	  *t = z__[k + 1] + dt;
	}

      n2 = (int) (*t / per);
      if (*t < 0.)
	{
	  --n2;
	}

      /*     add new point to the buffer */
      ++k;
      z__[k + 1] = *t;
      i__1 = nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  z__[n + 1 + (i__ - 1) * n + k] = u[i__ - 1];
	  /* L1: */
	}
      z__[1] = (double) k;
      if (n1 == n2 && k < n)
	{
	  *t = tsave;
	  return 0;
	}

      /*     plot 1:K points of the buffer */
      Xgc = scicos_set_win(wid,&cur);
      record= Xgc->graphic_engine->xget_recording(Xgc);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,ipar[2]);
      if (k > 0)
	{
	  i__1 = nu;
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      /*               if(ipar(3+i).ge.0.or.flag.eq.1) then */
	      Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[n + 2 +(i__ - 1) * n],&ipar[i__ + 3], c__1,k);
	      /* L10: */
	    }
	}
      /*     shift buffer left */
      z__[2] = z__[k + 1];
      i__1 = nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  z__[n + 1 + (i__ - 1) * n + 1] = z__[n + 1 + (i__ - 1) * n + k];
	  /* L15: */
	}
      z__[1] = 1.;
      if (n1 != n2)
	{
	  /*     clear window */
	  nax[0] = 2;
	  nax[1] = 10;
	  nax[2] = 2;
	  nax[3] = 10;
	  Xgc->graphic_engine->clearwindow(Xgc);
	  Xgc->graphic_engine->scale->xset_usecolor(Xgc,ipar[2]);
	  Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
	  rect[0] = per * (n1 + 1);
	  rect[1] = ymin;
	  rect[2] = per * (n1 + 2);
	  rect[3] = ymax;
	  Xgc->graphic_engine->scale->xset_dash(Xgc,c__0);
	  nsp_plot2d(Xgc,rect, &rect[1],&c__1, &c__1, &c_n1, "011","t@ @input and output",
		     0, rect, nax);
	}
      *t = tsave;
      Xgc->graphic_engine->xset_recording(Xgc,record);
    }
  else if (*flag__ == 4)
    {
      wid = ipar[1];
      n = ipar[3];
      ymin = rpar[2];
      ymax = rpar[3];
      per = rpar[4];
      nax[0] = 2;
      nax[1] = 10;
      nax[2] = 2;
      nax[3] = 10;
      n1 = (int) ((int) (*t) / per);
      if (*t <= 0.)
	{
	  --n1;
	}
      Xgc = scicos_set_win(wid,&cur);
      record= Xgc->graphic_engine->xget_recording(Xgc);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      if (ipar[iwp] >= 0)
	{
	  Xgc->graphic_engine->xset_windowpos(Xgc,ipar[iwp], ipar[iwp + 1]);
	}
      if (ipar[iwd] >= 0)
	{
	  Xgc->graphic_engine->xset_windowdim(Xgc,ipar[iwd], ipar[iwd + 1]);
	  /*     to force dimensions update */
	  Xgc = scicos_set_win(wid,&cur);
	}
      rect[0] = per * (n1 + 1);
      rect[1] = ymin;
      rect[2] = per * (n1 + 2);
      rect[3] = ymax;
      Nsetscale2d(Xgc,frect,NULL,rect,"nn");
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,ipar[2]);
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,c__3);
      Xgc->graphic_engine->clearwindow(Xgc);
      Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
      Xgc->graphic_engine->scale->xset_dash(Xgc,c__0);
      nsp_plot2d(Xgc,rect, &rect[1],&c__1, &c__1, &c_n1, "011","t@ @input and output",
		 0, rect, nax);
      z__[1] = 0.;
      z__[2] = *t;
      i__1 = nu * n;
      nsp_dset (&i__1, &c_b88, &z__[3], &c__1);
      Xgc->graphic_engine->xset_recording(Xgc,record);
    }
  else if (*flag__ == 5)
    {
      wid = ipar[1];
      n = ipar[3];
      k = (int) z__[1];
      if (k <= 1)
	{
	  return 0;
	}
      Xgc = scicos_set_win(wid,&cur);
      record= Xgc->graphic_engine->xget_recording(Xgc);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,ipar[2]);
      i__1 = nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  i__2 = k - 1;
	  Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[n + 2 + (i__ - 1) * n],&ipar[i__ + 3], c__1, i__2);
	}
      Xgc->graphic_engine->xset_recording(Xgc,record);
    }
  return 0;
}			


int scicos_fsv (scicos_args_poo) 
{
  double d__1, d__2;
  double a, g, a0, b0;
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  a = u[1];
  y[1] = 0.;
  if (a > 1.)
    {
      return 0;
    }
  g = 1.4;
  a0 = 2. / g;
  b0 = (g + 1) / g;
  if (a < .528)
    {
      d__1 = 2 / (g + 1.);
      d__2 = g / (g - 1.);
      a = pow_dd (&d__1, &d__2);
    }
  y[1] = sqrt (g * 2. * (pow_dd (&a, &a0) - pow_dd (&a, &b0)) / (g - 1.));

  return 0;
}			



int scicos_gensin (scicos_args_poo)
{
  /* Builtin functions */
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     Sine genrator */
  /*     ampl=rpar(1) */
  /*     freq=rpar(2) */
  /*     phase=rpar(3) */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  y[1] = rpar[1] * sin (rpar[2] * *t + rpar[3]);
  return 0;
}			



int scicos_gensqr (scicos_args_poo)
{
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     Square wave generator */
  /*     period=2*rpar(1) */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 2)
    {
      z__[1] = -z__[1];
    }
  else if (*flag__ == 1 || *flag__ == 6)
    {
      y[1] = z__[1];
    }
  return 0;
}			



int scicos_hltblk (scicos_args_poo)
{
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     Notify simulation to stop  when called */
  /*     ipar(1) : stop reference */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 2)
    {
      C2F(coshlt).halt = 1;
      if (*nipar > 0)
	{
	  z__[1] = (double) ipar[1];
	}
      else
	{
	  z__[1] = 0.;
	}
    }
  return 0;
}			



int
scicos_ifthel (int *flag__, int *nevprt, int *ntvec, double *rpar, int *nrpar,
	       int *ipar, int *nipar, double *u, int *nu)
{
  /*     Scicos block simulator */
  /*     if-then-else block */
  /*     if event input exits from then or else clock ouputs based */
  /*     on the sign of the unique input (if input>0 then  else ) */
  /*     Copyright INRIA */
  --u;
  --ipar;
  --rpar; 
  if (C2F(dbcos).idb == 1)
    {
      sciprint("ifthel: flag=%d\n",*flag__);
    }

  if (*flag__ == 3)
    {
      if (u[1] <= 0.)
	{
	  *ntvec = 2;
	}
      else
	{
	  *ntvec = 1;
	}
    }
  return 0;
}			


int
scicos_integr (scicos_args_poo)
{
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     Integrator */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 1 || *flag__ == 6)
    {
      y[1] = x[1];
    }
  else if (*flag__ == 0)
    {
      xd[1] = u[1];
    }
  return 0;
}			




int
scicos_intplt (scicos_args_poo)
{
  int np;

  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     y=f(t) for f a tabulated function from R to R^ny */

  /*     ipar(1)             : np number of mesh points */
  /*     rpar(1:ny+1,1:np) : matrix of mesh point coordinates */
  /*                       first row contains t coordinate mesh points */
  /*                       next rows contains y coordinates mesh points */
  /*                       (one row for each output) */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  np = ipar[1];
  Scierror("Error: intp to be done \n");
  /* 
     scicos_intp (t, &rpar[1], &rpar[np + 1], ny, &np, &y[1]);
  */
  return 0;
}			


int scicos_intpol (scicos_args_poo)
{
  int np;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     y=f(u) for f a tabulated function from R to R^ny */
  /*     rpar(1:ny+1,1:np) : matrix of mesh point coordinates */
  /*                       first row contains u coordinate mesh points */
  /*                       next rows contains y coordinates mesh points */
  /*                       (one row for each output) */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  np = ipar[1];
  sciprint("scicos_intp to be done \n");
  /* scicos_intp (&u[1], &rpar[1], &rpar[np + 1], ny, &np, &y[1]); */
  return 0;
}			



int scicos_intrp2(int *flag__, int *nevprt, double *t, double *xd, double *x,
		  int *nx, double *z__, int *nz, double *tvec, int *ntvec,
		  double *rpar, int *nrpar, int *ipar, int *nipar, double *u1,
		  int *nu1, double *u2, int *nu2, double *y1, int *ny1)
{
  int i__1;
  int i__, j;
  double vx1, vx2, vy1, vy2, vz1, vz2, vz3, vz4;

  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     ipar(1) : the number of input */



  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  i__1 = ipar[1];
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      if (*u1 <= rpar[i__])
	{
	  goto L200;
	}
      /* L100: */
    }
  i__ = ipar[1];
 L200:
  i__1 = ipar[2];
  for (j = 2; j <= i__1; ++j)
    {
      if (*u2 <= rpar[j + ipar[1]])
	{
	  goto L400;
	}
      /* L300: */
    }
  j = ipar[2];
 L400:
  vy1 = rpar[ipar[1] + j - 1];
  vy2 = rpar[ipar[1] + j];
  vz1 = rpar[ipar[1] + ipar[2] + (i__ - 2) * ipar[2] + j - 1];
  vz4 = rpar[ipar[1] + ipar[2] + (i__ - 2) * ipar[2] + j];
  vz2 = rpar[ipar[1] + ipar[2] + (i__ - 1) * ipar[2] + j - 1];
  vz3 = rpar[ipar[1] + ipar[2] + (i__ - 1) * ipar[2] + j];
  vx1 = rpar[i__ - 1];
  vx2 = rpar[i__];
  *y1 =
    (1. - (*u2 - vy1) / (vy2 - vy1)) * (vz1 +
					(vz2 - vz1) * (*u1 - vx1) / (vx2 -
								     vx1)) +
    (*u2 - vy1) / (vy2 - vy1) * (vz4 +
				 (vz3 - vz4) * (*u1 - vx1) / (vx2 - vx1));
  return 0;
}			



int scicos_intrpl (scicos_args_poo) 
{
  int i__1;
  int i__;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     ipar(1) : the number of input */
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  i__1 = *nrpar / 2;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      if (*u <= rpar[i__])
	{
	  goto L200;
	}
      /* L100: */
    }
  i__ = *nrpar / 2;
 L200:
  *y =
    rpar[*nrpar / 2 + i__ - 1] + (rpar[*nrpar / 2 + i__] -
				  rpar[*nrpar / 2 + i__ - 1]) / (rpar[i__] -
								 rpar[i__ -
								      1]) *
    (*u - rpar[i__ - 1]);
  return 0;
}			



int scicos_invblk (scicos_args_poo)
{
  int i__1;
  int i__;
  double ww;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     Outputs the inverse of the input */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 6)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  ww = u[i__];
	  if (ww != 0.)
	    {
	      y[i__] = 1. / ww;
	    }
	  /* L10: */
	}
    }

  if (*flag__ == 1)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  ww = u[i__];
	  if (ww != 0.)
	    {
	      y[i__] = 1. / ww;
	    }
	  else
	    {
	      *flag__ = -2;
	      return 0;
	    }
	  /* L15: */
	}
    }
  return 0;
}			


int scicos_iocopy (scicos_args_poo)
{
  int i__1;
  int i__;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (C2F(dbcos).idb == 1)
    {
      sciprint("ifthel: t=%10.3f, flag=%d\n",*t,*flag__);
    }

  i__1 = *nu;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      y[i__] = u[i__];
      /* L15: */
    }
  return 0;
}			



int scicos_logblk (scicos_args_poo)
{
  int i__1;
  int i__;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     y=log(u)/log(rpar(1)) */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 1)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  if (u[i__] > 0.)
	    {
	      y[i__] = log (u[i__]) / log (rpar[1]);
	    }
	  else
	    {
	      *flag__ = -2;
	      return 0;
	    }
	  /* L15: */
	}
    }
  if (*flag__ == 6)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  if (u[i__] > 0.)
	    {
	      y[i__] = log (u[i__]) / log (rpar[1]);
	    }
	  /* L20: */
	}
    }
  return 0;
}			

int scicos_lookup (scicos_args_poo)
{
  int i__1;
  double dout;
  int i__, n;
  double du;

  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     rpar(1:n)  =  u coordinate discretisation must be strictly increasing */
  /*     rpar(n+1:2*n)  =  y coordinate discretisation */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  n = *nrpar / 2;
  if (n > 2)
    {
      i__1 = n - 1;
      for (i__ = 2; i__ <= i__1; ++i__)
	{
	  if (u[1] <= rpar[i__])
	    {
	      goto L20;
	    }
	  /* L10: */
	}
    }
  else
    {
      if (n == 1)
	{
	  y[1] = rpar[2];
	  return 0;
	}
      i__ = 2;
    }
 L20:
  du = rpar[i__] - rpar[i__ - 1];
  dout = rpar[n + i__] - rpar[n + i__ - 1];
  y[1] = rpar[n + i__] - (rpar[i__] - u[1]) * dout / du;
  return 0;
}			



int scicos_lsplit (scicos_args_poo)
{

  int i__1, i__2;
  int i__, j, k;

  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     splitting signals */
  /* c */

  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  j = 0;
  i__1 = *ny / *nu;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *nu;
      for (k = 1; k <= i__2; ++k)
	{
	  ++j;
	  y[j] = u[k];
	  /* L1: */
	}
      /* L2: */
    }
  return 0;
}			


int scicos_lusat (int *flag__, int *nevprt, double *t, double *xd, double *x,
		  int *nx, double *z__, int *nz, double *tvec, int *ntvec,
		  double *rpar, int *nrpar, int *ipar, int *nipar, double *u,
		  int *nu, double *y, int *ny, double *g, int *ng)
{
  int i__1;
  int i__;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     Lower-Upper saturation */
  /*     Continous block, MIMO */
  --g;
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 9)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  g[i__] = u[i__] - rpar[1];
	  g[i__ + *nu] = u[i__] - rpar[2];
	  /* L10: */
	}
    }
  if (*flag__ == 1)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  if (u[i__] <= rpar[1])
	    {
	      y[i__] = rpar[1] * rpar[3];
	    }
	  else if (u[i__] >= rpar[2])
	    {
	      y[i__] = rpar[2] * rpar[3];
	    }
	  else
	    {
	      y[i__] = rpar[3] * u[i__];
	    }
	  /* L15: */
	}
    }
  return 0;
}			


int scicos_maxblk (scicos_args_poo)
{
  int i__1;
  double d__1, d__2;
  int i__;
  double ww;

  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     outputs the maximum of all inputs */

  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  ww = u[1];
  i__1 = *nu;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* Computing MAX */
      d__1 = ww, d__2 = u[i__];
      ww = Max (d__1, d__2);
      /* L15: */
    }
  y[1] = ww;
  return 0;
}			




int scicos_memo (scicos_args_poo) 
{
  int i__1;
  int i__;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     returns sample and hold  of the input */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 2)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  y[i__] = u[i__];
	  /* L15: */
	}
    }
  else if (*flag__ == 4)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  y[i__] = rpar[i__];
	  /* L25: */
	}
    }
  return 0;
}		


int scicos_mfclck (scicos_args_poo)
{
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     multifrequency clock */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 4)
    {
      z__[1] = 0.;
    }
  else if (*flag__ == 2)
    {
      z__[1] += 1.;
      if (z__[1] == (double) ipar[1])
	{
	  z__[1] = 0.;
	}
    }
  else if (*flag__ == 3)
    {
      if (z__[1] == (double) (ipar[1] - 1))
	{
	  tvec[1] = *t - 1.;
	  tvec[2] = *t + rpar[1];
	}
      else
	{
	  tvec[1] = *t + rpar[1];
	  tvec[2] = *t - 1.;
	}
    }
  return 0;
}			

int scicos_minblk (scicos_args_poo)
{
  int i__1;
  double d__1, d__2;
  int i__;
  double ww;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     outputs the minimum of all inputs */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  ww = u[1];
  i__1 = *nu;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* Computing MIN */
      d__1 = ww, d__2 = u[i__];
      ww = Min (d__1, d__2);
      /* L15: */
    }
  y[1] = ww;
  return 0;
}			



int scicos_mscope (scicos_args_poo)
{
  static int c__1 = 1;
  static int c__0 = 0;
  static int c_n1 = -1;
  static int c__3 = 3;
  static double c_b103 = 0.;  static int cur = 0;

  int i__1, i__2, i__3;
  BCG *Xgc;
  double rect[4];
  int kwid;
  int nwid;
  int i__, k, n;
  double frect[4], tsave;
  int n1, n2;
  double dt;
  int it;
  int wid, iwd;
  double per;
  int nax[4], ilt, iwp;
  int herited;
  int record;
  char *str;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     ipar(1) = win_num */
  /*     ipar(2) = number of subwindows (input ports) */
  /*     ipar(3) = buffer size */
  /*     ipar(4:5) : window position */
  /*     ipar(6:7) : window dimension */
  /*     ipar(8:7+ipar(2)) = input port sizes */
  /*     ipar(8+ipar(2):7+ipar(2)+nu) = line type for ith curve */
  /*     ipar(8+ipar(2)+nu) = acceptance of inherited events */
  /*     rpar(1)=dt */
  /*     rpar(2)=periode */
  /*     rpar(3)=ymin_1 */
  /*     rpar(4)=ymax_1 */
  /*     ... */
  /*     rpar(2*k+1)=ymin_k */
  /*     rpar(2*k+2)=ymax_k */


  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;

  wid = ipar[1];
  nwid = ipar[2];
  n = ipar[3];
  per = rpar[2];
  dt = rpar[1];
  if (*nipar < ipar[2] + 8 + *nu)
    {
      /*     compatibility */
      herited = TRUE;
    }
  else
    {
      herited = ipar[ipar[2] + 8 + *nu] != 0;
    }

  if (*flag__ <= 2)
    {
      k = (int) z__[1];
      if (k > 0)
	{
	  n1 = (int) (z__[k + 1] / per);
	  if (z__[k + 1] < 0.)
	    {
	      --n1;
	    }
	}
      else
	{
	  n1 = 0;
	}

      tsave = *t;
      if (dt > 0.)
	{
	  *t = z__[k + 1] + dt;
	}

      n2 = (int) (*t / per);
      if (*t < 0.)
	{
	  --n2;
	}

      /*     add new point to the buffer */
      ++k;
      z__[k + 1] = *t;
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  z__[n + 1 + (i__ - 1) * n + k] = u[i__];
	  /* L5: */
	}
      z__[1] = (double) k;
      if (n1 == n2 && k < n)
	{
	  *t = tsave;
	  return 0;
	}

      /*     plot 1:K points of the buffer */
      Xgc = scicos_set_win(wid,&cur);
      record= Xgc->graphic_engine->xget_recording(Xgc);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,1);
      Xgc->graphic_engine->scale->xset_dash(Xgc,c__0);
      ilt = ipar[2] + 8;
      it = 0;
      /*     loop on input ports */
      if (k > 0)
	{
	  i__1 = nwid;
	  for (kwid = 1; kwid <= i__1; ++kwid)
	    {
	      rect[0] = per * n1;
	      rect[1] = rpar[(kwid << 1) + 1];
	      rect[2] = per * (n1 + 1);
	      rect[3] = rpar[(kwid << 1) + 2];
	      frect[0] = 0.;
	      frect[1] = (kwid - 1) * (1. / nwid);
	      frect[2] = 1.;
	      frect[3] = 1. / nwid;
	      Nsetscale2d(Xgc,frect,NULL,rect,"nn");
	      Xgc->graphic_engine->scale->xset_clipgrf(Xgc);
	      /*     loop on input port elements */
	      i__2 = ipar[kwid + 7];
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[n + 2 + it * n],&ipar[ilt + it], c__1, k);
		  ++it;
		  /* L10: */
		}
	      Xgc->graphic_engine->scale->xset_unclip(Xgc);
	    }
	}
      /*     shift buffer left */
      z__[2] = z__[k + 1];
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  z__[n + 1 + (i__ - 1) * n + 1] = z__[n + 1 + (i__ - 1) * n + k];
	  /* L15: */
	}
      z__[1] = 1.;
      if (n1 != n2)
	{
	  /*     clear window */
	  nax[0] = 2;
	  nax[1] = 10;
	  nax[2] = 2;
	  nax[3] = 10;
	  Xgc->graphic_engine->clearwindow(Xgc);
	  Xgc->graphic_engine->scale->xset_usecolor(Xgc,1);
	  Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
	  Xgc->graphic_engine->scale->xset_dash(Xgc,c__0);
	  i__1 = nwid;
	  for (kwid = 1; kwid <= i__1; ++kwid)
	    {
	      rect[0] = per * (n1 + 1);
	      rect[1] = rpar[(kwid << 1) + 1];
	      rect[2] = per * (n1 + 2);
	      rect[3] = rpar[(kwid << 1) + 2];
	      frect[0] = 0.;
	      frect[1] = (kwid - 1) * (1. / nwid);
	      frect[2] = 1.;
	      frect[3] = 1. / nwid;
	      Nsetscale2d(Xgc,frect,NULL,rect,"nn");
	      nsp_plot2d(Xgc,rect, &rect[1],&c__1, &c__1, &c_n1, "011","t@ @input and output",
			 0, rect, nax);
	    }
	}
      *t = tsave;
      Xgc->graphic_engine->xset_recording(Xgc,record);
    }
  else if (*flag__ == 4)
    {
      nax[0] = 2;
      nax[1] = 10;
      nax[2] = 2;
      nax[3] = 10;
      n1 = (int) ((int) (*t) / per);
      if (*t <= 0.)
	{
	  --n1;
	}
      Xgc = scicos_set_win(wid,&cur);
      record= Xgc->graphic_engine->xget_recording(Xgc);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);

      iwp = 4;
      if (ipar[iwp] >= 0)
	{
	  Xgc->graphic_engine->xset_windowpos(Xgc,ipar[iwp], ipar[iwp + 1]);
	}
      iwd = 6;
      if (ipar[iwd] >= 0)
	{
	  Xgc->graphic_engine->xset_windowdim(Xgc,ipar[iwd], ipar[iwd + 1]);
	}
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,1);
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,c__3);
      Xgc->graphic_engine->clearwindow(Xgc);
      Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
      Xgc->graphic_engine->scale->xset_dash(Xgc,c__0);
      str = scicos_getlabel (C2F(curblk).kfun);
      if ( str != NULL && strlen(str) != 0 && strcmp(str," ") != 0 ) 
	Xgc->graphic_engine->setpopupname(Xgc,str);
      i__1 = nwid;
      for (kwid = 1; kwid <= i__1; ++kwid)
	{
	  rect[0] = per * (n1 + 1);
	  rect[1] = rpar[(kwid << 1) + 1];
	  rect[2] = per * (n1 + 2);
	  rect[3] = rpar[(kwid << 1) + 2];
	  frect[0] = 0.;
	  frect[1] = (kwid - 1) * (1. / nwid);
	  frect[2] = 1.;
	  frect[3] = 1. / nwid;
	  Nsetscale2d(Xgc,frect,NULL,rect,"nn");
	  nsp_plot2d(Xgc,rect, &rect[1],&c__1, &c__1, &c_n1, "011","t@ @input and output",
		     0, rect, nax);
	}

      z__[1] = 0.;
      z__[2] = *t;
      i__1 = *nu * n;
      nsp_dset (&i__1, &c_b103, &z__[3], &c__1);
      Xgc->graphic_engine->xset_recording(Xgc,record);
    }
  else if (*flag__ == 5)
    {
      k = (int) z__[1];
      if (k <= 1)
	{
	  return 0;
	}
      Xgc = scicos_set_win(wid,&cur);
      record= Xgc->graphic_engine->xget_recording(Xgc);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,1);
      ilt = ipar[2] + 8;
      it = 0;
      n1 = (int) ((int) (*t) / per);
      if (*t <= 0.)
	{
	  --n1;
	}
      /*     loop on input ports */
      i__1 = nwid;
      for (kwid = 1; kwid <= i__1; ++kwid)
	{
	  rect[0] = per * (n1 + 1);
	  rect[1] = rpar[(kwid << 1) + 1];
	  rect[2] = per * (n1 + 2);
	  rect[3] = rpar[(kwid << 1) + 2];
	  frect[0] = 0.;
	  frect[1] = (kwid - 1) * (1. / nwid);
	  frect[2] = 1.;
	  frect[3] = 1. / nwid;
	  Nsetscale2d(Xgc,frect,NULL,rect,"nn");
	  Xgc->graphic_engine->scale->xset_clipgrf(Xgc);
	  /*     loop on input port elements */
	  i__2 = ipar[kwid + 7];
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      i__3 = k - 1;
	      Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[n + 2 + it * n],&ipar[ilt + it], c__1, i__3);
	      ++it;
	    }
	  Xgc->graphic_engine->scale->xset_unclip(Xgc);
	}
      Xgc->graphic_engine->xset_recording(Xgc,record);
    }
  return 0;
}		



int scicos_mux (int *flag__, int *nevprt, double *t, double *xd, double *x,
		int *nx, double *z__, int *nz, double *tvec, int *ntvec,
		double *rpar, int *nrpar, int *ipar, int *nipar, double *uy1,
		int *nuy1, double *uy2, int *nuy2, double *uy3, int *nuy3,
		double *uy4, int *nuy4, double *uy5, int *nuy5, double *uy6,
		int *nuy6, double *uy7, int *nuy7, double *uy8, int *nuy8,
		double *uy9, int *nuy9)
{
  int i__1;
  int i__, k;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     ipar(1) : the number of input */
  --uy9;
  --uy8;
  --uy7;
  --uy6;
  --uy5;
  --uy4;
  --uy3;
  --uy2;
  --uy1;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  k = 0;
  switch (ipar[1] - 1)
    {
    case 1:
      goto L20;
    case 2:
      goto L30;
    case 3:
      goto L40;
    case 4:
      goto L50;
    case 5:
      goto L60;
    case 6:
      goto L70;
    case 7:
      goto L80;
    }

 L20:
  i__1 = *nuy1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy3[k] = uy1[i__];
      /* L25: */
    }
  i__1 = *nuy2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy3[k] = uy2[i__];
      /* L27: */
    }
  return 0;

 L30:
  i__1 = *nuy1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy4[k] = uy1[i__];
      /* L35: */
    }
  i__1 = *nuy2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy4[k] = uy2[i__];
      /* L37: */
    }
  i__1 = *nuy3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy4[k] = uy3[i__];
      /* L38: */
    }
  return 0;

 L40:
  i__1 = *nuy1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy5[k] = uy1[i__];
      /* L41: */
    }
  i__1 = *nuy2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy5[k] = uy2[i__];
      /* L42: */
    }
  i__1 = *nuy3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy5[k] = uy3[i__];
      /* L43: */
    }
  i__1 = *nuy4;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy5[k] = uy4[i__];
      /* L44: */
    }
  return 0;

 L50:
  i__1 = *nuy1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy6[k] = uy1[i__];
      /* L51: */
    }
  i__1 = *nuy2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy6[k] = uy2[i__];
      /* L52: */
    }
  i__1 = *nuy3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy6[k] = uy3[i__];
      /* L53: */
    }
  i__1 = *nuy4;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy6[k] = uy4[i__];
      /* L54: */
    }
  i__1 = *nuy5;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy6[k] = uy5[i__];
      /* L55: */
    }
  return 0;

 L60:
  i__1 = *nuy1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy7[k] = uy1[i__];
      /* L61: */
    }
  i__1 = *nuy2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy7[k] = uy2[i__];
      /* L62: */
    }
  i__1 = *nuy3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy7[k] = uy3[i__];
      /* L63: */
    }
  i__1 = *nuy4;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy7[k] = uy4[i__];
      /* L64: */
    }
  i__1 = *nuy5;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy7[k] = uy5[i__];
      /* L65: */
    }
  i__1 = *nuy6;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy7[k] = uy6[i__];
      /* L66: */
    }
  return 0;

 L70:
  i__1 = *nuy1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy8[k] = uy1[i__];
      /* L71: */
    }
  i__1 = *nuy2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy8[k] = uy2[i__];
      /* L72: */
    }
  i__1 = *nuy3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy8[k] = uy3[i__];
      /* L73: */
    }
  i__1 = *nuy4;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy8[k] = uy4[i__];
      /* L74: */
    }
  i__1 = *nuy5;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy8[k] = uy5[i__];
      /* L75: */
    }
  i__1 = *nuy6;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy8[k] = uy6[i__];
      /* L76: */
    }
  i__1 = *nuy7;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy8[k] = uy7[i__];
      /* L77: */
    }
  return 0;

 L80:
  i__1 = *nuy1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy9[k] = uy1[i__];
      /* L81: */
    }
  i__1 = *nuy2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy9[k] = uy2[i__];
      /* L82: */
    }
  i__1 = *nuy3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy9[k] = uy3[i__];
      /* L83: */
    }
  i__1 = *nuy4;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy9[k] = uy4[i__];
      /* L84: */
    }
  i__1 = *nuy5;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy9[k] = uy5[i__];
      /* L85: */
    }
  i__1 = *nuy6;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy9[k] = uy6[i__];
      /* L86: */
    }
  i__1 = *nuy7;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy9[k] = uy7[i__];
      /* L87: */
    }
  i__1 = *nuy8;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++k;
      uy9[k] = uy8[i__];
      /* L88: */
    }
  return 0;
}	


int scicos_pload ( scicos_args_poo) 
{
  int i__1;
  int i__;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     Preload function */
  /*     if u(i).lt.0 then y(i)=-u(i)-rpar(i) */
  /*     else y(i)=u(i)+rpar(i) */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  /* L10: */
  i__1 = *nu;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (u[i__] < 0.)
	{
	  y[i__] = u[i__] - rpar[i__];
	}
      else if (u[i__] > 0.)
	{
	  y[i__] = u[i__] + rpar[i__];
	}
      else
	{
	  y[i__] = 0.;
	}
      /* L15: */
    }
  return 0;
}			


int scicos_powblk (scicos_args_poo)
{
  int i__1;
  int i__;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     rpar(1) is power */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*nrpar == 1)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  if (u[i__] < 0.)
	    {
	      if (*flag__ >= 4)
		{
		  return 0;
		}
	      *flag__ = -2;
	      return 0;
	    }
	  else if (u[i__] == 0. && rpar[1] <= 0.)
	    {
	      if (*flag__ >= 4)
		{
		  return 0;
		}
	      *flag__ = -2;
	      return 0;
	    }
	  y[i__] = pow_dd (&u[i__], &rpar[1]);
	  /* L15: */
	}
    }
  else
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  if (ipar[1] <= 0 && u[i__] == 0.)
	    {
	      if (*flag__ >= 4)
		{
		  return 0;
		}
	      *flag__ = -2;
	      return 0;
	    }
	  y[i__] = pow_di (&u[i__], &ipar[1]);
	  /* L25: */
	}
    }
  return 0;
}			


int scicos_qzcel (scicos_args_poo) 
{

  int i__1;
  double d__1;

  /* Builtin functions */
  double d_nint (double *);
  int i__;

  /*     Copyright INRIA */
  /*     Scicos block simulator */

  /*     Gives quantized signal by ceiling method */
  /*     rpar(i) quantization step used for i input */


  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  i__1 = *nu;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      d__1 = u[i__] / rpar[i__] - .5;
      y[i__] = rpar[i__] * d_nint (&d__1);
      /* L15: */
    }
  return 0;
}			


int scicos_qzflr (scicos_args_poo) 
{

  int i__1;
  double d__1;

  /* Builtin functions */
  double d_nint (double *);
  int i__;

  /*     Copyright INRIA */
  /*     Scicos block simulator */

  /*     Gives quantized signal by floor method */
  /*     rpar(i) quantization step used for i input */


  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  i__1 = *nu;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      d__1 = u[i__] / rpar[i__] + .5;
      y[i__] = rpar[i__] * d_nint (&d__1);
      /* L15: */
    }
  return 0;
}			



int scicos_qzrnd (scicos_args_poo)
{

  int i__1;
  double d__1;

  /* Builtin functions */
  double d_nint (double *);
  int i__;

  /*     Copyright INRIA */
  /*     Scicos block simulator */

  /*     Gives quantized signal by round method */
  /*     rpar(i) quantization step used for i input */


  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  i__1 = *nu;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (u[i__] < 0.)
	{
	  d__1 = u[i__] / rpar[i__] + .5;
	  y[i__] = rpar[i__] * (d_nint (&d__1) - .5);
	}
      else
	{
	  d__1 = u[i__] / rpar[i__] - .5;
	  y[i__] = rpar[i__] * (d_nint (&d__1) + .5);
	}
      /* L15: */
    }
  return 0;
}			




int scicos_qztrn (scicos_args_poo)
{

  int i__1;
  int i__;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     Gives quantized signal by truncation method */
  /*     rpar(i) quantization step used for i input */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  i__1 = *nu;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (u[i__] < 0.)
	{
	  y[i__] = rpar[i__] * anint(u[i__] / rpar[i__] + .5);
	}
      else
	{
	  y[i__] = rpar[i__] * anint(u[i__] / rpar[i__] - .5);

	}
      /* L15: */
    }
  return 0;
}			


/*     Copyright INRIA */
/*     Scicos block simulator */
/*     ipar(1) */
/*            0 : uniform */
/*            1 : normal */
/*     rpar(1:ny)=mean */
/*     rpar(ny+1:2*ny)=deviation */
/*     rpar(2*ny+1)=dt */

int scicos_rndblk (scicos_args_poo)
{

  int i__1;
  int i__;
  double t1, si;
  int iy;
  double sr;
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 1 || *flag__ == 6)
    {
      i__1 = *ny;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  y[i__] = rpar[i__] + rpar[*ny + i__] * z__[i__ + 1];
	  /* L10: */
	}
    }
  else if (*flag__ == 2 || *flag__ == 4)
    {
      /*     uniform */
      if (ipar[1] == 0)
	{
	  iy = (int) z__[1];
	  i__1 = *nz - 1;
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      z__[i__ + 1] = nsp_urand(&iy);
	    }
	}
      else
	{
	  iy = (int) z__[1];
	  /*     normal */
	  i__1 = *nz - 1;
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	    L75:
	      sr = nsp_urand(&iy) * 2. - 1.;
	      si = nsp_urand(&iy) * 2. - 1.;
	      t1 = sr * sr + si * si;
	      if (t1 > 1.)
		{
		  goto L75;
		}
	      z__[i__ + 1] = sr * sqrt (log (t1) * -2. / t1);
	      /* L30: */
	    }
	}
      z__[1] = (double) iy;
      /*         if(ntvec.eq.1) tvec(1)=t+rpar(2*(nz-1)+1) */
    }
  return 0;
}			



int scicos_samphold (scicos_args_poo) 
{

  int i__1;
  int i__;

  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     returns sample and hold  of the input */


  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 1)
    {
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  y[i__] = u[i__];
	  /* L15: */
	}
    }
  return 0;
}			


int scicos_sawtth (scicos_args_poo)
{
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 1 && *nevprt == 0)
    {
      y[1] = *t - z__[1];
    }
  else if (*flag__ == 1 && *nevprt == 1)
    {
      y[1] = 0.;
    }
  else if (*flag__ == 2 && *nevprt == 1)
    {
      z__[1] = *t;
    }
  else if (*flag__ == 4)
    {
      z__[1] = 0.;
    }
  return 0;
}			


int
scicos_scope (int *flag__, int *nevprt, double *t, double *xd, double *x,
	      int *nx, double *z__, int *nz, double *tvec, int *ntvec,
	      double *rpar, int *nrpar, int *ipar, int *nipar, double *u,
	      int *nu)
{
  BCG *Xgc;
  int nax[]={2,10,2,10};
  static int c__1 = 1;
  static int c__0 = 0;
  static int c_n1 = -1;
  static int c__3 = 3;
  static double c_b86 = 0.;
  static double frect[4] = { 0., 0., 1., 1. };
  static int cur = 0;
  int i__1;
  char *str;
  double rect[4];
  double ymin, ymax;
  int i__, k, n;
  double tsave;
  int n1, n2;
  double dt;
  int wid, iwd;
  double per;
  int iwp;
  int herited;

  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     ipar(1) = win_num */
  /*     ipar(2) = 0/1 color flag */
  /*     ipar(3) = buffer size */
  /*     ipar(4:11) = line type for ith curve */
  /*     ipar(12:13) : window position */
  /*     ipar(14:15) : window dimension */
  /*     ipar(16) : acceptance of inherited events */
  /*     rpar(1)=dt */
  /*     rpar(2)=ymin */
  /*     rpar(3)=ymax */
  /*     rpar(4)=periode */
  /*      character*(4) logf */
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;

  if (*nipar < 16)
    {
      /*     compatibility */
      herited = TRUE;
      iwp = *nipar - 3;
      iwd = *nipar - 1;
    }
  else
    {
      herited = ipar[16] != 0;
      iwp = *nipar - 4;
      iwd = *nipar - 2;
    }

  if (*flag__ == 2)
    {
      dt = rpar[1];
      ymin = rpar[2];
      ymax = rpar[3];
      per = rpar[4];
      wid = ipar[1];
      n = ipar[3];
      k = (int) z__[1];
      if (k > 0)
	{
	  n1 = (int) (z__[k + 1] / per);
	  if (z__[k + 1] < 0.)
	    {
	      --n1;
	    }
	}
      else
	{
	  n1 = 0;
	}

      tsave = *t;
      if (dt > 0.)
	{
	  *t = z__[k + 1] + dt;
	}

      n2 = (int) (*t / per);
      if (*t < 0.)
	{
	  --n2;
	}

      /*     add new point to the buffer */
      ++k;
      z__[k + 1] = *t;
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  z__[n + 1 + (i__ - 1) * n + k] = u[i__];
	  /* L1: */
	}
      z__[1] = (double) k;
      if (n1 == n2 && k < n)
	{
	  *t = tsave;
	  return 0;
	}

      /*     plot 1:K points of the buffer */
      Xgc = scicos_set_win(wid,&cur);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      /*         call dr1('xset'//char(0),'use color'//char(0),ipar(2),0,0, */
      /*     &        0,0,v,dv,dv,dv,dv) */
      if (k > 0)
	{
	  Xgc->graphic_engine->scale->xset_clipgrf(Xgc);
	  i__1 = *nu;
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      /*               if(ipar(3+i).ge.0.or.flag.eq.1) then */
	      Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[n + 2 + (i__ - 1) * n],&ipar[i__ + 3], c__1,k);
	    }
	  Xgc->graphic_engine->scale->xset_unclip(Xgc);
	}
      /*     shift buffer left */
      z__[2] = z__[k + 1];
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  z__[n + 1 + (i__ - 1) * n + 1] = z__[n + 1 + (i__ - 1) * n + k];
	  /* L15: */
	}
      z__[1] = 1.;
      if (n1 != n2)
	{
	  /*     clear window */
	  Xgc->graphic_engine->clearwindow(Xgc);
	  Xgc->graphic_engine->scale->xset_usecolor(Xgc,ipar[2]);
	  Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
	  rect[0] = per * (n1 + 1);
	  rect[1] = ymin;
	  rect[2] = per * (n1 + 2);
	  rect[3] = ymax;
	  Xgc->graphic_engine->scale->xset_dash(Xgc,c__0);
	  nsp_plot2d(Xgc,rect, &rect[1],&c__1, &c__1, &c_n1, "011","t@ @input and output",
		     0, rect, nax);
	}
      *t = tsave;

    }
  else if (*flag__ == 4)
    {
      wid = ipar[1];
      n = ipar[3];
      ymin = rpar[2];
      ymax = rpar[3];
      per = rpar[4];
      n1 = (int) ((int) (*t) / per);
      if (*t <= 0.)
	{
	  --n1;
	}
      Xgc = scicos_set_win(wid,&cur);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      if (ipar[iwp] >= 0)
	{
	  Xgc->graphic_engine->xset_windowpos(Xgc,ipar[iwp], ipar[iwp + 1]);
	}
      if (ipar[iwd] >= 0)
	{
	  Xgc->graphic_engine->xset_windowdim(Xgc,ipar[iwd], ipar[iwd + 1]);
	  /*     to force dimensions update */
	  Xgc = scicos_set_win(wid,&cur);
	}
      rect[0] = per * (n1 + 1);
      rect[1] = ymin;
      rect[2] = per * (n1 + 2);
      rect[3] = ymax;
      Nsetscale2d(Xgc,frect,NULL,rect,"nn");
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,ipar[2]);
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,c__3);
      Xgc->graphic_engine->clearwindow(Xgc);
      Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
      Xgc->graphic_engine->scale->xset_dash(Xgc,c__0);
      nsp_plot2d(Xgc,rect, &rect[1],&c__1, &c__1, &c_n1, "011","t@ @input and output",
		 0, rect, nax);
      str = scicos_getlabel (C2F(curblk).kfun);
      if ( str != NULL && strlen(str) != 0 && strcmp(str," ") != 0 ) 
	Xgc->graphic_engine->setpopupname(Xgc,str);
      z__[1] = 0.;
      z__[2] = *t;
      i__1 = *nu * n;
      nsp_dset (&i__1, &c_b86, &z__[3], &c__1);
    }
  else if (*flag__ == 5)
    {
      wid = ipar[1];
      n = ipar[3];
      k = (int) z__[1];
      if (k <= 1)
	{
	  return 0;
	}
      Xgc = scicos_set_win(wid,&cur);
      Xgc->graphic_engine->scale->xset_clipgrf(Xgc);
      i__1 = *nu;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[n + 2 + (i__ - 1) * n],&ipar[i__ + 3], c__1,k);
	}
      Xgc->graphic_engine->scale->xset_unclip(Xgc);
    }
  return 0;
}			


/* Table of constant values */


int scicos_scopxy (scicos_args_poo)
{
  BCG *Xgc;
  static int c__1 = 1;
  static int c__2 = 2;
  static int c__0 = 0;
  static int c__3 = 3;
  static int c_n1 = -1;
  static int c__6 = 6;
  static double frect[4] = { 0., 0., 1., 1. };
  static int cur = 0;
  double rect[4];
  double xmin, ymin, xmax, ymax;
  int n;
  int wid, iwd, nax[4], iwp;
  char *str;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     ipar(1) = win_num */
  /*     ipar(2) = 0/1 color flag */
  /*     ipar(3) = buffer size */
  /*     ipar(4) = dash,color or mark choice */
  /*     ipar(5) = line or mark size */
  /*     ipar(6) = mode : animated =0 fixed=1 */
  /*     ipar(7:8) = window position */
  /*     ipar(9:10) = window size */

  /*     rpar(1)=xmin */
  /*     rpar(2)=xmax */
  /*     rpar(3)=ymin */
  /*     rpar(4)=ymax */



  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;

  if (*flag__ == 2)
    {
      wid = ipar[1];
      n = ipar[3];

      Xgc = scicos_set_win(wid,&cur);
      /*     erase first point */
      if (ipar[6] == 0)
	{
	  z__[1] += 1.;
	  if (ipar[4] < 0)
	    {
	      Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[n + 2],&ipar[4], c__1, c__1);
	    }
	  else
	    {
	      Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[n + 2],&ipar[4], c__1, c__2);
	    }
	}
      /*     shift buffer left */
      /* i__1 = n - 1;
	 scicos_unsfdcopy (&i__1, &z__[3], &c__1, &z__[2], &c__1);
      */
      memmove(&z__[2], &z__[3],(n-1)*sizeof(double));
      z__[n + 1] = u[1];
      /* i__1 = n - 1;
	 scicos_unsfdcopy (&i__1, &z__[n + 3], &c__1, &z__[n + 2], &c__1);
      */
      memmove(&z__[n+2], &z__[n+3],(n-1)*sizeof(double));
      z__[(n << 1) + 1] = u[2];
      /*     draw new point */
      if (ipar[4] < 0)
	{
	  Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[n + 1], &z__[(n << 1) + 1],&ipar[4], c__1, c__1);
	}
      else
	{
	  Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[n], &z__[n*3],&ipar[4], c__1, c__2);
	}
      if ((int) z__[1] > n && ipar[6] == 0)
	{
	  /*     erase memory */
	  Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
	  xmin = rpar[1];
	  xmax = rpar[2];
	  ymin = rpar[3];
	  ymax = rpar[4];
	  rect[0] = xmin;
	  rect[1] = ymin;
	  rect[2] = xmax;
	  rect[3] = ymax;
	  Nsetscale2d(Xgc,frect,NULL,rect,"nn");
	  z__[1] = 0.;
	}
    }
  else if (*flag__ == 4)
    {
      wid = ipar[1];
      n = ipar[3];
      xmin = rpar[1];
      xmax = rpar[2];
      ymin = rpar[3];
      ymax = rpar[4];
      nax[0] = 2;
      nax[1] = 10;
      nax[2] = 2;
      nax[3] = 10;
      Xgc = scicos_set_win(wid,&cur);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      iwp = 7;
      if (ipar[iwp] >= 0)
	{
	  Xgc->graphic_engine->xset_windowpos(Xgc,ipar[iwp], ipar[iwp + 1]);
	}
      iwd = 9;
      if (ipar[iwd] >= 0)
	{
	  Xgc->graphic_engine->xset_windowdim(Xgc,ipar[iwd], ipar[iwd + 1]);
	}
      rect[0] = xmin;
      rect[1] = ymin;
      rect[2] = xmax;
      rect[3] = ymax;
      Nsetscale2d(Xgc,frect,NULL,rect,"nn");
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,ipar[2]);
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,c__3);
      Xgc->graphic_engine->clearwindow(Xgc);
      Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
      Xgc->graphic_engine->xset_thickness(Xgc,c__1);
      Xgc->graphic_engine->scale->xset_dash(Xgc,c__0);
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,c__3);
      nsp_plot2d(Xgc,rect, &rect[1],&c__1, &c__1, &c_n1, "011","t@ @input and output",
		 0, rect, nax);
      str = scicos_getlabel (C2F(curblk).kfun);
      if ( str != NULL && strlen(str) != 0 && strcmp(str," ") != 0 ) 
	Xgc->graphic_engine->setpopupname(Xgc,str);
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,c__6);
      nsp_check_gtk_events ();
      /* first point drawing */
      if (ipar[4] < 0)
	{
	  Xgc->graphic_engine->xset_mark(Xgc,ipar[4], ipar[5]);
	  Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[n+2],&ipar[4], c__1, c__1);
	}
      else
	{
	  Xgc->graphic_engine->xset_thickness(Xgc,ipar[5]);
	  Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[n+2],&ipar[4], c__1, c__2);
	}
      z__[1] = 0.;
    }
  else if (*flag__ == 5)
    {
      wid = ipar[1];
      n = ipar[3];
      Xgc = scicos_set_win(wid,&cur);
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,c__3);
    }
  return 0;
}


int scicos_scoxy (scicos_args_poo)
{
  BCG *Xgc;
  char *str;
  static int c__1 = 1;
  static int c__0 = 0;
  static int c__3 = 3;
  static int c_n1 = -1;
  /* Initialized data */
  static double frect[4] = { 0., 0., 1., 1. };
  static int cur = 0;
  double rect[4];
  double xmin, ymin, xmax, ymax;
  int n;
  int wid, iwd, nax[4], iwp;

  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     ipar(1) = win_num */
  /*     ipar(2) = 0/1 color flag */
  /*     ipar(3) = buffer size */
  /*     ipar(4) = dash,color or mark choice */
  /*     ipar(5) = line or mark size */
  /*     ipar(6) = mode : animated =0 fixed=1 */
  /*     ipar(7:8) = window position */
  /*     ipar(9:10) = window size */

  /*     rpar(1)=xmin */
  /*     rpar(2)=xmax */
  /*     rpar(3)=ymin */
  /*     rpar(4)=ymax */



  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;



  if (*flag__ == 6)
    {
      n = ipar[3];
      z__[2] = u[1];
      z__[n + 2] = u[2];
      z__[1] = 1.;
    }
  else if (*flag__ == 2)
    {
      wid = ipar[1];
      n = ipar[3];

      Xgc = scicos_set_win(wid,&cur);
      /*     shift buffer left */
      z__[(int) z__[1] + 2] = u[1];
      z__[n + 2 + (int) z__[1]] = u[2];
      z__[1] += 1.;
      if (z__[1] < (double) n)
	{
	  return 0;
	}
      /*     draw new point */
      if (ipar[4] < 0)
	{
	  Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[n + 2],&ipar[4], c__1,n);
	  z__[1] = 0.;
	}
      else
	{
	  Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[n + 2],&ipar[4], c__1,n);
	  z__[2] = z__[n + 1];
	  z__[n + 2] = z__[(n << 1) + 1];
	  z__[1] = 1.;
	}
    }
  else if (*flag__ == 4)
    {
      wid = ipar[1];
      n = ipar[3];
      xmin = rpar[1];
      xmax = rpar[2];
      ymin = rpar[3];
      ymax = rpar[4];
      nax[0] = 2;
      nax[1] = 10;
      nax[2] = 2;
      nax[3] = 10;
      Xgc = scicos_set_win(wid,&cur);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      iwp = 7;
      if (ipar[iwp] >= 0)
	{
	  Xgc->graphic_engine->xset_windowpos(Xgc,ipar[iwp], ipar[iwp + 1]);
	}
      iwd = 9;
      if (ipar[iwd] >= 0)
	{
	  Xgc->graphic_engine->xset_windowdim(Xgc,ipar[iwd], ipar[iwd + 1]);
	}
      rect[0] = xmin;
      rect[1] = ymin;
      rect[2] = xmax;
      rect[3] = ymax;
      Nsetscale2d(Xgc,frect,NULL,rect,"nn");
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,ipar[2]);
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,c__3);
      Xgc->graphic_engine->clearwindow(Xgc);
      Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
      Xgc->graphic_engine->xset_thickness(Xgc,c__1);
      Xgc->graphic_engine->scale->xset_dash(Xgc,c__0);
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,c__3);
      str = scicos_getlabel (C2F(curblk).kfun);
      if ( str != NULL && strlen(str) != 0 && strcmp(str," ") != 0 ) 
	Xgc->graphic_engine->setpopupname(Xgc,str);
      nsp_plot2d(Xgc,rect, &rect[1],&c__1, &c__1, &c_n1, "011","t@ @input and output",
		 0, rect, nax);
      Xgc->graphic_engine->xset_thickness(Xgc,ipar[5]);
      nsp_check_gtk_events ();
      /* first point drawing */
      z__[1] = 0.;
    }
  else if (*flag__ == 5)
    {
      wid = ipar[1];
      n = ipar[3];

      Xgc = scicos_set_win(wid,&cur);
      if (ipar[4] < 0)
	{
	  int i_1 = (int) z__[1];
	  Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[n + 2],&ipar[4], c__1,i_1);
	}
      else
	{
	  int i_1 = (int) z__[1];
	  Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[n + 2],&ipar[4], c__1,i_1);
	}
      z__[1] = 0.;
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,c__3);
    }
  return 0;
}			




int scicos_selblk (scicos_args_poo)
{
  int ic, nev;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     Selector block */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 2 && *nevprt > 0)
    {
      ic = 0;
      nev = *nevprt;
    L10:
      if (nev >= 1)
	{
	  ++ic;
	  nev /= 2;
	  goto L10;
	}
      z__[1] = (double) ic;
    }
  else if (*flag__ == 1 || *flag__ == 6)
    {
      y[1] = u[(int) z__[1]];
    }
  return 0;
}			


int scicos_sinblk (scicos_args_poo)
{
  int i;
  for (i = 0 ; i < *nu ; ++i)  y[i] = sin (u[i]);
  return 0;
}			

/*
 * y=sqrt(u);
 */


int scicos_sqrblk (scicos_args_poo)
{
  int i;
  for (i = 0; i < *nu ; ++i)
    {
      if (u[i] >= 0.)
	{
	  y[i] = sqrt (u[i]);
	}
      else
	{
	  *flag__ = -2;
	  return 0;
	}
    }
  return 0;
}			



int scicos_sum2(int *flag__, int *nevprt, double *t, double *xd, double *x,
		int *nx, double *z__, int *nz, double *tvec, int *ntvec,
		double *rpar, int *nrpar, int *ipar, int *nipar, double *u1,
		int *nu1, double *u2, int *nu2, double *y, int *ny)
{
  int i__1;
  int i__;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     adds the inputs weighed by rpar */
  --y;
  --u2;
  --u1;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  i__1 = *nu1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      y[i__] = u1[i__] * rpar[1] + u2[i__] * rpar[2];
      /* L1: */
    }
  return 0;
}			



int scicos_sum3(int *flag__, int *nevprt, double *t, double *xd, double *x,
		int *nx, double *z__, int *nz, double *tvec, int *ntvec,
		double *rpar, int *nrpar, int *ipar, int *nipar, double *u1,
		int *nu1, double *u2, int *nu2, double *u3, int *nu3, double *y,
		int *ny)
{
  int i__1;
  int i__;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     adds the inputs weighed by rpar */
  --y;
  --u3;
  --u2;
  --u1;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  i__1 = *nu1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      y[i__] = u1[i__] * rpar[1] + u2[i__] * rpar[2] + u3[i__] * rpar[3];
      /* L1: */
    }
  return 0;
}			



int scicos_tanblk (scicos_args_poo)
{
  int i__1;
  int i__;
  double ww;

  /*     Copyright INRIA */
  /*     Scicos block simulator */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  i__1 = *nu;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ww = cos (u[i__]);
      if (ww != 0.)
	{
	  y[i__] = sin (u[i__]) / ww;
	}
      else
	{
	  *flag__ = -2;
	  return 0;
	}
      /* L15: */
    }
  return 0;
}			



/* Table of constant values */



int
scicos_tcslti (int *flag__, int *nevprt, double *t, double *xd, double *x,
	       int *nx, double *z__, int *nz, double *tvec, int *ntvec,
	       double *rpar, int *nrpar, int *ipar, int *nipar, double *u1,
	       int *nu1, double *u2, int *nu2, double *y, int *ny)
{
  int c__1 = 1;
  int la, lb, lc, ld;

  /*     Scicos block simulator */
  /*     continuous state space linear system simulator */
  /*     rpar(1:nx*nx)=A */
  /*     rpar(nx*nx+1:nx*nx+nx*nu)=B */
  /*     rpar(nx*nx+nx*nu+1:nx*nx+nx*nu+nx*ny)=C */
  /*     rpar(nx*nx+nx*nu+nx*ny+1:nx*nx+nx*nu+nx*ny+ny*nu)=D */
  /* ! */


  --y;
  --u2;
  --u1;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  la = 1;
  lb = *nx * *nx + la;
  lc = lb + *nx * *nu1;
  if (*flag__ == 1 || *flag__ == 6)
    {
      /*     y=c*x+d*u1 */
      ld = lc + *nx * *ny;
      dmmul_scicos (&rpar[lc], ny, &x[1], nx, &y[1], ny, ny, nx, &c__1);
      dmmul1_scicos(&rpar[ld], ny, &u1[1], nu1, &y[1], ny, ny, nu1, &c__1);
    }
  else if (*flag__ == 2 && *nevprt == 1)
    {
      /*     x+=u2 */
      C2F(dcopy) (nx, &u2[1], &c__1, &x[1], &c__1);
    }
  else if (*flag__ == 0 && *nevprt == 0)
    {
      /*     xd=a*x+b*u1 */
      dmmul_scicos (&rpar[la], nx, &x[1], nx, &xd[1], nx, nx, nx, &c__1);
      dmmul1_scicos(&rpar[lb], nx, &u1[1], nu1, &xd[1], nx, nx, nu1, &c__1);
    }
  return 0;
}			


int scicos_tcsltj (scicos_args_poo) 
{
  static int c__1 = 1;
  int la, lb, lc;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     continuous state space linear system simulator */
  /*     rpar(1:nx*nx)=A */
  /*     rpar(nx*nx+1:nx*nx+nx*nu)=B */
  /*     rpar(nx*nx+nx*nu+1:nx*nx+nx*nu+nx*ny)=C */
  /*     rpar(nx*nx+nx*nu+nx*ny+1:nx*nx+nx*nu+nx*ny+ny*nu)=D */
  /* ! */

  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  la = 1;
  lb = *nx * *nx + la;
  lc = lb;
  if (*flag__ == 1 || *flag__ == 6)
    {
      /*     y=c*x */
      dmmul_scicos (&rpar[lc], ny, &x[1], nx, &y[1], ny, ny, nx, &c__1);
    }
  else if (*flag__ == 2 && *nevprt == 1)
    {
      /*     x+=u2 */
      C2F(dcopy) (nx, &u[1], &c__1, &x[1], &c__1);
    }
  else if (*flag__ == 0 && *nevprt == 0)
    {
      /*     xd=a*x */
      dmmul_scicos  (&rpar[la], nx, &x[1], nx, &xd[1], nx, nx, nx, &c__1);
    }
  return 0;
}			


int scicos_timblk (scicos_args_poo)
{
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  y[0] = *t;
  return 0;
}



int scicos_trash (scicos_args_poo)
{
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  return 0;
}			


int  scicos_zcross (scicos_args_poo) 
{
  int i__1;
  double d__1;
  int i__, j, l, kev;

  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     zero crossing block */


  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 3 && *nevprt < 0)
    {
      kev = 0;
      i__1 = *ny;
      for (j = 1; j <= i__1; ++j)
	{
	  kev = (int) ((kev << 1) + (d__1 = y[*ny + 1 - j], Abs (d__1)));
	  /* L44: */
	}
      i__1 = *ny;
      for (j = 1; j <= i__1; ++j)
	{
	  kev <<= 1;
	  if (y[*ny + 1 - j] == -1.)
	    {
	      ++kev;
	    }
	  /* L45: */
	}
      l = kev * *ntvec;
      i__1 = *ntvec;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  tvec[i__] = rpar[l + i__] + *t;
	  /* L10: */
	}
    }
  else if (*flag__ == 9)
    {
      i__1 = *ny;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  y[i__] = u[i__];
	  /* L20: */
	}
    }
  return 0;
}


void 
plusblk(int *flag, int *nevprt, double *t, double *xd, double *x,
	int *nx, double *z, int *nz, double *tvec, int *ntvec, 
	double *rpar, int *nrpar, int *ipar, int *nipar, double **inptr, 
	int *insz, int *nin, double **outptr, int *outsz, int *nout)
{
  int k,i,n;
  double *y;
  double *u;

  n=outsz[0]; /* insz[0]==insz[1] .. ==insz[*nin]== outsz[0] */

  y=(double *)outptr[0];

  for (i=0;i<n;i++) {
    y[i]=0.0;
    for (k=0;k<*nin;k++) {
      u=(double *)inptr[k];
      y[i]=y[i]+u[i];
    }
  }
}


void  scicos_plusblk(int *flag, int *nevprt, double *t, double *xd, double *x,
		     int *nx, double *z, int *nz, double *tvec, int *ntvec,
		     double *rpar, int *nrpar, int *ipar, int *nipar, double **inptr,
		     int *insz, int *nin, double **outptr, int *outsz, int *nout)
{
  int k,i,n;
  double *y;
  double *u;

  n=outsz[0]; /* insz[0]==insz[1] .. ==insz[*nin]== outsz[0] */

  y=(double *)outptr[0];

  for (i=0;i<n;i++) {
    y[i]=0.0;
    for (k=0;k<*nin;k++) {
      u=(double *)inptr[k];
      y[i]=y[i]+u[i];
    }
  }
}


void switchn(int *flag, int *nevprt, double *t, double *xd, double *x,
	     int *nx, double *z, int *nz, double *tvec, int *ntvec,
	     double *rpar, int *nrpar, int *ipar, int *nipar, double **inptr,
	     int *insz, int *nin, double **outptr, int *outsz, int *nout)
{

  int k;
  double *y;
  double *u;
  int /*nev,*/ic;
  ic=ipar[0];
  if (*nin>1) {
    y=(double *)outptr[0];
    u=(double *)inptr[ic];
    for (k=0;k<outsz[0];k++)
      *(y++)=*(u++);  
  }
  else {
    y=(double *)outptr[ic];
    u=(double *)inptr[0];
    for (k=0;k<outsz[0];k++)
      *(y++)=*(u++);  
  }
}



void 
selector(int *flag, int *nevprt, double *t, double *xd, double *x,
	 int *nx, double *z, int *nz, double *tvec, int *ntvec,
	 double *rpar, int *nrpar, int *ipar, int *nipar, double **inptr,
	 int *insz, int *nin, double **outptr, int *outsz, int *nout)
{
  int k;
  double *y;
  double *u;
  int nev,ic;
    
  ic=(int)z[0];
  if ((*flag)<3) {
    ic=-1;
    nev=*nevprt;
    while (nev>=1) {
      ic=ic+1;
      nev=nev/2;
    }
  }
  if (*nin>1) {
    y=(double *)outptr[0];
    u=(double *)inptr[ic];
    for (k=0;k<outsz[0];k++)
      *(y++)=*(u++);  
  }
  else {
    y=(double *)outptr[ic];
    u=(double *)inptr[0];
    for (k=0;k<outsz[0];k++)
      *(y++)=*(u++);  
  }
}


void 
relay(int *flag, int *nevprt, double *t, double *xd, double *x,
      int *nx, double *z, int *nz, double *tvec, int *ntvec,
      double *rpar, int *nrpar, int *ipar, int *nipar, double **inptr,
      int *insz, int *nin, double **outptr, int *outsz, int *nout)
{
  int k;
  double *y;
  double *u;
  int nev,ic;
  ic=(int)z[0];
  if ((*flag)<3) {
    if ((*nevprt)>0) {
      ic=-1;
      nev=*nevprt;
      while (nev>=1) {
	ic=ic+1;
	nev=nev/2;
      }
    }
    if ((*flag)==2) {z[0]=ic;return ;}
    if (*nin>1) {
      y=(double *)outptr[0];
      u=(double *)inptr[ic];
      for (k=0;k<outsz[0];k++)
	*(y++)=*(u++);  
    }
    else {
      y=(double *)outptr[ic];
      u=(double *)inptr[0];
      for (k=0;k<outsz[0];k++)
	*(y++)=*(u++);  
    }
  }
}


void 
prod(int *flag, int *nevprt, double *t, double *xd, double *x,
     int *nx, double *z, int *nz, double *tvec, int *ntvec,
     double *rpar, int *nrpar, int *ipar, int *nipar, double **inptr,
     int *insz, int *nin, double **outptr, int *outsz, int *nout)
{
  int k,i,n;
  double *y;
  double *u;
    
  n=outsz[0]; /* insz[0]==insz[1] .. ==insz[*nin]== outsz[0] */

  y=(double *)outptr[0];

  for (i=0;i<n;i++) {
    y[i]=1.0;
    for (k=0;k<*nin;k++) {
      u=(double *)inptr[k];
      y[i]=y[i]*u[i];
    }
  }
}


void 
sum(int *flag, int *nevprt, double *t, double *xd, double *x,
    int *nx, double *z, int *nz, double *tvec, int *ntvec,
    double *rpar, int *nrpar, int *ipar, int *nipar, double **inptr,
    int *insz, int *nin, double **outptr, int *outsz, int *nout)
{
  int k,i,n;
  double *y;
  double *u;

  n=outsz[0]; /* insz[0]==insz[1] .. ==insz[*nin]== outsz[0] */

  y=(double *)outptr[0];

  for (i=0;i<n;i++) {
    y[i]=0.0;
    for (k=0;k<*nin;k++) {
      u=(double *)inptr[k];
      y[i]=y[i]+u[i]*rpar[k];
    }
  }
}


void 
zcross2(int *flag, int *nevprt, double *t, double *xd, double *x,
	int *nx, double *z, int *nz, double *tvec, int *ntvec,
	double *rpar, int *nrpar, int *ipar, int *nipar, double *u, int *nu, double *g, int *ng)
{
  
  int i,j;
  int surface_matched,exist_enabled_surface;

  exist_enabled_surface=0;
  if ((*flag==3) &&(*nevprt<0)){
    for(i=0;i<*ntvec;i++){
      surface_matched=1;
      exist_enabled_surface=0;
      
      for (j=0;j<*ng;j++){
	if (rpar[(*ng+1)*i+j]!=0){
	  exist_enabled_surface=1;
	  if((rpar[(*ng+1)*i+j]*g[j])<=0){
	    surface_matched=0;
	  }
	}
      }
      
      if(( surface_matched==1)&&(exist_enabled_surface ==1))
	tvec[i]=*t+rpar[(*ng+1)*i+*ng];
      else
	tvec[i]=-1;
      
    }
  }
  else{
    if(*flag==9){
      for(i=0;i<*ng;i++)
	g[i]=u[i];
    }
  }
}

int scicos_bound (scicos_args_poo)
{

  int i__1;
  int i__;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     Bound y(i)=rpar(nu+i) if u(i)>rpar(i) else y(i)=0 */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  i__1 = *nu;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (u[i__] >= rpar[i__])
	{
	  y[i__] = rpar[*nu + i__];
	}
      else
	{
	  y[i__] = 0.;
	}
      /* L15: */
    }
  return 0;
}



void  readau(int *flag, int *nevprt, double *t, double *xd, double *x, int *nx, double *z, int *nz, double *tvec, int *ntvec, double *rpar, int *nrpar, int *ipar, int *nipar, double **inptr, int *insz, int *nin, double **outptr, int *outsz, int *nout)
     /*
       ipar[1]   = lfil : file name length
       ipar[2:4] = fmt  : numbers type ascii code
       ipar[5]   = void
       ipar[6]   = n : buffer length in number of records
       ipar[7]   = maxvoie : record size
       ipar[8]   = swap
       ipar[9]   = first : first record to read
       ipar[10:9+lfil] = character codes for file name
     */
                                                           
                                   
                                       
                                  

{
  char str[100],type[4];
  /* int job = 1,three=3; */
  FILE *fd;
  int n, k, kmax, /*no, lfil,*/ m, i, irep,/* nm,*/ ierr;
  double *buffer,*record;
  long offset;
  double y;
  /*  div_t divt;*/int quot, rem;
  double SCALE=0.000030517578125;
  /*  int ETAB[8]={0, 132, 396, 924, 1980, 4092, 8316, 16764}; */
  int ETAB[8];
  int mu;
  int sig;
  int e;
  int f;
  /*  double ff;*/

  ETAB[0]=0; ETAB[1]=132; ETAB[2]= 396; ETAB[3]=924; ETAB[4]=1980;
  ETAB[5]=4092; ETAB[6]=8316; ETAB[7]=16764;

  --ipar;
  --z;
  fd=(FILE *)(long)z[3];
  buffer = (z+4);
    
  /*
    k    : record counter within the buffer
    kmax :  number of records in the buffer
  */

  if (*flag==1) {
    n    = ipar[6];
    k    = (int)z[1];
    /* copy current record to output */
    record=buffer+(k-1)*ipar[7];

    for (i=0;i<*nout;i++)
      {
	mu=(int) record[i];

	mu=255-mu;
	if(mu>127)
	  sig=1;
	else
	  sig=0;
	/* comment out for SUNOS SS 8/10/99 
	   divt=div(mu,16);
	   e=divt.quot-8*sig+1;
	   f=divt.rem;
	*/
	quot=mu/16;rem=mu-16*quot;
	e=quot-8*sig+1;
	f=rem;

	y=ldexp((double)(f),(e+2));
	/* ff=(double)(e+2);
	   y=((double) f) * pow(two, ff); */

	e=ETAB[e-1];

	y=SCALE*(1-2*sig)*(e+y);

	*outptr[i]=y;
      }
    if (*nevprt>0) {
      /*     discrete state */
      kmax =(int) z[2];
      if (k>=kmax&&kmax==n) {
	/*     read a new buffer */
	Scierror("Error: in readau to be done \n");
	/* XXXXXXX
	   m=ipar[6]*ipar[7];
	   F2C(cvstr)(&three,&(ipar[2]),type,&job,strlen(type));
	   for (i=2;i>=0;i--)
	   if (type[i]!=' ') { type[i+1]='\0';break;}
	   ierr=0;
	   mget2(fd,ipar[8],buffer,m,type,&ierr);
	*/
	ierr=1;
	if (ierr>0) {
	  sciprint("Read error!\n");
	  fclose(fd);
	  z[3] = 0.0;
	  *flag = -1;
	  return;
	}
	else if (ierr<0) { /* EOF reached */
	  kmax=-(ierr+1)/ipar[7];
	}
	else
	  kmax=ipar[6];

	z[1] = 1.0;
	z[2] = kmax;
      }
      else if (k<kmax) 
	z[1] = z[1]+1.0;
    }
  }
  else if (*flag==4) {
    /* F2C(cvstr)(&(ipar[1]),&(ipar[10]),str,&job,strlen(str)); */
    sciprint("Error: to be finished !\n");
    *flag = -1;
    return;
    str[ipar[1]] = '\0';
    fd = fopen(str,"rb");
    if (!fd ) {
      sciprint("Could not open the file!\n");
      *flag = -1;
      return;
    }
    z[3]=(long)fd;
    /* skip first records */
    if (ipar[9]>1) {
      sciprint("Error: to be done \r\n");
      /* F2C(cvstr)(&three,&(ipar[2]),type,&job,strlen(type)); */
      for (i=2;i>=0;i--)
	if (type[i]!=' ') { type[i+1]='\0';break;}
      offset=(ipar[9]-1)*ipar[7]*sizeof(char);
      irep = fseek(fd,offset,0) ;
      if ( irep != 0 ) 
	{
	  sciprint("Read error\r\n");
	  *flag = -1;
	  fclose(fd);
	  z[3] = 0.0;
	  return;
	}
    }
    /* read first buffer */
    m=ipar[6]*ipar[7];
    /* F2C(cvstr)(&three,&(ipar[2]),type,&job,strlen(type)); */
    sciprint("Error: cvstr to be done !\n");
    for (i=2;i>=0;i--)
      if (type[i]!=' ') { type[i+1]='\0';break;}
    /* XXXXXXXX mget2(fd,ipar[8],buffer,m,type,&ierr); */
    ierr=1;
    if (ierr>0) {
      sciprint("Read error!\n");
      *flag = -1;
      fclose(fd);
      z[3] = 0.0;
      return;
    }
    else if (ierr<0) { /* EOF reached */
      kmax=-(ierr+1)/ipar[7];
    }
    else
      kmax=ipar[6];

    z[1] = 1.0;
    z[2] = kmax;
  }
  else if (*flag==5) {
    if(z[3]==0) return;
    fclose(fd);
    z[3] = 0.0;
  }
  return;
}



static int worldsize(char type[4])
{
  char c = ( type[0]=='u' ) ? type[1] : type[0];
  switch ( c )
    {
    case 'l' : return sizeof(long);
    case 's' : return sizeof(short);
    case 'c' : return sizeof(char);
    case 'd' : return sizeof(double);
    case 'f' : return sizeof(float); 
    }
  return 0;
}


void  readc(int *flag, int *nevprt, double *t, double *xd, double *x, int *nx, double *z, int *nz, double *tvec, int *ntvec, double *rpar, int *nrpar, int *ipar, int *nipar, double **inptr, int *insz, int *nin, double **outptr, int *outsz, int *nout)
     /*
       ipar[1]   = lfil : file name length
       ipar[2:4] = fmt  : numbers type ascii code
       ipar[5]   = is there a time record
       ipar[6]   = n : buffer length in number of records
       ipar[7]   = maxvoie : record size
       ipar[8]   = swap
       ipar[9]   = first : first record to read
       ipar[10:9+lfil] = character codes for file name
       ipar[10+lfil:9+lfil++ny+ievt] = reading mask
     */
{
  /* ipar code model.ipar=[length(fname);str2code(frmt);N;swap;str2code(fname)] */
  typedef struct _writec_ipar writec_ipar ;
  struct _writec_ipar { int len, fmt[3],ievt,n,maxvoie,swap,first,fname;};
  writec_ipar *wi =  (writec_ipar*) ipar;
  NspFile *F;
  double *buffer,*record;
  int k, kmax, m, *mask, nread;
  long offset;

  --z;
  F=(NspFile *)(long)z[3];
  buffer = (z+4);
  
  /* pointer to the mask start position */
  mask = &wi->fname  + wi->len ;
    
  /*
   *    k    : record counter within the buffer
   *    kmax :  number of records in the buffer
   */

  if (*flag==1) 
    {
      char type[4];
      int i;
      /* get the type from its ascii code  */
      for ( i=0; i < 3; i++) type[i]= wi->fmt[i];
      for ( i=2 ; i >= 0 ; i--) if (type[i]==' ') type[i]='\0';
      /* value of k */
      k    = (int)z[1];
      /* copy current record to output */
      record=buffer+(k-1)*wi->maxvoie-1;
      for (i=0;i<outsz[0];i++)
	*(outptr[0]+i)=record[mask[wi->ievt+i]];
      if (*nevprt>0) {
	/*     discrete state */
	kmax = (int)z[2];
	if ( k >= kmax && kmax == wi->n) {
	  /*     read a new buffer */
	  m=wi->n*wi->maxvoie;
	  if ( nsp_mget(F,buffer,m,type,&nread) == FAIL) 
	    {
	      Scierror("Error: in readc, read error during fseek\n");
	      *flag = -1;
	      nsp_file_close(F);
	      nsp_file_destroy(F);
	      z[3] = 0.0;
	      return;
	    }
	  /* A faire si moins a lire  
	     else if (ierr<0) {
	    kmax=-(ierr+1)/wi->maxvoie;
	    }
	  */
	  kmax=wi->n;
	  z[1] = 1.0;
	  z[2] = kmax;
	}
	else if (k<kmax) 
	  z[1] = z[1]+1.0;
      }
    }
  else if (*flag==3) 
    {
      k    = (int)z[1];
      kmax = (int) z[2];
      if (k > kmax && kmax < wi->n) {
	if(wi->ievt) 
	  tvec[0] = *t-1.0;
	else
	  tvec[0] = *t*(1.0+0.0000000001);
      }
      else {
	record=buffer+(k-1)*wi->maxvoie-1;
	if(wi->ievt) tvec[0] = record[mask[0]];
      }
    }
  else if (*flag==4) 
    {
      char type[4];
      char str[FSIZE];
      int i;
      /* get the file name from its ascii code  */
      for ( i=0; i < wi->len; i++) str[i]= *(&wi->fname + i);
      str[wi->len]='\0';
      sciprint("Trying to open [%s]\n",str);
      if (( F= nsp_file_open(str,"rb",FALSE,wi->swap)) == NULL) 
	{
	  Scierror("Error: in writec, could not open the file %s !\n",str);
	  *flag = -3;
	  return;
	}
      z[3]=(long)F;
      
      /* get the type from its ascii code  */
      for ( i=0; i < 3; i++) type[i]= wi->fmt[i];
      for ( i=2 ; i >= 0 ; i--) if (type[i]==' ') type[i]='\0';
      
      /* skip first records */
      if ( wi->first > 1) 
	{
	  offset=(wi->first -1)*wi->maxvoie*worldsize(type);
	  if ( nsp_fseek(F,offset,"set") == FAIL) 
	    {
	      Scierror("Error: in readc, read error during fseek\n");
	      *flag = -1;
	      nsp_file_close(F);
	      nsp_file_destroy(F);
	      return;
	    }
	}
      /* read first buffer */
      m=wi->n*wi->maxvoie;
      if ( nsp_mget(F,buffer,m,type,&nread) == FAIL) 
	{
	  Scierror("Error: in readc, read error during fseek\n");
	  *flag = -1;
	  nsp_file_close(F);
	  nsp_file_destroy(F);
	  z[3] = 0.0;
	  return;
	}
      /*  XXXXX voir ce qui se passe quand nread < m 
	  else if (ierr<0) { 
	  kmax=-(ierr+1)/wi->maxvoie;
	  }
      */
      kmax=wi->n;
      z[1] = 1.0;
      z[2] = kmax;
    }
  else if (*flag==5) 
    {
      if(z[3]==0) return;
      nsp_file_close(F);
      nsp_file_destroy(F);
      z[3] = 0.0;
    }
  return;
}


/*
 * Copyright Enpc jpc 
 */

extern int scicos_getgeom(double *);

/*----------------------------------------------------
 * erase a rectangle 
 *----------------------------------------------------*/ 

static void block_draw_rect_1(BCG *Xgc,double r[],double percent)
{
  int alumode;
  static int in=6;
  double w = r[2]*percent;
  double x = r[0];
  double rect[]={x,r[1],w,r[3]};
  alumode  = Xgc->graphic_engine->xget_alufunction(Xgc);
  if ( alumode != in ) 
    Xgc->graphic_engine->scale->xset_alufunction1(Xgc,in);
  Xgc->graphic_engine->scale->fillrectangle(Xgc,rect);
  if ( alumode != in ) 
    Xgc->graphic_engine->scale->xset_alufunction1(Xgc,alumode);
}

static void block_draw_rect_2(BCG *Xgc,double r[],double percent)
{
  int alumode;
  /* int flag =0;  double ang=0.0;  char foo[24]; */
  static int in=6;
  double x = r[0]+ r[2]*percent-2;
  double w = 4;
  double rect[]={x,r[1],w,r[3]};
  alumode  = Xgc->graphic_engine->xget_alufunction(Xgc);
  if ( alumode != in )
    Xgc->graphic_engine->scale->xset_alufunction1(Xgc,in);
  Xgc->graphic_engine->scale->fillrectangle(Xgc,rect);
  /* sprintf(foo,"%5.3f",percent);
     C2F(dr1)("xstring",foo,PI0,PI0,PI0,&flag,PI0,PI0,r,r+1,&ang,PD0,0L,0L);
  */

  if ( alumode != in ) 
    Xgc->graphic_engine->scale->xset_alufunction1(Xgc,alumode);
}


static void block_draw_rect_3(BCG *Xgc,double r[],double percent)
{
  int alumode;
  int flag =0;  double ang=0.0;  char foo[24];
  static int in=6;
  double x = r[0] + (1.0/10)*r[2];
  double y = r[1] - r[3] + 2 ;
  alumode  = Xgc->graphic_engine->xget_alufunction(Xgc);
  if ( alumode != in ) 
    Xgc->graphic_engine->scale->xset_alufunction1(Xgc,in);
  sprintf(foo,"%5.3f",percent);
  Xgc->graphic_engine->scale->displaystring(Xgc,foo,x,y,flag,ang);
  if ( alumode != in ) 
    Xgc->graphic_engine->scale->xset_alufunction1(Xgc,alumode);
}





/*----------------------------------------------------
 * changes the current color to color 
 * returns the old color
 *----------------------------------------------------*/ 


static int set_slider_color(BCG *Xgc, int color)
{
  int cur;
  cur = Xgc->graphic_engine->xget_pattern(Xgc);
  if ( cur != color ) 
    Xgc->graphic_engine->xset_pattern(Xgc,color);
  return cur ;
}

/*----------------------------------------------------
 * follow an input value with a graphic slider 
 * 
 *  rpar = [min-range, max-range] 
 *  ipar = [type (1,2),color]
 *  z = [val,window,x,y,w,h] 
 *----------------------------------------------------*/

void slider(int *flag, int *nevprt, double *t, double *xd,
	    double *x, int *nx, double *z, int *nz,
	    double *tvec, int *ntvec, double *rpar,
	    int *nrpar, int *ipar, int *nipar,
	    double * u, int *nu, double * y, int *ny) 
{
  BCG *Xgc;
  int wid, idb = 0 ; /* XXX remettre idb avec son common */
  int cur; 
  static double th=2 ; /* border thickness */
  static double t3d = 4.0 ; /* 3d look thickness */
  int curcolor;
  if ( idb == 1 ) 
    sciprint("Slider t=%10.3f, flag=%d \r\n",*t,*flag);

  switch ( *flag ) {
  case 2 : 
    /* standard case */ 
    wid= (int) z[1];
    if( wid < 0) return;
    Xgc = scicos_set_win(wid,&cur);
    {
      double val = Min(rpar[1],Max(rpar[0],u[0]));
      double percent = (val - rpar[0])/(rpar[1]-rpar[0]);
      if ( Abs(z[0] - percent) > 0.01 ) /* a mettre en parametre XXXXX */
	{
	  int record= Xgc->graphic_engine->xget_recording(Xgc);
	  Xgc->graphic_engine->xset_recording(Xgc,FALSE);
	  curcolor=set_slider_color(Xgc,ipar[1]);
	  switch (ipar[0]) 
	    {
	    case 1 : 
	      block_draw_rect_1(Xgc,z+2,z[0]);
	      block_draw_rect_1(Xgc,z+2,percent);
	      break;
	    case 2 :
	      block_draw_rect_2(Xgc,z+2,z[0]);
	      block_draw_rect_2(Xgc,z+2,percent);
	      break;
	    case 3 :
	      block_draw_rect_3(Xgc,z+2,z[0]);
	      block_draw_rect_3(Xgc,z+2,percent);
	      break;
	    }
	  curcolor=set_slider_color(Xgc,curcolor);
	  z[0] = percent;
	  Xgc->graphic_engine->xset_recording(Xgc,record);
	}
    }
    Xgc = scicos_set_win(cur,&cur);
    break;
  case 4 : 
    /* initial case */ 
    z[0]= 0.0;
    scicos_getgeom(z+1);
    z[2] = z[2]+ t3d +th ; 
    z[4] -= t3d + 2*th  ;
    z[3] = z[3] + z[5] ;
    z[5] -= t3d  ;
    wid= (int) z[1];
    if( wid < 0) return;
    Xgc = scicos_set_win(wid,&cur);
    {
      int record= Xgc->graphic_engine->xget_recording(Xgc);
      Xgc->graphic_engine->xset_recording(Xgc,FALSE);
      curcolor=set_slider_color(Xgc,ipar[1]);
      Xgc->graphic_engine->scale->cleararea(Xgc,z[2],z[3],z[4],z[5]);
      switch (ipar[0]) 
	{
	case 1 : 
	  block_draw_rect_1(Xgc,z+2,z[0]);
	  break;
	case 2 :
	  block_draw_rect_2(Xgc,z+2,z[0]);
	  break;
	case 3 :
	  block_draw_rect_3(Xgc,z+2,z[0]);
	  break;

	}
      curcolor=set_slider_color(Xgc,curcolor);
      Xgc->graphic_engine->xset_recording(Xgc,record);
    }
    Xgc = scicos_set_win(cur,&cur);
    break;
  }
}


void writeau(int *flag, int *nevprt, double *t, double *xd, double *x,
	     int *nx, double *z, int *nz, double *tvec, int *ntvec,
	     double *rpar, int *nrpar, int *ipar, int *nipar, double **inptr,
	     int *insz, int *nin, double **outptr, int *outsz, int *nout)
{
  /*
    ipar[1]   = lfil : file name length
    ipar[2:4] = fmt  : numbers type ascii code
    ipar[5]   = n : buffer length in number of records
    ipar[6]   = swap
    ipar[7:6+lfil] = character codes for file name
  */
  FILE *fd;
  int n, k,/* m,*/ i, ierr;
  double *buffer,*record;
  /*  long offset;*/
  int SCALE  = 32768;
  int BIAS   =   132;
  int CLIP   = 32635;
  int OFFSET =   335;
  double y;
  int sig;
  int e;
  double f;
  --ipar;
  --z;
  fd=(FILE *)(long)z[2];
  buffer = (z+3);
  ierr=0;
  /*
    k    : record counter within the buffer
  */

  if (*flag==2&&*nevprt>0) 
    { /* add a new record to the buffer */
      n    = ipar[5];
      k    = (int)z[1];
      /* copy current record to output 
	 printf("%i\n",k);*/
      record=buffer+(k-1)*(*nin); 

      for (i=0;i<*nin;i++)
	{
	  y= *inptr[i];
	  y=SCALE*y;
	  if (y<0.0)
	    {
	      y=-y;
	      sig=-1;
	    }
	  else
	    sig=1;
	  if(y>CLIP)
	    y=CLIP;
	  y=y+BIAS;
	  f=frexp(y,&e);
	  y=64*sig-16*e- (int) (32*f)+OFFSET;
	  record[i] = y;
	}
      if (k<n) 
	z[1] = z[1]+1.0;
      else {
	/*XXXXXXXX mput2(fd,ipar[6],buffer,ipar[5]*(*nin),"uc",&ierr); */
	ierr=1;
	if(ierr!=0) {
	  *flag = -3;
	  return;
	}
	z[1] = 1.0;
	
      }
      
    }
  else if (*flag==4) {
    fd = fopen("/dev/audio","wb");
    if (!fd ) {
      sciprint("Could not open /dev/audio!\n");
      *flag = -3;
      return;
    }
    z[2]=(double)(long)fd;
    z[1] = 1.0;
  }
  else if (*flag==5) {
    if(z[2]==0) return;
    k    =(int) z[1];
    if (k>1) {/* flush rest of buffer */
      /*XXXXXXXX mput2(fd,ipar[6],buffer,(k-1)*(*nin),"uc",&ierr); */
      ierr=1;
      if(ierr!=0) {
	*flag = -3;
	return;
      }
    }
    fclose(fd);
    z[2] = 0.0;
  }
  return;
}

/*
 * Write in binary mode 
 */

void 
writec(int *flag, int *nevprt, double *t, double *xd, double *x, int *nx, 
       double *z, int *nz, double *tvec, int *ntvec, double *rpar, int *nrpar, 
       int *ipar, int *nipar, double **inptr, int *insz, int *nin, double **outptr, 
       int *outsz, int *nout)
{
  /* ipar code model.ipar=[length(fname);str2code(frmt);N;swap;str2code(fname)] */
  typedef struct _writec_ipar writec_ipar ;
  struct _writec_ipar { int len, fmt[3],n,swap,fname;};
  writec_ipar *wi =  (writec_ipar*) ipar;

  NspFile *F;
  int k, i;
  double *buffer,*record;

  --z;
  F=(NspFile *)(long)z[2];
  buffer = (z+3);
  k = (int) z[1];
  /*
   * k    : record counter within the buffer
   */

  if ( *flag==2 && *nevprt>0) 
    { 
      /* add a new record to the buffer */
      /* copy current record to output */
      record=buffer+(k-1)*(insz[0]);
      for ( i=0 ; i < insz[0] ; i++) record[i] = *(inptr[0]+i);
      if ( k < wi->n ) 
	{
	  z[1] = z[1]+1.0;
	}
      else 
	{
	  char type[4];
	  int i;
	  /* get the type from its ascii code  */
	  for ( i=0; i < 3; i++) type[i]= wi->fmt[i];
	  type[3]='\0';
	  /* buffer is full write it to the file */
	  if ( nsp_mput(F,buffer,wi->n*insz[0],type) == FAIL) 
	    {
	      *flag = -3;
	      return;
	    }
	  z[1] = 1.0;
	}
    }
  else if (*flag==4) 
    {
      char str[FSIZE];
      int i;
      /* get the file name from its ascii code  */
      for ( i=0; i < wi->len; i++) str[i]= *(&wi->fname + i);
      str[wi->len]='\0';
      sciprint("Trying to open [%s]\n",str);
      if (( F= nsp_file_open(str,"wb",FALSE,wi->swap)) == NULL) 
	{
	  Scierror("Error: in writec, could not open the file %s !\n",str);
	  *flag = -3;
	  return;
	}
      z[2]=(long)F;
      z[1] = 1.0;
    }
  else if (*flag==5) 
    {
      if(z[2]==0) return;
      k    =(int) z[1];
      if ( k >= 1 ) 
	{
	  /* flush rest of buffer */
	  char type[4];
	  int i;
	  /* get the type from its ascii code  */
	  for ( i=0; i < 3; i++) type[i]= wi->fmt[i];
	  type[3]='\0';
	  if ( nsp_mput(F,buffer,(k-1)*insz[0],type) == FAIL) 
	    {
	      *flag = -3;
	      return;
	    }
	}
      if (( nsp_file_close(F)) == FAIL) 
	{
	  *flag = -3;
	  return;
	}
      nsp_file_destroy(F);
      z[2] = 0.0;
    }
  return;
}


int scicos_affich (scicos_args_poo)
{
  BCG *Xgc;
  int record;
  /*     Copyright INRIA */
  /*     Scicos block simulator */
  /*     Displays the value of the input in a graphic window */
  static int cur = 0;
  int i__1;
  double  ur;
  int wid;
  /*     ipar(1) = font */
  /*     ipar(2) = fontsize */
  /*     ipar(3) = color */
  /*     ipar(4) = win */
  /*     ipar(5) = nt : total number of output digits */
  /*     ipar(6) = nd number of rationnal part digits */
  /*     z(1)=value */
  /*     w(2)=window */
  /*     z(3)=x */
  /*     z(4)=y */
  /*     z(5)=width */
  /*     z(6)=height */
  --y;
  --u;
  --ipar;
  --rpar;
  --tvec;
  --z__;
  --x;
  --xd;
  if (*flag__ == 2)
    {
      /*     state evolution */
      ur = pow(10.0, ipar[6]);
      ur = anint(u[1] * ur) / ur; /* round */
      if (ur == z__[1])
	{
	  return 0;
	}
      wid = (int) z__[2];
      if (wid < 0)
	{
	  return 0;
	}
      i__1 = (int) z__[2];
      Xgc = scicos_set_win(i__1,&cur);
      record= Xgc->graphic_engine->xget_recording(Xgc);
      Xgc->graphic_engine->xset_recording(Xgc,FALSE);
      scicos_recterase (Xgc,&z__[3]);
      z__[1] = ur;
      scicos_affdraw (Xgc,&ipar[1], &ipar[5], &z__[1], &z__[3]);
      Xgc->graphic_engine->xset_recording(Xgc,record);
    }
  else if (*flag__ == 4)
    {
      /*     init */
      /*     .  initial value */
      z__[1] = 0.;
      /*     .  get geometry of the block */
      scicos_getgeom (&z__[2]);
      if (z__[2] < 0.)
	{
	  return 0;
	}
      i__1 = (int) z__[2];
      Xgc = scicos_set_win(i__1,&cur);
      record= Xgc->graphic_engine->xget_recording(Xgc);
      Xgc->graphic_engine->xset_recording(Xgc,FALSE);
      scicos_recterase (Xgc,&z__[3]);
      scicos_affdraw (Xgc,&ipar[1], &ipar[5], &z__[1], &z__[3]);
      Xgc->graphic_engine->xset_recording(Xgc,record);
    }
  return 0;
}			

int scicos_recterase (BCG *Xgc,const double r[])
{
  const double dx = .06, dy = .06;
  double w,x,y,h;
  x = r[0] + dx * r[2];
  y = r[1] + r[3];
  w = r[2] * (1. - dx);
  h = r[3] * (1. - dy);
  Xgc->graphic_engine->scale->cleararea(Xgc,x,y,w,h);
  return 0;
}	


int scicos_affdraw (BCG *Xgc,const int fontd[],const int form[],const double *val,const double r[])
{
  int fontid[2],rect[4],flag=0,pixmode;
  char buf[128];
  double x,y,angle=0.0;
  sprintf(buf,"%*.*f",form[0],form[1],*val);
  Xgc->graphic_engine->xget_font(Xgc,fontid);
  Xgc->graphic_engine->xset_font(Xgc,fontd[0],fontd[1]);
  Xgc->graphic_engine->boundingbox(Xgc,buf,r[0],r[1],rect);
  x = r[0] + Max (0.0,(r[2] - rect[2]) / 2.);
  y = r[1] + Max (0.0,(r[3] - rect[3]) / 2.);
  Xgc->graphic_engine->displaystring(Xgc,buf,x,y,flag,angle);
  Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1]);
  pixmode = Xgc->graphic_engine->xget_pixmapOn(Xgc);
  if ( pixmode == 1) Xgc->graphic_engine->scale->xset_show(Xgc);
  return 0;
} 


int scicos_getgeom (double *g)
{
  /* FIXME XXXXXXXX
   */
  sciprint("getgeom is not implemented_n");
  return 0;
}	

/*------------------------------------------------
 *     Scicos block simulator 
 *     A set of elementary blocks 
 *------------------------------------------------*/

typedef  void (scicos0_block) (ARGS_scicos0);
extern scicos0_block blocks_absblk, blocks_andlog, blocks_bidon, blocks_gain;
extern scicos0_block blocks_cdummy, blocks_dband, blocks_cosblk;

/*------------------------------------------------
 *     Scicos block simulator 
 *     returns Absolute value of the input 
 *------------------------------------------------*/

void blocks_absblk(scicos_args_poo)
{
  int i;
  for (i = 0 ; i <  *nu ; ++i ) y[i] = Abs(u[i]);
} 


/*------------------------------------------------
 *     Scicos block simulator 
 *     Logical and block
 *     if event input exists synchronuously, output is 1 else -1
 *------------------------------------------------*/

void blocks_andlog(scicos_args_poo)
{
  if ( *flag__ == 1)  y[0] = ( *nevprt != 3 ) ? -1.00 :  1.00; 
}


/*------------------------------------------------
 *     Scicos block simulator 
 *     does nothing 
 *------------------------------------------------*/

void blocks_bidon(scicos_args_poo)
{
}



/*------------------------------------------------
 *     Scicos block simulator 
 *     input to output Gain
 *     rpar=gain matrix
 *------------------------------------------------*/

void blocks_gain(scicos_args_poo)
{
  int un=1;
  dmmul_scicos(rpar,ny,u,nu,y,ny,ny,nu,&un);
}

/*------------------------------------------------
 *     Scicos block simulator 
 *     Dummy state space x'=sin(t)
 *------------------------------------------------*/

void blocks_cdummy(scicos_args_poo)
{
  if ( *flag__ == 0 ) xd[0]=sin(*t);
}

/*------------------------------------------------
 *     Scicos block simulator 
 *     Dead Band, 
 *     if u(i)<0 ,y(i)=min(0,u+DB(i)/2) 
 *     else       y(i)=max(0,u-DB(i)/2) 
 *     DB(i)=rpar(i) 
 *------------------------------------------------*/

void blocks_dband(scicos_args_poo)
{
  int i;
  
  for ( i=0 ; i < *nu ; i++ ) 
    {
      if ( u[i] < 0 ) 
	y[i] = Min(0.00,u[i]+rpar[i]/2.00);
      else  
	y[i] = Max(0.00,u[i]-rpar[i]/2.00);
    }
}


/*------------------------------------------------
 *     Scicos block simulator 
 *------------------------------------------------*/

void blocks_cosblk(scicos_args_poo)
{

  int i;
  for ( i=0; i < *nu ; i++)     y[i]= cos(u[i]);
}

