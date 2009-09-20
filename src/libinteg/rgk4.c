#include <nsp/math.h>
#include "integ.h" 
#include "rk.h"

static struct
{
  int kmax, kount;
  double dxsav, xp[200], yp[10*200] ;
} path_rgk4;

/* Table of constant values */

static double c_b2 = 1e-4;
static double c_b3 = 0.;

static int rk_odeint (double *ystart, int *nvar, double *x1, double *x2, double *eps,
		      double *h1, double *hmin, int *nok, int *nbad, ode_f derivs,
		      rk_rkqc_f rkqc, void *param);
static int rk_rkqc (double *y, double *dydx, int *n, double *x, double *htry,
		    double *eps, double *yscal, double *hdid, double *hnext,ode_f derivs,
		    void *param);


/*    extraits de numerical recipies 
 *    runge kutta d'ordre 4 adaptatif 
 */

int rk_lsrgk (ode_f f, int *neq, double *y, double *t, double *tout, int *itol,
	      double *rtol, double *atol, int *itask, int *istate, int *iopt,
	      double *rwork, int *lrw, int *iwork, int *liw, ode_jac jac, int *mf,
	      void *param)
{
  int nbad;
  int nok;

  /* Parameter adjustments */
  --neq;
  --y;
  --rtol;
  --atol;
  --rwork;
  --iwork;

  /* Function Body */

  ierode_1.iero = 0;

  rk_odeint (&y[1], &neq[1], t, tout, &atol[1], &c_b2, &c_b3, &nok, &nbad, f, rk_rkqc,param);
  *t = *tout;
  if (ierode_1.iero > 0)
    {
      *istate = -1;
    }
  return 0;
}

/*
 *    subroutine de Num Recipies modifiee pour avoir le meme test 
 *    d'arret que lsode 
 */

static int rk_odeint (double *ystart, int *nvar, double *x1, double *x2, double *eps,
		      double *h1, double *hmin, int *nok, int *nbad, ode_f derivs,
		      rk_rkqc_f rkqc, void *param)
{
  int i__1;
  double d__1, d__2, hdid, dydx[10], xsav;
  int nstp;
  double h__;
  int i__;
  double x, y[10], yscal[10], hnext;
  
  /* Parameter adjustments */
  --ystart;

  /* Function Body */
  ierode_1.iero = 0;
  if ((d__1 = *x2 - *x1, Abs (d__1)) <= 1e-30)
    {
      return 0;
    }
  x = *x1;
  d__1 = *x2 - *x1;
  h__ = d_sign (h1, &d__1);
  *nok = 0;
  *nbad = 0;
  path_rgk4.kount = 0;
  i__1 = *nvar;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      y[i__ - 1] = ystart[i__];
      /* L11: */
    }
  xsav = x - path_rgk4.dxsav * 2.;
  for (nstp = 1; nstp <= 10000; ++nstp)
    {
      (*derivs) (nvar, &x, y, dydx, param);
      if (ierode_1.iero > 0)
	{
	  return 0;
	}
      i__1 = *nvar;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  yscal[i__ - 1] = (d__1 = y[i__ - 1], Abs (d__1)) + (d__2 =
							      h__ * dydx[i__ -
									 1],
							      Abs (d__2)) +
	    1e-30;
	  /* L12: */
	}
      if (path_rgk4.kmax > 0)
	{
	  if ((d__1 = x - xsav, Abs (d__1)) > Abs (path_rgk4.dxsav))
	    {
	      if (path_rgk4.kount < path_rgk4.kmax - 1)
		{
		  ++path_rgk4.kount;
		  path_rgk4.xp[path_rgk4.kount - 1] = x;
		  i__1 = *nvar;
		  for (i__ = 1; i__ <= i__1; ++i__)
		    {
		      path_rgk4.yp[i__ + path_rgk4.kount * 10 - 11] = y[i__ - 1];
		      /* L13: */
		    }
		  xsav = x;
		}
	    }
	}
      if ((x + h__ - *x2) * (x + h__ - *x1) > 0.)
	{
	  h__ = *x2 - x;
	}
      (*rkqc) (y, dydx, nvar, &x, &h__, eps, yscal, &hdid, &hnext,derivs,param);
      if (ierode_1.iero > 0)
	{
	  return 0;
	}
      if (hdid == h__)
	{
	  ++(*nok);
	}
      else
	{
	  ++(*nbad);
	}
      if ((x - *x2) * (*x2 - *x1) >= 0.)
	{
	  i__1 = *nvar;
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      ystart[i__] = y[i__ - 1];
	      /* L14: */
	    }
	  if (path_rgk4.kmax != 0)
	    {
	      ++path_rgk4.kount;
	      path_rgk4.xp[path_rgk4.kount - 1] = x;
	      i__1 = *nvar;
	      for (i__ = 1; i__ <= i__1; ++i__)
		{
		  path_rgk4.yp[i__ + path_rgk4.kount * 10 - 11] = y[i__ - 1];
		  /* L15: */
		}
	    }
	  return 0;
	}
      if (Abs (hnext) < *hmin)
	{
	  /* 
	     17   format('stepsize ',e10.3,' smaller than minimum ',e10.3)
	     write(messag, 17) hnext,hmin
	  */
	  hnext = *hmin;
	}
      h__ = hnext;
      /* L16: */
    }
  ierode_1.iero = -1;
  /*     print *, 'Trop d''iterations a faire pour la precision demandee.' 
   */
  return 0;
}	


static int rk_rk4 (double *y, double *dydx, int *n, double *x, double *h__, double *yout,
		   ode_f derivs, void *param)
{
  int i__1;
  double d__1;

  /* Local variables */
  int i__;
  double h6, hh, xh, yt[10], dym[10], dyt[10];

  /* Parameter adjustments */
  --yout;
  --dydx;
  --y;

  /* Function Body */
  ierode_1.iero = 0;
  hh = *h__ * .5;
  h6 = *h__ / 6.;
  xh = *x + hh;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      yt[i__ - 1] = y[i__] + hh * dydx[i__];
      /* L11: */
    }
  (*derivs) (n, &xh, yt, dyt,param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      yt[i__ - 1] = y[i__] + hh * dyt[i__ - 1];
      /* L12: */
    }
  (*derivs) (n, &xh, yt, dym,param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      yt[i__ - 1] = y[i__] + *h__ * dym[i__ - 1];
      dym[i__ - 1] = dyt[i__ - 1] + dym[i__ - 1];
      /* L13: */
    }
  d__1 = *x + *h__;
  (*derivs) (n, &d__1, yt, dyt,param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      yout[i__] =
	y[i__] + h6 * (dydx[i__] + dyt[i__ - 1] + dym[i__ - 1] * 2.);
      /* L14: */
    }
  return 0;
}		

static int rk_rkqc (double *y, double *dydx, int *n, double *x, double *htry,
		    double *eps, double *yscal, double *hdid, double *hnext,ode_f derivs,
		    void *param)
{
  /* System generated locals */
  int i__1;
  double d__1, d__2, d__3;

  /* Local variables */
  double dysav[10], pgrow, ytemp[10], hh, errmax, pshrnk;
  double xsav, ysav[10], h__;
  int i__;

  /*     implicit undefined (a-z) 
   */
  /* Parameter adjustments */
  --yscal;
  --dydx;
  --y;

  /* Function Body */
  ierode_1.iero = 0;
  pgrow = -.2;
  pshrnk = -.25;
  xsav = *x;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ysav[i__ - 1] = y[i__];
      dysav[i__ - 1] = dydx[i__];
      /* L11: */
    }
  h__ = *htry;
 L1:
  hh = h__ * .5;
  rk_rk4 (ysav, dysav, n, &xsav, &hh, ytemp, derivs, param);
  *x = xsav + hh;
  (*derivs) (n, x, ytemp, &dydx[1],param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  rk_rk4 (ytemp, &dydx[1], n, x, &hh, &y[1], derivs, param);
  *x = xsav + h__;
  if (*x == xsav)
    {
      /*        print *, 'stepsize not significant in rkqc.' 
       */
      ierode_1.iero = 1;
      return 0;
    }
  rk_rk4 (ysav, dysav, n, &xsav, &h__, ytemp, derivs, param);
  errmax = 0.;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ytemp[i__ - 1] = y[i__] - ytemp[i__ - 1];
      /*Computing MAX 
       */
      d__2 = errmax, d__3 = (d__1 =
			     ytemp[i__ - 1] / (yscal[i__] * *eps),
			     Abs (d__1));
      errmax = Max (d__2, d__3);
      /* L12: */
    }
  if (errmax > 1.)
    {
      h__ = h__ * .9 * pow (errmax,pshrnk);
      goto L1;
    }
  else
    {
      *hdid = h__;
      if (errmax > 6e-4)
	{
	  *hnext = h__ * .9 * pow (errmax, pgrow);
	}
      else
	{
	  *hnext = h__ * 4.;
	}
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      y[i__] += ytemp[i__ - 1] * .0666666667;
      /* L13: */
    }
  return 0;
}


