#include <nsp/math.h>
#include "rk.h"

static int rk_fehl2 (ode_f fydot2, int *neqn, double *y, double *t, double *h__,
		     double *yp, double *f1, double *f2, double *f3, double *f4,
		     double *f5, double *s);


/* 
 *    fehlberg fourth-fifth order runge-kutta method 
 */

int rk_rksimp (ode_f fydot2, int *neqn, double *y, double *t, double *tout,
	       int *itol, double *rerr, double *aerr__, int *itask, int *iflag,
	       int *iopt, double *work, int *lrw, int *iwork, int *liw,
	       double *bjac, int *mf)
{
  int i__1;
  double d__1, d__2;
  double h__;
  int k;
  double scale, eeoet;
  int k1, k2, k3, k4, k5, k6;
  double ae, ee, et, esttol;
  /* Parameter adjustments */
  --iwork;
  --work;
  --y;

  /* Function Body */
  C2F(ierode).iero = 0;
  /* 
   *    compute indices for the splitting of the work array 
   * 
   */
  scale = 2. / *rerr;
  ae = scale * *aerr__;
  k1 = *neqn + 1;
  k2 = k1 + *neqn;
  k3 = k2 + *neqn;
  k4 = k3 + *neqn;
  k5 = k4 + *neqn;
  k6 = k5 + *neqn;
  /* 
   *    this interfacing routine merely relieves the user of a long 
   *    calling list via the splitting apart of two working storage 
   *    arrays. if this is not compatible with the users compiler, 
   *    he must use rkfs directly. 
   * 
   */
  h__ = *tout - *t;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      work[k6 + k - 1] = y[k];
      /* L33: */
    }
  rk_fehl2 ( fydot2, neqn, &y[1], t, &h__, &work[1], &work[k1],
	     &work[k2], &work[k3], &work[k4], &work[k5], &work[k6]);
  /* 
   */
  eeoet = 0.;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      et = (d__1 = work[k6 + k - 1], Abs (d__1)) + (d__2 =
						    work[k1 - 1 + k],
						    Abs (d__2)) + ae;
      if (et > 0.)
	{
	  goto L240;
	}
      /* 
       *      inappropriate error tolerance 
       */
      *iflag = 4;
      return 0;
      /* 
       */
    L240:
      ee = (d__1 =
	    work[k] * -2090. + (work[k3 - 1 + k] * 21970. -
				work[k4 - 1 + k] * 15048.) + (work[k2 - 1 +
								   k] *
							      22528. -
							      work[k5 - 1 +
								   k] *
							      27360.),
	    Abs (d__1));
      /* L250: */
      /*Computing MAX 
       */
      d__1 = eeoet, d__2 = ee / et;
      eeoet = Max (d__1, d__2);
    }
  /* 
   */
  esttol = Abs (h__) * eeoet * scale / 752400.;
  /* 
   */
  if (esttol <= 1.)
    {
      /*       OK 
       */
      *iflag = 2;
      *t = *tout;
      /*     write(6,*) esttol,scale,eeoet,ee,et 
       */
      return 0;
    }
  *iflag = 3;
  /* 
   */
  return 0;
}				/* rksimp_ */

static int rk_fehl2 (ode_f fydot2, int *neqn, double *y, double *t, double *h__,
		     double *yp, double *f1, double *f2, double *f3, double *f4,
		     double *f5, double *s)
{
  int i__1;
  double d__1;

  int k;
  double ch;

  /* 
   *    fehlberg fourth-fifth order runge-kutta method 
   * 
   */

  /* Parameter adjustments */
  --s;
  --f5;
  --f4;
  --f3;
  --f2;
  --f1;
  --yp;
  --y;

  /* Function Body */
  (*fydot2) (neqn, t, &y[1], &yp[1]);
  if (C2F(ierode).iero > 0)
    {
      return 0;
    }
  ch = *h__ / 4.;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      /* L221: */
      y[k] += ch * yp[k];
    }
  d__1 = *t + ch;
  (*fydot2) (neqn, &d__1, &y[1], &f1[1]);
  if (C2F(ierode).iero > 0)
    {
      return 0;
    }
  /* 
   */
  ch = *h__ * 3. / 32.;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      /* L222: */
      y[k] = s[k] + ch * (yp[k] + f1[k] * 3.);
    }
  d__1 = *t + *h__ * 3. / 8.;
  (*fydot2) (neqn, &d__1, &y[1], &f2[1]);
  if (C2F(ierode).iero > 0)
    {
      return 0;
    }
  /* 
   */
  ch = *h__ / 2197.;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      /* L223: */
      y[k] = s[k] + ch * (yp[k] * 1932. + (f2[k] * 7296. - f1[k] * 7200.));
    }
  d__1 = *t + *h__ * 12. / 13.;
  (*fydot2) (neqn, &d__1, &y[1], &f3[1]);
  if (C2F(ierode).iero > 0)
    {
      return 0;
    }
  /* 
   */
  ch = *h__ / 4104.;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      /* L224: */
      y[k] =
	s[k] + ch * (yp[k] * 8341. - f3[k] * 845. +
		     (f2[k] * 29440. - f1[k] * 32832.));
    }
  d__1 = *t + *h__;
  (*fydot2) (neqn, &d__1, &y[1], &f4[1]);
  if (C2F(ierode).iero > 0)
    {
      return 0;
    }
  /* 
   */
  ch = *h__ / 20520.;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      /* L225: */
      y[k] =
	s[k] + ch * (yp[k] * -6080. + (f3[k] * 9295. - f4[k] * 5643.) +
		     (f1[k] * 41040. - f2[k] * 28352.));
    }
  d__1 = *t + *h__ / 2.;
  (*fydot2) (neqn, &d__1, &y[1], &f5[1]);
  if (C2F(ierode).iero > 0)
    {
      return 0;
    }
  /* 
   *    compute approximate solution at t+h 
   * 
   */
  ch = *h__ / 7618050.;
  i__1 = *neqn;
  for (k = 1; k <= i__1; ++k)
    {
      y[k] =
	s[k] + ch * (yp[k] * 902880. + (f3[k] * 3855735. - f4[k] * 1371249.) +
		     (f2[k] * 3953664. + f5[k] * 277020.));
      /* 
       */
      /* L230: */
    }
  /*     write(6,*) 'endfelh2:',y(1),y(2) 
   */
  return 0;
}

