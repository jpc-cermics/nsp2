#include "integ.h"

#define ls0001_1 ls0001_._1
#define lsr001_1 lsr001_._1

/* Table of constant values */

static int c__0 = 0;
static double c_b18 = 1.;
static int c__1 = 1;

int
nsp_ode_rchek (int *job, lsodar_g g, int *neq, double *y, double *yh, int *nyh,
		  double *g0, double *g1, double *gx, int *jroot, int *irt, double *param)
{
  /* System generated locals */
  int yh_dim1, yh_offset, i__1;
  double d__1;


  /* Local variables */
  double temp1, temp2;
  int i__, iflag, jflag;
  double x, hming;
  double t1;
  int zroot;

  /*lll. optimize 
   *!purpose 
   *this routine checks for the presence of a root in the 
   *vicinity of the current t, in a manner depending on the 
   *input flag job.  it calls subroutine roots to locate the root 
   *as precisely as possible. 
   * 
   *!calling sequence 
   *in addition to variables described previously, rchek 
   *uses the following for communication.. 
   *job    = int flag indicating type of call.. 
   *         job = 1 means the problem is being initialized, and rchek 
   *                 is to look for a root at or very near the initial t. 
   *         job = 2 means a continuation call to the solver was just 
   *                 made, and rchek is to check for a root in the 
   *                 relevant part of the step last taken. 
   *         job = 3 means a successful step was just taken, and rchek 
   *                 is to look for a root in the interval of the step. 
   *g0     = array of length ng, containing the value of g at t = t0. 
   *         g0 is input for job .ge. 2 and on output in all cases. 
   *g1,gx  = arrays of length ng for work space. 
   *irt    = completion flag.. 
   *         irt = 0  means no root was found. 
   *         irt = -1 means job = 1 and a root was found too near to t. 
   *         irt = 1  means a legitimate root was found (job = 2 or 3). 
   *                  on return, t0 is the root location, and y is the 
   *                  corresponding solution vector. 
   *t0     = value of t at one endpoint of interval of interest.  only 
   *         roots beyond t0 in the direction of integration are sought. 
   *         t0 is input if job .ge. 2, and output in all cases. 
   *         t0 is updated by rchek, whether a root is found or not. 
   *tlast  = last value of t returned by the solver (input only). 
   *toutc  = copy of tout (input only). 
   *irfnd  = input flag showing whether the last step taken had a root. 
   *         irfnd = 1 if it did, = 0 if not. 
   *itaskc = copy of itask (input only). 
   *ngc    = copy of ng (input only). 
   *! 
   * 
   */
  /* Parameter adjustments */
  --neq;
  --y;
  yh_dim1 = *nyh;
  yh_offset = yh_dim1 + 1;
  yh -= yh_offset;
  --g0;
  --g1;
  --gx;
  --jroot;

  /* Function Body */
  *irt = 0;
  i__1 = lsr001_1.ngc;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L10: */
      jroot[i__] = 0;
    }
  hming = (Abs (ls0001_1.tn) + Abs (ls0001_1.h__)) * ls0001_1.uround * 100.;
  /* 
   */
  switch (*job)
    {
    case 1:
      goto L100;
    case 2:
      goto L200;
    case 3:
      goto L300;
    }
  /* 
   *evaluate g at initial t, and check for zero values. ------------------ 
   */
 L100:
  lsr001_1.t0 = ls0001_1.tn;
  (*g) (&neq[1], &lsr001_1.t0, &y[1], &lsr001_1.ngc, &g0[1], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  lsr001_1.nge = 1;
  zroot = FALSE;
  i__1 = lsr001_1.ngc;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L110: */
      if ((d__1 = g0[i__], Abs (d__1)) <= 0.)
	{
	  zroot = TRUE;
	}
    }
  if (!zroot)
    {
      goto L190;
    }
  /*g has a zero at t.  look at g at t + (small increment). -------------- 
   */
  temp1 = d_sign (&hming, &ls0001_1.h__);
  lsr001_1.t0 += temp1;
  temp2 = temp1 / ls0001_1.h__;
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L120: */
      y[i__] += temp2 * yh[i__ + (yh_dim1 << 1)];
    }
  (*g) (&neq[1], &lsr001_1.t0, &y[1], &lsr001_1.ngc, &g0[1], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++lsr001_1.nge;
  zroot = FALSE;
  i__1 = lsr001_1.ngc;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L130: */
      if ((d__1 = g0[i__], Abs (d__1)) <= 0.)
	{
	  zroot = TRUE;
	}
    }
  if (!zroot)
    {
      goto L190;
    }
  /*g has a zero at t and also close to t.  take error return. ----------- 
   */
  *irt = -1;
  return 0;
  /* 
   */
 L190:
  return 0;
  /* 
   * 
   */
 L200:
  if (lsr001_1.irfnd == 0)
    {
      goto L260;
    }
  /*if a root was found on the previous step, evaluate g0 = g(t0). ------- 
   */
  nsp_ode_intdy (&lsr001_1.t0, &c__0, &yh[yh_offset], nyh, &y[1], &iflag);
  (*g) (&neq[1], &lsr001_1.t0, &y[1], &lsr001_1.ngc, &g0[1], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++lsr001_1.nge;
  zroot = FALSE;
  i__1 = lsr001_1.ngc;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if ((d__1 = g0[i__], Abs (d__1)) <= 0.)
	{
	  jroot[i__] = 1;
	  zroot = TRUE;
	}
      /* L210: */
    }
  if (!zroot)
    {
      goto L260;
    }
  /*g has a zero at t0.  look at g at t + (small increment). ------------- 
   */
  temp1 = d_sign (&hming, &ls0001_1.h__);
  lsr001_1.t0 += temp1;
  if ((lsr001_1.t0 - ls0001_1.tn) * ls0001_1.h__ < 0.)
    {
      goto L230;
    }
  temp2 = temp1 / ls0001_1.h__;
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L220: */
      y[i__] += temp2 * yh[i__ + (yh_dim1 << 1)];
    }
  goto L240;
 L230:
  nsp_ode_intdy (&lsr001_1.t0, &c__0, &yh[yh_offset], nyh, &y[1], &iflag);
 L240:
  (*g) (&neq[1], &lsr001_1.t0, &y[1], &lsr001_1.ngc, &g0[1], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++lsr001_1.nge;
  zroot = FALSE;
  i__1 = lsr001_1.ngc;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if ((d__1 = g0[i__], Abs (d__1)) > 0.)
	{
	  goto L250;
	}
      if (jroot[i__] == 1)
	{
	  *irt = -1;
	  return 0;
	}
      else
	{
	  jroot[i__] = (int) (-d_sign (&c_b18, &g0[i__]));
	  *irt = 1;
	}
    L250:
      ;
    }
  if (*irt == 1)
    {
      return 0;
    }
  /*g0 has no zero components.  proceed to check relevant interval. ------ 
   */
 L260:
  if (ls0001_1.tn == lsr001_1.tlast)
    {
      goto L390;
    }
  /* 
   */
 L300:
  /*set t1 to tn or toutc, whichever comes first, and get g at t1. ------- 
   */
  if (lsr001_1.itaskc == 2 || lsr001_1.itaskc == 3 || lsr001_1.itaskc == 5)
    {
      goto L310;
    }
  if ((lsr001_1.toutc - ls0001_1.tn) * ls0001_1.h__ >= 0.)
    {
      goto L310;
    }
  t1 = lsr001_1.toutc;
  if ((t1 - lsr001_1.t0) * ls0001_1.h__ <= 0.)
    {
      goto L390;
    }
  nsp_ode_intdy (&t1, &c__0, &yh[yh_offset], nyh, &y[1], &iflag);
  goto L330;
 L310:
  t1 = ls0001_1.tn;
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L320: */
      y[i__] = yh[i__ + yh_dim1];
    }
 L330:
  (*g) (&neq[1], &t1, &y[1], &lsr001_1.ngc, &g1[1], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++lsr001_1.nge;
  /*call roots to search for root in interval from t0 to t1. ------------- 
   */
  jflag = 0;
 L350:
  nsp_ode_roots (&lsr001_1.ngc, &hming, &jflag, &lsr001_1.t0, &t1, &g0[1],
		    &g1[1], &gx[1], &x, &jroot[1]);
  if (jflag > 1)
    {
      goto L360;
    }
  nsp_ode_intdy (&x, &c__0, &yh[yh_offset], nyh, &y[1], &iflag);
  (*g) (&neq[1], &x, &y[1], &lsr001_1.ngc, &gx[1], param);
  if (ierode_1.iero > 0)
    {
      return 0;
    }
  ++lsr001_1.nge;
  goto L350;
 L360:
  lsr001_1.t0 = x;
  C2F (dcopy) (&lsr001_1.ngc, &gx[1], &c__1, &g0[1], &c__1);
  if (jflag == 4)
    {
      goto L390;
    }
  /*found a root.  interpolate to x and return. -------------------------- 
   */
  nsp_ode_intdy (&x, &c__0, &yh[yh_offset], nyh, &y[1], &iflag);
  *irt = 1;
  return 0;
  /* 
   */
 L390:
  return 0;
  /*----------------------- end of subroutine rchek ----------------------- 
   */
}				/* rchek_ */
