#include "integ.h"

/* Common Block Declarations */

#define ls0001_1 ls0001_._1
#define lsr001_1 lsr001_._1


/* Table of constant values */

static int c__0 = 0;
static double c_b20 = 2.;
static double c_b21 = 1.;
static int c__1 = 1;


/*    ------------------ masking ---------------- 
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
 *         irt = 2 means a change from zero to a non-zero value has been 
 *         occured, so do a cold restart to reevaluate the modes 
 *         of if-then-else, because they have mode 
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

int
nsp_ode_rchek2 (int *job, lsodar_g g, int *neq, double *y, double *yh,
		   int *nyh, double *g0, double *g1, double *gx, int *jroot,
		   int *irt, int *iwork, double *param)
{
  /* Initialized data */

  static double zero = 0.;

  /* System generated locals */
  int yh_dim1, yh_offset, i__1, i__2;
  double d__1;



  /* Local variables */
  double t1, x, hming;
  int i__, iflag, jflag;
  int lmask;
  int mroot;
  int zroot;

  /*lll. optimize 
   *    ------------------ masking ---------------- 
   *    pointers into iwork: 
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
  --iwork;

  /* Function Body */
  *irt = 0;
  /*    -------------- masking obtaining the mask adresses----------------- 
   */
  lmask = iwork[18] - lsr001_1.ngc;
  /*    -------------- masking ----------------- 
   */
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
  /*    -------------- masking: disabling masks in cold-major-time-step------ 
   */
  i__1 = lsr001_1.ngc;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      jroot[i__] = 0;
      /* L103: */
      iwork[lmask + i__] = 0;
    }
  lsr001_1.t0 = ls0001_1.tn;
  (*g) (&neq[1], &lsr001_1.t0, &y[1], &lsr001_1.ngc, &g0[1], param);
  ++lsr001_1.nge;
  i__1 = lsr001_1.ngc;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if ((d__1 = g0[i__], Abs (d__1)) == zero)
	{
	  iwork[lmask + i__] = 1;
	}
      /* L110: */
    }
  return 0;
  /*    -------------- masking ----------------- 
   * 
   */
 L200:
  /*    in the previous call there was not a root, so this part can be ignored. 
   *     if (iwork(lirfnd) .eq. 0) go to 260 
   */
  i__1 = lsr001_1.ngc;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      jroot[i__] = 0;
      /* L203: */
      iwork[lmask + i__] = 0;
    }
  /*    if a root was found on the previous step, evaluate r0 = r(t0). ------- 
   */
  nsp_ode_intdy (&lsr001_1.t0, &c__0, &yh[yh_offset], nyh, &y[1], &iflag);
  (*g) (&neq[1], &lsr001_1.t0, &y[1], &lsr001_1.ngc, &g0[1], param);
  ++lsr001_1.nge;
  i__1 = lsr001_1.ngc;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if ((d__1 = g0[i__], Abs (d__1)) == zero)
	{
	  iwork[lmask + i__] = 1;
	}
      /* L210: */
    }
  /*    r0 has no zero components.  proceed to check relevant interval. ------ 
   *    260  continue 
   */
  if (ls0001_1.tn == lsr001_1.tlast)
    {
      return 0;
    }
  /* 
   *    -------------- try in manor-time-steps ----- 
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
  /*    call droots to search for root in interval from t0 to t1. ----------- 
   */
  jflag = 0;
  i__1 = lsr001_1.ngc;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      jroot[i__] = iwork[lmask + i__];
      /* L340: */
    }
 L350:
  nsp_ode_roots2 (&lsr001_1.ngc, &hming, &jflag, &lsr001_1.t0, &t1, &g0[1],
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
  if (jflag == 2)
    {
      /*root found 
       */
      zroot = FALSE;
      mroot = FALSE;
      i__1 = lsr001_1.ngc;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  if (iwork[lmask + i__] == 1)
	    {
	      if ((d__1 = g1[i__], Abs (d__1)) != zero)
		{
		  jroot[i__] = (int) d_sign (&c_b20, &g1[i__]);
		  mroot = TRUE;
		}
	      else
		{
		  jroot[i__] = 0;
		}
	    }
	  else
	    {
	      if ((d__1 = g1[i__], Abs (d__1)) == zero)
		{
		  jroot[i__] = (int) (-d_sign (&c_b21, &g0[i__]));
		  zroot = TRUE;
		}
	      else
		{
		  if (d_sign (&c_b21, &g0[i__]) != d_sign (&c_b21, &g1[i__]))
		    {
		      d__1 = g1[i__] - g0[i__];
		      jroot[i__] = (int) d_sign (&c_b21, &d__1);
		      zroot = TRUE;
		    }
		  else
		    {
		      jroot[i__] = 0;
		    }
		}
	    }
	  /* L361: */
	}
      nsp_ode_intdy (&x, &c__0, &yh[yh_offset], nyh, &y[1], &iflag);
      if (zroot)
	{
	  i__1 = lsr001_1.ngc;
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      if ((i__2 = jroot[i__], Abs (i__2)) == 2)
		{
		  jroot[i__] = 0;
		}
	      /* L380: */
	    }
	  mroot = FALSE;
	  *irt = 1;
	}
      if (mroot)
	{
	  *irt = 2;
	}
    }
  lsr001_1.t0 = x;
  C2F (dcopy) (&lsr001_1.ngc, &gx[1], &c__1, &g0[1], &c__1);
  return 0;
  /* 
   */
 L390:
  return 0;
}
