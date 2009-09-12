
#include "integ.h"

#define lsr001_1 lsr001_._2

/* Table of constant values */

static double c_b5 = 1.;
static int c__1 = 1;

int
nsp_ode_roots (int *ng, double *hmin, int *jflag, double *x0, double *x1,
		  double *g0, double *g1, double *gx, double *x, int *jroot)
{
  /* Initialized data */

  static double zero = 0.;

  /* System generated locals */
  int i__1;
  double d__1, d__2;


  /* Local variables */
  double tmax;
  int i__;
  double t2;
  int xroot, zroot, sgnchg;
  int imxold, nxlast;

  /*lll. optimize 
   *!purpose 
   *this subroutine finds the leftmost root of a set of arbitrary 
   *functions gi(x) (i = 1,...,ng) in an interval (x0,x1).  only roots 
   *of odd multiplicity (i.e. changes of sign of the gi) are found. 
   *here the sign of x1 - x0 is arbitrary, but is constant for a given 
   *problem, and -leftmost- means nearest to x0. 
   *the values of the vector-valued function g(x) = (gi, i=1...ng) 
   *are communicated through the call sequence of roots. 
   *the method used is the illinois algorithm. 
   * 
   *!reference.. 
   *kathie l. hiebert and lawrence f. shampine, implicitly defined 
   *output points for solutions of ode-s, sandia report sand80-0180, 
   *february, 1980. 
   * 
   *!Calling sequence 
   * 
   *ng     = number of functions gi, or the number of components of 
   *         the vector valued function g(x).  input only. 
   * 
   *hmin   = resolution parameter in x.  input only.  when a root is 
   *         found, it is located only to within an error of hmin in x. 
   *         typically, hmin should be set to something on the order of 
   *              100 * uround * Max(Abs(x0),abs(x1)), 
   *         where uround is the unit roundoff of the machine. 
   * 
   *jflag  = int flag for input and output communication. 
   * 
   *         on input, set jflag = 0 on the first call for the problem, 
   *         and leave it unchanged until the problem is completed. 
   *         (the problem is completed when jflag .ge. 2 on return.) 
   * 
   *         on output, jflag has the following values and meanings.. 
   *         jflag = 1 means roots needs a value of g(x).  set gx = g(x) 
   *                   and call roots again. 
   *         jflag = 2 means a root has been found.  the root is 
   *                   at x, and gx contains g(x).  (actually, x is the 
   *                   rightmost approximation to the root on an interval 
   *                   (x0,x1) of size hmin or less.) 
   *         jflag = 3 means x = x1 is a root, with one or more of the gi 
   *                   being zero at x1 and no sign changes in (x0,x1). 
   *                   gx contains g(x) on output. 
   *         jflag = 4 means no roots (of odd multiplicity) were 
   *                   found in (x0,x1) (no sign changes). 
   * 
   *x0,x1  = endpoints of the interval where roots are sought. 
   *         x1 and x0 are input when jflag = 0 (first call), and 
   *         must be left unchanged between calls until the problem is 
   *         completed.  x0 and x1 must be distinct, but x1 - x0 may be 
   *         of either sign.  however, the notion of -left- and -right- 
   *         will be used to mean nearer to x0 or x1, respectively. 
   *         when jflag .ge. 2 on return, x0 and x1 are output, and 
   *         are the endpoints of the relevant interval. 
   * 
   *g0,g1  = arrays of length ng containing the vectors g(x0) and g(x1), 
   *         respectively.  when jflag = 0, g0 and g1 are input and 
   *         none of the g0(i) should be be zero. 
   *         when jflag .ge. 2 on return, g0 and g1 are output. 
   * 
   *gx     = array of length ng containing g(x).  gx is input 
   *         when jflag = 1, and output when jflag .ge. 2. 
   * 
   *x      = independent variable value.  output only. 
   *         when jflag = 1 on output, x is the point at which g(x) 
   *         is to be evaluated and loaded into gx. 
   *         when jflag = 2 or 3, x is the root. 
   *         when jflag = 4, x is the right endpoint of the interval, x1. 
   * 
   *JROOT  = int array of length NRT.  Output only. 
   *        When JFLAG = 2 or 3, JROOT indicates which components 
   *        of R(x) have a root at X, and the direction of the sign 
   *        change across the root in the direction of integration. 
   *        JROOT(i) =  1 if Ri has a root and changes from - to +. 
   *        JROOT(i) = -1 if Ri has a root and changes from + to -. 
   *         Otherwise JROOT(i) = 0. 
   *    ! 
   *note.. this routine uses the common block /lsr001/ to save 
   *the values of certain variables between calls (own variables). 
   *----------------------------------------------------------------------- 
   */
  /* Parameter adjustments */
  --jroot;
  --gx;
  --g1;
  --g0;

  /* Function Body */
  /* 
   */
  if (*jflag == 1)
    {
      goto L200;
    }
  /*jflag .ne. 1.  check for change in sign of g or zero at x1. ---------- 
   */
  lsr001_1.imax = 0;
  tmax = zero;
  zroot = FALSE;
  i__1 = *ng;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if ((d__1 = g1[i__], Abs (d__1)) > zero)
	{
	  goto L110;
	}
      zroot = TRUE;
      goto L120;
      /*at this point, g0(i) has been checked and cannot be zero. ------------ 
       */
    L110:
      if (d_sign (&c_b5, &g0[i__]) == d_sign (&c_b5, &g1[i__]))
	{
	  goto L120;
	}
      t2 = (d__1 = g1[i__] / (g1[i__] - g0[i__]), Abs (d__1));
      if (t2 <= tmax)
	{
	  goto L120;
	}
      tmax = t2;
      lsr001_1.imax = i__;
    L120:
      ;
    }
  if (lsr001_1.imax > 0)
    {
      goto L130;
    }
  sgnchg = FALSE;
  goto L140;
 L130:
  sgnchg = TRUE;
 L140:
  if (!sgnchg)
    {
      goto L400;
    }
  /*there is a sign change.  find the first root in the interval. -------- 
   */
  xroot = FALSE;
  nxlast = 0;
  lsr001_1.last = 1;
  /* 
   *repeat until the first root in the interval is found.  loop point. --- 
   */
 L150:
  if (xroot)
    {
      goto L300;
    }
  if (nxlast == lsr001_1.last)
    {
      goto L160;
    }
  lsr001_1.alpha = 1.;
  goto L180;
 L160:
  if (lsr001_1.last == 0)
    {
      goto L170;
    }
  lsr001_1.alpha *= .5;
  goto L180;
 L170:
  lsr001_1.alpha *= 2.;
 L180:
  lsr001_1.x2 =
    *x1 - (*x1 - *x0) * g1[lsr001_1.imax] / (g1[lsr001_1.imax] -
					     lsr001_1.alpha *
					     g0[lsr001_1.imax]);
  if ((d__1 = lsr001_1.x2 - *x0, Abs (d__1)) < *hmin
      && (d__2 = *x1 - *x0, Abs (d__2)) > *hmin * 10.)
    {
      lsr001_1.x2 = *x0 + (*x1 - *x0) * .1;
    }
  *jflag = 1;
  *x = lsr001_1.x2;
  /*return to the calling routine to get a value of gx = g(x). ----------- 
   */
  return 0;
  /*check to see in which interval g changes sign. ----------------------- 
   */
 L200:
  imxold = lsr001_1.imax;
  lsr001_1.imax = 0;
  tmax = zero;
  zroot = FALSE;
  i__1 = *ng;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if ((d__1 = gx[i__], Abs (d__1)) > zero)
	{
	  goto L210;
	}
      zroot = TRUE;
      goto L220;
      /*neither g0(i) nor gx(i) can be zero at this point. ------------------- 
       */
    L210:
      if (d_sign (&c_b5, &g0[i__]) == d_sign (&c_b5, &gx[i__]))
	{
	  goto L220;
	}
      t2 = (d__1 = gx[i__] / (gx[i__] - g0[i__]), Abs (d__1));
      if (t2 <= tmax)
	{
	  goto L220;
	}
      tmax = t2;
      lsr001_1.imax = i__;
    L220:
      ;
    }
  if (lsr001_1.imax > 0)
    {
      goto L230;
    }
  sgnchg = FALSE;
  lsr001_1.imax = imxold;
  goto L240;
 L230:
  sgnchg = TRUE;
 L240:
  nxlast = lsr001_1.last;
  if (!sgnchg)
    {
      goto L250;
    }
  /*sign change between x0 and x2, so replace x1 with x2. ---------------- 
   */
  *x1 = lsr001_1.x2;
  C2F (dcopy) (ng, &gx[1], &c__1, &g1[1], &c__1);
  lsr001_1.last = 1;
  xroot = FALSE;
  goto L270;
 L250:
  if (!zroot)
    {
      goto L260;
    }
  /*zero value at x2 and no sign change in (x0,x2), so x2 is a root. ----- 
   */
  *x1 = lsr001_1.x2;
  C2F (dcopy) (ng, &gx[1], &c__1, &g1[1], &c__1);
  xroot = TRUE;
  goto L270;
  /*no sign change between x0 and x2.  replace x0 with x2. --------------- 
   */
 L260:
  C2F (dcopy) (ng, &gx[1], &c__1, &g0[1], &c__1);
  *x0 = lsr001_1.x2;
  lsr001_1.last = 0;
  xroot = FALSE;
 L270:
  if ((d__1 = *x1 - *x0, Abs (d__1)) <= *hmin)
    {
      xroot = TRUE;
    }
  goto L150;
  /* 
   *return with x1 as the root.  set jroot.  set x = x1 and gx = g1. ----- 
   */
 L300:
  *jflag = 2;
  *x = *x1;
  C2F (dcopy) (ng, &g1[1], &c__1, &gx[1], &c__1);
  i__1 = *ng;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      jroot[i__] = 0;
      if ((d__1 = g1[i__], Abs (d__1)) == 0.)
	{
	  jroot[i__] = (int) (-d_sign (&c_b5, &g0[i__]));
	  goto L320;
	}
      if (d_sign (&c_b5, &g0[i__]) != d_sign (&c_b5, &g1[i__]))
	{
	  d__1 = g1[i__] - g0[i__];
	  jroot[i__] = (int) d_sign (&c_b5, &d__1);
	}
    L320:
      ;
    }
  return 0;
  /* 
   *    no sign change in the interval.  check for zero at right endpoint. --- 
   */
 L400:
  if (!zroot)
    {
      goto L420;
    }
  /* 
   *zero value at x1 and no sign change in (x0,x1).  return jflag = 3. --- 
   */
  *x = *x1;
  C2F (dcopy) (ng, &g1[1], &c__1, &gx[1], &c__1);
  i__1 = *ng;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      jroot[i__] = 0;
      if ((d__1 = g1[i__], Abs (d__1)) <= zero)
	{
	  jroot[i__] = (int) (-d_sign (&c_b5, &g0[i__]));
	}
      /* L410: */
    }
  *jflag = 3;
  return 0;
  /* 
   *no sign changes in this interval.  set x = x1, return jflag = 4. ----- 
   */
 L420:
  C2F (dcopy) (ng, &g1[1], &c__1, &gx[1], &c__1);
  *x = *x1;
  *jflag = 4;
  return 0;
  /*----------------------- end of subroutine roots ----------------------- 
   */
}				/* roots_ */
