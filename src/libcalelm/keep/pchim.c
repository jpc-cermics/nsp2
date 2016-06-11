/* pchim.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

int nsp_calpack_dpchim (int *n, double *x, double *f, double *d__, int *incfd)
{
  /* Initialized data */

  static double zero = 0.;
  static double three = 3.;

  /* System generated locals */
  int f_dim1, f_offset, d_dim1, d_offset, i__1;
  double d__1, d__2;

  /* Local variables */
  double dmin__, dmax__, cres, hsum, drat1, drat2;
  int i__;
  double  h1, h2, w1, w2;
  int nless1;
  double hsumt3;
  double del1, del2;

  /* 
   *  This subroutine comes from the slatec / cmlib and has 
   *  been slightly modified for inclusion in scilab : all 
   *  the error treatment has been removed (and so the 
   *  parameter IERR) because this is done by the scilab 
   *  interface. 
   * 
   ****BEGIN PROLOGUE  DPCHIM 
   ****DATE WRITTEN   811103   (YYMMDD) 
   ****REVISION DATE  870707   (YYMMDD) 
   ****CATEGORY NO.  E1B 
   ****KEYWORDS  LIBRARY=SLATEC(PCHIP), 
   *            TYPE=DOUBLE PRECISION(PCHIM-S DPCHIM-D), 
   *            CUBIC HERMITE INTERPOLATION,MONOTONE INTERPOLATION, 
   *            PIECEWISE CUBIC INTERPOLATION 
   ****AUTHOR  FRITSCH, F. N., (LLNL) 
   *            MATHEMATICS AND STATISTICS DIVISION 
   *            LAWRENCE LIVERMORE NATIONAL LABORATORY 
   *            P.O. BOX 808  (L-316) 
   *            LIVERMORE, CA  94550 
   *            FTS 532-4275, (415) 422-4275 
   ****PURPOSE  Set derivatives needed to determine a monotone piecewise 
   *           cubic Hermite interpolant to given data.  Boundary values 
   *           are provided which are compatible with monotonicity.  The 
   *           interpolant will have an extremum at each point where mono- 
   *           tonicity switches direction.  (See DPCHIC if user control 
   *           is desired over boundary or switch conditions.) 
   ****DESCRIPTION 
   * 
   *      **** Double Precision version of PCHIM **** 
   * 
   *         DPCHIM:  Piecewise Cubic Hermite Interpolation to 
   *                 Monotone data. 
   * 
   *    Sets derivatives needed to determine a monotone piecewise cubic 
   *    Hermite interpolant to the data given in X and F. 
   * 
   *    Default boundary conditions are provided which are compatible 
   *    with monotonicity.  (See DPCHIC if user control of boundary con- 
   *    ditions is desired.) 
   * 
   *    If the data are only piecewise monotonic, the interpolant will 
   *    have an extremum at each point where monotonicity switches direc- 
   *    tion.  (See DPCHIC if user control is desired in such cases.) 
   * 
   *    To facilitate two-dimensional applications, includes an increment 
   *    between successive values of the F- and D-arrays. 
   * 
   *    The resulting piecewise cubic Hermite function may be evaluated 
   *    by DPCHFE or DPCHFD. 
   * 
   *---------------------------------------------------------------------- 
   * 
   * Calling sequence: 
   * 
   *       PARAMETER  (INCFD = ...) 
   *       INT  N, IERR 
   *       DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N) 
   * 
   *       CALL  DPCHIM (N, X, F, D, INCFD, IERR) 
   * 
   *  Parameters: 
   * 
   *    N -- (input) number of data points.  (Error return if N.LT.2 .) 
   *          If N=2, simply does linear interpolation. 
   * 
   *    X -- (input) real*8 array of independent variable values.  The 
   *          elements of X must be strictly increasing: 
   *               X(I-1) .LT. X(I),  I = 2(1)N. 
   *          (Error return if not.) 
   * 
   *    F -- (input) real*8 array of dependent variable values to be 
   *          interpolated.  F(1+(I-1)*INCFD) is value corresponding to 
   *          X(I).  DPCHIM is designed for monotonic data, but it will 
   *          work for any F-array.  It will force extrema at points where 
   *          monotonicity switches direction.  If some other treatment of 
   *          switch points is desired, DPCHIC should be used instead. 
   *                                    ----- 
   *    D -- (output) real*8 array of derivative values at the data 
   *          points.  If the data are monotonic, these values will 
   *          determine a monotone cubic Hermite function. 
   *          The value corresponding to X(I) is stored in 
   *               D(1+(I-1)*INCFD),  I=1(1)N. 
   *          No other entries in D are changed. 
   * 
   *    INCFD -- (input) increment between successive values in F and D. 
   *          This argument is provided primarily for 2-D applications. 
   *          (Error return if  INCFD.LT.1 .) 
   * 
   *    IERR -- (output) error flag.  --REMOVED IN THIS VERSION-- 
   * 
   ****REFERENCES  1. F.N.FRITSCH AND R.E.CARLSON, 'MONOTONE PIECEWISE 
   *                CUBIC INTERPOLATION,' SIAM J.NUMER.ANAL. 17, 2 (APRIL 
   *                1980), 238-246. 
   *              2. F.N.FRITSCH AND J.BUTLAND, 'A METHOD FOR CONSTRUCTING 
   *                LOCAL MONOTONE PIECEWISE CUBIC INTERPOLANTS,' SIAM 
   *                J.SCI.STAT.COMPUT.5,2 (JUNE 1984), 300-304. 
   ****ROUTINES CALLED  DPCHST,XERROR  ( XERROR is not called in this version ) 
   ****END PROLOGUE  DPCHIM 
   * 
   *---------------------------------------------------------------------- 
   * 
   * Change record: 
   *    82-02-01   1. Introduced  DPCHST  to reduce possible over/under- 
   *                  flow problems. 
   *               2. Rearranged derivative formula for same reason. 
   *    82-06-02   1. Modified end conditions to be continuous functions 
   *                  of data when monotonicity switches in next interval. 
   *               2. Modified formulas so end conditions are less prone 
   *                  of over/underflow problems. 
   *    82-08-03   Minor cosmetic changes for release 1. 
   *    87-07-07   Corrected XERROR calls for d.p. name(s). 
   * 
   *---------------------------------------------------------------------- 
   * 
   * Programming notes: 
   * 
   *    1. The function  DPCHST(ARG1,ARG2)  is assumed to return zero if 
   *       either argument is zero, +1 if they are of the same sign, and 
   *       -1 if they are of opposite sign. 
   *    2. To produce a single precision version, simply: 
   *       a. Change DPCHIM to PCHIM wherever it occurs, 
   *       b. Change DPCHST to PCHST wherever it occurs, 
   *       c. Change all references to the Fortran intrinsics to their 
   *          single precision equivalents, 
   *       d. Change the double precision declarations to real, and 
   *       e. Change the constants ZERO and THREE to single precision. 
   * 
   * DECLARE ARGUMENTS. 
   * 
   * 
   * DECLARE LOCAL VARIABLES. 
   * 
   */
  /* Parameter adjustments */
  --x;
  d_dim1 = *incfd;
  d_offset = d_dim1 + 1;
  d__ -= d_offset;
  f_dim1 = *incfd;
  f_offset = f_dim1 + 1;
  f -= f_offset;

  /* Function Body */
  nless1 = *n - 1;
  h1 = x[2] - x[1];
  del1 = (f[(f_dim1 << 1) + 1] - f[f_dim1 + 1]) / h1;
  /* dsave = del1;*/
  /* 
   * SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION. 
   * 
   *   patch Bruno N was NLESS1 
   */
  if (*n == 2)
    {
      d__[d_dim1 + 1] = del1;
      d__[*n * d_dim1 + 1] = del1;
      return 0;
    }
  /* 
   * NORMAL CASE  (N .GE. 3). 
   * 
   */
  h2 = x[3] - x[2];
  del2 = (f[f_dim1 * 3 + 1] - f[(f_dim1 << 1) + 1]) / h2;
  /* 
   * SET D(1) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE 
   *    SHAPE-PRESERVING. 
   * 
   */
  hsum = h1 + h2;
  w1 = (h1 + hsum) / hsum;
  w2 = -h1 / hsum;
  d__[d_dim1 + 1] = w1 * del1 + w2 * del2;
  if (nsp_calpack_dpchst (&d__[d_dim1 + 1], &del1) <= zero)
    {
      d__[d_dim1 + 1] = zero;
    }
  else if (nsp_calpack_dpchst (&del1, &del2) < zero)
    {
      /*       NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. 
       */
      dmax__ = three * del1;
      if ((d__1 = d__[d_dim1 + 1], Abs (d__1)) > Abs (dmax__))
	{
	  d__[d_dim1 + 1] = dmax__;
	}
    }
  /* 
   * LOOP THROUGH INTERIOR POINTS. 
   * 
   */
  i__1 = nless1;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      if (i__ == 2)
	{
	  goto L40;
	}
      /* 
       */
      h1 = h2;
      h2 = x[i__ + 1] - x[i__];
      hsum = h1 + h2;
      del1 = del2;
      del2 = (f[(i__ + 1) * f_dim1 + 1] - f[i__ * f_dim1 + 1]) / h2;
    L40:
      /* 
       *       SET D(I)=0 UNLESS DATA ARE STRICTLY MONOTONIC. 
       * 
       */
      d__[i__ * d_dim1 + 1] = zero;
      cres = nsp_calpack_dpchst (&del1, &del2);
      if (cres < 0.)
	{
	  goto L42;
	}
      else if (cres == 0.)
	{
	  goto L41;
	}
      else
	{
	  goto L45;
	}
      /* 
       *       COUNT NUMBER OF CHANGES IN DIRECTION OF MONOTONICITY. 
       * 
       */
    L41:
      if (del2 == zero)
	{
	  goto L50;
	}
      /* dsave = del2; */
      goto L50;
      /* 
       */
    L42:
      /* dsave = del2; */
      goto L50;
      /* 
       *       USE BRODLIE MODIFICATION OF BUTLAND FORMULA. 
       * 
       */
    L45:
      hsumt3 = hsum + hsum + hsum;
      w1 = (hsum + h1) / hsumt3;
      w2 = (hsum + h2) / hsumt3;
      /*Computing MAX 
       */
      d__1 = Abs (del1), d__2 = Abs (del2);
      dmax__ = Max (d__1, d__2);
      /*Computing MIN 
       */
      d__1 = Abs (del1), d__2 = Abs (del2);
      dmin__ = Min (d__1, d__2);
      drat1 = del1 / dmax__;
      drat2 = del2 / dmax__;
      d__[i__ * d_dim1 + 1] = dmin__ / (w1 * drat1 + w2 * drat2);
      /* 
       */
    L50:
      ;
    }
  /* 
   * SET D(N) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE 
   *    SHAPE-PRESERVING. 
   * 
   */
  w1 = -h2 / hsum;
  w2 = (h2 + hsum) / hsum;
  d__[*n * d_dim1 + 1] = w1 * del1 + w2 * del2;
  if (nsp_calpack_dpchst (&d__[*n * d_dim1 + 1], &del2) <= zero)
    {
      d__[*n * d_dim1 + 1] = zero;
    }
  else if (nsp_calpack_dpchst (&del1, &del2) < zero)
    {
      /*       NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES. 
       */
      dmax__ = three * del2;
      if ((d__1 = d__[*n * d_dim1 + 1], Abs (d__1)) > Abs (dmax__))
	{
	  d__[*n * d_dim1 + 1] = dmax__;
	}
    }
  /* 
   * NORMAL RETURN. 
   * 
   */
  return 0;
}				/* dpchim_ */

double nsp_calpack_dpchst (double *arg1, double *arg2)
{
  /* Initialized data */

  static double zero = 0.;
  static double one = 1.;

  /* System generated locals */
  double ret_val;

  /****BEGIN PROLOGUE  DPCHST 
   ****REFER TO  DPCHCE,DPCHCI,DPCHCS,DPCHIM 
   ****ROUTINES CALLED  (NONE) 
   ****REVISION DATE  870707   (YYMMDD) 
   ****DESCRIPTION 
   * 
   *        DPCHST:  DPCHIP Sign-Testing Routine. 
   * 
   * 
   *    Returns: 
   *       -1. if ARG1 and ARG2 are of opposite sign. 
   *        0. if either argument is zero. 
   *       +1. if ARG1 and ARG2 are of the same sign. 
   * 
   *    The object is to do this without multiplying ARG1*ARG2, to avoid 
   *    possible over/underflow problems. 
   * 
   * Fortran intrinsics used:  SIGN. 
   * 
   ****END PROLOGUE  DPCHST 
   * 
   *---------------------------------------------------------------------- 
   * 
   * Programmed by:  Fred N. Fritsch,  FTS 532-4275, (415) 422-4275, 
   *                 Mathematics and Statistics Division, 
   *                 Lawrence Livermore National Laboratory. 
   * 
   * Change record: 
   *    82-08-05   Converted to SLATEC library version. 
   * 
   *---------------------------------------------------------------------- 
   * 
   * Programming notes: 
   * 
   *    To produce a single precision version, simply: 
   *       a. Change DPCHST to PCHST wherever it occurs, 
   *       b. Change all references to the Fortran intrinsics to their 
   *          single presision equivalents, 
   *       c. Change the double precision declarations to real, and 
   *       d. Change the constants  ZERO  and  ONE  to single precision. 
   * 
   * DECLARE ARGUMENTS. 
   * 
   * 
   * DECLARE LOCAL VARIABLES. 
   * 
   */
  /* 
   * PERFORM THE TEST. 
   * 
   ****FIRST EXECUTABLE STATEMENT  DPCHST 
   */
  ret_val = d_sign (&one, arg1) * d_sign (&one, arg2);
  if (*arg1 == zero || *arg2 == zero)
    {
      ret_val = zero;
    }
  /* 
   */
  return ret_val;
  /*------------- LAST LINE OF DPCHST FOLLOWS ----------------------------- 
   */
}				/* dpchst_ */
