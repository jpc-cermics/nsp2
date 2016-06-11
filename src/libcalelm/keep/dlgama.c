/* dlgama.f -- translated by f2c (version 19961017). */

#include "calpack.h"

  /*---------------------------------------------------------------------- 
   * 
   *  This routine calculates the LOG(GAMMA) function for a positive real 
   *  argument X.  Computation is based on an algorithm outlined in 
   *  references 1 and 2.  The program uses rational functions that 
   *  theoretically approximate LOG(GAMMA) to at least 18 significant 
   *  decimal digits.  The approximation for X > 12 is from reference 
   *  3, while approximations for X < 12.0 are similar to those in 
   *  reference 1, but are unpublished.  The accuracy achieved depends 
   *  on the arithmetic system, the compiler, the intrinsic functions, 
   *  and proper selection of the machine-dependent constants. 
   * 
   * 
   *Explanation of machine-dependent constants 
   * 
   *beta   - radix for the floating-point representation 
   *maxexp - the smallest positive power of beta that overflows 
   *XBIG   - largest argument for which LN(GAMMA(X)) is representable 
   *         in the machine, i.e., the solution to the equation 
   *                 LN(GAMMA(XBIG)) = beta**maxexp 
   *XINF   - largest machine representable floating-point number; 
   *         approximately beta**maxexp. 
   *EPS    - The smallest positive floating-point number such that 
   *         1.0+EPS .GT. 1.0 
   *FRTBIG - Rough estimate of the fourth root of XBIG 
   * 
   * 
   *    Approximate values for some important machines are: 
   * 
   *                          beta      maxexp         XBIG 
   * 
   *CRAY-1        (S.P.)        2        8191       9.62E+2461 
   *Cyber 180/855 
   *  under NOS   (S.P.)        2        1070       1.72E+319 
   *IEEE (IBM/XT, 
   *  SUN, etc.)  (S.P.)        2         128       4.08E+36 
   *IEEE (IBM/XT, 
   *  SUN, etc.)  (D.P.)        2        1024       2.55D+305 
   *IBM 3033      (D.P.)       16          63       4.29D+73 
   *VAX D-Format  (D.P.)        2         127       2.05D+36 
   *VAX G-Format  (D.P.)        2        1023       1.28D+305 
   * 
   * 
   *                          XINF        EPS        FRTBIG 
   * 
   *CRAY-1        (S.P.)   5.45E+2465   7.11E-15    3.13E+615 
   *Cyber 180/855 
   *  under NOS   (S.P.)   1.26E+322    3.55E-15    6.44E+79 
   *IEEE (IBM/XT, 
   *  SUN, etc.)  (S.P.)   3.40E+38     1.19E-7     1.42E+9 
   *IEEE (IBM/XT, 
   *  SUN, etc.)  (D.P.)   1.79D+308    2.22D-16    2.25D+76 
   *IBM 3033      (D.P.)   7.23D+75     2.22D-16    2.56D+18 
   *VAX D-Format  (D.P.)   1.70D+38     1.39D-17    1.20D+9 
   *VAX G-Format  (D.P.)   8.98D+307    1.11D-16    1.89D+76 
   * 
   *************************************************************** 
   *************************************************************** 
   * 
   *Error returns 
   * 
   * The program returns the value XINF for X .LE. 0.0 or when 
   *    overflow would occur.  The computation is believed to 
   *    be free of underflow and overflow. 
   * 
   * 
   *Intrinsic functions required are: 
   * 
   *     LOG 
   * 
   * 
   *References: 
   * 
   * 1) W. J. Cody and K. E. Hillstrom, 'Chebyshev Approximations for 
   *    the Natural Logarithm of the Gamma Function,' Math. Comp. 21, 
   *    1967, pp. 198-203. 
   * 
   * 2) K. E. Hillstrom, ANL/AMD Program ANLC366S, DGAMMA/DLGAMA, May, 
   *    1969. 
   * 
   * 3) Hart, Et. Al., Computer Approximations, Wiley and sons, New 
   *    York, 1968. 
   * 
   * 
   * Authors: W. J. Cody and L. Stoltz 
   *          Argonne National Laboratory 
   * 
   * Latest modification: June 16, 1988 
   * 
   *---------------------------------------------------------------------- 
   * 
   * Some modifs from Bruno (25 Feb 2005): 
   *   - return Nan (in place of XINF) if x <= 0 
   *   - return Inf (in place of XINF) if x > XBIG 
   * 
   * In fact an error indicator should be returned in these cases to prevent 
   * the user... 
   * 
   *S    REAL 
   *---------------------------------------------------------------------- 
   * Mathematical constants 
   *---------------------------------------------------------------------- 
   *S    DATA ONE,HALF,TWELVE,ZERO/1.0E0,0.5E0,12.0E0,0.0E0/, 
   *S   1     FOUR,THRHAL,TWO,PNT68/4.0E0,1.5E0,2.0E0,0.6796875E0/, 
   *S   2     SQRTPI/0.9189385332046727417803297E0/ 
   */
  /*---------------------------------------------------------------------- 
   * Machine dependent parameters 
   *---------------------------------------------------------------------- 
   *S    DATA XBIG,XINF,EPS,FRTBIG/4.08E36,3.401E38,1.19E-7,1.42E9/ 
   *---------------------------------------------------------------------- 
   * Numerator and denominator coefficients for rational minimax 
   *    approximation over (0.5,1.5). 
   *---------------------------------------------------------------------- 
   *S    DATA D1/-5.772156649015328605195174E-1/ 
   *S    DATA P1/4.945235359296727046734888E0,2.018112620856775083915565E2, 
   *S   1        2.290838373831346393026739E3,1.131967205903380828685045E4, 
   *S   2        2.855724635671635335736389E4,3.848496228443793359990269E4, 
   *S   3        2.637748787624195437963534E4,7.225813979700288197698961E3/ 
   *S    DATA Q1/6.748212550303777196073036E1,1.113332393857199323513008E3, 
   *S   1        7.738757056935398733233834E3,2.763987074403340708898585E4, 
   *S   2        5.499310206226157329794414E4,6.161122180066002127833352E4, 
   *S   3        3.635127591501940507276287E4,8.785536302431013170870835E3/ 
   *---------------------------------------------------------------------- 
   * Numerator and denominator coefficients for rational minimax 
   *    Approximation over (1.5,4.0). 
   *---------------------------------------------------------------------- 
   *S    DATA D2/4.227843350984671393993777E-1/ 
   *S    DATA P2/4.974607845568932035012064E0,5.424138599891070494101986E2, 
   *S   1        1.550693864978364947665077E4,1.847932904445632425417223E5, 
   *S   2        1.088204769468828767498470E6,3.338152967987029735917223E6, 
   *S   3        5.106661678927352456275255E6,3.074109054850539556250927E6/ 
   *S    DATA Q2/1.830328399370592604055942E2,7.765049321445005871323047E3, 
   *S   1        1.331903827966074194402448E5,1.136705821321969608938755E6, 
   *S   2        5.267964117437946917577538E6,1.346701454311101692290052E7, 
   *S   3        1.782736530353274213975932E7,9.533095591844353613395747E6/ 
   *---------------------------------------------------------------------- 
   * Numerator and denominator coefficients for rational minimax 
   *    Approximation over (4.0,12.0). 
   *---------------------------------------------------------------------- 
   *S    DATA D4/1.791759469228055000094023E0/ 
   *S    DATA P4/1.474502166059939948905062E4,2.426813369486704502836312E6, 
   *S   1        1.214755574045093227939592E8,2.663432449630976949898078E9, 
   *S   2      2.940378956634553899906876E10,1.702665737765398868392998E11, 
   *S   3      4.926125793377430887588120E11,5.606251856223951465078242E11/ 
   *S    DATA Q4/2.690530175870899333379843E3,6.393885654300092398984238E5, 
   *S   2        4.135599930241388052042842E7,1.120872109616147941376570E9, 
   *S   3      1.488613728678813811542398E10,1.016803586272438228077304E11, 
   *S   4      3.417476345507377132798597E11,4.463158187419713286462081E11/ 
   *---------------------------------------------------------------------- 
   * Coefficients for minimax approximation over (12, INF). 
   *---------------------------------------------------------------------- 
   *S    DATA C/-1.910444077728E-03,8.4171387781295E-04, 
   *S   1     -5.952379913043012E-04,7.93650793500350248E-04, 
   *S   2     -2.777777777777681622553E-03,8.333333333333333331554247E-02, 
   *S   3      5.7083835261E-03/ 
   *---------------------------------------------------------------------- 
   */

double nsp_calpack_dlgama (double *x)
{
  /* Initialized data */

  const double one = 1.;
  const double xbig = 2.55e305;
  const double xinf = 1.79e308;
  const double eps = 2.22e-16;
  const double frtbig = 2.25e76;
  const double d1 = -.5772156649015328605195174;
  const double p1[8] =
    { 4.945235359296727046734888, 201.8112620856775083915565,
    2290.838373831346393026739, 11319.67205903380828685045,
    28557.24635671635335736389,
    38484.96228443793359990269, 26377.48787624195437963534,
    7225.813979700288197698961
  };
  const double q1[8] =
    { 67.48212550303777196073036, 1113.332393857199323513008,
    7738.757056935398733233834, 27639.87074403340708898585,
    54993.10206226157329794414,
    61611.22180066002127833352, 36351.27591501940507276287,
    8785.536302431013170870835
  };
  const double d2 = .4227843350984671393993777;
  const double p2[8] =
    { 4.974607845568932035012064, 542.4138599891070494101986,
    15506.93864978364947665077, 184793.2904445632425417223,
    1088204.76946882876749847,
    3338152.967987029735917223, 5106661.678927352456275255,
    3074109.054850539556250927
  };
  const double q2[8] =
    { 183.0328399370592604055942, 7765.049321445005871323047,
    133190.3827966074194402448, 1136705.821321969608938755,
    5267964.117437946917577538,
    13467014.54311101692290052, 17827365.30353274213975932,
    9533095.591844353613395747
  };
  const double half = .5;
  const double d4 = 1.791759469228055000094023;
  const double p4[8] =
    { 14745.02166059939948905062, 2426813.369486704502836312,
    121475557.4045093227939592, 2663432449.630976949898078,
    29403789566.34553899906876,
    170266573776.5398868392998, 492612579337.743088758812,
    560625185622.3951465078242
  };
  const double q4[8] =
    { 2690.530175870899333379843, 639388.5654300092398984238,
    41355999.30241388052042842, 1120872109.61614794137657,
    14886137286.78813811542398,
    101680358627.2438228077304, 341747634550.7377132798597,
    446315818741.9713286462081
  };
  const double c__[7] =
    { -.001910444077728, 8.4171387781295e-4, -5.952379913043012e-4,
    7.93650793500350248e-4, -.002777777777777681622553,
    .08333333333333333331554247,
    .0057083835261
  };
  const double twelve = 12.;
  const double zero = 0.;
  const double four = 4.;
  const double thrhal = 1.5;
  const double two = 2.;
  const double pnt68 = .6796875;
  const double sqrtpi = .9189385332046727417803297;

  /* System generated locals */
  double ret_val;

  /* Local variables */
  double xden, corr, xnum;
  int i__;
  double y, xm1, xm2, xm4, res, ysq;

  y = *x;
  if (y > zero && y <= xbig)
    {
      if (y <= eps)
	{
	  res = -log (y);
	}
      else if (y <= thrhal)
	{
	  /*---------------------------------------------------------------------- 
	   * EPS .LT. X .LE. 1.5 
	   *---------------------------------------------------------------------- 
	   */
	  if (y < pnt68)
	    {
	      corr = -log (y);
	      xm1 = y;
	    }
	  else
	    {
	      corr = zero;
	      xm1 = y - half - half;
	    }
	  if (y <= half || y >= pnt68)
	    {
	      xden = one;
	      xnum = zero;
	      for (i__ = 1; i__ <= 8; ++i__)
		{
		  xnum = xnum * xm1 + p1[i__ - 1];
		  xden = xden * xm1 + q1[i__ - 1];
		  /* L140: */
		}
	      res = corr + xm1 * (d1 + xm1 * (xnum / xden));
	    }
	  else
	    {
	      xm2 = y - half - half;
	      xden = one;
	      xnum = zero;
	      for (i__ = 1; i__ <= 8; ++i__)
		{
		  xnum = xnum * xm2 + p2[i__ - 1];
		  xden = xden * xm2 + q2[i__ - 1];
		  /* L220: */
		}
	      res = corr + xm2 * (d2 + xm2 * (xnum / xden));
	    }
	}
      else if (y <= four)
	{
	  /*---------------------------------------------------------------------- 
	   * 1.5 .LT. X .LE. 4.0 
	   *---------------------------------------------------------------------- 
	   */
	  xm2 = y - two;
	  xden = one;
	  xnum = zero;
	  for (i__ = 1; i__ <= 8; ++i__)
	    {
	      xnum = xnum * xm2 + p2[i__ - 1];
	      xden = xden * xm2 + q2[i__ - 1];
	      /* L240: */
	    }
	  res = xm2 * (d2 + xm2 * (xnum / xden));
	}
      else if (y <= twelve)
	{
	  /*---------------------------------------------------------------------- 
	   * 4.0 .LT. X .LE. 12.0 
	   *---------------------------------------------------------------------- 
	   */
	  xm4 = y - four;
	  xden = -one;
	  xnum = zero;
	  for (i__ = 1; i__ <= 8; ++i__)
	    {
	      xnum = xnum * xm4 + p4[i__ - 1];
	      xden = xden * xm4 + q4[i__ - 1];
	      /* L340: */
	    }
	  res = d4 + xm4 * (xnum / xden);
	}
      else
	{
	  /*---------------------------------------------------------------------- 
	   * Evaluate for argument .GE. 12.0, 
	   *---------------------------------------------------------------------- 
	   */
	  res = zero;
	  if (y <= frtbig)
	    {
	      res = c__[6];
	      ysq = y * y;
	      for (i__ = 1; i__ <= 6; ++i__)
		{
		  res = res / ysq + c__[i__ - 1];
		  /* L450: */
		}
	    }
	  res /= y;
	  corr = log (y);
	  res = res + sqrtpi - half * corr;
	  res += y * (corr - one);
	}
    }
  else
    {
      /*---------------------------------------------------------------------- 
       * Return for bad arguments 
       *---------------------------------------------------------------------- 
       *          modif from Bruno (see comment at the beginning) 
       *           RES = XINF 
       */
      if (*x <= 0.)
	{
	  res = return_a_nan_nsp_calpack_ ();
	}
      else
	{
	  /*this means that X > XBIG and so that log(gamma) over 
	   */
	  res = xinf * 2;
	  /*bad trick to get Inf 
	   */
	}
      /*          end modif 
       */
    }
  /*---------------------------------------------------------------------- 
   * Final adjustments and return 
   *---------------------------------------------------------------------- 
   *S    ALGAMA = RES 
   */
  ret_val = res;
  return ret_val;
}


