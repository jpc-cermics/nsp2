/* sgamma.f -- translated by f2c (version 19961017).
   
	
*/

#include "grand.h"

double
rand_sgamma (double *a)
{
  /* Initialized data */

  static double q1 = .04166669;
  static double a3 = .2000062;
  static double a4 = -.1662921;
  static double a5 = .1423657;
  static double a6 = -.1367177;
  static double a7 = .1233795;
  static double e1 = 1.;
  static double e2 = .4999897;
  static double e3 = .166829;
  static double e4 = .0407753;
  static double e5 = .010293;
  static double q2 = .02083148;
  static double aa = 0.;
  static double aaa = 0.;
  static double sqrt32 = 5.656854;
  static double q3 = .00801191;
  static double q4 = .00144121;
  static double q5 = -7.388e-5;
  static double q6 = 2.4511e-4;
  static double q7 = 2.424e-4;
  static double a1 = .3333333;
  static double a2 = -.250003;

  /* System generated locals */
  double ret_val, d__1;
  /* Local variables */
  static double b, c__, d__;
  double e, p, q, r__;
  static double s;
  double t, u, v, w, x, b0;
  static double q0, s2;
  static double si;

/* **********************************************************************C */
/*                                                                      C */
/*                                                                      C */
/*     (STANDARD-)  G A M M A  DISTRIBUTION                             C */
/*                                                                      C */
/*                                                                      C */
/* **********************************************************************C */
/* **********************************************************************C */
/*                                                                      C */
/*         C(  A >= 1.0  !                                 C */
/*                                                                      C */
/* **********************************************************************C */
/*                                                                      C */
/*     FOR DETAILS SEE:                                                 C */
/*                                                                      C */
/*               AHRENS, J.H. AND DIETER, U.                            C */
/*               GENERATING GAMMA VARIATES BY A                         C */
/*               MODIFIED REJECTION TECHNIQUE.                          C */
/*               COMM. ACM, 25,1 (JAN. 1982), 47 - 54.                  C */
/*                                                                      C */
/*     STEP NUMBERS CORRESPOND TO ALGORITHM 'GD' IN THE ABOVE PAPER     C */
/*                                 (STRAIGHTFORWARD IMPLEMENTATION)     C */
/*                                                                      C */
/*     Modified by Barry W. Brown, Feb 3, 1988 to use RANF instead of   C */
/*     SUNIF.  The argument IR thus goes away.                          C */
/*                                                                      C */
/* **********************************************************************C */
/*                                                                      C */
/*         C(  0.0 < A < 1.0  !                            C */
/*                                                                      C */
/* **********************************************************************C */
/*                                                                      C */
/*     FOR DETAILS SEE:                                                 C */
/*                                                                      C */
/*               AHRENS, J.H. AND DIETER, U.                            C */
/*               COMPUTER METHODS FOR SAMPLING FROM GAMMA,              C */
/*               BETA, POISSON AND BINOMIAL DISTRIBUTIONS.              C */
/*               COMPUTING, 12 (1974), 223 - 246.                       C */
/*                                                                      C */
/*     (ADAPTED IMPLEMENTATION OF ALGORITHM 'GS' IN THE ABOVE PAPER)    C */
/*                                                                      C */
/* **********************************************************************C */


/*     INPUT: A =PARAMETER (MEAN) OF THE STANDARD GAMMA DISTRIBUTION */
/*     OUTPUT: SGAMMA = SAMPLE FROM THE GAMMA-(A)-DISTRIBUTION */

/*     COEFFICIENTS Q(K) - FOR Q0 = SUM(Q(K)*A**(-K)) */
/*     COEFFICIENTS A(K) - FOR Q = Q0+(T*T/2)*SUM(A(K)*V**K) */
/*     COEFFICIENTS E(K) - FOR EXP(Q)-1 = SUM(E(K)*Q**K) */

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. (JJV added B0 to fix rare and subtle bug) */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Save statement .. */
/*     JJV added Save statement for vars in Data satatements */
/*     .. */
/*     .. Data statements .. */

/*     PREVIOUS A PRE-SET TO ZERO - AA IS A', AAA IS A" */
/*     SQRT32 IS THE SQUAREROOT OF 32 = 5.656854249492380 */

/*     .. */
/*     .. Executable Statements .. */

  if (*a == aa)
    {
      goto L10;
    }
  if (*a < 1.)
    {
      goto L130;
    }

/*     STEP  1:  RECALCULATIONS OF S2,S,D IF A HAS CHANGED */

  aa = *a;
  s2 = *a - .5;
  s = sqrt (s2);
  d__ = sqrt32 - s * 12.;

/*     STEP  2:  T=STANDARD NORMAL DEVIATE, */
/*               X=(S,1/2)-NORMAL DEVIATE. */
/*               IMMEDIATE ACCEPTANCE (I) */

L10:
  t = rand_nor_core(); /* rand_snorm (); */
  x = s + t * .5;
  ret_val = x * x;
  if (t >= 0.)
    {
      return ret_val;
    }

/*     STEP  3:  U= 0,1 -UNIFORM SAMPLE. SQUEEZE ACCEPTANCE (S) */

  u = rand_ranf ();
  if (d__ * u <= t * t * t)
    {
      return ret_val;
    }

/*     STEP  4:  RECALCULATIONS OF Q0,B,SI,C IF NECESSARY */

  if (*a == aaa)
    {
      goto L40;
    }
  aaa = *a;
  r__ = 1. / *a;
  q0 =
    ((((((q7 * r__ + q6) * r__ + q5) * r__ + q4) * r__ + q3) * r__ +
      q2) * r__ + q1) * r__;

/*               APPROXIMATION DEPENDING ON SIZE OF PARAMETER A */
/*               THE CONSTANTS IN THE EXPRESSIONS FOR B, SI AND */
/*               C WERE ESTABLISHED BY NUMERICAL EXPERIMENTS */

  if (*a <= 3.686)
    {
      goto L30;
    }
  if (*a <= 13.022)
    {
      goto L20;
    }

/*               CASE 3:  A .GT. 13.022 */

  b = 1.77;
  si = .75;
  c__ = .1515 / s;
  goto L40;

/*               CASE 2:  3.686 .LT. A .LE. 13.022 */

L20:
  b = s2 * .0076 + 1.654;
  si = 1.68 / s + .275;
  c__ = .062 / s + .024;
  goto L40;

/*               CASE 1:  A .LE. 3.686 */

L30:
  b = s + .463 + s2 * .178;
  si = 1.235;
  c__ = .195 / s - .079 + s * .16;

/*     STEP  5:  NO QUOTIENT TEST IF X NOT POSITIVE */

L40:
  if (x <= 0.)
    {
      goto L70;
    }

/*     STEP  6:  CALCULATION OF V AND QUOTIENT Q */

  v = t / (s + s);
  if (Abs (v) <= .25)
    {
      goto L50;
    }
  q = q0 - s * t + t * .25 * t + (s2 + s2) * log (v + 1.);
  goto L60;
L50:
  q =
    q0 +
    t * .5 * t * ((((((a7 * v + a6) * v + a5) * v + a4) * v + a3) * v + a2) *
		  v + a1) * v;

/*     STEP  7:  QUOTIENT ACCEPTANCE (Q) */

L60:
  if (log (1. - u) <= q)
    {
      return ret_val;
    }

/*     STEP  8:  E=STANDARD EXPONENTIAL DEVIATE */
/*               U= 0,1 -UNIFORM DEVIATE */
/*               T=(B,SI)-DOUBLE EXPONENTIAL (LAPLACE) SAMPLE */

L70:
/*   e = rand_sexpo (); */
  e = rand_exp_core ();
  u = rand_ranf ();
  u = u + u - 1.;
  d__1 = si * e;
  t = b + D_SIGN (d__1,u);

/*     STEP  9:  REJECTION IF T .LT. TAU(1) = -.71874483771719 */

/* L80: */
  if (t < -.7187449)
    {
      goto L70;
    }

/*     STEP 10:  CALCULATION OF V AND QUOTIENT Q */

  v = t / (s + s);
  if (Abs (v) <= .25)
    {
      goto L90;
    }
  q = q0 - s * t + t * .25 * t + (s2 + s2) * log (v + 1.);
  goto L100;
L90:
  q =
    q0 +
    t * .5 * t * ((((((a7 * v + a6) * v + a5) * v + a4) * v + a3) * v + a2) *
		  v + a1) * v;

/*     STEP 11:  HAT ACCEPTANCE (H) (IF Q NOT POSITIVE GO TO STEP 8) */

L100:
  if (q <= 0.)
    {
      goto L70;
    }
  if (q <= .5)
    {
      goto L110;
    }

/*     JJV modified the code through line 125 to handle large Q case */

  if (q < 15.)
    {
      goto L105;
    }

/*     JJV Here Q is large enough that Q = log(exp(Q) - 1.0) (for DOUBLE PRECISION Q) */
/*     JJV so reformulate test at 120 in terms of one EXP, if not too big */
/*     JJV 87.49823 is close to the largest DOUBLE PRECISION which can be */
/*     JJV exponentiated (87.49823 = log(1.0E38)) */

  if (q + e - t * .5 * t > 87.49823)
    {
      goto L125;
    }
  if (c__ * Abs (u) > exp (q + e - t * .5 * t))
    {
      goto L70;
    }
  goto L125;
L105:
  w = exp (q) - 1.;
  goto L120;
L110:
  w = ((((e5 * q + e4) * q + e3) * q + e2) * q + e1) * q;

/*               IF T IS REJECTED, SAMPLE AGAIN AT STEP 8 */

L120:
  if (c__ * Abs (u) > w * exp (e - t * .5 * t))
    {
      goto L70;
    }
L125:
  x = s + t * .5;
  ret_val = x * x;
  return ret_val;

/*     ALTERNATE METHOD FOR PARAMETERS A BELOW 1  (.3678794=EXP(-1.)) */

/*     JJV changed B to B0 (which was added to declarations for this) */
/*     JJV in 130 to END to fix rare and subtle bug. */
/*     JJV Line: '130 aa = 0.0' was removed (unnecessary, wasteful). */
/*     JJV Reasons: the state of AA only serves to tell the A .GE. 1.0 */
/*     JJV case if certain A-dependant constants need to be recalculated. */
/*     JJV The A .LT. 1.0 case (here) no longer changes any of these, and */
/*     JJV the recalculation of B (which used to change with an */
/*     JJV A .LT. 1.0 call) is governed by the state of AAA anyway. */

L130:
  b0 = *a * .3678794 + 1.;
L140:
  p = b0 * rand_ranf ();
  if (p >= 1.)
    {
      goto L150;
    }
  ret_val = exp (log (p) / *a);
/*   if (rand_sexpo () < ret_val) */
  if (rand_exp_core () < ret_val)
    {
      goto L140;
    }
  return ret_val;
L150:
  ret_val = -log ((b0 - p) / *a);
/*   if (rand_sexpo () < (1. - *a) * log (ret_val)) */
  if (rand_exp_core () < (1. - *a) * log (ret_val))
    {
      goto L140;
    }
  return ret_val;
}				/* sgamma_ */
