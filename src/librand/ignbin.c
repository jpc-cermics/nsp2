/* ignbin.f -- translated by f2c (version 19961017).
   
	
*/

#include "grand.h"

int
rand_ignbin (int *n, double *pp)
{
  /* Initialized data */

  static double psave = -1e37;
  static int nsave = -214748365;
  /* System generated locals */
  int ret_val, i__1;
  double d__1, d__2;
  /* Local variables */
  static double xnpq, c__;
  double f;
  static double g;
  int i__, k;
  static int m;
  static double p, q, r__;
  double u, v, w, x, z__, amaxp, f1, f2;
  static double p1, p2, p3, p4;
  double ynorm, w2, x1, x2, z2, al;
  static double fm;
  int mp;
  static double qn;
  int ix;
  static double xl, xm, xr;
  int ix1;
  double ffm, alv;
  static double xll, xlr, xnp;

/* ********************************************************************** */

/*     INT FUNCTION IGNBIN( N, PP ) */

/*                    GENerate BINomial random deviate */


/*                              Function */


/*     Generates a single random deviate from a binomial */
/*     distribution whose number of trials is N and whose */
/*     probability of an event in each trial is P. */


/*                              Arguments */


/*     N  --> The number of trials in the binomial distribution */
/*            from which a random deviate is to be generated. */
/*                              INT N */
/*     JJV                      (N >= 0) */

/*     PP --> The probability of an event in each trial of the */
/*            binomial distribution from which a random deviate */
/*            is to be generated. */
/*                              DOUBLE PRECISION PP */
/*     JJV                      (0.0 <= pp <= 1.0) */

/*     IGNBIN <-- A random deviate yielding the number of events */
/*                from N independent trials, each of which has */
/*                a probability of event P. */
/*                              INT IGNBIN */


/*                              Note */


/*     Uses RANF so the value of the seeds, ISEED1 and ISEED2 must be set */
/*     by a call similar to the following */
/*          DUM = RANSET( ISEED1, ISEED2 ) */


/*                              Method */


/*     This is algorithm BTPE from: */

/*         Kachitvichyanukul, V. and Schmeiser, B. W. */

/*         Binomial Random Variate Generation. */
/*         Communications of the ACM, 31, 2 */
/*         (February, 1988) 216. */

/* ********************************************************************** */
/*     SUBROUTINE BTPEC(N,PP,ISEED,JX) */

/*     BINOMIAL RANDOM VARIATE GENERATOR */
/*     MEAN .LT. 30 -- INVERSE CDF */
/*       MEAN .GE. 30 -- ALGORITHM BTPE:  ACCEPTANCE-REJECTION VIA */
/*       FOUR REGION COMPOSITION.  THE FOUR REGIONS ARE A TRIANGLE */
/*       (SYMMETRIC IN THE CENTER), A PAIR OF PARALLELOGRAMS (ABOVE */
/*       THE TRIANGLE), AND EXPONENTIAL LEFT AND RIGHT TAILS. */

/*     BTPE REFERS TO BINOMIAL-TRIANGLE-PARALLELOGRAM-EXPONENTIAL. */
/*     BTPEC REFERS TO BTPE AND "COMBINED."  THUS BTPE IS THE */
/*       RESEARCH AND BTPEC IS THE IMPLEMENTATION OF A COMPLETE */
/*       USABLE ALGORITHM. */
/*     REFERENCE:  VORATAS KACHITVICHYANUKUL AND BRUCE SCHMEISER, */
/*       "BINOMIAL RANDOM VARIATE GENERATION," */
/*       COMMUNICATIONS OF THE ACM, FORTHCOMING */
/*     WRITTEN:  SEPTEMBER 1980. */
/*       LAST REVISED:  MAY 1985, JULY 1987 */
/*     REQUIRED SUBPROGRAM:  RAND() -- A UNIFORM (0,1) RANDOM NUMBER */
/*                           GENERATOR */
/*     ARGUMENTS */

/*       N : NUMBER OF BERNOULLI TRIALS            (INPUT) */
/*       PP : PROBABILITY OF SUCCESS IN EACH TRIAL (INPUT) */
/*       ISEED:  RANDOM NUMBER SEED                (INPUT AND OUTPUT) */
/*       JX:  RANDOMLY GENERATED OBSERVATION       (OUTPUT) */

/*     VARIABLES */
/*       PSAVE: VALUE OF PP FROM THE LAST CALL TO BTPEC */
/*       NSAVE: VALUE OF N FROM THE LAST CALL TO BTPEC */
/*       XNP:  VALUE OF THE MEAN FROM THE LAST CALL TO BTPEC */

/*       P: PROBABILITY USED IN THE GENERATION PHASE OF BTPEC */
/*       FFM: TEMPORARY VARIABLE EQUAL TO XNP + P */
/*       M:  INT VALUE OF THE CURRENT MODE */
/*       FM:  FLOATING POINT VALUE OF THE CURRENT MODE */
/*       XNPQ: TEMPORARY VARIABLE USED IN SETUP AND SQUEEZING STEPS */
/*       P1:  AREA OF THE TRIANGLE */
/*       C:  HEIGHT OF THE PARALLELOGRAMS */
/*       XM:  CENTER OF THE TRIANGLE */
/*       XL:  LEFT END OF THE TRIANGLE */
/*       XR:  RIGHT END OF THE TRIANGLE */
/*       AL:  TEMPORARY VARIABLE */
/*       XLL:  RATE FOR THE LEFT EXPONENTIAL TAIL */
/*       XLR:  RATE FOR THE RIGHT EXPONENTIAL TAIL */
/*       P2:  AREA OF THE PARALLELOGRAMS */
/*       P3:  AREA OF THE LEFT EXPONENTIAL TAIL */
/*       P4:  AREA OF THE RIGHT EXPONENTIAL TAIL */
/*       U:  A U(0,P4) RANDOM VARIATE USED FIRST TO SELECT ONE OF THE */
/*           FOUR REGIONS AND THEN CONDITIONALLY TO GENERATE A VALUE */
/*           FROM THE REGION */
/*       V:  A U(0,1) RANDOM NUMBER USED TO GENERATE THE RANDOM VALUE */
/*           (REGION 1) OR TRANSFORMED INTO THE VARIATE TO ACCEPT OR */
/*           REJECT THE CANDIDATE VALUE */
/*       IX:  INT CANDIDATE VALUE */
/*       X:  PRELIMINARY CONTINUOUS CANDIDATE VALUE IN REGION 2 LOGIC */
/*           AND A FLOATING POINT IX IN THE ACCEPT/REJECT LOGIC */
/*       K:  ABSOLUTE VALUE OF (IX-M) */
/*       F:  THE HEIGHT OF THE SCALED DENSITY FUNCTION USED IN THE */
/*           ACCEPT/REJECT DECISION WHEN BOTH M AND IX ARE SMALL */
/*           ALSO USED IN THE INVERSE TRANSFORMATION */
/*       R: THE RATIO P/Q */
/*       G: CONSTANT USED IN CALCULATION OF PROBABILITY */
/*       MP:  MODE PLUS ONE, THE LOWER INDEX FOR EXPLICIT CALCULATION */
/*            OF F WHEN IX IS GREATER THAN M */
/*       IX1:  CANDIDATE VALUE PLUS ONE, THE LOWER INDEX FOR EXPLICIT */
/*             CALCULATION OF F WHEN IX IS LESS THAN M */
/*       I:  INDEX FOR EXPLICIT CALCULATION OF F FOR BTPE */
/*       AMAXP: MAXIMUM ERROR OF THE LOGARITHM OF NORMAL BOUND */
/*       YNORM: LOGARITHM OF NORMAL BOUND */
/*       ALV:  NATURAL LOGARITHM OF THE ACCEPT/REJECT VARIATE V */

/*       X1,F1,Z,W,Z2,X2,F2, AND W2 ARE TEMPORARY VARIABLES TO BE */
/*       USED IN THE FINAL ACCEPT/REJECT TEST */

/*       QN: PROBABILITY OF NO SUCCESS IN N TRIALS */

/*     REMARK */
/*       IX AND JX COULD INTLY BE THE SAME VARIABLE, WHICH WOULD */
/*       SAVE A MEMORY POSITION AND A LINE OF CODE.  HOWEVER, SOME */
/*       COMPILERS (E.G.,CDC MNF) OPTIMIZE BETTER WHEN THE ARGUMENTS */
/*       ARE NOT INVOLVED. */

/*     ISEED NEEDS TO BE DOUBLE PRECISION IF THE IMSL ROUTINE */
/*     GGUBFS IS USED TO GENERATE UNIFORM RANDOM NUMBER, OTHERWISE */
/*     TYPE OF ISEED SHOULD BE DICTATED BY THE UNIFORM GENERATOR */

/* ********************************************************************** */



/* *****DETERMINE APPROPRIATE ALGORITHM AND WHETHER SETUP IS NECESSARY */

/*     .. */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     JJV .. */
/*     JJV .. Save statement .. */
/*     JJV I am including the variables in data statements */
/*     .. */
/*     .. Data statements .. */
/*     JJV made these ridiculous starting values - the hope is that */
/*     JJV no one will call this the first time with them as args */
/*     .. */
/*     .. Executable Statements .. */
  if (*pp != psave)
    {
      goto L10;
    }
  if (*n != nsave)
    {
      goto L20;
    }
  if (xnp - 30. >= 0.)
    {
      goto L30;
    }
  else
    {
      goto L150;
    }

/* *****SETUP, PERFORM ONLY WHEN PARAMETERS CHANGE */

/*     JJV added the argument checker - involved only renaming 10 */
/*     JJV and 20 to the checkers and adding checkers */
/*     JJV Only remaining problem - if called initially with the */
/*     JJV initial values of psave and nsave, it will hang */
L10:
  psave = *pp;
/* Computing MIN */
  d__1 = psave, d__2 = 1. - psave;
  p = Min (d__1, d__2);
  q = 1. - p;
L20:
  xnp = *n * p;
  nsave = *n;
  if (xnp < 30.)
    {
      goto L140;
    }
  ffm = xnp + p;
  m = (int) ffm;
  fm = (double) m;
  xnpq = xnp * q;
  p1 = (int) (sqrt (xnpq) * 2.195 - q * 4.6) + .5;
  xm = fm + .5;
  xl = xm - p1;
  xr = xm + p1;
  c__ = 20.5 / (fm + 15.3) + .134;
  al = (ffm - xl) / (ffm - xl * p);
  xll = al * (al * .5 + 1.);
  al = (xr - ffm) / (xr * q);
  xlr = al * (al * .5 + 1.);
  p2 = p1 * (c__ + 1. + c__);
  p3 = p2 + c__ / xll;
  p4 = p3 + c__ / xlr;
/*      WRITE(6,100) N,P,P1,P2,P3,P4,XL,XR,XM,FM */
/*  100 FORMAT(I15,4F18.7/5F18.7) */

/* *****GENERATE VARIATE */

L30:
  u = rand_ranf () * p4;
  v = rand_ranf ();

/*     TRIANGULAR REGION */

  if (u > p1)
    {
      goto L40;
    }
  ix = (int) (xm - p1 * v + u);
  goto L170;

/*     PARALLELOGRAM REGION */

L40:
  if (u > p2)
    {
      goto L50;
    }
  x = xl + (u - p1) / c__;
  v = v * c__ + 1. - (d__1 = xm - x, Abs (d__1)) / p1;
  if (v > 1. || v <= 0.)
    {
      goto L30;
    }
  ix = (int) x;
  goto L70;

/*     LEFT TAIL */

L50:
  if (u > p3)
    {
      goto L60;
    }
  ix = (int) (xl + log (v) / xll);
  if (ix < 0)
    {
      goto L30;
    }
  v = v * (u - p2) * xll;
  goto L70;

/*     RIGHT TAIL */

L60:
  ix = (int) (xr - log (v) / xlr);
  if (ix > *n)
    {
      goto L30;
    }
  v = v * (u - p3) * xlr;

/* *****DETERMINE APPROPRIATE WAY TO PERFORM ACCEPT/REJECT TEST */

L70:
  k = (i__1 = ix - m, Abs (i__1));
  if (k > 20 && (double) k < xnpq / 2 - 1)
    {
      goto L130;
    }

/*     EXPLICIT EVALUATION */

  f = 1.;
  r__ = p / q;
  g = (*n + 1) * r__;
  if ((i__1 = m - ix) < 0)
    {
      goto L80;
    }
  else if (i__1 == 0)
    {
      goto L120;
    }
  else
    {
      goto L100;
    }
L80:
  mp = m + 1;
  i__1 = ix;
  for (i__ = mp; i__ <= i__1; ++i__)
    {
      f *= g / i__ - r__;
/* L90: */
    }
  goto L120;
L100:
  ix1 = ix + 1;
  i__1 = m;
  for (i__ = ix1; i__ <= i__1; ++i__)
    {
      f /= g / i__ - r__;
/* L110: */
    }
L120:
  if (v - f <= 0.)
    {
      goto L170;
    }
  else
    {
      goto L30;
    }

/*     SQUEEZING USING UPPER AND LOWER BOUNDS ON LOG(F(X)) */

L130:
  amaxp = k / xnpq * ((k * (k / 3. + .625) + .1666666666666) / xnpq + .5);
  ynorm = -k * k / (xnpq * 2.);
  alv = log (v);
  if (alv < ynorm - amaxp)
    {
      goto L170;
    }
  if (alv > ynorm + amaxp)
    {
      goto L30;
    }

/*     STIRLING'S FORMULA TO MACHINE ACCURACY FOR */
/*     THE FINAL ACCEPTANCE/REJECTION TEST */

  x1 = (double) (ix + 1);
  f1 = fm + 1.;
  z__ = *n + 1 - fm;
  w = *n - ix + 1.;
  z2 = z__ * z__;
  x2 = x1 * x1;
  f2 = f1 * f1;
  w2 = w * w;
  if (alv -
      (xm * log (f1 / x1) + (*n - m + .5) * log (z__ / w) +
       (ix - m) * log (w * p / (x1 * q)) + (13860. -
					    (462. -
					     (132. -
					      (99. -
					       140. / f2) / f2) / f2) / f2) /
       f1 / 166320. + (13860. -
		       (462. -
			(132. -
			 (99. - 140. / z2) / z2) / z2) / z2) / z__ / 166320. +
       (13860. -
	(462. - (132. - (99. - 140. / x2) / x2) / x2) / x2) / x1 / 166320. +
       (13860. -
	(462. - (132. - (99. - 140. / w2) / w2) / w2) / w2) / w / 166320.) <=
      0.)
    {
      goto L170;
    }
  else
    {
      goto L30;
    }

/*     INVERSE CDF LOGIC FOR MEAN LESS THAN 30 */

L140:
  qn = pow_di (&q, n);
  r__ = p / q;
  g = r__ * (*n + 1);
L150:
  ix = 0;
  f = qn;
  u = rand_ranf ();
L160:
  if (u < f)
    {
      goto L170;
    }
  if (ix > 110)
    {
      goto L150;
    }
  u -= f;
  ++ix;
  f *= g / ix - r__;
  goto L160;
L170:
  if (psave > .5)
    {
      ix = *n - ix;
    }
  ret_val = ix;
  return ret_val;
}				/* ignbin_ */
