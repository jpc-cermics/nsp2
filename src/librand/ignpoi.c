/* ********************************************************************** */
/*     INT FUNCTION IGNPOI( MU ) */
/*                    GENerate POIsson random deviate */
/*                              Function */
/*     Generates a single random deviate from a Poisson */
/*     distribution with mean MU. */
/*                              Arguments */
/*     MU --> The mean of the Poisson distribution from which */
/*            a random deviate is to be generated. */
/*                              DOUBLE PRECISION MU */
/*     JJV                    (MU >= 0.0) */
/*     IGNPOI <-- The random deviate. */
/*                              INT IGNPOI (non-negative) */
/*                              Method */
/*     Renames KPOIS from TOMS as slightly modified by BWB to use RANF */
/*     instead of SUNIF. */
/*     For details see: */
/*               Ahrens, J.H. and Dieter, U. */
/*               Computer Generation of Poisson Deviates */
/*               From Modified Normal Distributions. */
/*               ACM Trans. Math. Software, 8, 2 */
/*               (June 1982),163-179 */
/* **********************************************************************C */
/*     P O I S S O N  DISTRIBUTION                                      C */
/*                                                                      C */
/*     FOR DETAILS SEE:                                                 C */
/*                                                                      C */
/*               AHRENS, J.H. AND DIETER, U.                            C */
/*               COMPUTER GENERATION OF POISSON DEVIATES                C */
/*               FROM MODIFIED NORMAL DISTRIBUTIONS.                    C */
/*               ACM TRANS. MATH. SOFTWARE, 8,2 (JUNE 1982), 163 - 179. C */
/*                                                                      C */
/*     (SLIGHTLY MODIFIED VERSION OF THE PROGRAM IN THE ABOVE ARTICLE)  C */
/* **********************************************************************C */
/*      INT FUNCTION IGNPOI(IR,MU) */
/*     INPUT:  IR=CURRENT STATE OF BASIC RANDOM NUMBER GENERATOR */
/*             MU=MEAN MU OF THE POISSON DISTRIBUTION */
/*     OUTPUT: IGNPOI=SAMPLE FROM THE POISSON-(MU)-DISTRIBUTION */
/*     MUPREV=PREVIOUS MU, MUOLD=MU AT LAST EXECUTION OF STEP P OR CASE B */
/*     TABLES: COEFFICIENTS A0-A7 FOR STEP F. FACTORIALS FACT */
/*     COEFFICIENTS A(K) - FOR PX = FK*V*V*SUM(A(K)*V**K)-DEL */
/*     SEPARATION OF CASES A AND B */
/*     JJV I added a variable 'll' here - it is the 'l' for CASE A */
/*     JJV added this for case: mu unchanged */
/*     JJV end addition - I am including vars in Data statements */
/*     JJV changed initial values of MUPREV and MUOLD to -1.0E37 */
/*     JJV if no one calls IGNPOI with MU = -1.0E37 the first time, */
/*     JJV the code shouldn't break */

#include "grand.h"

int
rand_ignpoi (double *mu)
{
  const double a7 = .125006;
  const double fact[10] = { 1., 1., 2., 6., 24., 120., 720., 5040., 40320., 362880. };
  const double a0 = -.5;
  const double a1 = .3333333;
  const double a2 = -.2500068;
  const double a3 = .2000118;
  const double a4 = -.1661269;
  const double a5 = .1421878;
  const double a6 = -.1384794;

  static double muprev = -1e37;
  static double muold = -1e37;

  double e=0.0, fk=0.0,u=0.0, difmuk=0.0;
  double fx, fy,g, pp[35], px, py, xx, del;
  double t, v, x, b1, b2;
  int j, k;
  int kflag;
  int ret_val=0, i__1, i__2;

  static double c0, c1, c2, c3;
  static double c__, d__;
  static double omega;
  static double p, q, s;
  static double p0;
  static int l, m;
  static int ll;

  if (*mu == muprev)
    {
      goto L10;
    }
  if (*mu < 10.)
    {
      goto L120;
    }
  /*     C A S E  A. (RECALCULATION OF S,D,LL IF MU HAS CHANGED) */
  /*     JJV This is the case where I changed 'l' to 'll' */
  /*     JJV Here 'll' is set once and used in a comparison once */
  muprev = *mu;
  s = sqrt (*mu);
  d__ = *mu * 6. * *mu;
  /*             THE POISSON PROBABILITIES PK EXCEED THE DISCRETE NORMAL */
  /*             PROBABILITIES FK WHENEVER K >= M(MU). LL=IDINT(MU-1.1484) */
  /*             IS AN UPPER BOUND TO M(MU) FOR ALL MU >= 10 . */
  ll = (int) (*mu - 1.1484);
  /*     STEP N. NORMAL SAMPLE - SNORM(IR) FOR STANDARD NORMAL DEVIATE */
 L10:
  g = *mu + s * rand_snorm ();
  if (g < 0.)
    {
      goto L20;
    }
  ret_val = (int) g;
  /*     STEP I. IMMEDIATE ACCEPTANCE IF IGNPOI IS LARGE ENOUGH */
  if (ret_val >= ll)
    {
      return ret_val;
    }
  /*     STEP S. SQUEEZE ACCEPTANCE - SUNIF(IR) FOR (0,1)-SAMPLE U */
  fk = (double) ret_val;
  difmuk = *mu - fk;
  u = rand_ranf ();
  if (d__ * u >= difmuk * difmuk * difmuk)
    {
      return ret_val;
    }
  /*     STEP P. PREPARATIONS FOR STEPS Q AND H. */
  /*             (RECALCULATIONS OF PARAMETERS IF NECESSARY) */
  /*             .3989423=(2*PI)**(-.5)  .416667E-1=1./24.  .1428571=1./7. */
  /*             THE QUANTITIES B1, B2, C3, C2, C1, C0 ARE FOR THE HERMITE */
  /*             APPROXIMATIONS TO THE DISCRETE NORMAL PROBABILITIES FK. */
  /*             C=.1069/MU GUARANTEES MAJORIZATION BY THE 'HAT'-FUNCTION. */
L20:
  if (*mu == muold)
    {
      goto L30;
    }
  muold = *mu;
  omega = .3989423 / s;
  b1 = .04166667 / *mu;
  b2 = b1 * .3 * b1;
  c3 = b1 * .1428571 * b2;
  c2 = b2 - c3 * 15.;
  c1 = b1 - b2 * 6. + c3 * 45.;
  c0 = 1. - b1 + b2 * 3. - c3 * 15.;
  c__ = .1069 / *mu;
 L30:
  if (g < 0.)
    {
      goto L50;
    }
  /*             'SUBROUTINE' F IS CALLED (KFLAG=0 FOR CORRECT RETURN) */
  kflag = 0;
  goto L70;
  /*     STEP Q. QUOTIENT ACCEPTANCE (RARE CASE) */
L40:
  if (fy - u * fy <= py * exp (px - fx))
    {
      return ret_val;
    }
  /*     STEP E. EXPONENTIAL SAMPLE - SEXPO(IR) FOR STANDARD EXPONENTIAL */
  /*             DEVIATE E AND SAMPLE T FROM THE LAPLACE 'HAT' */
  /*             (IF T <= -.6744 THEN PK < FK FOR ALL MU >= 10.) */
 L50:
  e = rand_sexpo ();
  u = rand_ranf ();
  u = u + u - 1.;
  t = D_SIGN (e,u) + 1.8;
  if (t <= -.6744)
    {
      goto L50;
    }
  ret_val = (int) (*mu + s * t);
  fk = (double) ret_val;
  difmuk = *mu - fk;
  /*             'SUBROUTINE' F IS CALLED (KFLAG=1 FOR CORRECT RETURN) */
  kflag = 1;
  goto L70;
  /*     STEP H. HAT ACCEPTANCE (E IS REPEATED ON REJECTION) */
 L60:
  if (c__ * Abs (u) > py * exp (px + e) - fy * exp (fx + e))
    {
      goto L50;
    }
  return ret_val;
  /*     STEP F. 'SUBROUTINE' F. CALCULATION OF PX,PY,FX,FY. */
  /*             CASE IGNPOI .LT. 10 USES FACTORIALS FROM TABLE FACT */
  
L70:
  if (ret_val >= 10)
    {
      goto L80;
    }
  px = -(*mu);
  py = pow (*mu, ret_val) / fact[ret_val];
  goto L110;
  /*             CASE IGNPOI .GE. 10 USES POLYNOMIAL APPROXIMATION */
  /*             A0-A7 FOR ACCURACY WHEN ADVISABLE */
  /*             .8333333E-1=1./12.  .3989423=(2*PI)**(-.5) */
  
L80:
  del = .08333333 / fk;
  del -= del * 4.8 * del * del;
  v = difmuk / fk;
  if (Abs (v) <= .25)
    {
      goto L90;
    }
  px = fk * log (v + 1.) - difmuk - del;
  goto L100;
L90:
  px =
    fk * v * v *
    (((((((a7 * v + a6) * v + a5) * v + a4) * v + a3) * v + a2) * v +
      a1) * v + a0) - del;
L100:
  py = .3989423 / sqrt (fk);
L110:
  x = (.5 - difmuk) / s;
  xx = x * x;
  fx = xx * -.5;
  fy = omega * (((c3 * xx + c2) * xx + c1) * xx + c0);
  if (kflag <= 0)
    {
      goto L40;
    }
  else
    {
      goto L60;
    }

/*     C A S E  B. (START NEW TABLE AND CALCULATE P0 IF NECESSARY) */

/*     JJV changed MUPREV assignment from 0.0 to initial value */
L120:
  muprev = -1e37;
/*     Jpc 1999: the next lines seams to produce a bug */
/*     and I finaly commented them out */
/*     IF (mu.EQ.muold) GO TO 130 */
/*     JJV added argument checker here */
/*     JJV added line label here */
/* 125  muold = mu */
/* Computing MAX */
  i__1 = 1, i__2 = (int) (*mu);
  m = Max (i__1, i__2);
  l = 0;
  p = exp (-(*mu));
  q = p;
  p0 = p;

/*     STEP U. UNIFORM SAMPLE FOR INVERSION METHOD */

L130:
  u = rand_ranf ();
  ret_val = 0;
  if (u <= p0)
    {
      return ret_val;
    }

/*     STEP T. TABLE COMPARISON UNTIL THE END PP(L) OF THE */
/*             PP-TABLE OF CUMULATIVE POISSON PROBABILITIES */
/*             (0.458=PP(9) FOR MU=10) */

  if (l == 0)
    {
      goto L150;
    }
  j = 1;
  if (u > .458)
    {
      j = Min (l, m);
    }
  i__1 = l;
  for (k = j; k <= i__1; ++k)
    {
      if (u <= pp[k - 1])
	{
	  goto L180;
	}
/* L140: */
    }
  if (l == 35)
    {
      goto L130;
    }

/*     STEP C. CREATION OF NEW POISSON PROBABILITIES P */
/*             AND THEIR CUMULATIVES Q=PP(K) */

L150:
  ++l;
  for (k = l; k <= 35; ++k)
    {
      p = p * *mu / (double) k;
      q += p;
      pp[k - 1] = q;
      if (u <= q)
	{
	  goto L170;
	}
/* L160: */
    }
  l = 35;
  goto L130;
L170:
  l = k;
L180:
  ret_val = k;
  return ret_val;
}				/* ignpoi_ */
