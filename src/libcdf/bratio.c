#include "cdf.h"

static double cdf_apser (double *a, double *b, double *x, double *eps);
static double cdf_fpser (double *a, double *b, double *x, double *eps);
static double cdf_bpser (double *a, double *b, double *x, double *eps);
static double cdf_bfrac (double *a, double *b, double *x, double *y, 
			 double *lambda, double *eps);
static double cdf_bup (double *a, double *b, double *x, double *y, int *n, double *eps);
static int cdf_bgrat (double *a, double *b, double *x, double *y, double *w, 
		      double *eps,int *ierr);
static double cdf_basym (double *a, double *b, double *lambda, double *eps);


/* ----------------------------------------------------------------------- */
/*            EVALUATION OF THE INCOMPLETE BETA FUNCTION IX(A,B) */
/*                     -------------------- */
/*     IT IS ASSUMED THAT A AND B ARE NONNEGATIVE, AND THAT X .LE. 1 */
/*     AND Y = 1 - X.  BRATIO ASSIGNS W AND W1 THE VALUES */
/*                      W  = IX(A,B) */
/*                      W1 = 1 - IX(A,B) */
/*     IERR IS A VARIABLE THAT REPORTS THE STATUS OF THE RESULTS. */
/*     IF NO INPUT ERRORS ARE DETECTED THEN IERR IS SET TO 0 AND */
/*     W AND W1 ARE COMPUTED. OTHERWISE, IF AN ERROR IS DETECTED, */
/*     THEN W AND W1 ARE ASSIGNED THE VALUE 0 AND IERR IS SET TO */
/*     ONE OF THE FOLLOWING VALUES ... */
/*        IERR = 1  IF A OR B IS NEGATIVE */
/*        IERR = 2  IF A = B = 0 */
/*        IERR = 3  IF X .LT. 0 OR X .GT. 1 */
/*        IERR = 4  IF Y .LT. 0 OR Y .GT. 1 */
/*        IERR = 5  IF X + Y .NE. 1 */
/*        IERR = 6  IF X = A = 0 */
/*        IERR = 7  IF Y = B = 0 */
/* -------------------- */
/*     WRITTEN BY ALFRED H. MORRIS, JR. */
/*        NAVAL SURFACE WARFARE CENTER */
/*        DAHLGREN, VIRGINIA */
/*     REVISED ... NOV 1991 */
/* ----------------------------------------------------------------------- */

int cdf_bratio (double *a, double *b, double *x, double *y, double *w, double *w1,
		int *ierr)
{
  const int c__1 = 1;
  double d__1, d__2;
  int ierr1;
  int n;
  double t, z__;
  double a0, b0, x0, y0, lambda;
  int ind;
  double eps;
  /*     ****** EPS IS A MACHINE DEPENDENT CONSTANT. EPS IS THE SMALLEST */
  /*            FLOATING POINT NUMBER FOR WHICH 1.0 + EPS .GT. 1.0 */
  eps = cdf_spmpar (c__1);
  /* ----------------------------------------------------------------------- */
  *w = 0.;
  *w1 = 0.;
  if (*a < 0. || *b < 0.)
    {
      goto L270;
    }
  if (*a == 0. && *b == 0.)
    {
      goto L280;
    }
  if (*x < 0. || *x > 1.)
    {
      goto L290;
    }
  if (*y < 0. || *y > 1.)
    {
      goto L300;
    }
  z__ = *x + *y - .5 - .5;
  if (Abs (z__) > eps * 3.)
    {
      goto L310;
    }

  *ierr = 0;
  if (*x == 0.)
    {
      goto L210;
    }
  if (*y == 0.)
    {
      goto L230;
    }
  if (*a == 0.)
    {
      goto L240;
    }
  if (*b == 0.)
    {
      goto L220;
    }

  eps = Max (eps, 1e-15);
  if (Max (*a, *b) < eps * .001)
    {
      goto L260;
    }

  ind = 0;
  a0 = *a;
  b0 = *b;
  x0 = *x;
  y0 = *y;
  if (Min (a0, b0) > 1.)
    {
      goto L40;
    }

/*             PROCEDURE FOR A0 .LE. 1 OR B0 .LE. 1 */

  if (*x <= .5)
    {
      goto L10;
    }
  ind = 1;
  a0 = *b;
  b0 = *a;
  x0 = *y;
  y0 = *x;

L10:
/* Computing MIN */
  d__1 = eps, d__2 = eps * a0;
  if (b0 < Min (d__1, d__2))
    {
      goto L90;
    }
/* Computing MIN */
  d__1 = eps, d__2 = eps * b0;
  if (a0 < Min (d__1, d__2) && b0 * x0 <= 1.)
    {
      goto L100;
    }
  if (Max (a0, b0) > 1.)
    {
      goto L20;
    }
  if (a0 >= Min (.2, b0))
    {
      goto L110;
    }
  if (pow(x0,a0) <= .9)
    {
      goto L110;
    }
  if (x0 >= .3)
    {
      goto L120;
    }
  n = 20;
  goto L140;

L20:
  if (b0 <= 1.)
    {
      goto L110;
    }
  if (x0 >= .3)
    {
      goto L120;
    }
  if (x0 >= .1)
    {
      goto L30;
    }
  d__1 = x0 * b0;
  if (pow(d__1,a0) <= .7)
    {
      goto L110;
    }
L30:
  if (b0 > 15.)
    {
      goto L150;
    }
  n = 20;
  goto L140;

/*             PROCEDURE FOR A0 .GT. 1 AND B0 .GT. 1 */

L40:
  if (*a > *b)
    {
      goto L50;
    }
  lambda = *a - (*a + *b) * *x;
  goto L60;
L50:
  lambda = (*a + *b) * *y - *b;
L60:
  if (lambda >= 0.)
    {
      goto L70;
    }
  ind = 1;
  a0 = *b;
  b0 = *a;
  x0 = *y;
  y0 = *x;
  lambda = Abs (lambda);

L70:
  if (b0 < 40. && b0 * x0 <= .7)
    {
      goto L110;
    }
  if (b0 < 40.)
    {
      goto L160;
    }
  if (a0 > b0)
    {
      goto L80;
    }
  if (a0 <= 100.)
    {
      goto L130;
    }
  if (lambda > a0 * .03)
    {
      goto L130;
    }
  goto L200;
L80:
  if (b0 <= 100.)
    {
      goto L130;
    }
  if (lambda > b0 * .03)
    {
      goto L130;
    }
  goto L200;

/*            EVALUATION OF THE APPROPRIATE ALGORITHM */

L90:
  *w = cdf_fpser (&a0, &b0, &x0, &eps);
  *w1 = .5 - *w + .5;
  goto L250;

L100:
  *w1 = cdf_apser (&a0, &b0, &x0, &eps);
  *w = .5 - *w1 + .5;
  goto L250;

L110:
  *w = cdf_bpser (&a0, &b0, &x0, &eps);
  *w1 = .5 - *w + .5;
  goto L250;

L120:
  *w1 = cdf_bpser (&b0, &a0, &y0, &eps);
  *w = .5 - *w1 + .5;
  goto L250;

L130:
  d__1 = eps * 15.;
  *w = cdf_bfrac (&a0, &b0, &x0, &y0, &lambda, &d__1);
  *w1 = .5 - *w + .5;
  goto L250;

L140:
  *w1 = cdf_bup (&b0, &a0, &y0, &x0, &n, &eps);
  b0 += n;
L150:
  d__1 = eps * 15.;
  cdf_bgrat (&b0, &a0, &y0, &x0, w1, &d__1, &ierr1);
  *w = .5 - *w1 + .5;
  goto L250;

L160:
  n = (int) b0;
  b0 -= n;
  if (b0 != 0.)
    {
      goto L170;
    }
  --n;
  b0 = 1.;
L170:
  *w = cdf_bup (&b0, &a0, &y0, &x0, &n, &eps);
  if (x0 > .7)
    {
      goto L180;
    }
  *w += cdf_bpser (&a0, &b0, &x0, &eps);
  *w1 = .5 - *w + .5;
  goto L250;

L180:
  if (a0 > 15.)
    {
      goto L190;
    }
  n = 20;
  *w += cdf_bup (&a0, &b0, &x0, &y0, &n, &eps);
  a0 += n;
L190:
  d__1 = eps * 15.;
  cdf_bgrat (&a0, &b0, &x0, &y0, w, &d__1, &ierr1);
  *w1 = .5 - *w + .5;
  goto L250;

L200:
  d__1 = eps * 100.;
  *w = cdf_basym (&a0, &b0, &lambda, &d__1);
  *w1 = .5 - *w + .5;
  goto L250;

/*               TERMINATION OF THE PROCEDURE */

L210:
  if (*a == 0.)
    {
      goto L320;
    }
L220:
  *w = 0.;
  *w1 = 1.;
  return 0;

L230:
  if (*b == 0.)
    {
      goto L330;
    }
L240:
  *w = 1.;
  *w1 = 0.;
  return 0;

L250:
  if (ind == 0)
    {
      return 0;
    }
  t = *w;
  *w = *w1;
  *w1 = t;
  return 0;

/*           PROCEDURE FOR A AND B .LT. 1.E-3*EPS */

L260:
  *w = *b / (*a + *b);
  *w1 = *a / (*a + *b);
  return 0;

/*                       ERROR RETURN */

L270:
  *ierr = 1;
  return 0;
L280:
  *ierr = 2;
  return 0;
L290:
  *ierr = 3;
  return 0;
L300:
  *ierr = 4;
  return 0;
L310:
  *ierr = 5;
  return 0;
L320:
  *ierr = 6;
  return 0;
L330:
  *ierr = 7;
  return 0;
}				/* bratio_ */


/* ----------------------------------------------------------------------- */
/*     APSER YIELDS THE INCOMPLETE BETA RATIO I(SUB(1-X))(B,A) FOR */
/*     A .LE. MIN(EPS,EPS*B), B*X .LE. 1, AND X .LE. 0.5. USED WHEN */
/*     A IS VERY SMALL. USE ONLY IF ABOVE INEQUALITIES ARE SATISFIED. */
/* ----------------------------------------------------------------------- */

static double cdf_apser (double *a, double *b, double *x, double *eps)
{
  const  double g = .577215664901533;
  double ret_val;
  double c__, j, s, t, aj, bx, tol;

  bx = *b * *x;
  t = *x - bx;
  if (*b * *eps > .02)
    {
      goto L10;
    }
  c__ = log (*x) + cdf_psi1 (*b) + g + t;
  goto L20;
L10:
  c__ = log (bx) + g + t;

L20:
  tol = *eps * 5. * Abs (c__);
  j = 1.;
  s = 0.;
L30:
  j += 1.;
  t *= *x - bx / j;
  aj = t / j;
  s += aj;
  if (Abs (aj) > tol)
    {
      goto L30;
    }

  ret_val = -(*a) * (c__ + s);
  return ret_val;
}



/* ----------------------------------------------------------------------- */
/*                 EVALUATION OF I (A,B) */
/*                                X */
/*          FOR B .LT. MIN(EPS,EPS*A) AND X .LE. 0.5. */
/* ----------------------------------------------------------------------- */
/*                  SET  FPSER = X**A */

static double cdf_fpser (double *a, double *b, double *x, double *eps)
{
  const int c__1 = 1;

  double ret_val;
  double c__, s, t, an;
  double tol;

  ret_val = 1.;
  if (*a <= *eps * .001)
    {
      goto L10;
    }
  ret_val = 0.;
  t = *a * log (*x);
  if (t < cdf_exparg (c__1))
    {
      return ret_val;
    }
  ret_val = exp (t);

/*                NOTE THAT 1/B(A,B) = B */

L10:
  ret_val = *b / *a * ret_val;
  tol = *eps / *a;
  an = *a + 1.;
  t = *x;
  s = t / an;
L20:
  an += 1.;
  t = *x * t;
  c__ = t / an;
  s += c__;
  if (Abs (c__) > tol)
    {
      goto L20;
    }

  ret_val *= *a * s + 1.;
  return ret_val;
}				/* fpser_ */



/* ----------------------------------------------------------------------- */
/*     POWER SERIES EXPANSION FOR EVALUATING IX(A,B) WHEN B .LE. 1 */
/*     OR B*X .LE. 0.7.  EPS IS THE TOLERANCE USED. */
/* ----------------------------------------------------------------------- */

static double cdf_bpser (double *a, double *b, double *x, double *eps)
{
  int i__1;
  double ret_val;
  double c__;
  int i__, m;
  double n, t, u, w, z__, a0, b0;
  double apb, tol, sum;

  ret_val = 0.;
  if (*x == 0.)
    {
      return ret_val;
    }
/* ----------------------------------------------------------------------- */
/*            COMPUTE THE FACTOR X**A/(A*BETA(A,B)) */
/* ----------------------------------------------------------------------- */
  a0 = Min (*a, *b);
  if (a0 < 1.)
    {
      goto L10;
    }
  z__ = *a * log (*x) - cdf_betaln (*a, *b);
  ret_val = exp (z__) / *a;
  goto L100;
L10:
  b0 = Max (*a, *b);
  if (b0 >= 8.)
    {
      goto L90;
    }
  if (b0 > 1.)
    {
      goto L40;
    }

/*            PROCEDURE FOR A0 .LT. 1 AND B0 .LE. 1 */

  ret_val = pow(*x,*a);
  if (ret_val == 0.)
    {
      return ret_val;
    }

  apb = *a + *b;
  if (apb > 1.)
    {
      goto L20;
    }
  z__ = cdf_gam1 (apb) + 1.;
  goto L30;
L20:
  u = *a + *b - 1.;
  z__ = (cdf_gam1 (u) + 1.) / apb;

L30:
  c__ = (cdf_gam1 (*a) + 1.) * (cdf_gam1 (*b) + 1.) / z__;
  ret_val = ret_val * c__ * (*b / apb);
  goto L100;

/*         PROCEDURE FOR A0 .LT. 1 AND 1 .LT. B0 .LT. 8 */

L40:
  u = cdf_gamln1 (a0);
  m = (int) (b0 - 1.);
  if (m < 1)
    {
      goto L60;
    }
  c__ = 1.;
  i__1 = m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      b0 += -1.;
      c__ *= b0 / (a0 + b0);
/* L50: */
    }
  u = log (c__) + u;

L60:
  z__ = *a * log (*x) - u;
  b0 += -1.;
  apb = a0 + b0;
  if (apb > 1.)
    {
      goto L70;
    }
  t = cdf_gam1 (apb) + 1.;
  goto L80;
L70:
  u = a0 + b0 - 1.;
  t = (cdf_gam1 (u) + 1.) / apb;
L80:
  ret_val = exp (z__) * (a0 / *a) * (cdf_gam1 (b0) + 1.) / t;
  goto L100;

/*            PROCEDURE FOR A0 .LT. 1 AND B0 .GE. 8 */

L90:
  u = cdf_gamln1 (a0) + cdf_algdiv (a0, b0);
  z__ = *a * log (*x) - u;
  ret_val = a0 / *a * exp (z__);
L100:
  if (ret_val == 0. || *a <= *eps * .1)
    {
      return ret_val;
    }
/* ----------------------------------------------------------------------- */
/*                     COMPUTE THE SERIES */
/* ----------------------------------------------------------------------- */
  sum = 0.;
  n = 0.;
  c__ = 1.;
  tol = *eps / *a;
L110:
  n += 1.;
  c__ = c__ * (.5 - *b / n + .5) * *x;
  w = c__ / (*a + n);
  sum += w;
  if (Abs (w) > tol)
    {
      goto L110;
    }
  ret_val *= *a * sum + 1.;
  return ret_val;
}				/* bpser_ */


/* ----------------------------------------------------------------------- */
/*     CONTINUED FRACTION EXPANSION FOR IX(A,B) WHEN A,B .GT. 1. */
/*     IT IS ASSUMED THAT  LAMBDA = (A + B)*Y - B. */
/* ----------------------------------------------------------------------- */

static double cdf_bfrac (double *a, double *b, double *x, double *y, double *lambda, double *eps)
{
  double ret_val, d__1;
  double beta, c__, e, n, p, r__, s, t, alpha, w, c0, c1, r0, an, bn;
  double yp1, anp1, bnp1;

  ret_val = cdf_brcomp (*a, *b, *x, *y);
  if (ret_val == 0.)
    {
      return ret_val;
    }

  c__ = *lambda + 1.;
  c0 = *b / *a;
  c1 = 1. / *a + 1.;
  yp1 = *y + 1.;

  n = 0.;
  p = 1.;
  s = *a + 1.;
  an = 0.;
  bn = 1.;
  anp1 = 1.;
  bnp1 = c__ / c1;
  r__ = c1 / c__;

  /*        CONTINUED FRACTION CALCULATION */

L10:
  n += 1.;
  t = n / *a;
  w = n * (*b - n) * *x;
  e = *a / s;
  alpha = p * (p + c0) * e * e * (w * *x);
  e = (t + 1.) / (c1 + t + t);
  beta = n + w / s + e * (c__ + n * yp1);
  p = t + 1.;
  s += 2.;

  /*        UPDATE AN, BN, ANP1, AND BNP1 */

  t = alpha * an + beta * anp1;
  an = anp1;
  anp1 = t;
  t = alpha * bn + beta * bnp1;
  bn = bnp1;
  bnp1 = t;

  r0 = r__;
  r__ = anp1 / bnp1;
  if ((d__1 = r__ - r0, Abs (d__1)) <= *eps * r__)
    {
      goto L20;
    }

  /*        RESCALE AN, BN, ANP1, AND BNP1 */

  an /= bnp1;
  bn /= bnp1;
  anp1 = r__;
  bnp1 = 1.;
  goto L10;

  /*                 TERMINATION */

L20:
  ret_val *= r__;
  return ret_val;
}				/* bfrac_ */


/* ----------------------------------------------------------------------- */
/*     EVALUATION OF IX(A,B) - IX(A+N,B) WHERE N IS A POSITIVE INT. */
/*     EPS IS THE TOLERANCE USED. */
/* ----------------------------------------------------------------------- */

static double cdf_bup (double *a, double *b, double *x, double *y, int *n, double *eps)
{
  const int c__1 = 1;
  const int c__0 = 0;
  int i__1;
  double ret_val, d__1;
  double d__;
  int i__, k;
  double l, r__, t, w;
  int mu;
  double ap1;
  int kp1, nm1;
  double apb;

  /*          OBTAIN THE SCALING FACTOR EXP(-MU) AND */
  /*             EXP(MU)*(X**A*Y**B/BETA(A,B))/A */

  apb = *a + *b;
  ap1 = *a + 1.;
  mu = 0;
  d__ = 1.;
  if (*n == 1 || *a < 1.)
    {
      goto L10;
    }
  if (apb < ap1 * 1.1)
    {
      goto L10;
    }
  mu = (d__1 = cdf_exparg (c__1), (int) Abs (d__1));
  k = (int) cdf_exparg (c__0);
  if (k < mu)
    {
      mu = k;
    }
  t = (double) mu;
  d__ = exp (-t);

L10:
  ret_val = cdf_brcmp1 (&mu, a, b, x, y) / *a;
  if (*n == 1 || ret_val == 0.)
    {
      return ret_val;
    }
  nm1 = *n - 1;
  w = d__;

/*          LET K BE THE INDEX OF THE MAXIMUM TERM */

  k = 0;
  if (*b <= 1.)
    {
      goto L50;
    }
  if (*y > 1e-4)
    {
      goto L20;
    }
  k = nm1;
  goto L30;
L20:
  r__ = (*b - 1.) * *x / *y - *a;
  if (r__ < 1.)
    {
      goto L50;
    }
  k = nm1;
  t = (double) nm1;
  if (r__ < t)
    {
      k = (int) r__;
    }

/*          ADD THE INCREASING TERMS OF THE SERIES */

L30:
  i__1 = k;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      l = (double) (i__ - 1);
      d__ = (apb + l) / (ap1 + l) * *x * d__;
      w += d__;
/* L40: */
    }
  if (k == nm1)
    {
      goto L70;
    }

/*          ADD THE REMAINING TERMS OF THE SERIES */

L50:
  kp1 = k + 1;
  i__1 = nm1;
  for (i__ = kp1; i__ <= i__1; ++i__)
    {
      l = (double) (i__ - 1);
      d__ = (apb + l) / (ap1 + l) * *x * d__;
      w += d__;
      if (d__ <= *eps * w)
	{
	  goto L70;
	}
/* L60: */
    }

/*               TERMINATE THE PROCEDURE */

L70:
  ret_val *= w;
  return ret_val;
}				/* bup_ */



/* ----------------------------------------------------------------------- */
/*     ASYMPTOTIC EXPANSION FOR IX(A,B) WHEN A IS LARGER THAN B. */
/*     THE RESULT OF THE EXPANSION IS ADDED TO W. IT IS ASSUMED */
/*     THAT A .GE. 15 AND B .LE. 1.  EPS IS THE TOLERANCE USED. */
/*     IERR IS A VARIABLE THAT REPORTS THE STATUS OF THE RESULTS. */
/* ----------------------------------------------------------------------- */

static int cdf_bgrat (double *a, double *b, double *x, double *y, double *w, double *eps,int *ierr)
{
  int i__1;
  double d__1;
  double coef;
  double c__[30], d__[30];
  int i__;
  double j, l;
  int n;
  double p, q, r__, s, t, u, v, z__, n2, t2, dj, cn, nu;
  double bm1;
  int nm1;
  double lnx, sum;
  double bp2n;

  bm1 = *b - .5 - .5;
  nu = *a + bm1 * .5;
  if (*y > .375)
    {
      goto L10;
    }
  d__1 = -(*y);
  lnx = cdf_alnrel (d__1);
  goto L20;
L10:
  lnx = log (*x);
L20:
  z__ = -nu * lnx;
  if (*b * z__ == 0.)
    {
      goto L70;
    }

/*                 COMPUTATION OF THE EXPANSION */
/*                 SET R = EXP(-Z)*Z**B/GAMMA(B) */

  r__ = *b * (cdf_gam1 (*b) + 1.) * exp (*b * log (z__));
  r__ = r__ * exp (*a * lnx) * exp (bm1 * .5 * lnx);
  u = cdf_algdiv (*b, *a) + *b * log (nu);
  u = r__ * exp (-u);
  if (u == 0.)
    {
      goto L70;
    }
  cdf_grat1 (b, &z__, &r__, &p, &q, eps);

/* Computing 2nd power */
  d__1 = 1. / nu;
  v = d__1 * d__1 * .25;
  t2 = lnx * .25 * lnx;
  l = *w / u;
  j = q / r__;
  sum = j;
  t = 1.;
  cn = 1.;
  n2 = 0.;
  for (n = 1; n <= 30; ++n)
    {
      bp2n = *b + n2;
      j = (bp2n * (bp2n + 1.) * j + (z__ + bp2n + 1.) * t) * v;
      n2 += 2.;
      t *= t2;
      cn /= n2 * (n2 + 1.);
      c__[n - 1] = cn;
      s = 0.;
      if (n == 1)
	{
	  goto L40;
	}
      nm1 = n - 1;
      coef = *b - n;
      i__1 = nm1;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  s += coef * c__[i__ - 1] * d__[n - i__ - 1];
	  coef += *b;
/* L30: */
	}
    L40:
      d__[n - 1] = bm1 * cn + s / n;
      dj = d__[n - 1] * j;
      sum += dj;
      if (sum <= 0.)
	{
	  goto L70;
	}
      if (Abs (dj) <= *eps * (sum + l))
	{
	  goto L60;
	}
/* L50: */
    }

/*                    ADD THE RESULTS TO W */

L60:
  *ierr = 0;
  *w += u * sum;
  return 0;

/*               THE EXPANSION CANNOT BE COMPUTED */

L70:
  *ierr = 1;
  return 0;
}				/* bgrat_ */



/* ----------------------------------------------------------------------- */
/*     ASYMPTOTIC EXPANSION FOR IX(A,B) FOR LARGE A AND B. */
/*     LAMBDA = (A + B)*Y - B  AND EPS IS THE TOLERANCE USED. */
/*     IT IS ASSUMED THAT LAMBDA IS NONNEGATIVE AND THAT */
/*     A AND B ARE GREATER THAN OR EQUAL TO 15. */
/* ----------------------------------------------------------------------- */


static double cdf_basym (double *a, double *b, double *lambda, double *eps)
{
  const int c__1 = 1;
  /* Initialized data */
  const int num = 20;
  /* ------------------------ */
  /* NUM IS THE MAXIMUM VALUE THAT N CAN TAKE IN THE DO LOOP */
  /* ENDING AT STATEMENT 50. IT IS REQUIRED THAT NUM BE EVEN. */
  /* THE ARRAYS A0, B0, C, D HAVE DIMENSION NUM + 1. */
  /* ------------------------ */
  /*     E0 = 2/SQRT(PI) */
  /*     E1 = 2**(-3/2) */
  const double e0 = 1.12837916709551;
  const double e1 = .353553390593274;

  /* System generated locals */
  int i__1, i__2, i__3, i__4;
  double ret_val, d__1, d__2;
  /* Local variables */
  double bsum, dsum;
  double c__[num+1], d__[num+1], f, h__;
  int i__, j, m, n;
  double r__, s, t, u, w, z__;
  double a0[num+1], b0[num+1], h2, j0, j1, r0, r1, t0, t1, w0, z0, z2, hn, zn;
  int im1, mm1, np1, imj, mmj;
  double sum, znm1;

  ret_val = 0.;
  if (*a >= *b)
    {
      goto L10;
    }
  h__ = *a / *b;
  r0 = 1. / (h__ + 1.);
  r1 = (*b - *a) / *b;
  w0 = 1. / sqrt (*a * (h__ + 1.));
  goto L20;
L10:
  h__ = *b / *a;
  r0 = 1. / (h__ + 1.);
  r1 = (*b - *a) / *a;
  w0 = 1. / sqrt (*b * (h__ + 1.));

L20:
  d__1 = -(*lambda) / *a;
  d__2 = *lambda / *b;
  f = *a * cdf_rlog1 (d__1) + *b * cdf_rlog1 (d__2);
  t = exp (-f);
  if (t == 0.)
    {
      return ret_val;
    }
  z0 = sqrt (f);
  z__ = z0 / e1 * .5;
  z2 = f + f;

  a0[0] = r1 * .66666666666666663;
  c__[0] = a0[0] * -.5;
  d__[0] = -c__[0];
  j0 = .5 / e0 * cdf_erfc (c__1, z0);
  j1 = e1;
  sum = j0 + d__[0] * w0 * j1;

  s = 1.;
  h2 = h__ * h__;
  hn = 1.;
  w = w0;
  znm1 = z__;
  zn = z2;
  i__1 = num;
  for (n = 2; n <= i__1; n += 2)
    {
      hn = h2 * hn;
      a0[n - 1] = r0 * 2. * (h__ * hn + 1.) / (n + 2.);
      np1 = n + 1;
      s += hn;
      a0[np1 - 1] = r1 * 2. * s / (n + 3.);

      i__2 = np1;
      for (i__ = n; i__ <= i__2; ++i__)
	{
	  r__ = (i__ + 1.) * -.5;
	  b0[0] = r__ * a0[0];
	  i__3 = i__;
	  for (m = 2; m <= i__3; ++m)
	    {
	      bsum = 0.;
	      mm1 = m - 1;
	      i__4 = mm1;
	      for (j = 1; j <= i__4; ++j)
		{
		  mmj = m - j;
		  bsum += (j * r__ - mmj) * a0[j - 1] * b0[mmj - 1];
/* L30: */
		}
	      b0[m - 1] = r__ * a0[m - 1] + bsum / m;
/* L40: */
	    }
	  c__[i__ - 1] = b0[i__ - 1] / (i__ + 1.);

	  dsum = 0.;
	  im1 = i__ - 1;
	  i__3 = im1;
	  for (j = 1; j <= i__3; ++j)
	    {
	      imj = i__ - j;
	      dsum += d__[imj - 1] * c__[j - 1];
/* L50: */
	    }
	  d__[i__ - 1] = -(dsum + c__[i__ - 1]);
/* L60: */
	}

      j0 = e1 * znm1 + (n - 1.) * j0;
      j1 = e1 * zn + n * j1;
      znm1 = z2 * znm1;
      zn = z2 * zn;
      w = w0 * w;
      t0 = d__[n - 1] * w * j0;
      w = w0 * w;
      t1 = d__[np1 - 1] * w * j1;
      sum += t0 + t1;
      if (Abs (t0) + Abs (t1) <= *eps * sum)
	{
	  goto L80;
	}
/* L70: */
    }

L80:
  u = exp (-cdf_bcorr (a, b));
  ret_val = e0 * t * u * sum;
  return ret_val;
}				/* basym_ */
