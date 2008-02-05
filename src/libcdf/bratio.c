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


/*
 * evaluation of the incomplete beta function ix(a,b) 
 * It is assumed that a and b are nonnegative, and that x <= 1 
 * and y = 1 - x. bratio assigns w and w1 the values 
 *                      w  = ix(a,b) 
 *                      w1 = 1 - ix(a,b) 
 * ierr is a variable that reports the status of the results. 
 * if no input errors are detected then ierr is set to 0 and 
 * w and w1 are computed. otherwise, if an error is detected, 
 * then w and w1 are assigned the value 0 and ierr is set to 
 * one of the following values ... 
 *        ierr = 1  if a or b is negative 
 *        ierr = 2  if a = b = 0 
 *        ierr = 3  if x .lt. 0 or x .gt. 1 
 *        ierr = 4  if y .lt. 0 or y .gt. 1 
 *        ierr = 5  if x + y .ne. 1 
 *        ierr = 6  if x = a = 0 
 *        ierr = 7  if y = b = 0 
 *
 * Author: Alfred H. Morris, jr. 
 * naval surface warfare center (dahlgren, virginia) 
 * revised, nov 1991 
 */

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
  /*     ****** eps is a machine dependent constant. eps is the smallest 
   *            FLOATING POINT NUMBER FOR WHICH 1.0 + EPS .GT. 1.0 */
  eps = cdf_spmpar (c__1);
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

  /*             procedure for a0 .le. 1 or b0 .le. 1 */

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
  /* computing min */
  d__1 = eps, d__2 = eps * a0;
  if (b0 < Min (d__1, d__2))
    {
      goto L90;
    }
  /* computing min */
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

  /*             procedure for a0 .gt. 1 and b0 .gt. 1 */

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

  /*            evaluation of the appropriate algorithm */

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

  /*               termination of the procedure */

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

  /*           procedure for a and b .lt. 1.e-3*eps */

 L260:
  *w = *b / (*a + *b);
  *w1 = *a / (*a + *b);
  return 0;

  /*                       error return */

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
}


/*
 *     apser yields the incomplete beta ratio i(sub(1-x))(b,a) for 
 *     a .le. min(eps,eps*b), b*x .le. 1, and x .le. 0.5. used when 
 *     a is very small. use only if above inequalities are satisfied. 
 */

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



/*
 *                 EVALUATION OF I (A,B) 
 *                                X 
 *          FOR B .LT. MIN(EPS,EPS*A) AND X .LE. 0.5. 
 */


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

  /*                note that 1/b(a,b) = b */

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



/*
 *     POWER SERIES EXPANSION FOR EVALUATING IX(A,B) WHEN B .LE. 1 
 *     OR B*X .LE. 0.7.  EPS IS THE TOLERANCE USED. 
 */

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
  /*          compute the factor x**a/(a*beta(a,b))  */

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

  /*            procedure for a0 .lt. 1 and b0 .le. 1 */

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

  /*         procedure for a0 .lt. 1 and 1 .lt. b0 .lt. 8 */

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
      /* l50: */
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

  /*            procedure for a0 .lt. 1 and b0 .ge. 8 */

 L90:
  u = cdf_gamln1 (a0) + cdf_algdiv (a0, b0);
  z__ = *a * log (*x) - u;
  ret_val = a0 / *a * exp (z__);
 L100:
  if (ret_val == 0. || *a <= *eps * .1)
    {
      return ret_val;
    }
  /*                     compute the series   */

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
}			


/*
 *     continued fraction expansion for ix(a,b) when a,b .gt. 1. 
 *     it is assumed that  lambda = (a + b)*y - b. 
 */

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

  /*        continued fraction calculation */

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

  /*        update an, bn, anp1, and bnp1 */

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

  /*        rescale an, bn, anp1, and bnp1 */

  an /= bnp1;
  bn /= bnp1;
  anp1 = r__;
  bnp1 = 1.;
  goto L10;

  /*                 termination */

 L20:
  ret_val *= r__;
  return ret_val;
}				/* bfrac_ */


/*
 *     evaluation of ix(a,b) - ix(a+n,b) where n is a positive int. 
 *     eps is the tolerance used. 
 */

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

  /*          obtain the scaling factor exp(-mu) and
   *             EXP(MU)*(X**A*Y**B/BETA(A,B))/A 
   */

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
  ret_val = cdf_brcmp1 (mu, *a, *b, *x, *y) / *a;
  if (*n == 1 || ret_val == 0.)
    {
      return ret_val;
    }
  nm1 = *n - 1;
  w = d__;

  /*          let k be the index of the maximum term */

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

  /*          add the increasing terms of the series */

 L30:
  i__1 = k;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      l = (double) (i__ - 1);
      d__ = (apb + l) / (ap1 + l) * *x * d__;
      w += d__;
      /* l40: */
    }
  if (k == nm1)
    {
      goto L70;
    }

  /*          add the remaining terms of the series */

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
      /* l60: */
    }

  /*               terminate the procedure */

 L70:
  ret_val *= w;
  return ret_val;
}				



/*
 *     asymptotic expansion for ix(a,b) when a is larger than b. 
 *     the result of the expansion is added to w. it is assumed 
 *     that a .ge. 15 and b .le. 1.  eps is the tolerance used. 
 *     ierr is a variable that reports the status of the results. 
 */

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
  lnx = cdf_dln1px (d__1);
  goto L20;
 L10:
  lnx = log (*x);
 L20:
  z__ = -nu * lnx;
  if (*b * z__ == 0.)
    {
      goto L70;
    }

  /*                 computation of the expansion 
   *                 SET R = EXP(-Z)*Z**B/GAMMA(B) 
   */

    r__ = *b * (cdf_gam1 (*b) + 1.) * exp (*b * log (z__));
  r__ = r__ * exp (*a * lnx) * exp (bm1 * .5 * lnx);
  u = cdf_algdiv (*b, *a) + *b * log (nu);
  u = r__ * exp (-u);
  if (u == 0.)
    {
      goto L70;
    }
  cdf_grat1 (b, &z__, &r__, &p, &q, eps);

  /* computing 2nd power */
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
	  /* l30: */
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
      /* l50: */
    }

  /*                    add the results to w */

 L60:
  *ierr = 0;
  *w += u * sum;
  return 0;

  /*               the expansion cannot be computed */

 L70:
  *ierr = 1;
  return 0;
}



/*
 *     asymptotic expansion for ix(a,b) for large a and b. 
 *     lambda = (a + b)*y - b  and eps is the tolerance used. 
 *     it is assumed that lambda is nonnegative and that 
 *     a and b are greater than or equal to 15. 
 */


#define NUM 20

static double cdf_basym (double *a, double *b, double *lambda, double *eps)
{
  /*
   * num is the maximum value that n can take in the do loop 
   * ending at statement 50. it is required that num be even. 
   * the arrays a0, b0, c, d have dimension num + 1. 
   */
  const int num = NUM;
  const double e0 = 1.12837916709551; /*     e0 = 2/sqrt(pi) */
  const double e1 = .353553390593274; /*     e1 = 2**(-3/2)  */

  /* system generated locals */
  int i__1, i__2, i__3, i__4;
  double ret_val, d__1, d__2;
  /* local variables */
  double bsum, dsum;
  double c__[NUM+1], d__[NUM+1], f, h__;
  int i__, j, m, n;
  double r__, s, t, u, w, z__;
  double a0[NUM+1], b0[NUM+1], h2, j0, j1, r0, r1, t0, t1, w0, z0, z2, hn, zn;
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
  j0 = .5 / e0 * cdf_erfc (1, z0);
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
		  /* l30: */
		}
	      b0[m - 1] = r__ * a0[m - 1] + bsum / m;
	      /* l40: */
	    }
	  c__[i__ - 1] = b0[i__ - 1] / (i__ + 1.);

	  dsum = 0.;
	  im1 = i__ - 1;
	  i__3 = im1;
	  for (j = 1; j <= i__3; ++j)
	    {
	      imj = i__ - j;
	      dsum += d__[imj - 1] * c__[j - 1];
	      /* l50: */
	    }
	  d__[i__ - 1] = -(dsum + c__[i__ - 1]);
	  /* l60: */
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
      /* l70: */
    }

 L80:
  u = exp (-cdf_bcorr (*a, *b));
  ret_val = e0 * t * u * sum;
  return ret_val;
}				
