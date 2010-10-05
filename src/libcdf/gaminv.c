#include "cdf.h"

/*  inverse incomplete gamma ratio function 
 *
 *     given positive a, and nonegative p and q where p + q = 1.  then x is
 *     computed where p(a,x) = p and q(a,x) = q. schroder iteration is
 *     employed. the routine attempts to compute x to 10 significant digits if
 *     this is possible for the particular computer arithmetic being used.
 *
 *     x is a variable. if p = 0 then x is assigned the value 0, and if q = 0
 *     then x is set to the largest floating point number available. otherwise,
 *     gaminv attempts to obtain a solution for p(a,x) = p and q(a,x) = q. if
 *     the routine is successful then the solution is stored in x.
 *
 *     x0 is an optional initial approximation for x. if the user does not wish
 *     to supply an initial approximation, then set x0 .le. 0.
 *
 *     ierr is a variable that reports the status of the results.  when the
 *     routine terminates, ierr has one of the following values ...
 *
 *       ierr =  0    the solution was obtained. iteration was  not used. 
 *       ierr.gt.0    the solution was obtained. ierr iterations  were performed. 
 *       ierr = -2    (input error) a .le. 0 
 *       ierr = -3    no solution was obtained. the ratio q/a  is too large. 
 *       ierr = -4    (input error) p + q .ne. 1 
 *       ierr = -6 20 iterations were performed. the most recent value obtained
 *                    for x is given.  this cannot occur if x0 .le. 0.
 *       ierr = -7 iteration failed. no value is given for x.  this may occur
 *                    when x is approximately 0.
 *       ierr = -8 a value for x has been obtained, but the routine is not
 *                    certain of its accuracy.  iteration cannot be performed in
 *                    this case. if x0 .le. 0, this can occur only when p or q
 *                    is approximately 0. if x0 is positive then this can occur
 *                    when a is exceedingly close to x and a is extremely large
 *                    (say a .ge. 1.e20).
 * 
 * written by Alfred H. Lorris, jr. Naval Surface Weapons Center 
 * Dahlgren, Virginia 
 */

int cdf_gaminv (double a, double *x, double *x0, double *p, double *q, int *ierr)
{
  const int c__1 = 1;
  const int c__2 = 2;
  const int c__3 = 3;
  const int c__0 = 0;
  /*     LN10 = LN(10) */
  /*     C = EULER CONSTANT */
  const double ln10 = 2.302585;
  const double b4 = .036117081018842;
  const double eps0[2] = { 1e-10, 1e-8 };
  const double amin[2] = { 500., 100. };
  const double bmin[2] = { 1e-28, 1e-13 };
  const double dmin__[2] = { 1e-6, 1e-4 };
  const double emin[2] = { .002, .006 };
  const double tol = 1e-5;
  const double c__ = .577215664901533;
  const double a0 = 3.31125922108741;
  const double a1 = 11.6616720288968;
  const double a2 = 4.28342155967104;
  const double a3 = .213623493715853;
  const double b1 = 6.61053765625462;
  const double b2 = 6.40691597760039;
  const double b3 = 1.27364489782223;
  double d__1, d__2;
  double amax, xmin, xmax,bx=0, d__, e, g, h__, r__, s, t, u;
  double w, y, z__;
  double c1, c2, c3, c4, c5, e2, s2;
  double qg, pn, qn, xn;
  double am1, ap1, ap2, ap3;
  double apn, rta, eps;
  int iop;
  double sum;
  /* E, XMIN, AND XMAX ARE MACHINE DEPENDENT CONSTANTS. 
   *            E IS THE SMALLEST NUMBER FOR WHICH 1.0 + E .GT. 1.0. 
   *            XMIN IS THE SMALLEST POSITIVE NUMBER AND XMAX IS THE 
   *            LARGEST POSITIVE NUMBER. 
   */
  e = cdf_spmpar (c__1);
  xmin = cdf_spmpar (c__2);
  xmax = cdf_spmpar (c__3);

  *x = 0.;
  if (a <= 0.)
    {
      goto L300;
    }
  t = *p + *q - 1.;
  if (Abs (t) > e)
    {
      goto L320;
    }

  *ierr = 0;
  if (*p == 0.)
    {
      return 0;
    }
  if (*q == 0.)
    {
      goto L270;
    }
  if (a == 1.)
    {
      goto L280;
    }

  e2 = e * 2.;
  amax = 4e-11 / (e * e);
  iop = 1;
  if (e > 1e-10)
    {
      iop = 2;
    }
  eps = eps0[iop - 1];
  xn = *x0;
  if (*x0 > 0.)
    {
      goto L160;
    }
  /*   SELECTION OF THE INITIAL APPROXIMATION XN OF X 
   *                       WHEN A .LT. 1 
   */

  if (a > 1.)
    {
      goto L80;
    }
  d__1 = a + 1.;
  g = cdf_gamma (d__1);
  qg = *q * g;
  if (qg == 0.)
    {
      goto L360;
    }
  bx = qg / a;
  if (qg > a * .6)
    {
      goto L40;
    }
  if (a >= .3 || bx < .35)
    {
      goto L10;
    }
  t = exp (-(bx + c__));
  u = t * exp (t);
  xn = t * exp (u);
  goto L160;

 L10:
  if (bx >= .45)
    {
      goto L40;
    }
  if (bx == 0.)
    {
      goto L360;
    }
  y = -log (bx);
  s = .5 - a + .5;
  z__ = log (y);
  t = y - s * z__;
  if (bx < .15)
    {
      goto L20;
    }
  xn = y - s * log (t) - log (s / (t + 1.) + 1.);
  goto L220;
 L20:
  if (bx <= .01)
    {
      goto L30;
    }
  u =
    ((t + (3. - a) * 2.) * t +
     (2. - a) * (3. - a)) / ((t + (5. - a)) * t + 2.);
  xn = y - s * log (t) - log (u);
  goto L220;
 L30:
  c1 = -s * z__;
  c2 = -s * (c1 + 1.);
  c3 = s * ((c1 * .5 + (2. - a)) * c1 + (2.5 - a * 1.5));
  c4 =
    -s * (((c1 / 3. + (2.5 - a * 1.5)) * c1 + ((a - 6.) * a + 7.)) * c1 +
	  ((a * 11. - 46) * a + 47.) / 6.);
  c5 =
    -s *
    ((((-c1 / 4. + (a * 11. - 17.) / 6.) * c1 +
       ((a * -3. + 13.) * a - 13.)) * c1 + (((a * 2. - 25.) * a +
					     72.) * a - 61.) * .5) * c1 +
     (((a * 25. - 195.) * a + 477.) * a - 379.) / 12.);
  xn = (((c5 / y + c4) / y + c3) / y + c2) / y + c1 + y;
  if (a > 1.)
    {
      goto L220;
    }
  if (bx > bmin[iop - 1])
    {
      goto L220;
    }
  *x = xn;
  return 0;

 L40:
  if (bx * *q > 1e-8)
    {
      goto L50;
    }
  xn = exp (-(*q / a + c__));
  goto L70;
 L50:
  if (*p <= .9)
    {
      goto L60;
    }
  d__1 = -(*q);
  xn = exp ((cdf_dln1px (d__1) + cdf_gamln1 (a)) / a);
  goto L70;
 L60:
  xn = exp (log (*p * g) / a);
 L70:
  if (xn == 0.)
    {
      goto L310;
    }
  t = .5 - xn / (a + 1.) + .5;
  xn /= t;
  goto L160;

  /*        SELECTION OF THE INITIAL APPROXIMATION XN OF X */
  /*                       WHEN A .GT. 1 */

 L80:
  if (*q <= .5)
    {
      goto L90;
    }
  w = log (*p);
  goto L100;
 L90:
  w = log (*q);
 L100:
  t = sqrt (w * -2.);
  s =
    t - (((a3 * t + a2) * t + a1) * t +
	 a0) / ((((b4 * t + b3) * t + b2) * t + b1) * t + 1.);
  if (*q > .5)
    {
      s = -s;
    }

  rta = sqrt (a);
  s2 = s * s;
  xn =
    a + s * rta + (s2 - 1.) / 3. + s * (s2 - 7.) / (rta * 36.) -
    ((s2 * 3. + 7.) * s2 - 16.) / (a * 810.) + s * ((s2 * 9. + 256.) * s2 -
						    433.) / (a * 38880. *
							     rta);
  xn = Max (xn, 0.);
  if (a < amin[iop - 1])
    {
      goto L110;
    }
  *x = xn;
  d__ = .5 - *x / a + .5;
  if (Abs (d__) <= dmin__[iop - 1])
    {
      return 0;
    }

 L110:
  if (*p <= .5)
    {
      goto L130;
    }
  if (xn < a * 3.)
    {
      goto L220;
    }
  y = -(w + cdf_gamln (a));
  /* Computing MAX */
  d__1 = 2., d__2 = a * (a - 1.);
  d__ = Max (d__1, d__2);
  if (y < ln10 * d__)
    {
      goto L120;
    }
  s = 1. - a;
  z__ = log (y);
  goto L30;
 L120:
  t = a - 1.;
  d__1 = -t / (xn + 1.);
  xn = y + t * log (xn) - cdf_dln1px (d__1);
  d__1 = -t / (xn + 1.);
  xn = y + t * log (xn) - cdf_dln1px (d__1);
  goto L220;

 L130:
  ap1 = a + 1.;
  if (xn > ap1 * .7)
    {
      goto L170;
    }
  w += cdf_gamln (ap1);
  if (xn > ap1 * .15)
    {
      goto L140;
    }
  ap2 = a + 2.;
  ap3 = a + 3.;
  *x = exp ((w + *x) / a);
  *x = exp ((w + *x - log (*x / ap1 * (*x / ap2 + 1.) + 1.)) / a);
  *x = exp ((w + *x - log (*x / ap1 * (*x / ap2 + 1.) + 1.)) / a);
  *x =
    exp ((w + *x -
	  log (*x / ap1 * (*x / ap2 * (*x / ap3 + 1.) + 1.) + 1.)) / a);
  xn = *x;
  if (xn > ap1 * .01)
    {
      goto L140;
    }
  if (xn <= emin[iop - 1] * ap1)
    {
      return 0;
    }
  goto L170;

 L140:
  apn = ap1;
  t = xn / apn;
  sum = t + 1.;
 L150:
  apn += 1.;
  t *= xn / apn;
  sum += t;
  if (t > 1e-4)
    {
      goto L150;
    }
  t = w - log (sum);
  xn = exp ((xn + t) / a);
  xn *= 1. - (a * log (xn) - xn - t) / (a - xn);
  goto L170;

  /*                 SCHRODER ITERATION USING P */

 L160:
  if (*p > .5)
    {
      goto L220;
    }
 L170:
  if (*p <= xmin * 1e10)
    {
      goto L350;
    }
  am1 = a - .5 - .5;
 L180:
  if (a <= amax)
    {
      goto L190;
    }
  d__ = .5 - xn / a + .5;
  if (Abs (d__) <= e2)
    {
      goto L350;
    }

 L190:
  if (*ierr >= 20)
    {
      goto L330;
    }
  ++(*ierr);
  cdf_gratio (a, xn, &pn, &qn, &c__0);
  if (pn == 0. || qn == 0.)
    {
      goto L350;
    }
  r__ = cdf_rcomp (a, xn);
  if (r__ == 0.)
    {
      goto L350;
    }
  t = (pn - *p) / r__;
  w = (am1 - xn) * .5;
  if (Abs (t) <= .1 && (d__1 = w * t, Abs (d__1)) <= .1)
    {
      goto L200;
    }
  *x = xn * (1. - t);
  if (*x <= 0.)
    {
      goto L340;
    }
  d__ = Abs (t);
  goto L210;

 L200:
  h__ = t * (w * t + 1.);
  *x = xn * (1. - h__);
  if (*x <= 0.)
    {
      goto L340;
    }
  if (Abs (w) >= 1. && Abs (w) * t * t <= eps)
    {
      return 0;
    }
  d__ = Abs (h__);
 L210:
  xn = *x;
  if (d__ > tol)
    {
      goto L180;
    }
  if (d__ <= eps)
    {
      return 0;
    }
  if ((d__1 = *p - pn, Abs (d__1)) <= tol * *p)
    {
      return 0;
    }
  goto L180;

  /*                 SCHRODER ITERATION USING Q */

 L220:
  if (*q <= xmin * 1e10)
    {
      goto L350;
    }
  am1 = a - .5 - .5;
 L230:
  if (a <= amax)
    {
      goto L240;
    }
  d__ = .5 - xn / a + .5;
  if (Abs (d__) <= e2)
    {
      goto L350;
    }

 L240:
  if (*ierr >= 20)
    {
      goto L330;
    }
  ++(*ierr);
  cdf_gratio (a, xn, &pn, &qn, &c__0);
  if (pn == 0. || qn == 0.)
    {
      goto L350;
    }
  r__ = cdf_rcomp (a, xn);
  if (r__ == 0.)
    {
      goto L350;
    }
  t = (*q - qn) / r__;
  w = (am1 - xn) * .5;
  if (Abs (t) <= .1 && (d__1 = w * t, Abs (d__1)) <= .1)
    {
      goto L250;
    }
  *x = xn * (1. - t);
  if (*x <= 0.)
    {
      goto L340;
    }
  d__ = Abs (t);
  goto L260;

 L250:
  h__ = t * (w * t + 1.);
  *x = xn * (1. - h__);
  if (*x <= 0.)
    {
      goto L340;
    }
  if (Abs (w) >= 1. && Abs (w) * t * t <= eps)
    {
      return 0;
    }
  d__ = Abs (h__);
 L260:
  xn = *x;
  if (d__ > tol)
    {
      goto L230;
    }
  if (d__ <= eps)
    {
      return 0;
    }
  if ((d__1 = *q - qn, Abs (d__1)) <= tol * *q)
    {
      return 0;
    }
  goto L230;

  /*                       SPECIAL CASES */

 L270:
  *x = 2.0*DBL_MAX;  /* change to get Inf in place of xmax (Bruno, oct 2010) */
  return 0;

 L280:
  if (*q < .9)
    {
      goto L290;
    }
  d__1 = -(*p);
  *x = -cdf_dln1px (d__1);
  return 0;
 L290:
  *x = -log (*q);
  return 0;

  /*                       ERROR RETURN */

 L300:
  *ierr = -2;
  return 0;

 L310:
  *ierr = -3;
  return 0;

 L320:
  *ierr = -4;
  return 0;

 L330:
  *ierr = -6;
  return 0;

 L340:
  *ierr = -7;
  return 0;

 L350:
  *x = xn;
  *ierr = -8;
  return 0;

 L360:
  *x = xmax;
  *ierr = -8;
  return 0;
}

