#include "signal.h"

/*
 * design of filters 
 *   butterworth, 
 *   chebyshev (passband or stopband)
 *   elliptic 
 */

int
signal_desi01 (int *maxdeg, int *iapro, double *edeg, int *ndeg,
	       double *adelp, double *adels, double *vsn, double *adeg,
	       double *gd1, double *gd2, double *acap12, int *ierr)
{
  int m, n;
  double q;
  signal_parcha (iapro, adeg, adelp, adels, vsn, gd1, gd2, acap12);
  signal_degree (iapro, vsn, acap12, adeg);
  q = *adeg * (*edeg + 1.) + .5;
  n = (int) q;
  m = (int) (*adeg);
  if ((double) m < *adeg)
    {
      ++m;
    }
  n = Max (m, n);
  if (*ndeg == 0)
    {
      goto L10;
    }
  if (*ndeg >= n)
    {
      goto L20;
    }
  *ierr = 15;
  /* 
   */
 L10:
  *ndeg = n;
  /* 
   */
 L20:
  if (*ndeg <= *maxdeg)
    {
      return 0;
    }
  *ierr = 25;
  return 0;
}

  /*
   * computation of the minimum filter degree (adeg) 
   *        Nomenclature Rabiner-Gold (page 241) 
   *        acap12=1/k1 
   *        vsn=1/k 
   */

int
signal_degree (int *iapro, double *vsn, double *acap12, double *adeg)
{
  double d__1;
  double dadeg, dcap02, dcap12, dcap04, dcap14;
  double de;

  switch (*iapro)
    {
    case 1:
      *adeg = log (1. / *acap12) / log (*vsn);
      break;
    case 2:
      d__1 = 1. / *acap12;
      *adeg = acosh (d__1) / acosh (*vsn);
      break;
    case 4:
      de = 1.;
      dcap02 = de / *vsn;
      dcap04 = sqrt (de - dcap02 * dcap02);
      dcap12 = *acap12;
      dcap14 = sqrt (de - dcap12 * dcap12);
      dadeg =
	signal_dellk (&dcap02) * signal_dellk (&dcap14) /
	(signal_dellk (&dcap04) * signal_dellk (&dcap12));
      *adeg = dadeg;
      break;
    }
  return 0;
}

/*
 * computation of the parameters of the characteristic function 
 *        Nomenclature Rabiner-Gold (page 241) 
 *        Si adelp > 0 et adelps > 0 
 *        acap12=1/k1 
 */

int
signal_parcha (int *iapro, double *adeg, double *adelp, double *adels,
	       double *vsn, double *gd1, double *gd2, double *acap12)
{
  double d__1;
  double q;

  *gd1 = 0.;
  *gd2 = -1.;
  if (*adelp > 0.)
    {
      *gd1 = sqrt ((2. - *adelp) * *adelp) / (1. - *adelp);
    }
  if (*adels > 0.)
    {
      *gd2 = sqrt (1. - *adels * *adels) / *adels;
    }
  *acap12 = *gd1 / *gd2;
  if (*acap12 > 0.)
    {
      return 0;
    }
  switch (*iapro)
    {
    case 1:
      d__1 = -(*adeg);
      *acap12 = pow_dd (vsn, &d__1);
      break;
    case 2:
    case 3:
      q = acosh (*vsn) * *adeg;
      *acap12 = 1. / cosh (q);
      break;
    case 4:
      signal_bounn (adeg, acap12, vsn);
      break;
    }
  if (*gd2 == -1.)
    {
      *gd2 = *gd1 / *acap12;
      *adels = 1. / sqrt (*gd2 * *gd2 + 1.);
    }
  else
    {
      *gd1 = *acap12 * *gd2;
      *adelp = 1. - 1. / sqrt (*gd1 * *gd1 + 1.);
    }
  return 0;
}

/*
 * calculation of a bounn for vsn or acap12 for elliptic filters 
 */

int
signal_bounn (double *adeg, double *acap12, double *vsn)
{
  static double de = 1.;
  double d__1, d__2;
  double ddeg, dmax__, dcap12;
  int j;
  double dcap14;
  double df[3], dk[3];
  int ii, jj = 0;
  double dq, dk1, dab, dde, deg, dkk, dpi;

  dpi = atan (1.) * 4.;
  if (*acap12 <= 0.)
    {
      goto L10;
    }
  dcap12 = *acap12;
  deg = 1. / *adeg;
  ii = 1;
  goto L20;
 L10:
  dcap12 = de / *vsn;
  deg = *adeg;
  ii = -1;
 L20:
  dcap14 = sqrt (de - dcap12 * dcap12);
  dkk = signal_dellk (&dcap14) / signal_dellk (&dcap12);
  dq = exp (-dpi * dkk * deg);
  dk1 = sqrt (dq) * 4.;
  if (dk1 < de)
    {
      goto L30;
    }
  dq *= 2.;
  dq = (de - dq) / (de + dq);
  dq *= dq;
  dk1 = sqrt (de - dq * dq);
 L30:
  dk[0] = dk1;
  dk[1] = (de + dk[0]) / 2.;
  ddeg = *adeg;
  d__2 = sqrt (de - dk[0] * dk[0]);
  d__1 = signal_dellk (dk) * dkk / signal_dellk (&d__2);
  df[0] = nsp_pow_di (d__1, ii) - ddeg;
  d__2 = sqrt (de - dk[1] * dk[1]);
  d__1 = signal_dellk (&dk[1]) * dkk / signal_dellk (&d__2);
  df[1] = nsp_pow_di (d__1, ii) - ddeg;
 L40:
  dk[2] = dk[0] - df[0] * (dk[0] - dk[1]) / (df[0] - df[1]);
  d__2 = sqrt (de - dk[2] * dk[2]);
  d__1 = signal_dellk (&dk[2]) * dkk / signal_dellk (&d__2);
  df[2] = nsp_pow_di (d__1, ii) - ddeg;
  if (Abs (df[2]) < 1e-6)
    {
      goto L60;
    }
  dmax__ = 0.;
  for (j = 1; j <= 3; ++j)
    {
      dab = (d__1 = df[j - 1], Abs (d__1));
      if (dmax__ > dab)
	{
	  goto L50;
	}
      jj = j;
      dmax__ = dab;
    L50:
      ;
    }
  if (jj == 3)
    {
      goto L40;
    }
  dk[jj - 1] = dk[2];
  df[jj - 1] = df[2];
  goto L40;
 L60:
  if (*acap12 <= 0.)
    {
      goto L70;
    }
  dde = de / dk[2];
  *vsn = dde;
  return 0;
 L70:
  *acap12 = dk[2];
  return 0;
}

