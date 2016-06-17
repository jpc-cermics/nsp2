#include "signal.h"

/*!purpose 
 *butterworth filter 
 *computation of the poles 
 *! 
 * 
 */




static double c_b5 = .33333333333333331;

int
signal_desi21 (int *ndeg, double *adelp, double *adels, double *adelta,
	       double *pren, double *pimn, double *ugc, double *ogc, int *nj,
	       double *acx, double *ac, double *rdelp, double *rdels,
	       double *sfa, double *spr, double *spi)
{
  int i__1;
  double d__1, d__2;

  /* Local variables */
  double flmi;
  int i__;
  double q;

  /* Parameter adjustments */
  --spi;
  --spr;
  --pimn;
  --pren;

  /* Function Body */
  flmi = nsp_dlamch ("p") * 2.;
  /* 
   *computation of constant c and reduced tolerance scheme 
   * 
   */
  if (*acx < 999.)
    {
      goto L20;
    }
  if (*ogc - *ugc < flmi)
    {
      goto L10;
    }
  d__1 = *adelp * 2. / (*adelta * *adels);
  *ac = pow_dd (&d__1, &c_b5);
  d__1 = *ac / *ugc;
  d__2 = *ogc / *ugc;
  *acx = log10 (d__1) / log10 (d__2);
  if (*acx >= 0. && *acx <= 1.)
    {
      goto L30;
    }
 L10:
  *acx = .5;
 L20:
  d__1 = *ogc / *ugc;
  *ac = *ugc * pow_dd (&d__1, acx);
 L30:
  *rdelp = 1. - sqrt (1. / (*ac * *ac + 1.));
  q = *ac * *adelta;
  *rdels = sqrt (1. / (q * q + 1.));
  /* 
   *computation of factor sfa and poles 
   * 
   */
  *sfa = 1. / *ac;
  d__1 = -1. / (double) (*ndeg);
  q = pow_dd (ac, &d__1);
  /* 
   */
  i__1 = *nj;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      spr[i__] = -q * pren[i__];
      spi[i__] = q * pimn[i__];
      /* L40: */
    }
  return 0;
}				/* desi21_ */
