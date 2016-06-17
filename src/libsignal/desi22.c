#include "signal.h"

/*!purpose 
 *chebyshev filter (passband or stopband) 
 *computation of the poles 
 *!calling sequence 
 *    subroutine desi22 (iapro,ndeg,adelp,adels,adelta,vsn, 
 *   *pren,pimn,ugc,ogc,ack,nj,nh,acx,ac,rdels,sfa,spr,spi) 
 * 
 *    implicit double precision (a-h,o-z) 
 *    dimension spr(*),spi(*) 
 *    dimension pren(*),pimn(*) 
 *! 
 * 
 * 
 */




static double c_b5 = .33333333333333331;
static double c_b8 = 2.;

int
signal_desi22 (int *iapro, int *ndeg, double *adelp, double *adels,
	       double *adelta, double *vsn, double *pren, double *pimn,
	       double *ugc, double *ogc, double *ack, int *nj, int *nh,
	       double *acx, double *ac, double *rdels, double *sfa,
	       double *spr, double *spi)
{
  int i1, i__ ;
  double d__1, d__2, flmi, q = 0 , qa, qi, qq, qr,  qqa;

  --spi;
  --spr;
  --pimn;
  --pren;

  /* Function Body */
  flmi = nsp_dlamch ("p") * 2.;
  if (*acx < 999.)
    {
      goto L20;
    }
  if (*ogc - *ugc < flmi)
    {
      goto L10;
    }
  if (*iapro == 2)
    {
      q = 1. / *adelta;
    }
  if (*iapro == 3)
    {
      q = *adelta * *adelta;
    }
  d__1 = *adelp * 2. * q / *adels;
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
  /* 
   *computation of the reduced tolerance scheme 
   * 
   */
 L30:
  q = *ac;
  if (*iapro == 3)
    {
      q /= *adelta;
    }
  q = q * q + 1.;
  /* rdelp = 1. - sqrt (1. / q); */
  q = *ac;
  if (*iapro == 2)
    {
      q *= *adelta;
    }
  q = q * q + 1.;
  *rdels = sqrt (1. / q);
  /* 
   *computation of the factor sfa and the poles 
   */
  if (*iapro == 3)
    {
      goto L40;
    }
  /*cccccccccccccccccccccccccccccccccccccccccccc 
   *    The next line was missing 
   */
  *sfa = 2. / (*ac * nsp_pow_di (c_b8, *ndeg));
  /*cccccccccccccccccccccccccccccccccccccccccccc 
   */
  q = -1. / *ac;
  goto L50;
 L40:
  *sfa = *ack;
  q = *ac;
 L50:
  q = asinh (q) / (double) (*ndeg);
  qr = sinh (q);
  qi = cosh (q);
  if (*iapro == 3)
    {
      goto L70;
    }
  i1 = *nj;
  for (i__ = 1; i__ <= i1; ++i__)
    {
      spr[i__] = qr * pren[i__];
      spi[i__] = qi * pimn[i__];
      /* L60: */
    }
  return 0;
 L70:
  i1 = *nh;
  for (i__ = 1; i__ <= i1 ; ++i__)
    {
      q = pimn[i__] * qi;
      qa = pren[i__] * qr;
      qq = q * q;
      qqa = qa * qa;
      *sfa /= qq + qqa;
      spr[i__] = -(*vsn) / (qq / qa + qa);
      spi[i__] = *vsn / (q + qqa / q);
      /* L80: */
    }
  if (*nh == *nj)
    {
      return 0;
    }
  spi[*nj] = 0.;
  q = *vsn / qr;
  *sfa *= q;
  spr[*nj] = -q;
  return 0;
}				/* desi22_ */
