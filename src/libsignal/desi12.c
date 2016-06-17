#include "signal.h"

/*!purpose 
 *chebyshev filter (passband or stopband) 
 *computation of the zeros and the locations of the extrema 
 *!calling sequence 
 *    subroutine desi12 (nmaxi,maxdeg,iapro,ndeg,vsn,gd1,gd2,adelta, 
 *   *nzm,sm,nzero,pren,pimn,ugc,ack,nj,nh) 
 *! 
 * 
 * 
 * 
 */




static double c_b3 = 2.;

int
signal_desi12 (int *nmaxi, int *maxdeg, int *iapro, int *ndeg, double *vsn,
	       double *gd1, double *gd2, double *adelta, int *nzm, double *sm,
	       int *nzero, double *pren, double *pimn, double *ugc,
	       double *ack, int *nj, int *nh)
{
  int sm_dim1, sm_offset, i__1;

  /* Local variables */
  double flma;
  int i__, j, m;
  double q, fa, fn;
  int mi;
  double pi;
  int inj;

  /* Parameter adjustments */
  sm_dim1 = *maxdeg;
  sm_offset = sm_dim1 + 1;
  sm -= sm_offset;
  --nzm;
  --nzero;
  --pren;
  --pimn;

  /* Function Body */
  pi = atan (1.) * 4.;
  i__1 = (int) C2F(slamch) ("l", 1L) - 2;
  flma = nsp_pow_di (c_b3, i__1);
  *adelta = cosh ((double) (*ndeg) * acosh (*vsn));
  /* 
   */
  fa = 1.;
  *nh = *ndeg / 2;
  *nj = (*ndeg + 1) / 2;
  fn = pi / ((double) (*ndeg) * 2.);
  /* 
   */
  i__1 = *nj;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      nzero[i__] = 0;
      inj = i__ + i__ - 1;
      q = fn * (double) inj;
      pren[i__] = sin (q);
      pimn[i__] = cos (q);
      /* L10: */
    }
  /* 
   */
  fn *= 2.;
  /* 
   */
  if (*iapro == 3)
    {
      goto L40;
    }
  /* 
   */
  m = *nj + 1;
  i__1 = *nj;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      j = m - i__;
      sm[i__ + sm_dim1] = pimn[j];
      /* L20: */
    }
  nzm[1] = *nj;
  m = *nh + 1;
  i__1 = m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      mi = m - i__;
      q = (double) mi *fn;
      sm[i__ + (sm_dim1 << 1)] = cos (q);
      /* L30: */
    }
  nzm[2] = m;
  sm[sm_dim1 * 3 + 1] = *vsn;
  nzm[3] = 1;
  sm[(sm_dim1 << 2) + 1] = flma;
  nzm[4] = 1;
  nzero[1] = *ndeg;
  /* 
   */
  *ugc = *gd2 / *adelta;
  /*ccccccccccccccccccccccccccccccccccccc 
   *The two lines below were missing 
   */
  /* ogc = *gd1; */
  goto L80;
  /*ccccccccccccccccccccccccccccccccccccc 
   */
 L40:
  sm[sm_dim1 + 1] = 0.;
  nzm[1] = 1;
  sm[(sm_dim1 << 1) + 1] = 1.;
  nzm[2] = 1;
  i__1 = *nj;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      inj = *nj - i__;
      q = (double) inj *fn;
      sm[inj + 1 + sm_dim1 * 3] = *vsn / cos (q);
      /* L50: */
    }
  nzm[3] = *nj;
  i__1 = *nh;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      nzero[i__] = 2;
      q = pimn[i__];
      fa = fa * q * q;
      sm[i__ + (sm_dim1 << 2)] = *vsn / q;
      /* L60: */
    }
  if (*nh == *nj)
    {
      goto L70;
    }
  nzero[*nj] = 1;
  sm[*nj + (sm_dim1 << 2)] = flma;
 L70:
  nzm[4] = *nj;
  /* 
   */
  *ugc = *gd2;
  /* ogc = *gd1 * *adelta; */
 L80:
  *ack = fa;
  sm[*nmaxi - 1 + (sm_dim1 << 2)] = 1.;
  return 0;
}				/* desi12_ */
