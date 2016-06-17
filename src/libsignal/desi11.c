#include "signal.h"

/*!purpose 
 *butterworth filter 
 *computation of the zeros and locations of the extrema 
 *! 
 * 
 * 
 */




static double c_b3 = 2.;

int
signal_desi11 (int *nmaxi, int *maxdeg, double *vsn, int *ndeg, double *gd1,
	       double *gd2, double *adelta, int *nzm, double *sm, int *nzero,
	       double *pren, double *pimn, double *ugc, double *ogc, int *nj,
	       int *nh)
{
  int sm_dim1, sm_offset, i__1;

  /* Local variables */
  double fdeg, flma;
  int i__;
  double q, fn, pi;
  int iii;

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
  /* 
   */
  *adelta = nsp_pow_di (*vsn, *ndeg);
  /* 
   */
  *nh = *ndeg / 2;
  *nj = (*ndeg + 1) / 2;
  fdeg = (double) (*ndeg);
  fn = pi / 2. / fdeg;
  /* 
   */
  i__1 = *nj;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      nzero[i__] = 0;
      iii = i__ + i__ - 1;
      q = fn * (double) iii;
      pren[i__] = sin (q);
      pimn[i__] = cos (q);
      /* L10: */
    }
  /* 
   */
  fn *= 2.;
  nzero[1] = *ndeg;
  nzm[1] = 1;
  sm[sm_dim1 + 1] = 0.;
  nzm[2] = 1;
  sm[(sm_dim1 << 1) + 1] = 1.;
  nzm[3] = 1;
  sm[sm_dim1 * 3 + 1] = *vsn;
  nzm[4] = 1;
  sm[(sm_dim1 << 2) + 1] = flma;
  /* 
   */
  *ugc = *gd2 / *adelta;
  *ogc = *gd1;
  sm[*nmaxi - 1 + (sm_dim1 << 2)] = 1.;
  return 0;
}				/* desi11_ */
