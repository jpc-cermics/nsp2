#include "signal.h"

/*!purpose 
 *elliptic filter 
 *computation of the zeros and the locations of the extrema 
 *! 
 * 
 * 
 * 
 */




static double c_b3 = 2.;

int
signal_desi14 (int *nmaxi, int *maxdeg, int *ndeg, double *vsn, double *gd1,
	       double *gd2, double *adelta, int *nzm, double *sm, int *nzero,
	       double *dsk, double *ugc, double *ogc, double *ack, int *nj,
	       int *nh, double *dk, double *dks, double *dcap02,
	       double *dcap04)
{
  /* Initialized data */

  static double de = 1.;

  int sm_dim1, sm_offset, i__1;

  /* Local variables */
  double flma, dcap01;
  int i__, j, m, ndegi;
  double ddelt, dm, dn, dq;
  int mh;
  double du;
  int kj, mj;
  double ddelta;
  double dde, dpi;
  int inh;
  double del1, del2;

  /* Parameter adjustments */
  sm_dim1 = *maxdeg;
  sm_offset = sm_dim1 + 1;
  sm -= sm_offset;
  --nzm;
  --nzero;
  --dsk;

  /* Function Body */
  dpi = atan (1.) * 4.;
  i__1 = (int) C2F(slamch) ("l", 1L) - 2;
  flma = nsp_pow_di (c_b3, i__1);
  /* 
   */
  *dcap02 = de / *vsn;
  dcap01 = sqrt (*dcap02);
  *dcap04 = sqrt (de - *dcap02 * *dcap02);
  *dk = signal_dellk (dcap02);
  *dks = signal_dellk (dcap04);
  /* 
   */
  dq = exp (-dpi * *dks / *dk);
  /* 
   */
  *nh = *ndeg / 2;
  m = *ndeg + 1;
  *nj = m / 2;
  mh = *nh + 1;
  /* 
   */
  dn = *dk / (double) (*ndeg);
  /* 
   */
  del1 = de;
  if (*nh == 0)
    {
      goto L20;
    }
  i__1 = *nh;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      inh = m - i__ - i__;
      du = dn * (double) inh;
      dm = signal_dsn2 (&du, dk, &dq);
      del1 = del1 * dm * dcap01;
      dsk[i__] = dm;
      j = mh - i__;
      sm[j + sm_dim1] = dm;
      nzero[i__] = 2;
      dde = de / (*dcap02 * dm);
      sm[i__ + (sm_dim1 << 2)] = dde;
      /* L10: */
    }
  goto L30;
 L20:
  sm[sm_dim1 + 1] = 0.;
 L30:
  kj = *nj - 1;
  mj = *nj + 1;
  del2 = de;
  if (kj == 0)
    {
      goto L50;
    }
  i__1 = kj;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ndegi = *ndeg - i__ - i__;
      du = dn * (double) ndegi;
      dm = signal_dsn2 (&du, dk, &dq);
      j = mj - i__;
      sm[j + (sm_dim1 << 1)] = dm;
      dde = de / (*dcap02 * dm);
      sm[i__ + 1 + sm_dim1 * 3] = dde;
      del2 = del2 * dm * dcap01;
      /* L40: */
    }
  goto L60;
 L50:
  sm[*ndeg + (sm_dim1 << 1)] = 1.;
  sm[sm_dim1 * 3 + 1] = *vsn;
  /* 
   */
 L60:
  ddelt = del1 * del1;
  *adelta = ddelt;
  *ack = 1. / *adelta;
  if (*nh == *nj)
    {
      goto L80;
    }
  *ack *= dcap01;
  ddelta = del2 * del2 * dcap01;
  *adelta = ddelta;
  dsk[*nj] = 0.;
  nzero[*nj] = 1;
  sm[*nj + (sm_dim1 << 2)] = flma;
  /* 
   */
  if (*nh == 0)
    {
      goto L90;
    }
  i__1 = *nh;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      j = mj - i__;
      sm[j + sm_dim1] = sm[j - 1 + sm_dim1];
      sm[i__ + (sm_dim1 << 1)] = sm[i__ + 1 + (sm_dim1 << 1)];
      /* L70: */
    }
  sm[sm_dim1 + 1] = 0.;
  goto L90;
  /* 
   */
 L80:
  sm[mh + sm_dim1 * 3] = flma;
  sm[(sm_dim1 << 1) + 1] = 0.;
  /* 
   */
 L90:
  nzm[1] = *nj;
  nzm[4] = *nj;
  nzm[2] = mh;
  nzm[3] = mh;
  sm[mh + (sm_dim1 << 1)] = 1.;
  sm[sm_dim1 * 3 + 1] = *vsn;
  *ugc = *gd2 * *adelta;
  *ogc = *gd1 / *adelta;
  sm[*nmaxi - 1 + (sm_dim1 << 2)] = 1.;
  return 0;
}				/* desi14_ */
