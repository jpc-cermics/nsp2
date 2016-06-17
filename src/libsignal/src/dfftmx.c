/* dfftmx.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "signal.h"

/* Subroutine */ int
signal_dfftmx (double *a, double *b, int *ntot, int *n, int *nspan, int *isn,
	       int *m, int *kt, double *wt, double *ck, double *bt,
	       double *sk, int *np, int *nfac)
{
  /* System generated locals */
  int i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  int maxf, klim, i__, j, k, kspan;
  double c1, c2, c3;
  int kspnn, k1, k2, k3, k4;
  double s1, s2, s3, aa, bb, cd, aj, c72, ak;
  int jc, jf;
  double bk, bj;
  int jj;
  double dr, sd;
  int kk, mm;
  double s72;
  int nn, ks, nt;
  double s120, rad, ajm, akm;
  int inc;
  double ajp, akp, bkp, bkm, bjp, bjm;
  int lim;

  /* 
   * 
   */
  /* Parameter adjustments */
  --nfac;
  --np;
  --sk;
  --bt;
  --ck;
  --wt;
  --b;
  --a;

  /* Function Body */
  inc = Abs (*isn);
  nt = inc * *ntot;
  ks = inc * *nspan;
  rad = atan (1.);
  s72 = rad / .625;
  c72 = cos (s72);
  s72 = sin (s72);
  s120 = sqrt (.75);
  if (*isn > 0)
    {
      goto L10;
    }
  s72 = -s72;
  s120 = -s120;
  rad = -rad;
  goto L30;
  /* 
   *scale by 1/n for isn .gt. 0 
   * 
   */
 L10:
  ak = 1. / (double) (*n);
  i__1 = nt;
  i__2 = inc;
  for (j = 1; i__2 < 0 ? j >= i__1 : j <= i__1; j += i__2)
    {
      a[j] *= ak;
      b[j] *= ak;
      /* L20: */
    }
  /* 
   */
 L30:
  kspan = ks;
  nn = nt - inc;
  jc = ks / *n;
  /* 
   *sin, cos values are reinitialized each lim steps 
   * 
   */
  lim = 32;
  klim = lim * jc;
  i__ = 0;
  jf = 0;
  maxf = *m - *kt;
  maxf = nfac[maxf];
  if (*kt > 0)
    {
      /*Computing MAX 
       */
      i__2 = nfac[*kt];
      maxf = Max (i__2, maxf);
    }
  /* 
   *compute fourier transform 
   * 
   */
 L40:
  dr = (double) jc *8. / (double) kspan;
  /*Computing 2nd power 
   */
  d__1 = sin (dr * .5 * rad);
  cd = d__1 * d__1 * 2.;
  sd = sin (dr * rad);
  kk = 1;
  ++i__;
  if (nfac[i__] != 2)
    {
      goto L110;
    }
  /* 
   *transform for factor of 2 (including rotation factor) 
   * 
   */
  kspan /= 2;
  k1 = kspan + 2;
 L50:
  k2 = kk + kspan;
  ak = a[k2];
  bk = b[k2];
  a[k2] = a[kk] - ak;
  b[k2] = b[kk] - bk;
  a[kk] += ak;
  b[kk] += bk;
  kk = k2 + kspan;
  if (kk <= nn)
    {
      goto L50;
    }
  kk -= nn;
  if (kk <= jc)
    {
      goto L50;
    }
  if (kk > kspan)
    {
      goto L350;
    }
 L60:
  c1 = 1. - cd;
  s1 = sd;
  /*Computing MIN 
   */
  i__2 = k1 / 2;
  mm = Min (i__2, klim);
  goto L80;
 L70:
  ak = c1 - (cd * c1 + sd * s1);
  s1 = sd * c1 - cd * s1 + s1;
  /* 
   *the following three statements compensate for truncation 
   *error.  if rounded arithmetic is used, substitute 
   *c1=ak 
   * 
   *Computing 2nd power 
   */
  d__1 = ak;
  /*Computing 2nd power 
   */
  d__2 = s1;
  c1 = .5 / (d__1 * d__1 + d__2 * d__2) + .5;
  s1 = c1 * s1;
  c1 *= ak;
 L80:
  k2 = kk + kspan;
  ak = a[kk] - a[k2];
  bk = b[kk] - b[k2];
  a[kk] += a[k2];
  b[kk] += b[k2];
  a[k2] = c1 * ak - s1 * bk;
  b[k2] = s1 * ak + c1 * bk;
  kk = k2 + kspan;
  if (kk < nt)
    {
      goto L80;
    }
  k2 = kk - nt;
  c1 = -c1;
  kk = k1 - k2;
  if (kk > k2)
    {
      goto L80;
    }
  kk += jc;
  if (kk <= mm)
    {
      goto L70;
    }
  if (kk < k2)
    {
      goto L90;
    }
  k1 = k1 + inc + inc;
  kk = (k1 - kspan) / 2 + jc;
  if (kk <= jc + jc)
    {
      goto L60;
    }
  goto L40;
 L90:
  s1 = (double) ((kk - 1) / jc) * dr * rad;
  c1 = cos (s1);
  s1 = sin (s1);
  /*Computing MIN 
   */
  i__2 = k1 / 2, i__1 = mm + klim;
  mm = Min (i__2, i__1);
  goto L80;
  /* 
   *transform for factor of 3 (optional code) 
   * 
   */
 L100:
  k1 = kk + kspan;
  k2 = k1 + kspan;
  ak = a[kk];
  bk = b[kk];
  aj = a[k1] + a[k2];
  bj = b[k1] + b[k2];
  a[kk] = ak + aj;
  b[kk] = bk + bj;
  ak = aj * -.5 + ak;
  bk = bj * -.5 + bk;
  aj = (a[k1] - a[k2]) * s120;
  bj = (b[k1] - b[k2]) * s120;
  a[k1] = ak - bj;
  b[k1] = bk + aj;
  a[k2] = ak + bj;
  b[k2] = bk - aj;
  kk = k2 + kspan;
  if (kk < nn)
    {
      goto L100;
    }
  kk -= nn;
  if (kk <= kspan)
    {
      goto L100;
    }
  goto L290;
  /* 
   *transform for factor of 4 
   * 
   */
 L110:
  if (nfac[i__] != 4)
    {
      goto L230;
    }
  kspnn = kspan;
  kspan /= 4;
 L120:
  c1 = 1.;
  s1 = 0.;
  mm = Min (kspan, klim);
  goto L150;
 L130:
  c2 = c1 - (cd * c1 + sd * s1);
  s1 = sd * c1 - cd * s1 + s1;
  /* 
   *the following three statements compensate for truncation 
   *error.  if rounded arithmetic is used, substitute 
   *c1=c2 
   * 
   *Computing 2nd power 
   */
  d__1 = c2;
  /*Computing 2nd power 
   */
  d__2 = s1;
  c1 = .5 / (d__1 * d__1 + d__2 * d__2) + .5;
  s1 = c1 * s1;
  c1 *= c2;
 L140:
  /*Computing 2nd power 
   */
  d__1 = c1;
  /*Computing 2nd power 
   */
  d__2 = s1;
  c2 = d__1 * d__1 - d__2 * d__2;
  s2 = c1 * s1 * 2.;
  c3 = c2 * c1 - s2 * s1;
  s3 = c2 * s1 + s2 * c1;
 L150:
  k1 = kk + kspan;
  k2 = k1 + kspan;
  k3 = k2 + kspan;
  akp = a[kk] + a[k2];
  akm = a[kk] - a[k2];
  ajp = a[k1] + a[k3];
  ajm = a[k1] - a[k3];
  a[kk] = akp + ajp;
  ajp = akp - ajp;
  bkp = b[kk] + b[k2];
  bkm = b[kk] - b[k2];
  bjp = b[k1] + b[k3];
  bjm = b[k1] - b[k3];
  b[kk] = bkp + bjp;
  bjp = bkp - bjp;
  if (*isn < 0)
    {
      goto L180;
    }
  akp = akm - bjm;
  akm += bjm;
  bkp = bkm + ajm;
  bkm -= ajm;
  if (s1 == 0.)
    {
      goto L190;
    }
 L160:
  a[k1] = akp * c1 - bkp * s1;
  b[k1] = akp * s1 + bkp * c1;
  a[k2] = ajp * c2 - bjp * s2;
  b[k2] = ajp * s2 + bjp * c2;
  a[k3] = akm * c3 - bkm * s3;
  b[k3] = akm * s3 + bkm * c3;
  kk = k3 + kspan;
  if (kk <= nt)
    {
      goto L150;
    }
 L170:
  kk = kk - nt + jc;
  if (kk <= mm)
    {
      goto L130;
    }
  /*     MODIF HERE  (WAS .lt.) 
   */
  if (kk <= kspan)
    {
      goto L200;
    }
  kk = kk - kspan + inc;
  if (kk <= jc)
    {
      goto L120;
    }
  if (kspan == jc)
    {
      goto L350;
    }
  goto L40;
 L180:
  akp = akm + bjm;
  akm -= bjm;
  bkp = bkm - ajm;
  bkm += ajm;
  if (s1 != 0.)
    {
      goto L160;
    }
 L190:
  a[k1] = akp;
  b[k1] = bkp;
  a[k2] = ajp;
  b[k2] = bjp;
  a[k3] = akm;
  b[k3] = bkm;
  kk = k3 + kspan;
  if (kk <= nt)
    {
      goto L150;
    }
  goto L170;
 L200:
  s1 = (double) ((kk - 1) / jc) * dr * rad;
  c1 = cos (s1);
  s1 = sin (s1);
  /*Computing MIN 
   */
  i__2 = kspan, i__1 = mm + klim;
  mm = Min (i__2, i__1);
  goto L140;
  /* 
   *transform for factor of 5 (optional code) 
   * 
   */
 L210:
  /*Computing 2nd power 
   */
  d__1 = c72;
  /*Computing 2nd power 
   */
  d__2 = s72;
  c2 = d__1 * d__1 - d__2 * d__2;
  s2 = c72 * 2. * s72;
 L220:
  k1 = kk + kspan;
  k2 = k1 + kspan;
  k3 = k2 + kspan;
  k4 = k3 + kspan;
  akp = a[k1] + a[k4];
  akm = a[k1] - a[k4];
  bkp = b[k1] + b[k4];
  bkm = b[k1] - b[k4];
  ajp = a[k2] + a[k3];
  ajm = a[k2] - a[k3];
  bjp = b[k2] + b[k3];
  bjm = b[k2] - b[k3];
  aa = a[kk];
  bb = b[kk];
  a[kk] = aa + akp + ajp;
  b[kk] = bb + bkp + bjp;
  ak = akp * c72 + ajp * c2 + aa;
  bk = bkp * c72 + bjp * c2 + bb;
  aj = akm * s72 + ajm * s2;
  bj = bkm * s72 + bjm * s2;
  a[k1] = ak - bj;
  a[k4] = ak + bj;
  b[k1] = bk + aj;
  b[k4] = bk - aj;
  ak = akp * c2 + ajp * c72 + aa;
  bk = bkp * c2 + bjp * c72 + bb;
  aj = akm * s2 - ajm * s72;
  bj = bkm * s2 - bjm * s72;
  a[k2] = ak - bj;
  a[k3] = ak + bj;
  b[k2] = bk + aj;
  b[k3] = bk - aj;
  kk = k4 + kspan;
  if (kk < nn)
    {
      goto L220;
    }
  kk -= nn;
  if (kk <= kspan)
    {
      goto L220;
    }
  goto L290;
  /* 
   *transform for odd factors 
   * 
   */
 L230:
  k = nfac[i__];
  kspnn = kspan;
  kspan /= k;
  if (k == 3)
    {
      goto L100;
    }
  if (k == 5)
    {
      goto L210;
    }
  if (k == jf)
    {
      goto L250;
    }
  jf = k;
  s1 = rad / ((double) k / 8.);
  c1 = cos (s1);
  s1 = sin (s1);
  ck[jf] = 1.;
  sk[jf] = 0.;
  j = 1;
 L240:
  ck[j] = ck[k] * c1 + sk[k] * s1;
  sk[j] = ck[k] * s1 - sk[k] * c1;
  --k;
  ck[k] = ck[j];
  sk[k] = -sk[j];
  ++j;
  if (j < k)
    {
      goto L240;
    }
 L250:
  k1 = kk;
  k2 = kk + kspnn;
  aa = a[kk];
  bb = b[kk];
  ak = aa;
  bk = bb;
  j = 1;
  k1 += kspan;
 L260:
  k2 -= kspan;
  ++j;
  wt[j] = a[k1] + a[k2];
  ak = wt[j] + ak;
  bt[j] = b[k1] + b[k2];
  bk = bt[j] + bk;
  ++j;
  wt[j] = a[k1] - a[k2];
  bt[j] = b[k1] - b[k2];
  k1 += kspan;
  if (k1 < k2)
    {
      goto L260;
    }
  a[kk] = ak;
  b[kk] = bk;
  k1 = kk;
  k2 = kk + kspnn;
  j = 1;
 L270:
  k1 += kspan;
  k2 -= kspan;
  jj = j;
  ak = aa;
  bk = bb;
  aj = 0.;
  bj = 0.;
  k = 1;
 L280:
  ++k;
  ak = wt[k] * ck[jj] + ak;
  bk = bt[k] * ck[jj] + bk;
  ++k;
  aj = wt[k] * sk[jj] + aj;
  bj = bt[k] * sk[jj] + bj;
  jj += j;
  if (jj > jf)
    {
      jj -= jf;
    }
  if (k < jf)
    {
      goto L280;
    }
  k = jf - j;
  a[k1] = ak - bj;
  b[k1] = bk + aj;
  a[k2] = ak + bj;
  b[k2] = bk - aj;
  ++j;
  if (j < k)
    {
      goto L270;
    }
  kk += kspnn;
  if (kk <= nn)
    {
      goto L250;
    }
  kk -= nn;
  if (kk <= kspan)
    {
      goto L250;
    }
  /* 
   *multiply by rotation factor (except for factors of 2 and 4) 
   * 
   */
 L290:
  if (i__ == *m)
    {
      goto L350;
    }
  kk = jc + 1;
 L300:
  c2 = 1. - cd;
  s1 = sd;
  mm = Min (kspan, klim);
  goto L320;
 L310:
  c2 = c1 - (cd * c1 + sd * s1);
  s1 += sd * c1 - cd * s1;
  /* 
   *the following three statements compensate for truncation 
   *error.  if rounded arithmetic is used, they may 
   *be deleted. 
   * 
   *Computing 2nd power 
   */
  d__1 = c2;
  /*Computing 2nd power 
   */
  d__2 = s1;
  c1 = .5 / (d__1 * d__1 + d__2 * d__2) + .5;
  s1 = c1 * s1;
  c2 = c1 * c2;
 L320:
  c1 = c2;
  s2 = s1;
  kk += kspan;
 L330:
  ak = a[kk];
  a[kk] = c2 * ak - s2 * b[kk];
  b[kk] = s2 * ak + c2 * b[kk];
  kk += kspnn;
  if (kk <= nt)
    {
      goto L330;
    }
  ak = s1 * s2;
  s2 = s1 * c2 + c1 * s2;
  c2 = c1 * c2 - ak;
  kk = kk - nt + kspan;
  if (kk <= kspnn)
    {
      goto L330;
    }
  kk = kk - kspnn + jc;
  if (kk <= mm)
    {
      goto L310;
    }
  /*            MODIFICATION OF ORIGINAL CODE: 
   */
  if (kk <= kspan)
    {
      goto L340;
    }
  /*        SINGLETON's CODE was: 
   *    if (kk.lt.kspan) go to 340 
   */
  kk = kk - kspan + jc + inc;
  if (kk <= jc + jc)
    {
      goto L300;
    }
  goto L40;
 L340:
  s1 = (double) ((kk - 1) / jc) * dr * rad;
  c2 = cos (s1);
  s1 = sin (s1);
  /*Computing MIN 
   */
  i__2 = kspan, i__1 = mm + klim;
  mm = Min (i__2, i__1);
  goto L320;
  /* 
   *permute the results to normal order---done in two stages 
   *permutation for square factors of n 
   * 
   */
 L350:
  np[1] = ks;
  if (*kt == 0)
    {
      goto L440;
    }
  k = *kt + *kt + 1;
  if (*m < k)
    {
      --k;
    }
  j = 1;
  np[k + 1] = jc;
 L360:
  np[j + 1] = np[j] / nfac[j];
  np[k] = np[k + 1] * nfac[j];
  ++j;
  --k;
  if (j < k)
    {
      goto L360;
    }
  k3 = np[k + 1];
  kspan = np[2];
  kk = jc + 1;
  k2 = kspan + 1;
  j = 1;
  if (*n != *ntot)
    {
      goto L400;
    }
  /* 
   *permutation for single-variate transform (optional code) 
   * 
   */
 L370:
  ak = a[kk];
  a[kk] = a[k2];
  a[k2] = ak;
  bk = b[kk];
  b[kk] = b[k2];
  b[k2] = bk;
  kk += inc;
  k2 = kspan + k2;
  if (k2 < ks)
    {
      goto L370;
    }
 L380:
  k2 -= np[j];
  ++j;
  k2 = np[j + 1] + k2;
  if (k2 > np[j])
    {
      goto L380;
    }
  j = 1;
 L390:
  if (kk < k2)
    {
      goto L370;
    }
  kk += inc;
  k2 = kspan + k2;
  if (k2 < ks)
    {
      goto L390;
    }
  if (kk < ks)
    {
      goto L380;
    }
  jc = k3;
  goto L440;
  /* 
   *permutation for multivariate transform 
   * 
   */
 L400:
  k = kk + jc;
 L410:
  ak = a[kk];
  a[kk] = a[k2];
  a[k2] = ak;
  bk = b[kk];
  b[kk] = b[k2];
  b[k2] = bk;
  kk += inc;
  k2 += inc;
  if (kk < k)
    {
      goto L410;
    }
  kk = kk + ks - jc;
  k2 = k2 + ks - jc;
  if (kk < nt)
    {
      goto L400;
    }
  k2 = k2 - nt + kspan;
  kk = kk - nt + jc;
  if (k2 < ks)
    {
      goto L400;
    }
 L420:
  k2 -= np[j];
  ++j;
  k2 = np[j + 1] + k2;
  if (k2 > np[j])
    {
      goto L420;
    }
  j = 1;
 L430:
  if (kk < k2)
    {
      goto L400;
    }
  kk += jc;
  k2 = kspan + k2;
  if (k2 < ks)
    {
      goto L430;
    }
  if (kk < ks)
    {
      goto L420;
    }
  jc = k3;
 L440:
  if ((*kt << 1) + 1 >= *m)
    {
      return 0;
    }
  kspnn = np[*kt + 1];
  /* 
   *permutation for square-free factors of n 
   * 
   */
  j = *m - *kt;
  nfac[j + 1] = 1;
 L450:
  nfac[j] *= nfac[j + 1];
  --j;
  if (j != *kt)
    {
      goto L450;
    }
  ++(*kt);
  nn = nfac[*kt] - 1;
  jj = 0;
  j = 0;
  goto L480;
 L460:
  jj -= k2;
  k2 = kk;
  ++k;
  kk = nfac[k];
 L470:
  jj = kk + jj;
  if (jj >= k2)
    {
      goto L460;
    }
  np[j] = jj;
 L480:
  k2 = nfac[*kt];
  k = *kt + 1;
  kk = nfac[k];
  ++j;
  if (j <= nn)
    {
      goto L470;
    }
  /* 
   *determine the permutation cycles of length greater than 1 
   * 
   */
  j = 0;
  goto L500;
 L490:
  k = kk;
  kk = np[k];
  np[k] = -kk;
  if (kk != j)
    {
      goto L490;
    }
  k3 = kk;
 L500:
  ++j;
  kk = np[j];
  if (kk < 0)
    {
      goto L500;
    }
  if (kk != j)
    {
      goto L490;
    }
  np[j] = -j;
  if (j != nn)
    {
      goto L500;
    }
  maxf = inc * maxf;
  /* 
   *reorder a and b, following the permutation cycles 
   * 
   */
  goto L570;
 L510:
  --j;
  if (np[j] < 0)
    {
      goto L510;
    }
  jj = jc;
 L520:
  kspan = jj;
  if (jj > maxf)
    {
      kspan = maxf;
    }
  jj -= kspan;
  k = np[j];
  kk = jc * k + i__ + jj;
  k1 = kk + kspan;
  k2 = 0;
 L530:
  ++k2;
  wt[k2] = a[k1];
  bt[k2] = b[k1];
  k1 -= inc;
  if (k1 != kk)
    {
      goto L530;
    }
 L540:
  k1 = kk + kspan;
  k2 = k1 - jc * (k + np[k]);
  k = -np[k];
 L550:
  a[k1] = a[k2];
  b[k1] = b[k2];
  k1 -= inc;
  k2 -= inc;
  if (k1 != kk)
    {
      goto L550;
    }
  kk = k2;
  if (k != j)
    {
      goto L540;
    }
  k1 = kk + kspan;
  k2 = 0;
 L560:
  ++k2;
  a[k1] = wt[k2];
  b[k1] = bt[k2];
  k1 -= inc;
  if (k1 != kk)
    {
      goto L560;
    }
  if (jj != 0)
    {
      goto L520;
    }
  if (j != 1)
    {
      goto L510;
    }
 L570:
  j = k3 + 1;
  nt -= kspnn;
  i__ = nt - inc + 1;
  if (nt >= 0)
    {
      goto L510;
    }
  return 0;
}				/* dfftmx_ */
