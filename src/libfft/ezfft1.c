/* ezfft1.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "fftpack.h"

/* Subroutine */ int
fftpack_ezfft1 (int *n, double *wa, int *ifac)
{
  /* Initialized data */

  static int ntryh[4] = { 4, 2, 3, 5 };
  static double tpi = 6.28318530717958647692;

  /* System generated locals */
  int i__1, i__2, i__3;

  /* Builtin functions */
  double cos (double), sin (double);

  /* Local variables */
  double argh;
  int ntry, i__, j, k1, l1, l2, ib, ii, nf, ip, nl, is, nq, nr;
  double ch1, sh1;
  int ido, ipm;
  double dch1, ch1h, arg1, dsh1;
  int nfm1;

  /* Parameter adjustments */
  --ifac;
  --wa;

  /* Function Body */
  nl = *n;
  nf = 0;
  j = 0;
  ntry = 1;
 L101:
  ++j;
  if (j - 4 <= 0)
    {
      goto L102;
    }
  else
    {
      goto L103;
    }
 L102:
  ntry = ntryh[j - 1];
  goto L104;
 L103:
  ntry += 2;
 L104:
  nq = nl / ntry;
  nr = nl - ntry * nq;
  if (nr != 0)
    {
      goto L101;
    }
  else
    {
      goto L105;
    }
 L105:
  ++nf;
  ifac[nf + 2] = ntry;
  nl = nq;
  if (ntry != 2)
    {
      goto L107;
    }
  if (nf == 1)
    {
      goto L107;
    }
  i__1 = nf;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      ib = nf - i__ + 2;
      ifac[ib + 2] = ifac[ib + 1];
      /* L106: */
    }
  ifac[3] = 2;
 L107:
  if (nl != 1)
    {
      goto L104;
    }
  ifac[1] = *n;
  ifac[2] = nf;
  argh = tpi / (double) (*n);
  is = 0;
  nfm1 = nf - 1;
  l1 = 1;
  if (nfm1 == 0)
    {
      return 0;
    }
  i__1 = nfm1;
  for (k1 = 1; k1 <= i__1; ++k1)
    {
      ip = ifac[k1 + 2];
      l2 = l1 * ip;
      ido = *n / l2;
      ipm = ip - 1;
      arg1 = (double) l1 *argh;
      ch1 = 1.;
      sh1 = 0.;
      dch1 = cos (arg1);
      dsh1 = sin (arg1);
      i__2 = ipm;
      for (j = 1; j <= i__2; ++j)
	{
	  ch1h = dch1 * ch1 - dsh1 * sh1;
	  sh1 = dch1 * sh1 + dsh1 * ch1;
	  ch1 = ch1h;
	  i__ = is + 2;
	  wa[i__ - 1] = ch1;
	  wa[i__] = sh1;
	  if (ido < 5)
	    {
	      goto L109;
	    }
	  i__3 = ido;
	  for (ii = 5; ii <= i__3; ii += 2)
	    {
	      i__ += 2;
	      wa[i__ - 1] = ch1 * wa[i__ - 3] - sh1 * wa[i__ - 2];
	      wa[i__] = ch1 * wa[i__ - 2] + sh1 * wa[i__ - 3];
	      /* L108: */
	    }
	L109:
	  is += ido;
	  /* L110: */
	}
      l1 = l2;
      /* L111: */
    }
  return 0;
}				/* ezfft1_ */
