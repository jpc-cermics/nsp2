/* cffti1.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "fftpack.h"

/* Subroutine */ int
fftpack_cffti1 (int *n, double *wa, int *ifac)
{
  /* Initialized data */

  static int ntryh[4] = { 3, 4, 2, 5 };

  /* System generated locals */
  int i__1, i__2, i__3;

  /* Builtin functions */
  double cos (double), sin (double);

  /* Local variables */
  double argh;
  int idot, ntry, i__, j;
  double argld;
  int i1, k1, l1, l2, ib;
  double fi;
  int ld, ii, nf, ip, nl, nq, nr;
  double arg;
  int ido, ipm;
  double tpi;

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
  tpi = 6.28318530717958647692;
  argh = tpi / (double) (*n);
  i__ = 2;
  l1 = 1;
  i__1 = nf;
  for (k1 = 1; k1 <= i__1; ++k1)
    {
      ip = ifac[k1 + 2];
      ld = 0;
      l2 = l1 * ip;
      ido = *n / l2;
      idot = ido + ido + 2;
      ipm = ip - 1;
      i__2 = ipm;
      for (j = 1; j <= i__2; ++j)
	{
	  i1 = i__;
	  wa[i__ - 1] = 1.;
	  wa[i__] = 0.;
	  ld += l1;
	  fi = 0.;
	  argld = (double) ld *argh;
	  i__3 = idot;
	  for (ii = 4; ii <= i__3; ii += 2)
	    {
	      i__ += 2;
	      fi += 1.;
	      arg = fi * argld;
	      wa[i__ - 1] = cos (arg);
	      wa[i__] = sin (arg);
	      /* L108: */
	    }
	  if (ip <= 5)
	    {
	      goto L109;
	    }
	  wa[i1 - 1] = wa[i__ - 1];
	  wa[i1] = wa[i__];
	L109:
	  ;
	}
      l1 = l2;
      /* L110: */
    }
  return 0;
}				/* cffti1_ */
