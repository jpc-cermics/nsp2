#include "signal.h"

/*!purpose 
 * elliptic filter 
 *computation of the reduced tolerance scheme, the factor sfa, and 
 * the poles 
 *! 
 */




static double c_b5 = .33333333333333331;

/* Scilab ( http://www.scilab.org/ ) - This file is part of Scilab 
 * Copyright (C) INRIA 
 */

int
signal_desi24 (int *ndeg, double *adelp, double *adels, double *adelta,
	       double *dsk, double *ugc, double *ogc, double *ack, int *nj,
	       int *nh, double *dk, double *dks, double *dcap02,
	       double *dcap04, double *acx, double *ac, double *rdelp,
	       double *rdels, double *sfa, double *spr, double *spi)
{
  /* Initialized data */

  static double de = 1.;

  int i__1;
  double d__1, d__2;

  /* Local variables */
  double flmi;
  int i__;
  double q;
  double dm, dq, dr;
  double du, drc, dpi, dud, duc, drd;

  /* Parameter adjustments */
  --spi;
  --spr;
  --dsk;

  /* Function Body */
  flmi = nsp_dlamch ("p") * 2.;
  dpi = atan (1.) * 4.;
  /* if acx not defined, compute a symmetrical usage of the tolerance 
   * scheme 
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
  /* computation of the reduced tolerance scheme 
   */
 L30:
  q = *ac * *adelta;
  du = de / q;
  *rdelp = 1. - sqrt (1. / (q * q + 1.));
  q = *ac * *ac / (*adelta * *adelta) + 1.;
  *rdels = sqrt (1. / q);
  /* computation of the factor sfa and the poles 
   */
  q = *ac * *ack;
  if (*nh == *nj)
    {
      q = sqrt (q * q + 1.);
    }
  *sfa = 1. / q;
  dr = *adelta;
  dr *= dr;
  dq = q;
  signal_deli11 (&du, &dr, &dq);
  du = dq;
  dq = sqrt (de - dr * dr);
  dq = signal_dellk (&dr);
  du = *dk * du / (dq * (double) (*ndeg));
  dq = exp (-dpi * *dk / *dks);
  du = -signal_dsn2 (&du, dks, &dq);
  dq = du * du;
  dud = de - *dcap04 * *dcap04 * dq;
  dud = sqrt (dud);
  duc = sqrt (de - dq);
  i__1 = *nj;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      dr = dsk[i__];
      drc = dr * dr;
      drd = de - *dcap02 * *dcap02 * drc;
      drc = sqrt (de - drc);
      dm = de - dq * drd;
      drd = sqrt (drd);
      drd = drd * du * duc * drc / dm;
      spr[i__] = drd;
      dr = dr * dud / dm;
      spi[i__] = dr;
      /* L40: */
    }
  return 0;
}				/* desi24_ */
