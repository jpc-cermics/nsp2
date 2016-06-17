#include "signal.h"

static int signal_bldenz (int *ndeg, int *nj, double *zfa, double *zpr,
			  double *zpi, int *nb, double *fact, double *c1,
			  double *c0);

static int signal_tranpo (int *nmaxi, int *maxdeg, int *ityp, double *vd,
			  double *sm, double *a, int *ndeg, double *sfa,
			  double *spr, double *spi, int *nj);

static int signal_trbipo (int *nmaxi, int *maxdeg, int *nj, double *sfa,
			  double *sm, double *spr, double *spi, double *zfa,
			  double *zpr, double *zpi);

static int signal_dsqrtc (const double *dx,const double *dy, double *du, double *dv);
  
/*
 * filter design  --  second section 
 */

int
signal_desib (int *nmaxi, int *maxdeg, double *vsnn, int *ndegn, int *nbn,
	      int *ityp, int *iapro, double *om, int *nh, double *adelp,
	      double *adels, double *vd, double *a, double *adelta,
	      double *pren, double *pimn, double *ugc, double *ogc,
	      double *ack, double *dk, double *dks, double *dcap02,
	      double *dcap04, double *acx, double *spr, double *spi,
	      double *zpr, double *zpi, int *nb, double *fact, double *c1,
	      double *c0, double *sm, int *ierr, int *ndeg)
{
  int sm_dim1, sm_offset;

  /* Local variables */
  double rdelp, rdels, ac;
  int nj;
  double sfa, zfa, vsn;

  /* Parameter adjustments */
  sm_dim1 = *maxdeg;
  sm_offset = sm_dim1 + 1;
  sm -= sm_offset;
  --om;
  --pren;
  --pimn;
  --spr;
  --spi;
  --zpr;
  --zpi;
  --c1;
  --c0;

  /* Function Body */
  vsn = *vsnn;
  *ndeg = *ndegn;
  nj = *nbn;
  switch (*iapro)
    {
    case 1:
      goto L10;
    case 2:
      goto L20;
    case 3:
      goto L20;
    case 4:
      goto L30;
    }
 L10:
  signal_desi21 (ndeg, adelp, adels, adelta, &pren[1], &pimn[1], ugc, ogc,
		 &nj, acx, &ac, &rdelp, &rdels, &sfa, &spr[1], &spi[1]);
  goto L40;
 L20:
  signal_desi22 (iapro, ndeg, adelp, adels, adelta, &vsn, &pren[1], &pimn[1],
		 ugc, ogc, ack, &nj, nh, acx, &ac, &rdels, &sfa, &spr[1],
		 &spi[1]);
  goto L40;
 L30:
  signal_desi24 (ndeg, adelp, adels, adelta, &pren[1], ugc, ogc, ack, &nj, nh,
		 dk, dks, dcap02, dcap04, acx, &ac, &rdelp, &rdels, &sfa,
		 &spr[1], &spi[1]);
  /* 
   */
 L40:
  signal_tranpo (nmaxi, maxdeg, ityp, vd, &sm[sm_offset], a, ndeg, &sfa,
		 &spr[1], &spi[1], &nj);
  signal_trbipo (nmaxi, maxdeg, &nj, &sfa, &sm[sm_offset], &spr[1], &spi[1],
		 &zfa, &zpr[1], &zpi[1]);
  signal_bldenz (ndeg, &nj, &zfa, &zpr[1], &zpi[1], nb, fact, &c1[1], &c0[1]);
  return 0;
}


/*!purpose 
 * reactance transformation of the poles 
 *! 
 * 
 */

static double c_b4 = 2.;

int
signal_tranpo (int *nmaxi, int *maxdeg, int *ityp, double *vd, double *sm,
	       double *a, int *ndeg, double *sfa, double *spr, double *spi,
	       int *nj)
{
  int sm_dim1, sm_offset, i__1, i__2;

  /* Local variables */
  double flma, flmi;
  int i__, j;
  double di, qa;
  int me, jj;
  double dq, dr;
  int ii;
  double qi, qh;
  int nn;
  double qn, qr;
  double qz;

  /* Parameter adjustments */
  sm_dim1 = *maxdeg;
  sm_offset = sm_dim1 + 1;
  sm -= sm_offset;
  --spr;
  --spi;

  /* Function Body */
  flmi = nsp_dlamch ("p") * 2.;
  i__1 = (int) C2F(slamch) ("l",1L) - 2;
  flma = nsp_pow_di (c_b4, i__1);
  if (*ityp == 1)
    {
      goto L90;
    }
  if (*ityp == 3)
    {
      goto L40;
    }
  i__1 = *nj;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      qr = spr[i__];
      qi = spi[i__];
      qh = qr * qr + qi * qi;
      if (Abs (qi) > flmi)
	{
	  goto L10;
	}
      *sfa = -(*sfa) / qr;
      goto L20;
    L10:
      *sfa /= qh;
    L20:
      qi /= qh;
      if (Abs (qi) < flmi)
	{
	  qi = 0.;
	}
      spi[i__] = qi;
      spr[i__] = qr / qh;
      /* L30: */
    }
  if (*ityp == 2)
    {
      goto L90;
    }
 L40:
  qa = *a * 2.;
  nn = *nj;
  *nj = *ndeg;
  *ndeg <<= 1;
  me = *nj;
  i__1 = nn;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      qr = spr[i__] / qa;
      qi = spi[i__] / qa;
      if (Abs (qi) >= flma)
	{
	  goto L70;
	}
      dr = qr;
      di = qi;
      dq = di * di;
      di = di * dr * 2.;
      dr = dr * dr - dq - 1.;
      signal_dsqrtc (&dr, &di, &dr, &di);
      qz = dr;
      qn = di;
      if (Abs (qn) > flmi)
	{
	  goto L60;
	}
      jj = *nj + me;
      i__2 = *nj;
      for (ii = me; ii <= i__2; ++ii)
	{
	  j = jj - ii;
	  spr[j + 1] = spr[j];
	  spi[j + 1] = spi[j];
	  /* L50: */
	}
      ++(*nj);
      ++me;
    L60:
      spr[i__] = qr + qz;
      spi[i__] = qi + qn;
      spr[me] = qr - qz;
      spi[me] = qn - qi;
      --me;
      goto L80;
    L70:
      spr[i__] = qr;
      spi[i__] = flma;
      ++(*nj);
      spr[*nj] = qr;
      spi[*nj] = 0.;
    L80:
      ;
    }
 L90:
  i__1 = *nj;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      spr[i__] *= *vd;
      spi[i__] *= *vd;
      /* L100: */
    }
  *sfa *= sm[*nmaxi - 1 + (sm_dim1 << 2)];
  return 0;
}


/*!purpose 
 * bilinear transformation of the poles 
 *! 
 */


/*Scilab ( http://www.scilab.org/ ) - This file is part of Scilab 
 *Copyright (C) INRIA 
 * 
 */

int
signal_trbipo (int *nmaxi, int *maxdeg, int *nj, double *sfa, double *sm,
	       double *spr, double *spi, double *zfa, double *zpr,
	       double *zpi)
{
  int sm_dim1, sm_offset, i__1;

  /* Local variables */
  double flmi;
  int i__;
  double q;
  double qi, qr, qqi, qqr;

  /* Parameter adjustments */
  sm_dim1 = *maxdeg;
  sm_offset = sm_dim1 + 1;
  sm -= sm_offset;
  --spr;
  --spi;
  --zpr;
  --zpi;

  /* Function Body */
  flmi = nsp_dlamch ("p") * 2.;
  *zfa = *sfa * sm[*nmaxi - 1 + sm_dim1];
  i__1 = *nj;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      qr = spr[i__];
      q = 1. - qr;
      qi = spi[i__];
      if (Abs (qi) < flmi)
	{
	  goto L10;
	}
      qqr = qr * qr;
      qqi = qi * qi;
      *zfa /= q - qr + qqr + qqi;
      q = 1. / (q * q + qqi);
      zpr[i__] = (1. - qqr - qqi) * q;
      zpi[i__] = qi * 2. * q;
      goto L20;
    L10:
      zpr[i__] = (qr + 1.) / q;
      zpi[i__] = 0.;
      *zfa /= q;
    L20:
      ;
    }
  return 0;
}				/* trbipo_ */


/*
 * build denominator blocks of second order  --  z-domain 
 */

int
signal_bldenz (int *ndeg, int *nj, double *zfa, double *zpr, double *zpi,
	       int *nb, double *fact, double *c1, double *c0)
{
  int i__1;
  double d__1;

  /* Local variables */
  double flmi;
  int i__, n;
  double qi, qr;

  /* Parameter adjustments */
  --c0;
  --c1;
  --zpi;
  --zpr;

  /* Function Body */
  flmi = nsp_dlamch ("p") * 2.;
  *nb = (*ndeg + 1) / 2;
  n = 0;
  *fact = *zfa;
  i__1 = *nb;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++n;
      qr = zpr[n];
      qi = zpi[n];
      if (Abs (qi) < flmi)
	{
	  goto L10;
	}
      c1[i__] = qr * -2.;
      c0[i__] = qr * qr + qi * qi;
      goto L40;
    L10:
      if (n >= *nj)
	{
	  goto L20;
	}
      if ((d__1 = zpi[n + 1], Abs (d__1)) < flmi)
	{
	  goto L30;
	}
    L20:
      c1[i__] = -qr;
      c0[i__] = 0.;
      goto L40;
    L30:
      ++n;
      qi = zpr[n];
      c1[i__] = -qr - qi;
      c0[i__] = qr * qi;
    L40:
      ;
    }
  return 0;
}

/*
 *    computation of the complex square root in double precision 
 *    subroutine dsqrtc(dx, dy, du, dv) 
 *     du + j*dv = sqrt ( dx + j*dy ) 
 */

int signal_dsqrtc (const double *dx,const double *dy, double *du, double *dv)
{
  double domi, dp, dq;
  domi = nsp_dlamch ("p") * 2.;
  dq = *dx;
  dp = *dy;
  *dv = dq * .5;
  *du = dq * dq + dp * dp;
  *du = sqrt (*du);
  *du *= .5;
  *dv = *du - *dv;
  *du = *dv + dq;
  if (Abs (*du) <= nsp_dlamch ("e") * 3.)
    {
      *du = 0.;
    }
  *du = sqrt (*du);
  if (Abs (*dv) <= nsp_dlamch ("e") * 3.)
    {
      *dv = 0.;
    }
  *dv = sqrt (*dv);
  if (dp < -domi)
    {
      *du = -(*du);
    }
  return 0;
}

