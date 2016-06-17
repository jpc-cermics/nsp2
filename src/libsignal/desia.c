#include "signal.h"

static int
signal_trbize (int *nmaxi, int *maxdeg, int *nzm, int *nzero, double *sm,
	       double *zm, double *zzr, double *zzi);

static int
signal_blnumz (int *nzm, int *nzero, double *zzr, double *zzi, double *b2,
	       double *b1, double *b0, int *nze);

static int
signal_tranze (int *nmaxi, int *maxdeg, int *ityp, int *ndeg, int *nzm,
	       double *a, double *vd, double *sm, int *nzero);

/*
 *filter design -- first section 
 */

int
signal_desia (int *nmaxi, int *maxdeg, int *ityp, int *iapro, double *om,
	      int *norma, double *edeg, int *ndeg, double *adelp,
	      double *adels, int *nbn, int *nzero, int *nzm, double *vsn,
	      double *a, double *adelta, double *adeg, double *sm,
	      double *pren, double *pimn, double *ugc, double *ogc,
	      double *ack, double *zm, double *zzr, double *zzi, double *rom,
	      double *b2, double *b1, double *b0, double *dk, double *dks,
	      double *dcap02, double *dcap04, double *vsnn, int *ndegn,
	      int *nh, double *vd, int *nze, int *ierr)
{
  int sm_dim1, sm_offset, zm_dim1, zm_offset;

  /* Local variables */
  double acap12;
  int nj;
  double gd1, gd2;

  /* Parameter adjustments */
  zm_dim1 = *maxdeg;
  zm_offset = zm_dim1 + 1;
  zm -= zm_offset;
  sm_dim1 = *maxdeg;
  sm_offset = sm_dim1 + 1;
  sm -= sm_offset;
  --om;
  --nzero;
  --nzm;
  --pren;
  --pimn;
  --zzr;
  --zzi;
  --rom;
  --b2;
  --b1;
  --b0;
  --nze;

  /* Function Body */

  signal_desi00 (ityp, &om[1], norma, edeg, ndeg, adeg, vsn, vd, a);
  signal_desi01 (maxdeg, iapro, edeg, ndeg, adelp, adels, vsn, adeg, &gd1,
		 &gd2, &acap12, ierr);
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
  signal_desi11 (nmaxi, maxdeg, vsn, ndeg, &gd1, &gd2, adelta, &nzm[1],
		 &sm[sm_offset], &nzero[1], &pren[1], &pimn[1], ugc, ogc, &nj,
		 nh);
  goto L40;
 L20:
  signal_desi12 (nmaxi, maxdeg, iapro, ndeg, vsn, &gd1, &gd2, adelta, &nzm[1],
		 &sm[sm_offset], &nzero[1], &pren[1], &pimn[1], ugc, ack, &nj,
		 nh);
  goto L40;
 L30:
  signal_desi14 (nmaxi, maxdeg, ndeg, vsn, &gd1, &gd2, adelta, &nzm[1],
		 &sm[sm_offset], &nzero[1], &pren[1], ugc, ogc, ack, &nj, nh,
		 dk, dks, dcap02, dcap04);
  /* 
   */
 L40:
  *vsnn = *vsn;
  *ndegn = *ndeg;
  *nbn = nj;
  signal_tranze (nmaxi, maxdeg, ityp, ndeg, &nzm[1], a, vd, &sm[sm_offset],
		 &nzero[1]);
  signal_trbize (nmaxi, maxdeg, &nzm[1], &nzero[1], &sm[sm_offset],
		 &zm[zm_offset], &zzr[1], &zzi[1]);
  signal_blnumz (&nzm[1], &nzero[1], &zzr[1], &zzi[1], &b2[1], &b1[1], &b0[1],
		 &nze[1]);
  signal_romeg (nmaxi, maxdeg, ityp, &nzm[1], &zm[zm_offset], &rom[1]);
  return 0;
}

/*
 *bilinear transformation of the zeros and the locations of the extrema 
 */

static double c_b3 = 2.;

static int
signal_trbize (int *nmaxi, int *maxdeg, int *nzm, int *nzero, double *sm,
	       double *zm, double *zzr, double *zzi)
{
  int sm_dim1, sm_offset, zm_dim1, zm_offset, i__1;

  /* Local variables */
  double flma, flmi;
  int i__, j;
  double q, fa;
  int me;
  double qi;
  int nu;
  double qqi;

  /* Parameter adjustments */
  zm_dim1 = *maxdeg;
  zm_offset = zm_dim1 + 1;
  zm -= zm_offset;
  sm_dim1 = *maxdeg;
  sm_offset = sm_dim1 + 1;
  sm -= sm_offset;
  --nzm;
  --nzero;
  --zzr;
  --zzi;

  /* Function Body */
  i__1 = (int) C2F(slamch) ("l", 1L) - 2;
  flma = nsp_pow_di (c_b3, i__1);
  flmi = nsp_dlamch ("p") * 2.;
  fa = 1.;
  for (j = 1; j <= 4; ++j)
    {
      me = nzm[j];
      i__1 = me;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  qi = sm[i__ + j * sm_dim1];
	  zm[i__ + j * zm_dim1] = atan (qi) * 2.;
	  if (j != 4)
	    {
	      goto L40;
	    }
	  if (qi >= flma)
	    {
	      goto L10;
	    }
	  if (qi < flmi)
	    {
	      goto L20;
	    }
	  qqi = qi * qi;
	  q = qqi + 1.;
	  zzr[i__] = (1. - qqi) / q;
	  zzi[i__] = qi * 2. / q;
	  nu = nzero[i__] / 2;
	  fa *= nsp_pow_di (q, nu);
	  goto L40;
	  /* 
	   */
	L10:
	  zzr[i__] = -1.;
	  goto L30;
	  /* 
	   */
	L20:
	  zzr[i__] = 1.;
	L30:
	  zzi[i__] = 0.;
	L40:
	  ;
	}
      /* L50: */
    }
  /* 
   */
  sm[*nmaxi - 1 + sm_dim1] = fa;
  return 0;
}				/* trbize_ */


/*
 *reactance transformation of the zeros and the locations of the 
 *extrema 
 */

static int
signal_tranze (int *nmaxi, int *maxdeg, int *ityp, int *ndeg, int *nzm,
	       double *a, double *vd, double *sm, int *nzero)
{
  int sm_dim1, sm_offset, i__1;
  double d__1;


  /* Local variables */
  double flma, flmi;
  int i__, j;
  double q, s, fa;
  int ma, me, ii;
  double qa, dr;
  double qi;
  int nn;
  double qr;
  int nu;
  double dqi;
  int msm[4];

  /* Parameter adjustments */
  sm_dim1 = *maxdeg;
  sm_offset = sm_dim1 + 1;
  sm -= sm_offset;
  --nzm;
  --nzero;

  /* Function Body */
  i__1 = (int) C2F(slamch) ("l",1L) - 2;
  flma = nsp_pow_di (c_b3, i__1);
  flmi = nsp_dlamch ("p") * 2.;
  fa = 1.;
  if (*ityp == 1)
    {
      goto L190;
    }
  if (*ityp == 3)
    {
      goto L60;
    }
  /* 
   */
  me = nzm[4];
  i__1 = me;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      q = sm[i__ + (sm_dim1 << 2)];
      if (q < flma)
	{
	  fa *= q;
	}
      /* L10: */
    }
  /* 
   */
  fa *= fa;
  /* 
   *lowpass - highpass 
   * 
   */
  for (j = 1; j <= 4; ++j)
    {
      me = nzm[j];
      i__1 = me;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  qi = sm[i__ + j * sm_dim1];
	  if (Abs (qi) < flmi)
	    {
	      goto L20;
	    }
	  qi = 1. / qi;
	  goto L30;
	L20:
	  qi = flma;
	L30:
	  sm[i__ + j * sm_dim1] = qi;
	  /* L40: */
	}
      /* L50: */
    }
  goto L90;
 L60:
  for (j = 1; j <= 2; ++j)
    {
      me = nzm[j];
      ma = me + 1;
      me /= 2;
      i__1 = me;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  qi = sm[i__ + j * sm_dim1];
	  ii = ma - i__;
	  sm[i__ + j * sm_dim1] = sm[ii + j * sm_dim1];
	  sm[ii + j * sm_dim1] = qi;
	  /* L70: */
	}
      /* L80: */
    }
  /* 
   */
 L90:
  if (*ityp == 2)
    {
      goto L190;
    }
  /* 
   *lowpass - bandpass transformation 
   * 
   */
  qa = *a * 2.;
  nn = *ndeg + 1;
  if (*ityp == 4)
    {
      goto L110;
    }
  /* 
   */
  msm[0] = 1;
  if (nzm[1] != 1)
    {
      msm[0] = *ndeg;
    }
  msm[1] = 2;
  if (nzm[2] != 1)
    {
      msm[1] = nn;
    }
  for (j = 3; j <= 4; ++j)
    {
      msm[j - 1] = nzm[j] << 1;
      /* L100: */
    }
  goto L130;
  /* 
   */
 L110:
  for (j = 1; j <= 2; ++j)
    {
      msm[j - 1] = nzm[j] << 1;
      /* L120: */
    }
  msm[2] = 2;
  if (nzm[3] != 1)
    {
      msm[2] = nn;
    }
  msm[3] = 1;
  if (nzm[4] != 1)
    {
      msm[3] = *ndeg;
    }
  /* 
   */
 L130:
  s = 1.;
  for (j = 1; j <= 4; ++j)
    {
      me = nzm[j];
      ma = msm[j - 1];
      nzm[j] = ma;
      if (j == 3)
	{
	  s = -1.;
	}
      i__1 = me;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  qr = sm[i__ + j * sm_dim1];
	  nu = nzero[i__];
	  if (Abs (qr) < flma)
	    {
	      goto L150;
	    }
	  if (j != 4)
	    {
	      goto L140;
	    }
	  d__1 = *vd / *a;
	  fa *= nsp_pow_di (d__1, nu);
	L140:
	  qi = qr;
	  goto L160;
	  /* 
	   */
	L150:
	  qr /= qa;
	  dr = qr;
	  dqi = sqrt (dr * dr + 1.);
	  qi = dqi;
	L160:
	  sm[i__ + j * sm_dim1] = qi - s * qr;
	  ii = ma - i__ + 1;
	  if (Abs (qr) < flmi)
	    {
	      nu <<= 1;
	    }
	  if (j == 4)
	    {
	      nzero[ii] = nu;
	    }
	  sm[ii + j * sm_dim1] = qi + s * qr;
	  /* L170: */
	}
      /* L180: */
    }
  /* 
   */
 L190:
  for (j = 1; j <= 4; ++j)
    {
      me = nzm[j];
      i__1 = me;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  q = sm[i__ + j * sm_dim1];
	  if (q < flma)
	    {
	      goto L200;
	    }
	  if (j != 4 || *ityp >= 3)
	    {
	      goto L210;
	    }
	  nu = nzero[i__];
	  fa *= nsp_pow_di (*vd, nu);
	  goto L210;
	L200:
	  sm[i__ + j * sm_dim1] = q * *vd;
	L210:
	  ;
	}
      /* L220: */
    }
  sm[*nmaxi - 1 + (sm_dim1 << 2)] *= fa;
  return 0;
}				/* tranze_ */

/*
 *build numerator blocks of second order 
 */
 
static int
signal_blnumz (int *nzm, int *nzero, double *zzr, double *zzi, double *b2,
	       double *b1, double *b0, int *nze)
{
  int i__1, i__2;
  int i__, n, ma, me, ii;
  double qr;
  int nz;
  double qrr;

  --nze;
  --b0;
  --b1;
  --b2;
  --zzi;
  --zzr;
  --nzero;
  --nzm;
 
  n = 0;
  me = nzm[4];
  i__1 = me;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      nze[i__] = nzero[i__];
    }
  /* 
   */
  i__1 = me;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      qr = zzr[i__];
      nz = nze[i__];
      /* 
       */
    L20:
      if (nz == 0)
	{
	  goto L70;
	}
      ++n;
      b2[n] = 1.;
      if (nz == 1)
	{
	  goto L30;
	}
      b1[n] = qr * -2.;
      b0[n] = 1.;
      nz += -2;
      if (nz > 0)
	{
	  goto L20;
	}
      goto L70;
      /* 
       */
    L30:
      if (i__ == me)
	{
	  goto L60;
	}
      ma = i__ + 1;
      i__2 = me;
      for (ii = ma; ii <= i__2; ++ii)
	{
	  if (zzi[ii] == 0.)
	    {
	      goto L50;
	    }
	  /* L40: */
	}
      goto L60;
      /* 
       */
    L50:
      qrr = zzr[ii];
      b1[n] = -qr - qrr;
      b0[n] = qr * qrr;
      --nze[ii];
      goto L70;
      /* 
       */
    L60:
      b1[n] = -qr;
      b0[n] = 0.;
      /* 
       */
    L70:
      ;
    }
  return 0;
}

