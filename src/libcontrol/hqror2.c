/* hqror2.f -- translated by f2c (version 19961017).
 *
 *
 */

/* 
 * 
 *    this subroutine is a translation of the algol procedure hqr2, 
 *    num. math. 16, 181-204(1970) by peters and wilkinson. 
 *    handbook for auto. comp., vol.ii-linear algebra, 372-395(1971). 
 * 
 *ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
 * 
 *    MODIFICATIONS WRT EISPACK VERSION 
 *    --------------------------------- 
 *      1. 1x1 and 2x2 diagonal blocks are clearly isolated by 
 *         forcing subdiagonal entries to zero 
 *      2. Merging of hqr/hqr2 driven by a job parameter 
 * 
 *ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
 * 
 *    This subroutine finds the eigenvalues of a real upper 
 *    hessenberg matrix by the qr method. In addition, the 
 *    orthogonal transformation leading to the Schur form is 
 *    accumulated 
 * 
 *    on input 
 * 
 *       nm must be set to the row dimension of two-dimensional 
 *         array parameters as declared in the calling program 
 *         dimension statement. 
 * 
 *       n is the order of the matrix. 
 * 
 *       low and igh are ints determined by the balancing 
 *         subroutine  balanc.  if  balanc  has not been used, 
 *         set low=1, igh=n. 
 * 
 *       h contains the upper hessenberg matrix. 
 * 
 *       z contains the transformation matrix produced by  eltran 
 *         after the reduction by  elmhes, or by  ortran  after the 
 *         reduction by  orthes, if performed.  if the eigenvectors 
 *         of the hessenberg matrix are desired, z must contain the 
 *         identity matrix. 
 * 
 *       job  has the decimal decomposition xy; 
 *           if x=0 hqror2 compute eigen-decomposition of h 
 *           if x=1 hqror2 computes schur decomposition of h 
 *           if x=2 eigenvalues are computed via schur decomposition 
 *           if y=0 coordinate transformation is not accumulated 
 *           if y=1 coordinate transformation is accumulated 
 * 
 * 
 *    on output 
 * 
 *       h contains the Schur form 
 * 
 *       wr and wi contain the real and imaginary parts, 
 *         respectively, of the eigenvalues.  the eigenvalues 
 *         are unordered except that complex conjugate pairs 
 *         of values appear consecutively with the eigenvalue 
 *         having the positive imaginary part first.  if an 
 *         error exit is made, the eigenvalues should be correct 
 *         for indices ierr+1,...,n. 
 * 
 *       z contains the orthogonal transformation to the real schur 
 *         form. If an error exit is made, z may be incorrect. 
 * 
 *       ierr is set to 
 *         zero       for normal return, 
 *         j          if the limit of 30*n iterations is exhausted 
 *                    while the j-th eigenvalue is being sought. 
 * 
 *    calls cdiv for complex division. 
 * 
 *    questions and comments should be directed to burton s. garbow, 
 *    mathematics and computer science div, argonne national laboratory 
 * 
 *    this version dated august 1983. 
 * 
 *    ------------------------------------------------------------------ 
 * 
 */

#include "ctrlpack.h"

int
nsp_ctrlpack_hqror2 (int *nm, int *n, int *low, int *igh, double *h__,
		     double *wr, double *wi, double *z__, int *ierr, int *job)
{

  int h_dim1, h_offset, z_dim1, z_offset, i__1, i__2, i__3;
  double d__1, d__2;

  double cres, norm;
  int i__, j, k, l=0, m=0;
  double p, q=0, r__=0, s=0, t, w, x, y, z3;
  int na, ii, en;
  double ra, sa;
  int jj, ll;
  int mm, nn;
  double machep, vi;
  int jx, jy;
  double vr, zz;
  int notlas;
  int mp2;
  double z3i, z3r;
  int itn, its, enm2;
  double tst1, tst2;

  /* Parameter adjustments */
  z_dim1 = *nm;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  --wi;
  --wr;
  h_dim1 = *nm;
  h_offset = h_dim1 + 1;
  h__ -= h_offset;

  /* Function Body */
  jx = *job / 10;
  jy = *job - jx * 10;
  /* 
   */
  machep = nsp_dlamch ("p");
  /* 
   */
  *ierr = 0;
  norm = 0.;
  k = 1;
  /*    .......... store roots isolated by balanc 
   *               and compute matrix norm .......... 
   */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       */
      i__2 = *n;
      for (j = k; j <= i__2; ++j)
	{
	  /* L40: */
	  norm += (d__1 = h__[i__ + j * h_dim1], Abs (d__1));
	}
      /* 
       */
      k = i__;
      if (jx == 1)
	{
	  goto L50;
	}
      if (i__ >= *low && i__ <= *igh)
	{
	  goto L50;
	}
      wr[i__] = h__[i__ + i__ * h_dim1];
      wi[i__] = 0.;
    L50:
      ;
    }
  /* 
   */
  en = *igh;
  t = 0.;
  itn = *n * 30;
  /*    .......... search for next eigenvalues .......... 
   */
 L60:
  if (en < *low)
    {
      goto L340;
    }
  its = 0;
  na = en - 1;
  enm2 = na - 1;
  /*    .......... look for single small sub-diagonal element 
   *               for l=en step -1 until low do -- .......... 
   */
 L70:
  i__1 = en;
  for (ll = *low; ll <= i__1; ++ll)
    {
      l = en + *low - ll;
      if (l == *low)
	{
	  goto L100;
	}
      s = (d__1 = h__[l - 1 + (l - 1) * h_dim1], Abs (d__1)) + (d__2 =
								h__[l +
								    l *
								    h_dim1],
								Abs (d__2));
      if (s == 0.)
	{
	  s = norm;
	}
      tst1 = s;
      tst2 = tst1 + (d__1 = h__[l + (l - 1) * h_dim1], Abs (d__1));
      if (tst2 == tst1)
	{
	  goto L100;
	}
      /* L80: */
    }
  /*    .......... form shift .......... 
   */
 L100:
  x = h__[en + en * h_dim1];
  if (l == en)
    {
      goto L270;
    }
  y = h__[na + na * h_dim1];
  w = h__[en + na * h_dim1] * h__[na + en * h_dim1];
  if (l == na)
    {
      goto L280;
    }
  if (itn == 0)
    {
      goto L1000;
    }
  if (its != 10 && its != 20)
    {
      goto L130;
    }
  /*    .......... form exceptional shift .......... 
   */
  t += x;
  /* 
   */
  i__1 = en;
  for (i__ = *low; i__ <= i__1; ++i__)
    {
      /* L120: */
      h__[i__ + i__ * h_dim1] -= x;
    }
  /* 
   */
  s = (d__1 = h__[en + na * h_dim1], Abs (d__1)) + (d__2 =
						    h__[na + enm2 * h_dim1],
						    Abs (d__2));
  x = s * .75;
  y = x;
  w = s * -.4375 * s;
 L130:
  ++its;
  --itn;
  /*    .......... look for two consecutive small 
   *               sub-diagonal elements. 
   *               for m=en-2 step -1 until l do -- .......... 
   */
  i__1 = enm2;
  for (mm = l; mm <= i__1; ++mm)
    {
      m = enm2 + l - mm;
      zz = h__[m + m * h_dim1];
      r__ = x - zz;
      s = y - zz;
      p = (r__ * s - w) / h__[m + 1 + m * h_dim1] + h__[m + (m + 1) * h_dim1];
      q = h__[m + 1 + (m + 1) * h_dim1] - zz - r__ - s;
      r__ = h__[m + 2 + (m + 1) * h_dim1];
      s = Abs (p) + Abs (q) + Abs (r__);
      p /= s;
      q /= s;
      r__ /= s;
      if (m == l)
	{
	  goto L150;
	}
      tst1 =
	Abs (p) * ((d__1 = h__[m - 1 + (m - 1) * h_dim1], Abs (d__1)) +
		   Abs (zz) + (d__2 =
			       h__[m + 1 + (m + 1) * h_dim1], Abs (d__2)));
      tst2 = tst1 + (d__1 =
		     h__[m + (m - 1) * h_dim1],
		     Abs (d__1)) * (Abs (q) + Abs (r__));
      if (tst2 == tst1)
	{
	  goto L150;
	}
      /* L140: */
    }
  /* 
   */
 L150:
  mp2 = m + 2;
  /* 
   */
  i__1 = en;
  for (i__ = mp2; i__ <= i__1; ++i__)
    {
      h__[i__ + (i__ - 2) * h_dim1] = 0.;
      if (i__ == mp2)
	{
	  goto L160;
	}
      h__[i__ + (i__ - 3) * h_dim1] = 0.;
    L160:
      ;
    }
  /*    .......... double qr step involving rows l to en and 
   *               columns m to en .......... 
   */
  i__1 = na;
  for (k = m; k <= i__1; ++k)
    {
      notlas = k != na;
      if (k == m)
	{
	  goto L170;
	}
      p = h__[k + (k - 1) * h_dim1];
      q = h__[k + 1 + (k - 1) * h_dim1];
      r__ = 0.;
      if (notlas)
	{
	  r__ = h__[k + 2 + (k - 1) * h_dim1];
	}
      x = Abs (p) + Abs (q) + Abs (r__);
      if (x == 0.)
	{
	  goto L260;
	}
      p /= x;
      q /= x;
      r__ /= x;
    L170:
      d__1 = sqrt (p * p + q * q + r__ * r__);
      s = d_sign (&d__1, &p);
      if (k == m)
	{
	  goto L180;
	}
      h__[k + (k - 1) * h_dim1] = -s * x;
      goto L190;
    L180:
      if (l != m)
	{
	  h__[k + (k - 1) * h_dim1] = -h__[k + (k - 1) * h_dim1];
	}
    L190:
      p += s;
      x = p / s;
      y = q / s;
      zz = r__ / s;
      q /= p;
      r__ /= p;
      if (notlas)
	{
	  goto L225;
	}
      /*    .......... row modification .......... 
       */
      i__2 = *n;
      for (j = k; j <= i__2; ++j)
	{
	  p = h__[k + j * h_dim1] + q * h__[k + 1 + j * h_dim1];
	  h__[k + j * h_dim1] -= p * x;
	  h__[k + 1 + j * h_dim1] -= p * y;
	  /* L200: */
	}
      /* 
       *Computing MIN 
       */
      i__2 = en, i__3 = k + 3;
      j = Min (i__2, i__3);
      /*    .......... column modification .......... 
       */
      i__2 = j;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  p = x * h__[i__ + k * h_dim1] + y * h__[i__ + (k + 1) * h_dim1];
	  h__[i__ + k * h_dim1] -= p;
	  h__[i__ + (k + 1) * h_dim1] -= p * q;
	  /* L210: */
	}
      if (jy == 1)
	{
	  /*    .......... accumulate transformations .......... 
	   */
	  i__2 = *igh;
	  for (i__ = *low; i__ <= i__2; ++i__)
	    {
	      p = x * z__[i__ + k * z_dim1] + y * z__[i__ + (k + 1) * z_dim1];
	      z__[i__ + k * z_dim1] -= p;
	      z__[i__ + (k + 1) * z_dim1] -= p * q;
	      /* L220: */
	    }
	}
      goto L255;
    L225:
      /*    .......... row modification .......... 
       */
      i__2 = *n;
      for (j = k; j <= i__2; ++j)
	{
	  p =
	    h__[k + j * h_dim1] + q * h__[k + 1 + j * h_dim1] + r__ * h__[k +
									  2 +
									  j *
									  h_dim1];
	  h__[k + j * h_dim1] -= p * x;
	  h__[k + 1 + j * h_dim1] -= p * y;
	  h__[k + 2 + j * h_dim1] -= p * zz;
	  /* L230: */
	}
      /* 
       *Computing MIN 
       */
      i__2 = en, i__3 = k + 3;
      j = Min (i__2, i__3);
      /*    .......... column modification .......... 
       */
      i__2 = j;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  p =
	    x * h__[i__ + k * h_dim1] + y * h__[i__ + (k + 1) * h_dim1] +
	    zz * h__[i__ + (k + 2) * h_dim1];
	  h__[i__ + k * h_dim1] -= p;
	  h__[i__ + (k + 1) * h_dim1] -= p * q;
	  h__[i__ + (k + 2) * h_dim1] -= p * r__;
	  /* L240: */
	}
      if (jy == 1)
	{
	  /*    .......... accumulate transformations .......... 
	   */
	  i__2 = *igh;
	  for (i__ = *low; i__ <= i__2; ++i__)
	    {
	      p =
		x * z__[i__ + k * z_dim1] + y * z__[i__ + (k + 1) * z_dim1] +
		zz * z__[i__ + (k + 2) * z_dim1];
	      z__[i__ + k * z_dim1] -= p;
	      z__[i__ + (k + 1) * z_dim1] -= p * q;
	      z__[i__ + (k + 2) * z_dim1] -= p * r__;
	      /* L250: */
	    }
	}
    L255:
      /* 
       */
    L260:
      ;
    }
  /* 
   */
  goto L70;
  /*    .......... one root found .......... 
   */
 L270:
  h__[en + en * h_dim1] = x + t;
  /*cccc ADDED TO MARK BLOCK SEPARATION BY HARD ZEROS 
   */
  if (en + 1 <= *n)
    {
      h__[en + 1 + en * h_dim1] = 0.;
    }
  /*ccccccccccccccccccccccccccccccccccccccccccccccccc 
   */
  if (jx != 1)
    {
      wr[en] = h__[en + en * h_dim1];
      wi[en] = 0.;
    }
  en = na;
  goto L60;
  /*    .......... two roots found .......... 
   */
 L280:
  p = (y - x) / 2.;
  q = p * p + w;
  zz = sqrt ((Abs (q)));
  h__[en + en * h_dim1] = x + t;
  x = h__[en + en * h_dim1];
  h__[na + na * h_dim1] = y + t;
  if (q < 0.)
    {
      goto L320;
    }
  /*    .......... real pair .......... 
   */
  zz = p + d_sign (&zz, &p);
  if (jx != 1)
    {
      wr[na] = x + zz;
      wr[en] = wr[na];
      if (zz != 0.)
	{
	  wr[en] = x - w / zz;
	}
      wi[na] = 0.;
      wi[en] = 0.;
    }
  x = h__[en + na * h_dim1];
  s = Abs (x) + Abs (zz);
  p = x / s;
  q = zz / s;
  r__ = sqrt (p * p + q * q);
  p /= r__;
  q /= r__;
  /*    .......... row modification .......... 
   */
  i__1 = *n;
  for (j = na; j <= i__1; ++j)
    {
      zz = h__[na + j * h_dim1];
      h__[na + j * h_dim1] = q * zz + p * h__[en + j * h_dim1];
      h__[en + j * h_dim1] = q * h__[en + j * h_dim1] - p * zz;
      /* L290: */
    }
  /*    .......... column modification .......... 
   */
  i__1 = en;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      zz = h__[i__ + na * h_dim1];
      h__[i__ + na * h_dim1] = q * zz + p * h__[i__ + en * h_dim1];
      h__[i__ + en * h_dim1] = q * h__[i__ + en * h_dim1] - p * zz;
      /* L300: */
    }
  if (jy == 1)
    {
      /*    .......... accumulate transformations .......... 
       */
      i__1 = *igh;
      for (i__ = *low; i__ <= i__1; ++i__)
	{
	  zz = z__[i__ + na * z_dim1];
	  z__[i__ + na * z_dim1] = q * zz + p * z__[i__ + en * z_dim1];
	  z__[i__ + en * z_dim1] = q * z__[i__ + en * z_dim1] - p * zz;
	  /* L310: */
	}
    }
  /*cccc ADDED TO MARK BLOCK SEPARATION BY HARD ZEROS 
   */
  h__[en + na * h_dim1] = 0.;
  if (en + 1 <= *n)
    {
      h__[en + 1 + en * h_dim1] = 0.;
    }
  /*ccccccccccccccccccccccccccccccccccccccccccccccccc 
   * 
   */
  goto L330;
  /*    .......... complex pair .......... 
   */
 L320:
  if (jx != 1)
    {
      wr[na] = x + p;
      wr[en] = x + p;
      wi[na] = zz;
      wi[en] = -zz;
    }
  /*cccc ADDED TO MARK BLOCK SEPARATION BY HARD ZEROS 
   */
  if (en + 1 <= *n)
    {
      h__[en + 1 + en * h_dim1] = 0.;
    }
  /*ccccccccccccccccccccccccccccccccccccccccccccccccc 
   */
 L330:
  en = enm2;
  goto L60;
 L340:
  if (jx != 0)
    {
      goto L1001;
    }
  if (norm == 0.)
    {
      goto L1001;
    }
  /*    :::::::::: for en=n step -1 until 1 do -- :::::::::: 
   */
  i__1 = *n;
  for (nn = 1; nn <= i__1; ++nn)
    {
      en = *n + 1 - nn;
      p = wr[en];
      q = wi[en];
      na = en - 1;
      q += 1.;
      cres = q - 1.;
      if (cres < 0.)
	{
	  goto L710;
	}
      else if (cres == 0.)
	{
	  goto L600;
	}
      else
	{
	  goto L800;
	}
      /*    :::::::::: real vector :::::::::: 
       */
    L600:
      m = en;
      h__[en + en * h_dim1] = 1.;
      if (na == 0)
	{
	  goto L800;
	}
      /*    :::::::::: for i=en-1 step -1 until 1 do -- :::::::::: 
       */
      i__2 = na;
      for (ii = 1; ii <= i__2; ++ii)
	{
	  i__ = en - ii;
	  w = h__[i__ + i__ * h_dim1] - p;
	  r__ = h__[i__ + en * h_dim1];
	  if (m > na)
	    {
	      goto L620;
	    }
	  /* 
	   */
	  i__3 = na;
	  for (j = m; j <= i__3; ++j)
	    {
	      /* L610: */
	      r__ += h__[i__ + j * h_dim1] * h__[j + en * h_dim1];
	    }
	  /* 
	   */
	L620:
	  if (wi[i__] >= 0.)
	    {
	      goto L630;
	    }
	  zz = w;
	  s = r__;
	  goto L700;
	L630:
	  m = i__;
	  if (wi[i__] != 0.)
	    {
	      goto L640;
	    }
	  t = w;
	  if (w == 0.)
	    {
	      t = machep * norm;
	    }
	  h__[i__ + en * h_dim1] = -r__ / t;
	  goto L700;
	  /*    :::::::::: solve real equations :::::::::: 
	   */
	L640:
	  x = h__[i__ + (i__ + 1) * h_dim1];
	  y = h__[i__ + 1 + i__ * h_dim1];
	  q = (wr[i__] - p) * (wr[i__] - p) + wi[i__] * wi[i__];
	  t = (x * s - zz * r__) / q;
	  h__[i__ + en * h_dim1] = t;
	  if (Abs (x) <= Abs (zz))
	    {
	      goto L650;
	    }
	  h__[i__ + 1 + en * h_dim1] = (-r__ - w * t) / x;
	  goto L700;
	L650:
	  h__[i__ + 1 + en * h_dim1] = (-s - y * t) / zz;
	L700:
	  ;
	}
      /*    :::::::::: end real vector :::::::::: 
       */
      goto L800;
      /*    :::::::::: complex vector :::::::::: 
       */
    L710:
      m = na;
      /*    :::::::::: last vector component chosen imaginary so that 
       *               eigenvector matrix is triangular :::::::::: 
       */
      if ((d__1 = h__[en + na * h_dim1], Abs (d__1)) <= (d__2 =
							 h__[na +
							     en * h_dim1],
							 Abs (d__2)))
	{
	  goto L720;
	}
      h__[na + na * h_dim1] = q / h__[en + na * h_dim1];
      h__[na + en * h_dim1] =
	-(h__[en + en * h_dim1] - p) / h__[en + na * h_dim1];
      goto L730;
    L720:
      z3r = h__[na + na * h_dim1] - p;
      z3 = z3r * z3r + q * q;
      h__[na + na * h_dim1] = -h__[na + en * h_dim1] * q / z3;
      h__[na + en * h_dim1] = -h__[na + en * h_dim1] * z3r / z3;
    L730:
      h__[en + na * h_dim1] = 0.;
      h__[en + en * h_dim1] = 1.;
      enm2 = na - 1;
      if (enm2 == 0)
	{
	  goto L800;
	}
      /*    :::::::::: for i=en-2 step -1 until 1 do -- :::::::::: 
       */
      i__2 = enm2;
      for (ii = 1; ii <= i__2; ++ii)
	{
	  i__ = na - ii;
	  w = h__[i__ + i__ * h_dim1] - p;
	  ra = 0.;
	  sa = h__[i__ + en * h_dim1];
	  /* 
	   */
	  i__3 = na;
	  for (j = m; j <= i__3; ++j)
	    {
	      ra += h__[i__ + j * h_dim1] * h__[j + na * h_dim1];
	      sa += h__[i__ + j * h_dim1] * h__[j + en * h_dim1];
	      /* L760: */
	    }
	  /* 
	   */
	  if (wi[i__] >= 0.)
	    {
	      goto L770;
	    }
	  zz = w;
	  r__ = ra;
	  s = sa;
	  goto L790;
	L770:
	  m = i__;
	  if (wi[i__] != 0.)
	    {
	      goto L780;
	    }
	  z3 = w * w + q * q;
	  z3r = -ra * w - sa * q;
	  z3i = ra * q - sa * w;
	  h__[i__ + na * h_dim1] = z3r / z3;
	  h__[i__ + en * h_dim1] = z3i / z3;
	  goto L790;
	  /*    :::::::::: solve complex equations :::::::::: 
	   */
	L780:
	  x = h__[i__ + (i__ + 1) * h_dim1];
	  y = h__[i__ + 1 + i__ * h_dim1];
	  vr = (wr[i__] - p) * (wr[i__] - p) + wi[i__] * wi[i__] - q * q;
	  vi = (wr[i__] - p) * 2. * q;
	  if (vr == 0. && vi == 0.)
	    {
	      vr =
		machep * norm * (Abs (w) + Abs (q) + Abs (x) + Abs (y) +
				 Abs (zz));
	    }
	  z3r = x * r__ - zz * ra + q * sa;
	  z3i = x * s - zz * sa - q * ra;
	  z3 = vr * vr + vi * vi;
	  h__[i__ + na * h_dim1] = (z3r * vr + z3i * vi) / z3;
	  h__[i__ + en * h_dim1] = (-z3r * vi + z3i * vr) / z3;
	  if (Abs (x) <= Abs (zz) + Abs (q))
	    {
	      goto L785;
	    }
	  h__[i__ + 1 + na * h_dim1] =
	    (-ra - w * h__[i__ + na * h_dim1] +
	     q * h__[i__ + en * h_dim1]) / x;
	  h__[i__ + 1 + en * h_dim1] =
	    (-sa - w * h__[i__ + en * h_dim1] -
	     q * h__[i__ + na * h_dim1]) / x;
	  goto L790;
	L785:
	  z3r = -r__ - y * h__[i__ + na * h_dim1];
	  z3i = -s - y * h__[i__ + en * h_dim1];
	  z3 = zz * zz + q * q;
	  h__[i__ + 1 + na * h_dim1] = (z3r * zz + z3i * q) / z3;
	  h__[i__ + 1 + en * h_dim1] = (-z3r * q + z3i * zz) / z3;
	L790:
	  ;
	}
      /*    :::::::::: end complex vector :::::::::: 
       */
    L800:
      ;
    }
  /*    :::::::::: end back substitution. 
   */
  if (jy == 0)
    {
      goto L1001;
    }
  /*               vectors of isolated roots :::::::::: 
   */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (i__ >= *low && i__ <= *igh)
	{
	  goto L840;
	}
      /* 
       */
      i__2 = *n;
      for (j = i__; j <= i__2; ++j)
	{
	  /* L820: */
	  z__[i__ + j * z_dim1] = h__[i__ + j * h_dim1];
	}
      /* 
       */
    L840:
      ;
    }
  /*    :::::::::: multiply by transformation matrix to give 
   *               vectors of original full matrix. 
   *               for j=n step -1 until low do -- :::::::::: 
   */
  i__1 = *n;
  for (jj = *low; jj <= i__1; ++jj)
    {
      j = *n + *low - jj;
      m = Min (j, *igh);
      /* 
       */
      i__2 = *igh;
      for (i__ = *low; i__ <= i__2; ++i__)
	{
	  zz = 0.;
	  /* 
	   */
	  i__3 = m;
	  for (k = *low; k <= i__3; ++k)
	    {
	      /* L860: */
	      zz += z__[i__ + k * z_dim1] * h__[k + j * h_dim1];
	    }
	  /* 
	   */
	  z__[i__ + j * z_dim1] = zz;
	  /* L880: */
	}
    }
  /* 
   */
  goto L1001;
  /*    .......... set error -- all eigenvalues have not 
   *               converged after 30*n iterations .......... 
   */
 L1000:
  *ierr = en;
 L1001:
  return 0;
}				/* hqror2_ */

int
nsp_ctrlpack_cdiv (double *ar, double *ai, double *br, double *bi, double *cr,
		   double *ci)
{
  /* System generated locals */
  double d__1, d__2;

  /* Local variables */
  double s, ais, bis, ars, brs;

  /* 
   *    complex division, (cr,ci) = (ar,ai)/(br,bi) 
   * 
   */
  s = Abs (*br) + Abs (*bi);
  ars = *ar / s;
  ais = *ai / s;
  brs = *br / s;
  bis = *bi / s;
  /*Computing 2nd power 
   */
  d__1 = brs;
  /*Computing 2nd power 
   */
  d__2 = bis;
  s = d__1 * d__1 + d__2 * d__2;
  *cr = (ars * brs + ais * bis) / s;
  *ci = (ais * brs - ars * bis) / s;
  return 0;
}				/* cdiv_ */
