/* qvalz.f -- translated by f2c (version 19961017). */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/* 
 * 
 *! purpose 
 *    this subroutine accepts a pair of real matrices, one of them 
 *    in quasi-triangular form and the other in upper triangular form. 
 *    it reduces the quasi-triangular matrix further, so that any 
 *    remaining 2-by-2 blocks correspond to pairs of complex 
 *    eigenvalues, and returns quantities whose ratios give the 
 *    generalized eigenvalues.  it is usually preceded by  qzhes 
 *    and  qzit  and may be followed by  qzvec. 
 * 
 *    MODIFIED FROM EISPACK ROUTINE ``QZVAL'' TO ALSO RETURN THE Q 
 *    MATRIX. IN ADDITION, THE TOLERANCE epsb IS DIRECTLY PASSED IN 
 *    THE CALLING LIST INSTEAD OF VIA b(n,1) 
 * 
 *! calling sequence 
 * 
 *     subroutine qvalz(nm,n,a,b,epsb,alfr,alfi,beta,matq,q,matz,z) 
 *     on input: 
 * 
 *       nm must be set to the row dimension of two-dimensional 
 *         array parameters as declared in the calling program 
 *         dimension statement; 
 * 
 *       n is the order of the matrices; 
 * 
 *       a contains a real upper quasi-triangular matrix; 
 * 
 *       b contains a real upper triangular matrix. 
 * 
 *       epsb: tolerance computed and saved in qitz (qzit) 
 * 
 *       matz (resp matq) should be set to .true. if the right 
 *       (resp left) hand transformations are to be accumulated 
 *       for later use in computing eigenvectors, and to .false. 
 *       otherwise; 
 * 
 *       z (resp q) contains, if matz (resp matq) has been set 
 *       to .true., the transformation matrix produced in the 
 *       reductions by qzhes and qzit, if performed, or else the 
 *       identity matrix. if matz has been set to .false., z is not 
 *       referenced. 
 * 
 *    on output: 
 * 
 *       a has been reduced further to a quasi-triangular matrix 
 *         in which all nonzero subdiagonal elements correspond to 
 *         pairs of complex eigenvalues; 
 * 
 *       b is still in upper triangular form, although its elements 
 *         have been altered.  b(n,1) is unaltered; 
 * 
 *       alfr and alfi contain the real and imaginary parts of the 
 *         diagonal elements of the triangular matrix that would be 
 *         obtained if a were reduced completely to triangular form 
 *         by unitary transformations.  non-zero values of alfi occur 
 *         in pairs, the first member positive and the second negative; 
 * 
 *       beta contains the diagonal elements of the corresponding b, 
 *         normalized to be real and non-negative.  the generalized 
 *         eigenvalues are then the ratios ((alfr+i*alfi)/beta); 
 * 
 *       z (resp q) contains the product of the right resp left hand 
 *         (for all three steps) if matz (resp, matq) has been set 
 *         to .true. 
 * 
 *! originator 
 * 
 *    this subroutine is the third step of the qz algorithm 
 *    for solving generalized matrix eigenvalue problems, 
 *    siam j. numer. anal. 10, 241-256(1973) by moler and stewart. 
 *    modification de la routine qzval de eispack pour avoir la matrice 
 *     q en option 
 *! 
 *    questions and comments should be directed to b. s. garbow, 
 *    applied mathematics division, argonne national laboratory 
 * 
 *    ------------------------------------------------------------------ 
 * 
 */

int
nsp_ctrlpack_qvalz (const int *nm, const int *n, double *a, double *b, const double *epsb,
		    double *alfr, double *alfi, double *beta, const int *matq,
		    double *q, const int *matz, double *z__)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, q_dim1, q_offset,
    i__1, i__2;
  double d__1, d__2, d__3, d__4;

  /* Local variables */
  double c__, d__, e = 0;
  int i__, j;
  double r__, s, t, a1, a2, u1, u2, v1, v2, a11, a12, a21, a22, b11, b12, b22,
    di, ei;
  int na;
  double an = 0, bn;
  int en;
  double cq, dr;
  int nn;
  double cz, ti, tr, a1i, a2i, a11i, a12i, a22i, a11r, a12r, a22r, sqi, ssi;
  int isw;
  double sqr, szi, ssr, szr;

  /* Parameter adjustments */
  z_dim1 = *nm;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  q_dim1 = *nm;
  q_offset = q_dim1 + 1;
  q -= q_offset;
  --beta;
  --alfi;
  --alfr;
  b_dim1 = *nm;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  a_dim1 = *nm;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  isw = 1;
  /*    :::::::::: find eigenvalues of quasi-triangular matrices. 
   *               for en=n step -1 until 1 do -- :::::::::: 
   */
  i__1 = *n;
  for (nn = 1; nn <= i__1; ++nn)
    {
      en = *n + 1 - nn;
      na = en - 1;
      if (isw == 2)
	{
	  goto L505;
	}
      if (en == 1)
	{
	  goto L410;
	}
      if (a[en + na * a_dim1] != 0.)
	{
	  goto L420;
	}
      /*    :::::::::: 1-by-1 block, one real root :::::::::: 
       */
    L410:
      alfr[en] = a[en + en * a_dim1];
      if (b[en + en * b_dim1] < 0.)
	{
	  alfr[en] = -alfr[en];
	}
      beta[en] = (d__1 = b[en + en * b_dim1], Abs (d__1));
      alfi[en] = 0.;
      goto L510;
      /*    :::::::::: 2-by-2 block :::::::::: 
       */
    L420:
      if ((d__1 = b[na + na * b_dim1], Abs (d__1)) <= *epsb)
	{
	  goto L455;
	}
      if ((d__1 = b[en + en * b_dim1], Abs (d__1)) > *epsb)
	{
	  goto L430;
	}
      a1 = a[en + en * a_dim1];
      a2 = a[en + na * a_dim1];
      bn = 0.;
      goto L435;
    L430:
      an = (d__1 = a[na + na * a_dim1], Abs (d__1)) + (d__2 =
						       a[na + en * a_dim1],
						       Abs (d__2)) + (d__3 =
								      a[en +
									na *
									a_dim1],
								      Abs
								      (d__3))
	+ (d__4 = a[en + en * a_dim1], Abs (d__4));
      bn = (d__1 = b[na + na * b_dim1], Abs (d__1)) + (d__2 =
						       b[na + en * b_dim1],
						       Abs (d__2)) + (d__3 =
								      b[en +
									en *
									b_dim1],
								      Abs
								      (d__3));
      a11 = a[na + na * a_dim1] / an;
      a12 = a[na + en * a_dim1] / an;
      a21 = a[en + na * a_dim1] / an;
      a22 = a[en + en * a_dim1] / an;
      b11 = b[na + na * b_dim1] / bn;
      b12 = b[na + en * b_dim1] / bn;
      b22 = b[en + en * b_dim1] / bn;
      e = a11 / b11;
      ei = a22 / b22;
      s = a21 / (b11 * b22);
      t = (a22 - e * b22) / b22;
      if (Abs (e) <= Abs (ei))
	{
	  goto L431;
	}
      e = ei;
      t = (a11 - e * b11) / b11;
    L431:
      c__ = (t - s * b12) * .5;
      d__ = c__ * c__ + s * (a12 - e * b12);
      if (d__ < 0.)
	{
	  goto L480;
	}
      /*    :::::::::: two real roots. 
       *               zero both a(en,na) and b(en,na) :::::::::: 
       */
      d__1 = sqrt (d__);
      e += c__ + d_sign (&d__1, &c__);
      a11 -= e * b11;
      a12 -= e * b12;
      a22 -= e * b22;
      if (Abs (a11) + Abs (a12) < Abs (a21) + Abs (a22))
	{
	  goto L432;
	}
      a1 = a12;
      a2 = a11;
      goto L435;
    L432:
      a1 = a22;
      a2 = a21;
      /*    :::::::::: choose and apply real z :::::::::: 
       */
    L435:
      s = Abs (a1) + Abs (a2);
      u1 = a1 / s;
      u2 = a2 / s;
      d__1 = sqrt (u1 * u1 + u2 * u2);
      r__ = d_sign (&d__1, &u1);
      v1 = -(u1 + r__) / r__;
      v2 = -u2 / r__;
      u2 = v2 / v1;
      /* 
       */
      i__2 = en;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  t = a[i__ + en * a_dim1] + u2 * a[i__ + na * a_dim1];
	  a[i__ + en * a_dim1] += t * v1;
	  a[i__ + na * a_dim1] += t * v2;
	  t = b[i__ + en * b_dim1] + u2 * b[i__ + na * b_dim1];
	  b[i__ + en * b_dim1] += t * v1;
	  b[i__ + na * b_dim1] += t * v2;
	  /* L440: */
	}
      /* 
       */
      if (!(*matz))
	{
	  goto L450;
	}
      /* 
       */
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  t = z__[i__ + en * z_dim1] + u2 * z__[i__ + na * z_dim1];
	  z__[i__ + en * z_dim1] += t * v1;
	  z__[i__ + na * z_dim1] += t * v2;
	  /* L445: */
	}
      /* 
       */
    L450:
      if (bn == 0.)
	{
	  goto L475;
	}
      if (an < Abs (e) * bn)
	{
	  goto L455;
	}
      a1 = b[na + na * b_dim1];
      a2 = b[en + na * b_dim1];
      goto L460;
    L455:
      a1 = a[na + na * a_dim1];
      a2 = a[en + na * a_dim1];
      /*    :::::::::: choose and apply real q :::::::::: 
       */
    L460:
      s = Abs (a1) + Abs (a2);
      if (s == 0.)
	{
	  goto L475;
	}
      u1 = a1 / s;
      u2 = a2 / s;
      d__1 = sqrt (u1 * u1 + u2 * u2);
      r__ = d_sign (&d__1, &u1);
      v1 = -(u1 + r__) / r__;
      v2 = -u2 / r__;
      u2 = v2 / v1;
      /* 
       */
      i__2 = *n;
      for (j = na; j <= i__2; ++j)
	{
	  t = a[na + j * a_dim1] + u2 * a[en + j * a_dim1];
	  a[na + j * a_dim1] += t * v1;
	  a[en + j * a_dim1] += t * v2;
	  t = b[na + j * b_dim1] + u2 * b[en + j * b_dim1];
	  b[na + j * b_dim1] += t * v1;
	  b[en + j * b_dim1] += t * v2;
	  /* L470: */
	}
      /*cccccccccccccccccccccccccccccccccccccccc 
       *    MODIFIED TO ACCUMULATE Q AS WELL 
       *cccccccccccccccccccccccccccccccccccccc 
       */
      if (!(*matq))
	{
	  goto L471;
	}
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  t = q[na + j * q_dim1] + u2 * q[en + j * q_dim1];
	  q[na + j * q_dim1] += t * v1;
	  q[en + j * q_dim1] += t * v2;
	  /* L472: */
	}
      /*ccccccccccccccccccccccccccccccccccccccc 
       */
    L471:
      /* 
       */
    L475:
      a[en + na * a_dim1] = 0.;
      b[en + na * b_dim1] = 0.;
      alfr[na] = a[na + na * a_dim1];
      alfr[en] = a[en + en * a_dim1];
      if (b[na + na * b_dim1] < 0.)
	{
	  alfr[na] = -alfr[na];
	}
      if (b[en + en * b_dim1] < 0.)
	{
	  alfr[en] = -alfr[en];
	}
      beta[na] = (d__1 = b[na + na * b_dim1], Abs (d__1));
      beta[en] = (d__1 = b[en + en * b_dim1], Abs (d__1));
      alfi[en] = 0.;
      alfi[na] = 0.;
      goto L505;
      /*    :::::::::: two complex roots :::::::::: 
       */
    L480:
      e += c__;
      ei = sqrt (-d__);
      a11r = a11 - e * b11;
      a11i = ei * b11;
      a12r = a12 - e * b12;
      a12i = ei * b12;
      a22r = a22 - e * b22;
      a22i = ei * b22;
      if (Abs (a11r) + Abs (a11i) + Abs (a12r) + Abs (a12i) <
	  Abs (a21) + Abs (a22r) + Abs (a22i))
	{
	  goto L482;
	}
      a1 = a12r;
      a1i = a12i;
      a2 = -a11r;
      a2i = -a11i;
      goto L485;
    L482:
      a1 = a22r;
      a1i = a22i;
      a2 = -a21;
      a2i = 0.;
      /*    :::::::::: choose complex z :::::::::: 
       */
    L485:
      cz = sqrt (a1 * a1 + a1i * a1i);
      if (cz == 0.)
	{
	  goto L487;
	}
      szr = (a1 * a2 + a1i * a2i) / cz;
      szi = (a1 * a2i - a1i * a2) / cz;
      r__ = sqrt (cz * cz + szr * szr + szi * szi);
      cz /= r__;
      szr /= r__;
      szi /= r__;
      goto L490;
    L487:
      szr = 1.;
      szi = 0.;
    L490:
      if (an < (Abs (e) + ei) * bn)
	{
	  goto L492;
	}
      a1 = cz * b11 + szr * b12;
      a1i = szi * b12;
      a2 = szr * b22;
      a2i = szi * b22;
      goto L495;
    L492:
      a1 = cz * a11 + szr * a12;
      a1i = szi * a12;
      a2 = cz * a21 + szr * a22;
      a2i = szi * a22;
      /*    :::::::::: choose complex q :::::::::: 
       */
    L495:
      cq = sqrt (a1 * a1 + a1i * a1i);
      if (cq == 0.)
	{
	  goto L497;
	}
      sqr = (a1 * a2 + a1i * a2i) / cq;
      sqi = (a1 * a2i - a1i * a2) / cq;
      r__ = sqrt (cq * cq + sqr * sqr + sqi * sqi);
      cq /= r__;
      sqr /= r__;
      sqi /= r__;
      goto L500;
    L497:
      sqr = 1.;
      sqi = 0.;
      /*    :::::::::: compute diagonal elements that would result 
       *               if transformations were applied :::::::::: 
       */
    L500:
      ssr = sqr * szr + sqi * szi;
      ssi = sqr * szi - sqi * szr;
      i__ = 1;
      tr = cq * cz * a11 + cq * szr * a12 + sqr * cz * a21 + ssr * a22;
      ti = cq * szi * a12 - sqi * cz * a21 + ssi * a22;
      dr = cq * cz * b11 + cq * szr * b12 + ssr * b22;
      di = cq * szi * b12 + ssi * b22;
      goto L503;
    L502:
      i__ = 2;
      tr = ssr * a11 - sqr * cz * a12 - cq * szr * a21 + cq * cz * a22;
      ti = -ssi * a11 - sqi * cz * a12 + cq * szi * a21;
      dr = ssr * b11 - sqr * cz * b12 + cq * cz * b22;
      di = -ssi * b11 - sqi * cz * b12;
    L503:
      t = ti * dr - tr * di;
      j = na;
      if (t < 0.)
	{
	  j = en;
	}
      r__ = sqrt (dr * dr + di * di);
      beta[j] = bn * r__;
      alfr[j] = an * (tr * dr + ti * di) / r__;
      alfi[j] = an * t / r__;
      if (i__ == 1)
	{
	  goto L502;
	}
    L505:
      isw = 3 - isw;
    L510:
      ;
    }
  /* 
   */
  return 0;
  /*    :::::::::: last card of qzval :::::::::: 
   */
}				/* qvalz_ */
