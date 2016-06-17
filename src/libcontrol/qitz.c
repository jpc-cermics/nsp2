/* qitz.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"
/* 
 * 
 * 
 *    this subroutine is the second step of the qz algorithm 
 *    for solving generalized matrix eigenvalue problems, 
 *    siam j. numer. anal. 10, 241-256(1973) by moler and stewart, 
 *    as modified in technical note nasa tn d-7305(1973) by ward. 
 * 
 *! purpose 
 *    this subroutine accepts a pair of real matrices, one of them 
 *    in upper hessenberg form and the other in upper triangular form. 
 *    it reduces the hessenberg matrix to quasi-triangular form using 
 *    orthogonal transformations while maintaining the triangular form 
 *    of the other matrix.  it is usually preceded by  qhesz  and 
 *    followed by  qvalz  and, possibly,  qvecz. 
 * 
 *    MODIFIED FROM EISPACK ROUTINE ``QZIT'' TO ALSO RETURN THE Q 
 *    MATRIX. 
 * 
 *! calling sequence 
 *    subroutine qitz(nm,n,a,b,eps1,matq,q,matz,z,ierr) 
 *    double precision a(nm,n),b(nm,n),z(nm,n),q(nm,n),eps1 
 *    int matz,matq 
 *    int nm,n,ierr 
 * 
 *       nm must be set to the row dimension of two-dimensional 
 *         array parameters as declared in the calling program 
 *         dimension statement; 
 * 
 *       n is the order of the matrices; 
 * 
 *       a contains a real upper hessenberg matrix; 
 * 
 *       b contains a real upper triangular matrix; 
 * 
 *       eps1 is a tolerance used to determine negligible elements. 
 *         eps1 = 0.0 (or negative) may be input, in which case an 
 *         element will be neglected only if it is less than roundoff 
 *         error times the norm of its matrix.  if the input eps1 is 
 *         positive, then an element will be considered negligible 
 *         if it is less than eps1 times the norm of its matrix.  a 
 *         positive value of eps1 may result in faster execution, 
 *         but less accurate results; 
 *         en sortie eps1 vaut eps1*(norme de b),utilise par qzval 
 *         et qzvec 
 * 
 *       matz should be set to .true. if the right hand transformations 
 *         are to be accumulated for later use in computing 
 *         eigenvectors, and to .false. otherwise; 
 * 
 *       z contains, if matz has been set to .true., the 
 *         transformation matrix produced in the reduction 
 *         by  qzhes, if performed, or else the identity matrix. 
 *         if matz has been set to .false., z is not referenced. 
 * 
 *       matq should be set to .true. if left hand transformation is 
 *         required, and to .false. otherwise 
 * 
 *        q contains, if the left hand transformation is required, 
 *          the transformation matrix produced by qhesz. 
 * 
 *    on output: 
 * 
 *       a has been reduced to quasi-triangular form.  the elements 
 *         below the first subdiagonal are still zero and no two 
 *         consecutive subdiagonal elements are nonzero; 
 * 
 *       b is still in upper triangular form, although its elements 
 *         have been altered. 
 * 
 *       z contains the product of the right hand transformations 
 *         (for both steps) if matz has been set to .true.; 
 * 
 *       q contains the product of the right hand transformation with 
 *         initial q 
 * 
 *       ierr is set to 
 *         zero       for normal return, 
 *         j          if neither a(j,j-1) nor a(j-1,j-2) has become 
 *                    zero after 30*n iterations. 
 * 
 *! originator 
 * 
 *    F Delebecque  INRIA 
 * 
 *    This subroutine is a modification of qzit (eispack). 
 *    Modifications concern computation of the left vector space q, and 
 *    treatment of upper left 2 x 2 block of a to make sure it is really 
 *    in relation with complex eigenvalues. 
 * 
 *    this version dated august 1983. 
 *c! 
 * 
 */

int
nsp_ctrlpack_qitz (const int *nm, const int *n, double *a, double *b, double *eps1,
		   const int *matq, double *q,const int *matz, double *z__, int *ierr)
{
  int a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, q_dim1, q_offset,
    i__1, i__2, i__3;
  double d__1, d__2, d__3;

  double epsa, epsb;
  int i__, j, k, l;
  double r__, s, t, anorm, bnorm;
  int enorn;
  double a1, a2, a3 = 0;
  int k1, k2, l1;
  double u1, u2, u3, v1, v2, v3, a11, a12, a21, a22, a33, a34, a43, a44, b11,
    b12, b22, b33;
  int na, ld;
  double b34, b44;
  int en;
  double ep;
  int ll;
  double sh;
  int notlas;
  int km1, lm1;
  double ani, bni;
  int ish, itn, its, enm2, lor1;

  /* Parameter adjustments */
  z_dim1 = *nm;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  q_dim1 = *nm;
  q_offset = q_dim1 + 1;
  q -= q_offset;
  b_dim1 = *nm;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  a_dim1 = *nm;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  *ierr = 0;
  /*    :::::::::: compute epsa,epsb :::::::::: 
   */
  anorm = 0.;
  bnorm = 0.;
  /* 
   */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ani = 0.;
      if (i__ != 1)
	{
	  ani = (d__1 = a[i__ + (i__ - 1) * a_dim1], Abs (d__1));
	}
      bni = 0.;
      /* 
       */
      i__2 = *n;
      for (j = i__; j <= i__2; ++j)
	{
	  ani += (d__1 = a[i__ + j * a_dim1], Abs (d__1));
	  bni += (d__1 = b[i__ + j * b_dim1], Abs (d__1));
	  /* L20: */
	}
      /* 
       */
      if (ani > anorm)
	{
	  anorm = ani;
	}
      if (bni > bnorm)
	{
	  bnorm = bni;
	}
      /* L30: */
    }
  /* 
   */
  if (anorm == 0.)
    {
      anorm = 1.;
    }
  if (bnorm == 0.)
    {
      bnorm = 1.;
    }
  ep = *eps1;
  if (ep > 0.)
    {
      goto L50;
    }
  /*    .......... use roundoff level if eps1 is zero .......... 
   */
  ep = nsp_dlamch ("p");
 L50:
  epsa = ep * anorm;
  epsb = ep * bnorm;
  /*    :::::::::: reduce a to quasi-triangular form, while 
   *               keeping b triangular :::::::::: 
   */
  lor1 = 1;
  enorn = *n;
  en = *n;
  itn = *n * 30;
  /*    :::::::::: begin qz step :::::::::: 
   */
 L60:
  if (en <= 1)
    {
      goto L1001;
    }
  if (!(*matz))
    {
      enorn = en;
    }
  its = 0;
  na = en - 1;
  enm2 = na - 1;
 L70:
  ish = 2;
  /*    :::::::::: check for convergence or reducibility. 
   *               for l=en step -1 until 1 do -- :::::::::: 
   */
  i__1 = en;
  for (ll = 1; ll <= i__1; ++ll)
    {
      lm1 = en - ll;
      l = lm1 + 1;
      if (l == 1)
	{
	  goto L95;
	}
      if ((d__1 = a[l + lm1 * a_dim1], Abs (d__1)) <= epsa)
	{
	  goto L90;
	}
      /* L80: */
    }
  /* 
   */
 L90:
  a[l + lm1 * a_dim1] = 0.;
  if (l < na)
    {
      goto L95;
    }
  /*    :::::::::: 1-by-1 or 2-by-2 block isolated :::::::::: 
   */
  en = lm1;
  goto L60;
  /*    :::::::::: check for small top of b :::::::::: 
   */
 L95:
  ld = l;
 L100:
  l1 = l + 1;
  b11 = b[l + l * b_dim1];
  if (Abs (b11) > epsb)
    {
      goto L120;
    }
  b[l + l * b_dim1] = 0.;
  s = (d__1 = a[l + l * a_dim1], Abs (d__1)) + (d__2 =
						a[l1 + l * a_dim1],
						Abs (d__2));
  u1 = a[l + l * a_dim1] / s;
  u2 = a[l1 + l * a_dim1] / s;
  d__1 = sqrt (u1 * u1 + u2 * u2);
  r__ = d_sign (&d__1, &u1);
  v1 = -(u1 + r__) / r__;
  v2 = -u2 / r__;
  u2 = v2 / v1;
  /* 
   */
  i__1 = enorn;
  for (j = l; j <= i__1; ++j)
    {
      t = a[l + j * a_dim1] + u2 * a[l1 + j * a_dim1];
      a[l + j * a_dim1] += t * v1;
      a[l1 + j * a_dim1] += t * v2;
      t = b[l + j * b_dim1] + u2 * b[l1 + j * b_dim1];
      b[l + j * b_dim1] += t * v1;
      b[l1 + j * b_dim1] += t * v2;
      /* L110: */
    }
  if (!(*matq))
    {
      goto L111;
    }
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      t = q[l + j * q_dim1] + u2 * q[l1 + j * q_dim1];
      q[l + j * q_dim1] += t * v1;
      q[l1 + j * q_dim1] += t * v2;
      /* L112: */
    }
 L111:
  /* 
   */
  if (l != 1)
    {
      a[l + lm1 * a_dim1] = -a[l + lm1 * a_dim1];
    }
  lm1 = l;
  l = l1;
  goto L90;
 L120:
  a11 = a[l + l * a_dim1] / b11;
  a21 = a[l1 + l * a_dim1] / b11;
  if (ish == 1)
    {
      goto L140;
    }
  /*    :::::::::: iteration strategy :::::::::: 
   */
  if (itn == 0)
    {
      goto L1000;
    }
  if (its == 10)
    {
      goto L155;
    }
  /*    :::::::::: determine type of shift :::::::::: 
   */
  b22 = b[l1 + l1 * b_dim1];
  if (Abs (b22) < epsb)
    {
      b22 = epsb;
    }
  b33 = b[na + na * b_dim1];
  if (Abs (b33) < epsb)
    {
      b33 = epsb;
    }
  b44 = b[en + en * b_dim1];
  if (Abs (b44) < epsb)
    {
      b44 = epsb;
    }
  a33 = a[na + na * a_dim1] / b33;
  a34 = a[na + en * a_dim1] / b44;
  a43 = a[en + na * a_dim1] / b33;
  a44 = a[en + en * a_dim1] / b44;
  b34 = b[na + en * b_dim1] / b44;
  t = (a43 * b34 - a33 - a44) * .5;
  r__ = t * t + a34 * a43 - a33 * a44;
  if (r__ < 0.)
    {
      goto L150;
    }
  /*    :::::::::: determine single shift zeroth column of a :::::::::: 
   */
  ish = 1;
  r__ = sqrt (r__);
  sh = -t + r__;
  s = -t - r__;
  if ((d__1 = s - a44, Abs (d__1)) < (d__2 = sh - a44, Abs (d__2)))
    {
      sh = s;
    }
  /*     if(enm2.le.0) goto 140 
   *    :::::::::: look for two consecutive small 
   *               sub-diagonal elements of a. 
   *               for l=en-2 step -1 until ld do -- :::::::::: 
   */
  i__1 = enm2;
  for (ll = ld; ll <= i__1; ++ll)
    {
      l = enm2 + ld - ll;
      if (l == ld)
	{
	  goto L140;
	}
      lm1 = l - 1;
      l1 = l + 1;
      t = a[l + l * a_dim1];
      if ((d__1 = b[l + l * b_dim1], Abs (d__1)) > epsb)
	{
	  t -= sh * b[l + l * b_dim1];
	}
      if ((d__1 = a[l + lm1 * a_dim1], Abs (d__1)) <= (d__2 =
						       t / a[l1 + l * a_dim1],
						       Abs (d__2)) * epsa)
	{
	  goto L100;
	}
      /* L130: */
    }
  /* 
   */
 L140:
  a1 = a11 - sh;
  a2 = a21;
  if (l != ld)
    {
      a[l + lm1 * a_dim1] = -a[l + lm1 * a_dim1];
    }
  goto L160;
  /*    :::::::::: determine double shift zeroth column of a :::::::::: 
   */
 L150:
  if (en <= 2)
    {
      goto L1001;
    }
  a12 = a[l + l1 * a_dim1] / b22;
  a22 = a[l1 + l1 * a_dim1] / b22;
  b12 = b[l + l1 * b_dim1] / b22;
  a1 =
    ((a33 - a11) * (a44 - a11) - a34 * a43 + a43 * b34 * a11) / a21 + a12 -
    a11 * b12;
  a2 = a22 - a11 - a21 * b12 - (a33 - a11) - (a44 - a11) + a43 * b34;
  a3 = a[l1 + 1 + l1 * a_dim1] / b22;
  goto L160;
  /*    :::::::::: ad hoc shift :::::::::: 
   */
 L155:
  a1 = 0.;
  a2 = 1.;
  a3 = 1.1605;
 L160:
  ++its;
  --itn;
  if (!(*matz))
    {
      lor1 = ld;
    }
  /*    :::::::::: main loop :::::::::: 
   */
  i__1 = na;
  for (k = l; k <= i__1; ++k)
    {
      notlas = k != na && ish == 2;
      k1 = k + 1;
      k2 = k + 2;
      /*Computing MAX 
       */
      i__2 = k - 1;
      km1 = Max (i__2, l);
      /*Computing MIN 
       */
      i__2 = en, i__3 = k1 + ish;
      ll = Min (i__2, i__3);
      if (notlas)
	{
	  goto L190;
	}
      /*    :::::::::: zero a(k+1,k-1) :::::::::: 
       */
      if (k == l)
	{
	  goto L170;
	}
      a1 = a[k + km1 * a_dim1];
      a2 = a[k1 + km1 * a_dim1];
    L170:
      s = Abs (a1) + Abs (a2);
      if (s == 0.)
	{
	  goto L70;
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
      i__2 = enorn;
      for (j = km1; j <= i__2; ++j)
	{
	  t = a[k + j * a_dim1] + u2 * a[k1 + j * a_dim1];
	  a[k + j * a_dim1] += t * v1;
	  a[k1 + j * a_dim1] += t * v2;
	  t = b[k + j * b_dim1] + u2 * b[k1 + j * b_dim1];
	  b[k + j * b_dim1] += t * v1;
	  b[k1 + j * b_dim1] += t * v2;
	  /* L180: */
	}
      if (!(*matq))
	{
	  goto L181;
	}
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  t = q[k + j * q_dim1] + u2 * q[k1 + j * q_dim1];
	  q[k + j * q_dim1] += t * v1;
	  q[k1 + j * q_dim1] += t * v2;
	  /* L182: */
	}
    L181:
      /* 
       */
      if (k != l)
	{
	  a[k1 + km1 * a_dim1] = 0.;
	}
      goto L240;
      /*    :::::::::: zero a(k+1,k-1) and a(k+2,k-1) :::::::::: 
       */
    L190:
      if (k == l)
	{
	  goto L200;
	}
      a1 = a[k + km1 * a_dim1];
      a2 = a[k1 + km1 * a_dim1];
      a3 = a[k2 + km1 * a_dim1];
    L200:
      s = Abs (a1) + Abs (a2) + Abs (a3);
      if (s == 0.)
	{
	  goto L260;
	}
      u1 = a1 / s;
      u2 = a2 / s;
      u3 = a3 / s;
      d__1 = sqrt (u1 * u1 + u2 * u2 + u3 * u3);
      r__ = d_sign (&d__1, &u1);
      v1 = -(u1 + r__) / r__;
      v2 = -u2 / r__;
      v3 = -u3 / r__;
      u2 = v2 / v1;
      u3 = v3 / v1;
      /* 
       */
      i__2 = enorn;
      for (j = km1; j <= i__2; ++j)
	{
	  t =
	    a[k + j * a_dim1] + u2 * a[k1 + j * a_dim1] + u3 * a[k2 +
								 j * a_dim1];
	  a[k + j * a_dim1] += t * v1;
	  a[k1 + j * a_dim1] += t * v2;
	  a[k2 + j * a_dim1] += t * v3;
	  t =
	    b[k + j * b_dim1] + u2 * b[k1 + j * b_dim1] + u3 * b[k2 +
								 j * b_dim1];
	  b[k + j * b_dim1] += t * v1;
	  b[k1 + j * b_dim1] += t * v2;
	  b[k2 + j * b_dim1] += t * v3;
	  /* L210: */
	}
      if (!(*matq))
	{
	  goto L211;
	}
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  t =
	    q[k + j * q_dim1] + u2 * q[k1 + j * q_dim1] + u3 * q[k2 +
								 j * q_dim1];
	  q[k + j * q_dim1] += t * v1;
	  q[k1 + j * q_dim1] += t * v2;
	  q[k2 + j * q_dim1] += t * v3;
	  /* L212: */
	}
    L211:
      /* 
       */
      if (k == l)
	{
	  goto L220;
	}
      a[k1 + km1 * a_dim1] = 0.;
      a[k2 + km1 * a_dim1] = 0.;
      /*    :::::::::: zero b(k+2,k+1) and b(k+2,k) :::::::::: 
       */
    L220:
      s = (d__1 = b[k2 + k2 * b_dim1], Abs (d__1)) + (d__2 =
						      b[k2 + k1 * b_dim1],
						      Abs (d__2)) + (d__3 =
								     b[k2 +
								       k *
								       b_dim1],
								     Abs
								     (d__3));
      if (s == 0.)
	{
	  goto L240;
	}
      u1 = b[k2 + k2 * b_dim1] / s;
      u2 = b[k2 + k1 * b_dim1] / s;
      u3 = b[k2 + k * b_dim1] / s;
      d__1 = sqrt (u1 * u1 + u2 * u2 + u3 * u3);
      r__ = d_sign (&d__1, &u1);
      v1 = -(u1 + r__) / r__;
      v2 = -u2 / r__;
      v3 = -u3 / r__;
      u2 = v2 / v1;
      u3 = v3 / v1;
      /* 
       */
      i__2 = ll;
      for (i__ = lor1; i__ <= i__2; ++i__)
	{
	  t =
	    a[i__ + k2 * a_dim1] + u2 * a[i__ + k1 * a_dim1] + u3 * a[i__ +
								      k *
								      a_dim1];
	  a[i__ + k2 * a_dim1] += t * v1;
	  a[i__ + k1 * a_dim1] += t * v2;
	  a[i__ + k * a_dim1] += t * v3;
	  t =
	    b[i__ + k2 * b_dim1] + u2 * b[i__ + k1 * b_dim1] + u3 * b[i__ +
								      k *
								      b_dim1];
	  b[i__ + k2 * b_dim1] += t * v1;
	  b[i__ + k1 * b_dim1] += t * v2;
	  b[i__ + k * b_dim1] += t * v3;
	  /* L230: */
	}
      /* 
       */
      b[k2 + k * b_dim1] = 0.;
      b[k2 + k1 * b_dim1] = 0.;
      if (!(*matz))
	{
	  goto L240;
	}
      /* 
       */
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  t =
	    z__[i__ + k2 * z_dim1] + u2 * z__[i__ + k1 * z_dim1] +
	    u3 * z__[i__ + k * z_dim1];
	  z__[i__ + k2 * z_dim1] += t * v1;
	  z__[i__ + k1 * z_dim1] += t * v2;
	  z__[i__ + k * z_dim1] += t * v3;
	  /* L235: */
	}
      /*    :::::::::: zero b(k+1,k) :::::::::: 
       */
    L240:
      s = (d__1 = b[k1 + k1 * b_dim1], Abs (d__1)) + (d__2 =
						      b[k1 + k * b_dim1],
						      Abs (d__2));
      if (s == 0.)
	{
	  goto L260;
	}
      u1 = b[k1 + k1 * b_dim1] / s;
      u2 = b[k1 + k * b_dim1] / s;
      d__1 = sqrt (u1 * u1 + u2 * u2);
      r__ = d_sign (&d__1, &u1);
      v1 = -(u1 + r__) / r__;
      v2 = -u2 / r__;
      u2 = v2 / v1;
      /* 
       */
      i__2 = ll;
      for (i__ = lor1; i__ <= i__2; ++i__)
	{
	  t = a[i__ + k1 * a_dim1] + u2 * a[i__ + k * a_dim1];
	  a[i__ + k1 * a_dim1] += t * v1;
	  a[i__ + k * a_dim1] += t * v2;
	  t = b[i__ + k1 * b_dim1] + u2 * b[i__ + k * b_dim1];
	  b[i__ + k1 * b_dim1] += t * v1;
	  b[i__ + k * b_dim1] += t * v2;
	  /* L250: */
	}
      /* 
       */
      b[k1 + k * b_dim1] = 0.;
      if (!(*matz))
	{
	  goto L260;
	}
      /* 
       */
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  t = z__[i__ + k1 * z_dim1] + u2 * z__[i__ + k * z_dim1];
	  z__[i__ + k1 * z_dim1] += t * v1;
	  z__[i__ + k * z_dim1] += t * v2;
	  /* L255: */
	}
      /* 
       */
    L260:
      ;
    }
  /*    :::::::::: end qz step :::::::::: 
   */
  goto L70;
  /*    :::::::::: set error -- neither bottom subdiagonal element 
   *               has become negligible after 50 iterations :::::::::: 
   */
 L1000:
  *ierr = en;
  /*    :::::::::: save epsb for use by qzval and qzvec :::::::::: 
   */
 L1001:
  if (*n > 1)
    {
      *eps1 = epsb;
    }
  return 0;
  /*    :::::::::: last card of qzit :::::::::: 
   */
}				/* qitz_ */
