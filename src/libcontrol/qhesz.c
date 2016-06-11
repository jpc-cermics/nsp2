/* qhesz.f -- translated by f2c (version 19961017).
 *
 *
 */



#include "ctrlpack.h"

/* 
 * 
 *! purpose 
 *    this subroutine accepts a pair of real general matrices and 
 *    reduces one of them to upper hessenberg form and the other 
 *    to upper triangular form using orthogonal transformations. 
 *    it is usually followed by  qzit,  qzval  and, possibly,  qzvec. 
 * 
 *! calling sequence 
 * 
 *    subroutine qhesz(nm,n,a,b,matq,q,matz,z) 
 *    on input: 
 *       nm must be set to the row dimension of two-dimensional 
 *         array parameters as declared in the calling program 
 *         dimension statement; 
 *       n is the order of the matrices; 
 *       a contains a real general matrix; 
 *       b contains a real general matrix; 
 *       matz should be set to .true. if the right hand transformations 
 *         are to be accumulated for later use in computing 
 *         eigenvectors, and to .false. otherwise. 
 *    on output: 
 *       a has been reduced to upper hessenberg form.  the elements 
 *         below the first subdiagonal have been set to zero; 
 *       b has been reduced to upper triangular form.  the elements 
 *         below the main diagonal have been set to zero; 
 *       z contains the product of the right hand transformations if 
 *         matz has been set to .true.  otherwise, z is not referenced. 
 * 
 *! originator 
 * 
 *    this subroutine is the first step of the qz algorithm 
 *    for solving generalized matrix eigenvalue problems, 
 *    siam j. numer. anal. 10, 241-256(1973) by moler and stewart. 
 *    (modification de la routine qzhes de eispack pour avoir 
 *    la matrice unitaire de changement de base sur les lignes 
 *    donne par la matrice q .memes conventions que pour z.) 
 *    f.d. 
 *! 
 *    questions and comments should be directed to b. s. garbow, 
 *    applied mathematics division, argonne national laboratory 
 */

int
nsp_ctrlpack_qhesz (const int *nm,const int *n, double *a, double *b, const int *matq,
		    double *q, const int *matz, double *z__)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, q_dim1, q_offset,
    i__1, i__2, i__3;
  double d__1, d__2;

  /* Local variables */
  int i__, j, k, l;
  double r__, s, t;
  int l1;
  double u1, u2, v1, v2;
  int lb, nk1, nm1, nm2;
  double rho;

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
  if (!(*matz))
    {
      goto L10;
    }
  /* 
   */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       */
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  z__[i__ + j * z_dim1] = 0.;
	  /* L2: */
	}
      /* 
       */
      z__[i__ + i__ * z_dim1] = 1.;
      /* L3: */
    }
 L10:
  if (!(*matq))
    {
      goto L11;
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  q[i__ + j * q_dim1] = 0.;
	  /* L21: */
	}
      q[i__ + i__ * q_dim1] = 1.;
      /* L31: */
    }
 L11:
  /*    :::::::::: reduce b to upper triangular form :::::::::: 
   */
  if (*n <= 1)
    {
      goto L170;
    }
  nm1 = *n - 1;
  /* 
   */
  i__1 = nm1;
  for (l = 1; l <= i__1; ++l)
    {
      l1 = l + 1;
      s = 0.;
      /* 
       */
      i__2 = *n;
      for (i__ = l1; i__ <= i__2; ++i__)
	{
	  s += (d__1 = b[i__ + l * b_dim1], Abs (d__1));
	  /* L20: */
	}
      /* 
       */
      if (s == 0.)
	{
	  goto L100;
	}
      s += (d__1 = b[l + l * b_dim1], Abs (d__1));
      r__ = 0.;
      /* 
       */
      i__2 = *n;
      for (i__ = l; i__ <= i__2; ++i__)
	{
	  b[i__ + l * b_dim1] /= s;
	  /*Computing 2nd power 
	   */
	  d__1 = b[i__ + l * b_dim1];
	  r__ += d__1 * d__1;
	  /* L25: */
	}
      /* 
       */
      d__1 = sqrt (r__);
      r__ = d_sign (&d__1, &b[l + l * b_dim1]);
      b[l + l * b_dim1] += r__;
      rho = r__ * b[l + l * b_dim1];
      /* 
       */
      i__2 = *n;
      for (j = l1; j <= i__2; ++j)
	{
	  t = 0.;
	  /* 
	   */
	  i__3 = *n;
	  for (i__ = l; i__ <= i__3; ++i__)
	    {
	      t += b[i__ + l * b_dim1] * b[i__ + j * b_dim1];
	      /* L30: */
	    }
	  /* 
	   */
	  t = -t / rho;
	  /* 
	   */
	  i__3 = *n;
	  for (i__ = l; i__ <= i__3; ++i__)
	    {
	      b[i__ + j * b_dim1] += t * b[i__ + l * b_dim1];
	      /* L40: */
	    }
	  /* 
	   */
	  /* L50: */
	}
      /* 
       */
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  t = 0.;
	  /* 
	   */
	  i__3 = *n;
	  for (i__ = l; i__ <= i__3; ++i__)
	    {
	      t += b[i__ + l * b_dim1] * a[i__ + j * a_dim1];
	      /* L60: */
	    }
	  /* 
	   */
	  t = -t / rho;
	  /* 
	   */
	  i__3 = *n;
	  for (i__ = l; i__ <= i__3; ++i__)
	    {
	      a[i__ + j * a_dim1] += t * b[i__ + l * b_dim1];
	      /* L70: */
	    }
	  /* 
	   */
	  /* L80: */
	}
      if (!(*matq))
	{
	  goto L99;
	}
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  t = 0.;
	  /* 
	   */
	  i__3 = *n;
	  for (i__ = l; i__ <= i__3; ++i__)
	    {
	      t += b[i__ + l * b_dim1] * q[i__ + j * q_dim1];
	      /* L760: */
	    }
	  /* 
	   */
	  t = -t / rho;
	  /* 
	   */
	  i__3 = *n;
	  for (i__ = l; i__ <= i__3; ++i__)
	    {
	      q[i__ + j * q_dim1] += t * b[i__ + l * b_dim1];
	      /* L770: */
	    }
	  /* 
	   */
	  /* L780: */
	}
    L99:
      /* 
       */
      b[l + l * b_dim1] = -s * r__;
      /* 
       */
      i__2 = *n;
      for (i__ = l1; i__ <= i__2; ++i__)
	{
	  b[i__ + l * b_dim1] = 0.;
	  /* L90: */
	}
      /* 
       */
    L100:
      ;
    }
  /*    :::::::::: reduce a to upper hessenberg form, while 
   *               keeping b triangular :::::::::: 
   */
  if (*n == 2)
    {
      goto L170;
    }
  nm2 = *n - 2;
  /* 
   */
  i__1 = nm2;
  for (k = 1; k <= i__1; ++k)
    {
      nk1 = nm1 - k;
      /*    :::::::::: for l=n-1 step -1 until k+1 do -- :::::::::: 
       */
      i__2 = nk1;
      for (lb = 1; lb <= i__2; ++lb)
	{
	  l = *n - lb;
	  l1 = l + 1;
	  /*    :::::::::: zero a(l+1,k) :::::::::: 
	   */
	  s = (d__1 = a[l + k * a_dim1], Abs (d__1)) + (d__2 =
							a[l1 + k * a_dim1],
							Abs (d__2));
	  if (s == 0.)
	    {
	      goto L150;
	    }
	  u1 = a[l + k * a_dim1] / s;
	  u2 = a[l1 + k * a_dim1] / s;
	  d__1 = sqrt (u1 * u1 + u2 * u2);
	  r__ = d_sign (&d__1, &u1);
	  v1 = -(u1 + r__) / r__;
	  v2 = -u2 / r__;
	  u2 = v2 / v1;
	  /* 
	   */
	  i__3 = *n;
	  for (j = k; j <= i__3; ++j)
	    {
	      t = a[l + j * a_dim1] + u2 * a[l1 + j * a_dim1];
	      a[l + j * a_dim1] += t * v1;
	      a[l1 + j * a_dim1] += t * v2;
	      /* L110: */
	    }
	  /* 
	   */
	  a[l1 + k * a_dim1] = 0.;
	  /* 
	   */
	  i__3 = *n;
	  for (j = l; j <= i__3; ++j)
	    {
	      t = b[l + j * b_dim1] + u2 * b[l1 + j * b_dim1];
	      b[l + j * b_dim1] += t * v1;
	      b[l1 + j * b_dim1] += t * v2;
	      /* L120: */
	    }
	  if (!(*matq))
	    {
	      goto L122;
	    }
	  i__3 = *n;
	  for (j = 1; j <= i__3; ++j)
	    {
	      t = q[l + j * q_dim1] + u2 * q[l1 + j * q_dim1];
	      q[l + j * q_dim1] += t * v1;
	      q[l1 + j * q_dim1] += t * v2;
	      /* L121: */
	    }
	L122:
	  /*    :::::::::: zero b(l+1,l) :::::::::: 
	   */
	  s = (d__1 = b[l1 + l1 * b_dim1], Abs (d__1)) + (d__2 =
							  b[l1 + l * b_dim1],
							  Abs (d__2));
	  if (s == 0.)
	    {
	      goto L150;
	    }
	  u1 = b[l1 + l1 * b_dim1] / s;
	  u2 = b[l1 + l * b_dim1] / s;
	  d__1 = sqrt (u1 * u1 + u2 * u2);
	  r__ = d_sign (&d__1, &u1);
	  v1 = -(u1 + r__) / r__;
	  v2 = -u2 / r__;
	  u2 = v2 / v1;
	  /* 
	   */
	  i__3 = l1;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      t = b[i__ + l1 * b_dim1] + u2 * b[i__ + l * b_dim1];
	      b[i__ + l1 * b_dim1] += t * v1;
	      b[i__ + l * b_dim1] += t * v2;
	      /* L130: */
	    }
	  /* 
	   */
	  b[l1 + l * b_dim1] = 0.;
	  /* 
	   */
	  i__3 = *n;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      t = a[i__ + l1 * a_dim1] + u2 * a[i__ + l * a_dim1];
	      a[i__ + l1 * a_dim1] += t * v1;
	      a[i__ + l * a_dim1] += t * v2;
	      /* L140: */
	    }
	  /* 
	   */
	  if (!(*matz))
	    {
	      goto L150;
	    }
	  /* 
	   */
	  i__3 = *n;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      t = z__[i__ + l1 * z_dim1] + u2 * z__[i__ + l * z_dim1];
	      z__[i__ + l1 * z_dim1] += t * v1;
	      z__[i__ + l * z_dim1] += t * v2;
	      /* L145: */
	    }
	  /* 
	   */
	L150:
	  ;
	}
      /* 
       */
      /* L160: */
    }
  /* 
   */
 L170:
  return 0;
  /*    :::::::::: last card of qzhes :::::::::: 
   */
}				/* qhesz_ */
