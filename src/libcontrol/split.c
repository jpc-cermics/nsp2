/* split.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

int
nsp_ctrlpack_split (double *a, double *v, int *n, int *l, double *e1,
		    double *e2, int *na, int *nv)
{
  /* Initialized data */

  static double zero = 0.;
  static double two = 2.;

  /* System generated locals */
  int a_dim1, a_offset, v_dim1, v_offset, i__1;
  double d__1, d__2;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  int i__, j;
  double p, q, r__, t, u, w, x, y, z__;
  int l1;

  /* 
   *!purpose 
   * 
   *    given the upper hessenberg matrix a with a 2x2 block 
   *    starting at a(l,l), split determines if the 
   *    corresponding eigenvalues are real or complex, if they 
   *    are real, a rotation is determined that reduces the 
   *    block to upper triangular form with the eigenvalue 
   *    of largest absolute value appearing first.  the 
   *    rotation is accumulated in v. the eigenvalues (real 
   *    or complex) are returned in e1 and e2. 
   *!calling sequence 
   * 
   *    subroutine split(a, v, n, l, e1, e2, na, nv) 
   * 
   *    double precision a,v,e1,e2 
   *    int n,l,na,nv 
   *    dimension a(na,n),v(nv,n) 
   * 
   *    starred parameters are  altered by the subroutine 
   * 
   *       *a        the upper hessenberg matrix whose 2x2 
   *                 block is to be dsplit. 
   *       *v        the array in which the dsplitting trans- 
   *                 formation is to be accumulated. 
   *        n        the order of the matrix a. 
   *        l        the position of the 2x2 block. 
   *       *e1       on return if the eigenvalues are complex 
   *       *e2       e1 contains their common real part and 
   *                 e2 contains the positive imaginary part. 
   *                 if the eigenvalues are real. e1 contains 
   *                 the one largest in absolute value and f2 
   *                 contains the other one. 
   *       na        the first dimension of the array a. 
   *       nv        the first dimension of the array v. 
   *!auxiliary routines 
   *    abs sqrt (fortran) 
   *! 
   *originator 
   * 
   *    internal variables 
   * 
   *    internal variables 
   */
  /* Parameter adjustments */
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  v_dim1 = *nv;
  v_offset = v_dim1 + 1;
  v -= v_offset;

  /* Function Body */
  l1 = *l + 1;
  /* 
   */
  x = a[l1 + l1 * a_dim1];
  y = a[*l + *l * a_dim1];
  w = a[*l + l1 * a_dim1] * a[l1 + *l * a_dim1];
  p = (y - x) / two;
  /*Computing 2nd power 
   */
  d__1 = p;
  q = d__1 * d__1 + w;
  if (q >= zero)
    {
      goto L10;
    }
  /* 
   *      complex eigenvalue. 
   * 
   */
  *e1 = p + x;
  *e2 = sqrt (-q);
  return 0;
 L10:
  /* 
   *    two real eigenvalues. set up transformation. 
   * 
   */
  z__ = sqrt (q);
  if (p < zero)
    {
      goto L20;
    }
  z__ = p + z__;
  goto L30;
 L20:
  z__ = p - z__;
 L30:
  if (z__ == zero)
    {
      goto L40;
    }
  r__ = -w / z__;
  goto L50;
 L40:
  r__ = zero;
 L50:
  if ((d__1 = x + z__, Abs (d__1)) >= (d__2 = x + r__, Abs (d__2)))
    {
      z__ = r__;
    }
  y = y - x - z__;
  x = -z__;
  t = a[*l + l1 * a_dim1];
  u = a[l1 + *l * a_dim1];
  if (Abs (y) + Abs (u) <= Abs (t) + Abs (x))
    {
      goto L60;
    }
  q = u;
  p = y;
  goto L70;
 L60:
  q = x;
  p = t;
 L70:
  /*Computing 2nd power 
   */
  d__1 = p;
  /*Computing 2nd power 
   */
  d__2 = q;
  r__ = sqrt (d__1 * d__1 + d__2 * d__2);
  if (r__ > zero)
    {
      goto L80;
    }
  *e1 = a[*l + *l * a_dim1];
  *e2 = a[l1 + l1 * a_dim1];
  a[l1 + *l * a_dim1] = zero;
  return 0;
 L80:
  p /= r__;
  q /= r__;
  /* 
   *    premultiply. 
   * 
   */
  i__1 = *n;
  for (j = *l; j <= i__1; ++j)
    {
      z__ = a[*l + j * a_dim1];
      a[*l + j * a_dim1] = p * z__ + q * a[l1 + j * a_dim1];
      a[l1 + j * a_dim1] = p * a[l1 + j * a_dim1] - q * z__;
      /* L90: */
    }
  /* 
   *    postmultiply. 
   * 
   */
  i__1 = l1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      z__ = a[i__ + *l * a_dim1];
      a[i__ + *l * a_dim1] = p * z__ + q * a[i__ + l1 * a_dim1];
      a[i__ + l1 * a_dim1] = p * a[i__ + l1 * a_dim1] - q * z__;
      /* L100: */
    }
  /* 
   *    accumulate the transformation in v. 
   * 
   */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      z__ = v[i__ + *l * v_dim1];
      v[i__ + *l * v_dim1] = p * z__ + q * v[i__ + l1 * v_dim1];
      v[i__ + l1 * v_dim1] = p * v[i__ + l1 * v_dim1] - q * z__;
      /* L110: */
    }
  a[l1 + *l * a_dim1] = zero;
  *e1 = a[*l + *l * a_dim1];
  *e2 = a[l1 + l1 * a_dim1];
  return 0;
}				/* split_ */
