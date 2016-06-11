/* exchqz.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*
 * given the upper triangular matrix b and upper hessenberg matrix a 
 * with consecutive ls1xls1 and ls2xls2 diagonal blocks (ls1,ls2.le.2) 
 * starting at row/column l, exchqz produces equivalence transforma- 
 * tions qt and zt that exchange the blocks along with their generalized 
 * eigenvalues.  
 *
 *    subroutine exchqz(nmax,n,a,b,z,l,ls1,ls2,eps,fail) 
 *    int nmax,n,l,ls1,ls2 
 *    double precision a(nmax,n),b(nmax,n),z(nmax,n),eps 
 *    int fail 
 * 
 *    nmax     the first dimension of a, b and z 
 *    n        the order of a, b and z 
 *   *a,*b     the matrix pair whose blocks are to be interchanged 
 *   *z        upon return this array is multiplied by the column 
 *             transformation zt. 
 *    l        the position of the blocks 
 *    ls1      the size of the first block 
 *    ls2      the size of the second block 
 *    eps      the required absolute accuracy of the result 
 *   *fail     a int variable which is false on a normal return, 
 *             true otherwise. 
 * 
 *    VanDooren 
 */

static int c__1 = 1;
static int c__3 = 3;

int
nsp_ctrlpack_exchqz (int *nmax, int *n, double *a, double *b, double *z__,
		     int *l, int *ls1, int *ls2, double *eps, int *fail)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1;
  double d__1, d__2, d__3, d__4;

  /* Local variables */
  int altb;
  double d__, e, f, g;
  int i__, j;
  double u[9] /* was [3][3] */ ;
  int l1, l2, l3;
  double sa;
  int li, lj;
  double sb;
  int ll;
  double ammbmm, anmbmm, amnbnn, bmnbnn, annbnn;
  int it1, it2;
  double a11b11, a21b11, a12b22, b12b22, a22b22;

  /* Parameter adjustments */
  z_dim1 = *nmax;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  b_dim1 = *nmax;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  a_dim1 = *nmax;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  *fail = FALSE;
  l1 = *l + 1;
  ll = *ls1 + *ls2;
  if (ll > 2)
    {
      goto L50;
    }
  /*** interchange 1x1 and 1x1 blocks via an equivalence 
   *** transformation       a:=q*a*z , b:=q*b*z 
   *** where q and z are givens rotations 
   *Computing MAX 
   */
  d__3 = (d__1 = a[l1 + l1 * a_dim1], Abs (d__1)), d__4 = (d__2 =
							   b[l1 +
							     l1 * b_dim1],
							   Abs (d__2));
  f = Max (d__3, d__4);
  altb = TRUE;
  if ((d__1 = a[l1 + l1 * a_dim1], Abs (d__1)) >= f)
    {
      altb = FALSE;
    }
  sa = a[l1 + l1 * a_dim1] / f;
  sb = b[l1 + l1 * b_dim1] / f;
  f = sa * b[*l + *l * b_dim1] - sb * a[*l + *l * a_dim1];
  /*construct the column transformation z 
   */
  g = sa * b[*l + l1 * b_dim1] - sb * a[*l + l1 * a_dim1];
  nsp_ctrlpack_giv (&f, &g, &d__, &e);
  d__1 = -d__;
  C2F (drot) (&l1, &a[*l * a_dim1 + 1], &c__1, &a[l1 * a_dim1 + 1],
	      &c__1, &e, &d__1);
  d__1 = -d__;
  C2F (drot) (&l1, &b[*l * b_dim1 + 1], &c__1, &b[l1 * b_dim1 + 1],
	      &c__1, &e, &d__1);
  d__1 = -d__;
  C2F (drot) (n, &z__[*l * z_dim1 + 1], &c__1, &z__[l1 * z_dim1 + 1],
	      &c__1, &e, &d__1);
  /*construct the row transformation q 
   */
  if (altb)
    {
      nsp_ctrlpack_giv (&b[*l + *l * b_dim1], &b[l1 + *l * b_dim1], &d__, &e);
    }
  if (!altb)
    {
      nsp_ctrlpack_giv (&a[*l + *l * a_dim1], &a[l1 + *l * a_dim1], &d__, &e);
    }
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &a[*l + *l * a_dim1], nmax, &a[l1 + *l * a_dim1],
	      nmax, &d__, &e);
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &b[*l + *l * b_dim1], nmax, &b[l1 + *l * b_dim1],
	      nmax, &d__, &e);
  a[l1 + *l * a_dim1] = 0.;
  b[l1 + *l * b_dim1] = 0.;
  return 0;
  /*** interchange 1x1 and 2x2 blocks via an equivalence 
   *** transformation  a:=q2*q1*a*z1*z2 , b:=q2*q1*b*z1*z2 
   *** where each qi and zi is a givens rotation 
   */
 L50:
  l2 = *l + 2;
  if (*ls1 == 2)
    {
      goto L100;
    }
  /*Computing MAX 
   */
  d__3 = (d__1 = a[*l + *l * a_dim1], Abs (d__1)), d__4 = (d__2 =
							   b[*l +
							     *l * b_dim1],
							   Abs (d__2));
  g = Max (d__3, d__4);
  altb = TRUE;
  if ((d__1 = a[*l + *l * a_dim1], Abs (d__1)) < g)
    {
      goto L60;
    }
  altb = FALSE;
  nsp_ctrlpack_giv (&a[l1 + l1 * a_dim1], &a[l2 + l1 * a_dim1], &d__, &e);
  i__1 = *n - *l;
  C2F (drot) (&i__1, &a[l1 + l1 * a_dim1], nmax, &a[l2 + l1 * a_dim1],
	      nmax, &d__, &e);
  i__1 = *n - *l;
  C2F (drot) (&i__1, &b[l1 + l1 * b_dim1], nmax, &b[l2 + l1 * b_dim1],
	      nmax, &d__, &e);
  /**  evaluate the pencil at the eigenvalue corresponding 
   **  to the 1x1 block 
   */
 L60:
  sa = a[*l + *l * a_dim1] / g;
  sb = b[*l + *l * b_dim1] / g;
  for (j = 1; j <= 2; ++j)
    {
      lj = *l + j;
      for (i__ = 1; i__ <= 3; ++i__)
	{
	  li = *l + i__ - 1;
	  /* L80: */
	  u[i__ + j * 3 - 4] =
	    sa * b[li + lj * b_dim1] - sb * a[li + lj * a_dim1];
	}
    }
  nsp_ctrlpack_giv (&u[2], &u[5], &d__, &e);
  d__1 = -d__;
  C2F (drot) (&c__3, u, &c__1, &u[3], &c__1, &e, &d__1);
  /*perform the row transformation q1 
   */
  nsp_ctrlpack_giv (u, &u[1], &d__, &e);
  u[4] = -u[3] * e + u[4] * d__;
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &a[*l + *l * a_dim1], nmax, &a[l1 + *l * a_dim1],
	      nmax, &d__, &e);
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &b[*l + *l * b_dim1], nmax, &b[l1 + *l * b_dim1],
	      nmax, &d__, &e);
  /*perform the column transformation z1 
   */
  if (altb)
    {
      nsp_ctrlpack_giv (&b[l1 + *l * b_dim1], &b[l1 + l1 * b_dim1], &d__, &e);
    }
  if (!altb)
    {
      nsp_ctrlpack_giv (&a[l1 + *l * a_dim1], &a[l1 + l1 * a_dim1], &d__, &e);
    }
  d__1 = -d__;
  C2F (drot) (&l2, &a[*l * a_dim1 + 1], &c__1, &a[l1 * a_dim1 + 1],
	      &c__1, &e, &d__1);
  d__1 = -d__;
  C2F (drot) (&l2, &b[*l * b_dim1 + 1], &c__1, &b[l1 * b_dim1 + 1],
	      &c__1, &e, &d__1);
  d__1 = -d__;
  C2F (drot) (n, &z__[*l * z_dim1 + 1], &c__1, &z__[l1 * z_dim1 + 1],
	      &c__1, &e, &d__1);
  /*perform the row transformation q2 
   */
  nsp_ctrlpack_giv (&u[4], &u[5], &d__, &e);
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &a[l1 + *l * a_dim1], nmax, &a[l2 + *l * a_dim1],
	      nmax, &d__, &e);
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &b[l1 + *l * b_dim1], nmax, &b[l2 + *l * b_dim1],
	      nmax, &d__, &e);
  /*perform the column transformation z2 
   */
  if (altb)
    {
      nsp_ctrlpack_giv (&b[l2 + l1 * b_dim1], &b[l2 + l2 * b_dim1], &d__, &e);
    }
  if (!altb)
    {
      nsp_ctrlpack_giv (&a[l2 + l1 * a_dim1], &a[l2 + l2 * a_dim1], &d__, &e);
    }
  d__1 = -d__;
  C2F (drot) (&l2, &a[l1 * a_dim1 + 1], &c__1, &a[l2 * a_dim1 + 1],
	      &c__1, &e, &d__1);
  d__1 = -d__;
  C2F (drot) (&l2, &b[l1 * b_dim1 + 1], &c__1, &b[l2 * b_dim1 + 1],
	      &c__1, &e, &d__1);
  d__1 = -d__;
  C2F (drot) (n, &z__[l1 * z_dim1 + 1], &c__1, &z__[l2 * z_dim1 + 1],
	      &c__1, &e, &d__1);
  if (altb)
    {
      goto L90;
    }
  nsp_ctrlpack_giv (&b[*l + *l * b_dim1], &b[l1 + *l * b_dim1], &d__, &e);
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &a[*l + *l * a_dim1], nmax, &a[l1 + *l * a_dim1],
	      nmax, &d__, &e);
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &b[*l + *l * b_dim1], nmax, &b[l1 + *l * b_dim1],
	      nmax, &d__, &e);
  /* put the neglectable elements equal to zero 
   */
 L90:
  a[l2 + *l * a_dim1] = 0.;
  a[l2 + l1 * a_dim1] = 0.;
  b[l1 + *l * b_dim1] = 0.;
  b[l2 + *l * b_dim1] = 0.;
  b[l2 + l1 * b_dim1] = 0.;
  return 0;
  /*** interchange 2x2 and 1x1 blocks via an equivalence 
   *** transformation  a:=q2*q1*a*z1*z2 , b:=q2*q1*b*z1*z2 
   *** where each qi and zi is a givens rotation 
   */
 L100:
  if (*ls2 == 2)
    {
      goto L150;
    }
  /*Computing MAX 
   */
  d__3 = (d__1 = a[l2 + l2 * a_dim1], Abs (d__1)), d__4 = (d__2 =
							   b[l2 +
							     l2 * b_dim1],
							   Abs (d__2));
  g = Max (d__3, d__4);
  altb = TRUE;
  if ((d__1 = a[l2 + l2 * a_dim1], Abs (d__1)) < g)
    {
      goto L120;
    }
  altb = FALSE;
  nsp_ctrlpack_giv (&a[*l + *l * a_dim1], &a[l1 + *l * a_dim1], &d__, &e);
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &a[*l + *l * a_dim1], nmax, &a[l1 + *l * a_dim1],
	      nmax, &d__, &e);
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &b[*l + *l * b_dim1], nmax, &b[l1 + *l * b_dim1],
	      nmax, &d__, &e);
  /**  evaluate the pencil at the eigenvalue corresponding 
   **  to the 1x1 block 
   */
 L120:
  sa = a[l2 + l2 * a_dim1] / g;
  sb = b[l2 + l2 * b_dim1] / g;
  for (i__ = 1; i__ <= 2; ++i__)
    {
      li = *l + i__ - 1;
      for (j = 1; j <= 3; ++j)
	{
	  lj = *l + j - 1;
	  /* L130: */
	  u[i__ + j * 3 - 4] =
	    sa * b[li + lj * b_dim1] - sb * a[li + lj * a_dim1];
	}
    }
  nsp_ctrlpack_giv (u, &u[1], &d__, &e);
  C2F (drot) (&c__3, u, &c__3, &u[1], &c__3, &d__, &e);
  /*perform the column transformation z1 
   */
  nsp_ctrlpack_giv (&u[4], &u[7], &d__, &e);
  u[3] = u[3] * e - u[6] * d__;
  d__1 = -d__;
  C2F (drot) (&l2, &a[l1 * a_dim1 + 1], &c__1, &a[l2 * a_dim1 + 1],
	      &c__1, &e, &d__1);
  d__1 = -d__;
  C2F (drot) (&l2, &b[l1 * b_dim1 + 1], &c__1, &b[l2 * b_dim1 + 1],
	      &c__1, &e, &d__1);
  d__1 = -d__;
  C2F (drot) (n, &z__[l1 * z_dim1 + 1], &c__1, &z__[l2 * z_dim1 + 1],
	      &c__1, &e, &d__1);
  /*perform the row transformation q1 
   */
  if (altb)
    {
      nsp_ctrlpack_giv (&b[l1 + l1 * b_dim1], &b[l2 + l1 * b_dim1], &d__, &e);
    }
  if (!altb)
    {
      nsp_ctrlpack_giv (&a[l1 + l1 * a_dim1], &a[l2 + l1 * a_dim1], &d__, &e);
    }
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &a[l1 + *l * a_dim1], nmax, &a[l2 + *l * a_dim1],
	      nmax, &d__, &e);
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &b[l1 + *l * b_dim1], nmax, &b[l2 + *l * b_dim1],
	      nmax, &d__, &e);
  /*perform the column transformation z2 
   */
  nsp_ctrlpack_giv (u, &u[3], &d__, &e);
  d__1 = -d__;
  C2F (drot) (&l2, &a[*l * a_dim1 + 1], &c__1, &a[l1 * a_dim1 + 1],
	      &c__1, &e, &d__1);
  d__1 = -d__;
  C2F (drot) (&l2, &b[*l * b_dim1 + 1], &c__1, &b[l1 * b_dim1 + 1],
	      &c__1, &e, &d__1);
  d__1 = -d__;
  C2F (drot) (n, &z__[*l * z_dim1 + 1], &c__1, &z__[l1 * z_dim1 + 1],
	      &c__1, &e, &d__1);
  /*perform the row transformation q2 
   */
  if (altb)
    {
      nsp_ctrlpack_giv (&b[*l + *l * b_dim1], &b[l1 + *l * b_dim1], &d__, &e);
    }
  if (!altb)
    {
      nsp_ctrlpack_giv (&a[*l + *l * a_dim1], &a[l1 + *l * a_dim1], &d__, &e);
    }
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &a[*l + *l * a_dim1], nmax, &a[l1 + *l * a_dim1],
	      nmax, &d__, &e);
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &b[*l + *l * b_dim1], nmax, &b[l1 + *l * b_dim1],
	      nmax, &d__, &e);
  if (altb)
    {
      goto L140;
    }
  nsp_ctrlpack_giv (&b[l1 + l1 * b_dim1], &b[l2 + l1 * b_dim1], &d__, &e);
  i__1 = *n - *l;
  C2F (drot) (&i__1, &a[l1 + l1 * a_dim1], nmax, &a[l2 + l1 * a_dim1],
	      nmax, &d__, &e);
  i__1 = *n - *l;
  C2F (drot) (&i__1, &b[l1 + l1 * b_dim1], nmax, &b[l2 + l1 * b_dim1],
	      nmax, &d__, &e);
  /* put the neglectable elements equal to zero 
   */
 L140:
  a[l1 + *l * a_dim1] = 0.;
  a[l2 + *l * a_dim1] = 0.;
  b[l1 + *l * b_dim1] = 0.;
  b[l2 + b_dim1] = 0.;
  b[l2 + l1 * b_dim1] = 0.;
  return 0;
  /*** interchange 2x2 and 2x2 blocks via a sequence of 
   *** qz-steps realized by the equivalence transformations 
   ***          a:=q5*q4*q3*q2*q1*a*z1*z2*z3*z4*z5 
   ***          b:=q5*q4*q3*q2*q1*b*z1*z2*z3*z4*z5 
   *** where each qi and zi is a givens rotation 
   */
 L150:
  l3 = *l + 3;
  /*compute implicit shift 
   */
  ammbmm = a[*l + *l * a_dim1] / b[*l + *l * b_dim1];
  anmbmm = a[l1 + *l * a_dim1] / b[*l + *l * b_dim1];
  amnbnn = a[*l + l1 * a_dim1] / b[l1 + l1 * b_dim1];
  annbnn = a[l1 + l1 * a_dim1] / b[l1 + l1 * b_dim1];
  bmnbnn = b[*l + l1 * b_dim1] / b[l1 + l1 * b_dim1];
  for (it1 = 1; it1 <= 3; ++it1)
    {
      u[0] = 1.;
      u[1] = 1.;
      u[2] = 1.;
      for (it2 = 1; it2 <= 10; ++it2)
	{
	  /*perform row transformations q1 and q2 
	   */
	  nsp_ctrlpack_giv (&u[1], &u[2], &d__, &e);
	  i__1 = *n - *l + 1;
	  C2F (drot) (&i__1, &a[l1 + *l * a_dim1], nmax,
		      &a[l2 + *l * a_dim1], nmax, &d__, &e);
	  i__1 = *n - *l;
	  C2F (drot) (&i__1, &b[l1 + l1 * b_dim1], nmax,
		      &b[l2 + l1 * b_dim1], nmax, &d__, &e);
	  u[1] = d__ * u[1] + e * u[2];
	  nsp_ctrlpack_giv (u, &u[1], &d__, &e);
	  i__1 = *n - *l + 1;
	  C2F (drot) (&i__1, &a[*l + *l * a_dim1], nmax,
		      &a[l1 + *l * a_dim1], nmax, &d__, &e);
	  i__1 = *n - *l + 1;
	  C2F (drot) (&i__1, &b[*l + *l * b_dim1], nmax,
		      &b[l1 + *l * b_dim1], nmax, &d__, &e);
	  /*perform column transformations z1 and z2 
	   */
	  nsp_ctrlpack_giv (&b[l2 + l1 * b_dim1], &b[l2 + l2 * b_dim1], &d__,
			    &e);
	  d__1 = -d__;
	  C2F (drot) (&l3, &a[l1 * a_dim1 + 1], &c__1,
		      &a[l2 * a_dim1 + 1], &c__1, &e, &d__1);
	  d__1 = -d__;
	  C2F (drot) (&l2, &b[l1 * b_dim1 + 1], &c__1,
		      &b[l2 * b_dim1 + 1], &c__1, &e, &d__1);
	  d__1 = -d__;
	  C2F (drot) (n, &z__[l1 * z_dim1 + 1], &c__1,
		      &z__[l2 * z_dim1 + 1], &c__1, &e, &d__1);
	  nsp_ctrlpack_giv (&b[l1 + *l * b_dim1], &b[l1 + l1 * b_dim1], &d__,
			    &e);
	  d__1 = -d__;
	  C2F (drot) (&l3, &a[*l * a_dim1 + 1], &c__1,
		      &a[l1 * a_dim1 + 1], &c__1, &e, &d__1);
	  d__1 = -d__;
	  C2F (drot) (&l1, &b[*l * b_dim1 + 1], &c__1,
		      &b[l1 * b_dim1 + 1], &c__1, &e, &d__1);
	  d__1 = -d__;
	  C2F (drot) (n, &z__[*l * z_dim1 + 1], &c__1,
		      &z__[l1 * z_dim1 + 1], &c__1, &e, &d__1);
	  /*perform transformations q3,z3,q4,z4,q5 and z5 in 
	   *order to reduce the pencil to hessenberg form 
	   */
	  nsp_ctrlpack_giv (&a[l2 + *l * a_dim1], &a[l3 + *l * a_dim1], &d__,
			    &e);
	  i__1 = *n - *l + 1;
	  C2F (drot) (&i__1, &a[l2 + *l * a_dim1], nmax,
		      &a[l3 + *l * a_dim1], nmax, &d__, &e);
	  i__1 = *n - l1;
	  C2F (drot) (&i__1, &b[l2 + l2 * b_dim1], nmax,
		      &b[l3 + l2 * b_dim1], nmax, &d__, &e);
	  nsp_ctrlpack_giv (&b[l3 + l2 * b_dim1], &b[l3 + l3 * b_dim1], &d__,
			    &e);
	  d__1 = -d__;
	  C2F (drot) (&l3, &a[l2 * a_dim1 + 1], &c__1,
		      &a[l3 * a_dim1 + 1], &c__1, &e, &d__1);
	  d__1 = -d__;
	  C2F (drot) (&l3, &b[l2 * b_dim1 + 1], &c__1,
		      &b[l3 * b_dim1 + 1], &c__1, &e, &d__1);
	  d__1 = -d__;
	  C2F (drot) (n, &z__[l2 * z_dim1 + 1], &c__1,
		      &z__[l3 * z_dim1 + 1], &c__1, &e, &d__1);
	  nsp_ctrlpack_giv (&a[l1 + *l * a_dim1], &a[l2 + *l * a_dim1], &d__,
			    &e);
	  i__1 = *n - *l + 1;
	  C2F (drot) (&i__1, &a[l1 + *l * a_dim1], nmax,
		      &a[l2 + *l * a_dim1], nmax, &d__, &e);
	  i__1 = *n - *l;
	  C2F (drot) (&i__1, &b[l1 + l1 * b_dim1], nmax,
		      &b[l2 + l1 * b_dim1], nmax, &d__, &e);
	  nsp_ctrlpack_giv (&b[l2 + l1 * b_dim1], &b[l2 + l2 * b_dim1], &d__,
			    &e);
	  d__1 = -d__;
	  C2F (drot) (&l3, &a[l1 * a_dim1 + 1], &c__1,
		      &a[l2 * a_dim1 + 1], &c__1, &e, &d__1);
	  d__1 = -d__;
	  C2F (drot) (&l2, &b[l1 * b_dim1 + 1], &c__1,
		      &b[l2 * b_dim1 + 1], &c__1, &e, &d__1);
	  d__1 = -d__;
	  C2F (drot) (n, &z__[l1 * z_dim1 + 1], &c__1,
		      &z__[l2 * z_dim1 + 1], &c__1, &e, &d__1);
	  nsp_ctrlpack_giv (&a[l2 + l1 * a_dim1], &a[l3 + l1 * a_dim1], &d__,
			    &e);
	  i__1 = *n - *l;
	  C2F (drot) (&i__1, &a[l2 + l1 * a_dim1], nmax,
		      &a[l3 + l1 * a_dim1], nmax, &d__, &e);
	  i__1 = *n - l1;
	  C2F (drot) (&i__1, &b[l2 + l2 * b_dim1], nmax,
		      &b[l3 + l2 * b_dim1], nmax, &d__, &e);
	  nsp_ctrlpack_giv (&b[l3 + l2 * b_dim1], &b[l3 + l3 * b_dim1], &d__,
			    &e);
	  d__1 = -d__;
	  C2F (drot) (&l3, &a[l2 * a_dim1 + 1], &c__1,
		      &a[l3 * a_dim1 + 1], &c__1, &e, &d__1);
	  d__1 = -d__;
	  C2F (drot) (&l3, &b[l2 * b_dim1 + 1], &c__1,
		      &b[l3 * b_dim1 + 1], &c__1, &e, &d__1);
	  d__1 = -d__;
	  C2F (drot) (n, &z__[l2 * z_dim1 + 1], &c__1,
		      &z__[l3 * z_dim1 + 1], &c__1, &e, &d__1);
	  /*test of convergence on the element separating the blocks 
	   */
	  if ((d__1 = a[l2 + l1 * a_dim1], Abs (d__1)) <= *eps)
	    {
	      goto L190;
	    }
	  /*compute a new shift in case of no convergence 
	   */
	  a11b11 = a[*l + *l * a_dim1] / b[*l + *l * b_dim1];
	  a12b22 = a[*l + l1 * a_dim1] / b[l1 + l1 * b_dim1];
	  a21b11 = a[l1 + *l * a_dim1] / b[*l + *l * b_dim1];
	  a22b22 = a[l1 + l1 * a_dim1] / b[l1 + l1 * b_dim1];
	  b12b22 = b[*l + l1 * b_dim1] / b[l1 + l1 * b_dim1];
	  u[0] =
	    ((ammbmm - a11b11) * (annbnn - a11b11) - amnbnn * anmbmm +
	     anmbmm * bmnbnn * a11b11) / a21b11 + a12b22 - a11b11 * b12b22;
	  u[1] =
	    a22b22 - a11b11 - a21b11 * b12b22 - (ammbmm - a11b11) - (annbnn -
								     a11b11) +
	    anmbmm * bmnbnn;
	  /* L180: */
	  u[2] = a[l2 + l1 * a_dim1] / b[l1 + l1 * b_dim1];
	}
    }
  *fail = TRUE;
  return 0;
  /* put the neglectable elements equal to zero in 
   * case of convergence 
   */
 L190:
  a[l2 + *l * a_dim1] = 0.;
  a[l2 + l1 * a_dim1] = 0.;
  a[l3 + *l * a_dim1] = 0.;
  a[l3 + l1 * a_dim1] = 0.;
  b[l1 + *l * b_dim1] = 0.;
  b[l2 + *l * b_dim1] = 0.;
  b[l2 + l1 * b_dim1] = 0.;
  b[l3 + *l * b_dim1] = 0.;
  b[l3 + l1 * b_dim1] = 0.;
  b[l3 + l2 * b_dim1] = 0.;
  return 0;
}				/* exchqz_ */
