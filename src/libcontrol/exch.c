/* exch.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/*
 * given  upper hessenberg matrix a 
 * with consecutive ls1xls1 and ls2xls2 diagonal blocks (ls1,ls2.le.2) 
 * starting at row/column l, exch produces equivalence transforma- 
 * tion zt that exchange the blocks along with their 
 * eigenvalues. 
 * 
 *    subroutine exch(nmax,n,a,z,l,ls1,ls2) 
 *    int nmax,n,l,ls1,ls2 
 *    double precision a(nmax,n),z(nmax,n) 
 * 
 *    nmax     the first dimension of a, b and z 
 *    n        the order of a, and z 
 *   *a        the matrix whose blocks are to be interchanged 
 *   *z        upon return this array is multiplied by the column 
 *             transformation zt. 
 *    l        the position of the blocks 
 *    ls1      the size of the first block 
 *    ls2      the size of the second block 
 * 
 *    Delebecque f. and Steer s. INRIA adapted from exchqz (VanDooren) 
 */


static int c__1 = 1;
static int c__3 = 3;
static int c__2 = 2;
static int c__4 = 4;

int
nsp_ctrlpack_exch (int *nmax, int *n, double *a, double *z__, int *l,
		   int *ls1, int *ls2)
{
  /* System generated locals */
  int a_dim1, a_offset, z_dim1, z_offset, i__1;
  double d__1, d__2;

  /* Local variables */
  double d__, e, f, g;
  int i__, j;
  double u[12] /* was [3][4] */ ;
  int l1, l2, l3;
  double sa;
  int li, lj;
  double sb;
  int ll;

  /* Parameter adjustments */
  z_dim1 = *nmax;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  a_dim1 = *nmax;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  l1 = *l + 1;
  ll = *ls1 + *ls2;
  if (ll > 2)
    {
      goto L50;
    }
  /*** interchange 1x1 and 1x1 blocks via an equivalence 
   *** transformation       a:=z'*a*z , 
   *** where z is givens rotation 
   *Computing MAX 
   */
  d__2 = (d__1 = a[l1 + l1 * a_dim1], Abs (d__1));
  f = Max (d__2, 1.);
  sa = a[l1 + l1 * a_dim1] / f;
  sb = 1. / f;
  f = sa - sb * a[*l + *l * a_dim1];
  /*construct the column transformation z 
   */
  g = -sb * a[*l + l1 * a_dim1];
  nsp_ctrlpack_giv (&f, &g, &d__, &e);
  d__1 = -d__;
  C2F (drot) (&l1, &a[*l * a_dim1 + 1], &c__1, &a[l1 * a_dim1 + 1],
	      &c__1, &e, &d__1);
  d__1 = -d__;
  C2F (drot) (n, &z__[*l * z_dim1 + 1], &c__1, &z__[l1 * z_dim1 + 1],
	      &c__1, &e, &d__1);
  /*construct the row transformation q 
   */
  i__1 = *n - *l + 1;
  d__1 = -d__;
  C2F (drot) (&i__1, &a[*l + *l * a_dim1], nmax, &a[l1 + *l * a_dim1],
	      nmax, &e, &d__1);
  a[l1 + *l * a_dim1] = 0.;
  return 0;
  /*** interchange 1x1 and 2x2 blocks via an equivalence 
   *** transformation  a:=z2'*z1'*a*z1*z2 , 
   *** where each zi is a givens rotation 
   */
 L50:
  l2 = *l + 2;
  if (*ls1 == 2)
    {
      goto L100;
    }
  /*Computing MAX 
   */
  d__2 = (d__1 = a[*l + *l * a_dim1], Abs (d__1));
  g = Max (d__2, 1.);
  /**  evaluate the pencil at the eigenvalue corresponding 
   **  to the 1x1 block 
   */
  /* L60: */
  sa = a[*l + *l * a_dim1] / g;
  sb = 1. / g;
  for (j = 1; j <= 2; ++j)
    {
      lj = *l + j;
      for (i__ = 1; i__ <= 3; ++i__)
	{
	  li = *l + i__ - 1;
	  u[i__ + j * 3 - 4] = -sb * a[li + lj * a_dim1];
	  /* L80: */
	  if (li == lj)
	    {
	      u[i__ + j * 3 - 4] += sa;
	    }
	}
    }
  nsp_ctrlpack_giv (&u[2], &u[5], &d__, &e);
  d__1 = -d__;
  C2F (drot) (&c__3, u, &c__1, &u[3], &c__1, &e, &d__1);
  /*perform the row transformation z1' 
   */
  nsp_ctrlpack_giv (u, &u[1], &d__, &e);
  u[4] = -u[3] * e + u[4] * d__;
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &a[*l + *l * a_dim1], nmax, &a[l1 + *l * a_dim1],
	      nmax, &d__, &e);
  /*perform the column transformation z1 
   */
  C2F (drot) (&l2, &a[*l * a_dim1 + 1], &c__1, &a[l1 * a_dim1 + 1],
	      &c__1, &d__, &e);
  C2F (drot) (n, &z__[*l * z_dim1 + 1], &c__1, &z__[l1 * z_dim1 + 1],
	      &c__1, &d__, &e);
  /*perform the row transformation z2' 
   */
  nsp_ctrlpack_giv (&u[4], &u[5], &d__, &e);
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &a[l1 + *l * a_dim1], nmax, &a[l2 + *l * a_dim1],
	      nmax, &d__, &e);
  /*perform the column transformation z2 
   */
  C2F (drot) (&l2, &a[l1 * a_dim1 + 1], &c__1, &a[l2 * a_dim1 + 1],
	      &c__1, &d__, &e);
  C2F (drot) (n, &z__[l1 * z_dim1 + 1], &c__1, &z__[l2 * z_dim1 + 1],
	      &c__1, &d__, &e);
  /* put the neglectable elements equal to zero 
   */
  a[l2 + *l * a_dim1] = 0.;
  a[l2 + l1 * a_dim1] = 0.;
  return 0;
  /*** interchange 2x2 and 1x1 blocks via an equivalence 
   *** transformation  a:=z2'*z1'*a*z1*z2 , 
   *** where each zi is a givens rotation 
   */
 L100:
  if (*ls2 == 2)
    {
      goto L150;
    }
  /*Computing MAX 
   */
  d__2 = (d__1 = a[l2 + l2 * a_dim1], Abs (d__1));
  g = Max (d__2, 1.);
  /**  evaluate the pencil at the eigenvalue corresponding 
   **  to the 1x1 block 
   */
  /* L120: */
  sa = a[l2 + l2 * a_dim1] / g;
  sb = 1. / g;
  for (i__ = 1; i__ <= 2; ++i__)
    {
      li = *l + i__ - 1;
      for (j = 1; j <= 3; ++j)
	{
	  lj = *l + j - 1;
	  u[i__ + j * 3 - 4] = -sb * a[li + lj * a_dim1];
	  /* L130: */
	  if (i__ == j)
	    {
	      u[i__ + j * 3 - 4] += sa;
	    }
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
  C2F (drot) (n, &z__[l1 * z_dim1 + 1], &c__1, &z__[l2 * z_dim1 + 1],
	      &c__1, &e, &d__1);
  /*perform the row transformation z1' 
   */
  i__1 = *n - *l + 1;
  d__1 = -d__;
  C2F (drot) (&i__1, &a[l1 + *l * a_dim1], nmax, &a[l2 + *l * a_dim1],
	      nmax, &e, &d__1);
  /*perform the column transformation z2 
   */
  nsp_ctrlpack_giv (u, &u[3], &d__, &e);
  d__1 = -d__;
  C2F (drot) (&l2, &a[*l * a_dim1 + 1], &c__1, &a[l1 * a_dim1 + 1],
	      &c__1, &e, &d__1);
  d__1 = -d__;
  C2F (drot) (n, &z__[*l * z_dim1 + 1], &c__1, &z__[l1 * z_dim1 + 1],
	      &c__1, &e, &d__1);
  /*perform the row transformation z2' 
   */
  i__1 = *n - *l + 1;
  d__1 = -d__;
  C2F (drot) (&i__1, &a[*l + *l * a_dim1], nmax, &a[l1 + *l * a_dim1],
	      nmax, &e, &d__1);
  /* put the neglectable elements equal to zero 
   */
  /* L140: */
  a[l1 + *l * a_dim1] = 0.;
  a[l2 + *l * a_dim1] = 0.;
  return 0;
  /*** interchange 2x2 and 2x2 blocks via a sequence of 
   ***  equivalence transformations 
   ***          a:=z5'*z4'*z3'*z2'*z1'*a*z1*z2*z3*z4*z5 
   *** where each zi is a givens rotation 
   */
 L150:
  l3 = *l + 3;
  d__ =
    a[l2 + l2 * a_dim1] * a[l3 + l3 * a_dim1] - a[l2 + l3 * a_dim1] * a[l3 +
									l2 *
									a_dim1];
  e = a[l2 + l2 * a_dim1] + a[l3 + l3 * a_dim1];
  nsp_calpack_dmmul (&a[*l + *l * a_dim1], nmax, &a[*l + *l * a_dim1], nmax,
		     u, &c__3, &c__2, &c__4, &c__4);
  for (i__ = 1; i__ <= 2; ++i__)
    {
      u[i__ + i__ * 3 - 4] += d__;
      for (j = 1; j <= 4; ++j)
	{
	  u[i__ + j * 3 - 4] -= e * a[*l - 1 + i__ + (*l - 1 + j) * a_dim1];
	  /* L10: */
	}
      /* L20: */
    }
  /*g0 
   */
  nsp_ctrlpack_giv (u, &u[1], &d__, &e);
  C2F (drot) (&c__4, u, &c__3, &u[1], &c__3, &d__, &e);
  /*z1 
   */
  nsp_ctrlpack_giv (&u[10], &u[7], &d__, &e);
  C2F (drot) (&c__2, &u[9], &c__1, &u[6], &c__1, &d__, &e);
  C2F (drot) (&l3, &a[l3 * a_dim1 + 1], &c__1, &a[l2 * a_dim1 + 1],
	      &c__1, &d__, &e);
  C2F (drot) (n, &z__[l3 * z_dim1 + 1], &c__1, &z__[l2 * z_dim1 + 1],
	      &c__1, &d__, &e);
  /*z1' 
   */
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &a[l3 + *l * a_dim1], nmax, &a[l2 + *l * a_dim1],
	      nmax, &d__, &e);
  /*z2 
   */
  nsp_ctrlpack_giv (&u[10], &u[4], &d__, &e);
  C2F (drot) (&c__2, &u[9], &c__1, &u[3], &c__1, &d__, &e);
  C2F (drot) (&l3, &a[l3 * a_dim1 + 1], &c__1, &a[l1 * a_dim1 + 1],
	      &c__1, &d__, &e);
  C2F (drot) (n, &z__[l3 * z_dim1 + 1], &c__1, &z__[l1 * z_dim1 + 1],
	      &c__1, &d__, &e);
  /*z2' 
   */
  u[10] = d__ * u[10];
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &a[l3 + *l * a_dim1], nmax, &a[l1 + *l * a_dim1],
	      nmax, &d__, &e);
  /*z3 
   */
  nsp_ctrlpack_giv (&u[6], &u[3], &d__, &e);
  C2F (drot) (&c__1, &u[6], &c__1, &u[3], &c__1, &d__, &e);
  C2F (drot) (&l3, &a[l2 * a_dim1 + 1], &c__1, &a[l1 * a_dim1 + 1],
	      &c__1, &d__, &e);
  C2F (drot) (n, &z__[l2 * z_dim1 + 1], &c__1, &z__[l1 * z_dim1 + 1],
	      &c__1, &d__, &e);
  /*z3' 
   */
  u[10] = d__ * u[10];
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &a[l2 + *l * a_dim1], nmax, &a[l1 + *l * a_dim1],
	      nmax, &d__, &e);
  /*z4 
   */
  nsp_ctrlpack_giv (&u[6], u, &d__, &e);
  C2F (drot) (&l3, &a[l2 * a_dim1 + 1], &c__1, &a[*l * a_dim1 + 1],
	      &c__1, &d__, &e);
  C2F (drot) (n, &z__[l2 * z_dim1 + 1], &c__1, &z__[*l * z_dim1 + 1],
	      &c__1, &d__, &e);
  /*z4' 
   */
  i__1 = *n - *l + 1;
  C2F (drot) (&i__1, &a[l2 + *l * a_dim1], nmax, &a[*l + *l * a_dim1],
	      nmax, &d__, &e);
  /*zeroes negligible elements 
   */
  a[l2 + *l * a_dim1] = 0.;
  a[l3 + *l * a_dim1] = 0.;
  a[l2 + l1 * a_dim1] = 0.;
  a[l3 + l1 * a_dim1] = 0.;
  return 0;
}				/* exch_ */
