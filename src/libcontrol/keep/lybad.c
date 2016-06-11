/* lybad.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*
 *       subroutine lybad(n,a,na,c,x,u,wrk,mode,ierr) 
 * 
 *       int n,na,mode,ierr 
 *       double precision a(na,n),c(na,n),x(na,n),u(na,n),wrk(n) 
 * 
 *       to solve the double precision matrix equation 
 *              trans(a)*x*a - x = c 
 *       where  c  is symmetric, and  trans(a)  denotes 
 *       the transpose of  a. 
 * 
 *       this routine is a modification of the subroutine d2lyb, 
 *       written and discussed by a.y.barraud (see reference). 
 * 
 *       a.y.barraud 
 *       "a numerical algorithm to solve  a' * x * a  -  x  =  q  ", 
 *       ieee trans. automat. control, vol. ac-22, 1977, pp 883-885 
 *arguments in 
 * 
 *      n        int 
 *               -the dimension of a. 
 *      a        double precision(n,n) 
 *               -the coefficient matrix  a  of the equation. on 
 *               exit, a  is overwritten, but see  comments  below. 
 *      c        double precision(n,n) 
 *               -the coefficient matrix  c  of the equation. 
 *      na       int 
 *               -the declared first dimension of  a, c, x  and  u 
 *      mode     int 
 *               - mode = 0  if  a  has not already been reduced to 
 *                               upper triangular form 
 *               - mode = 1  if  a  has been reduced to triangular form 
 *                        by (e.g.) a previous call to this routine 
 * 
 *arguments out 
 *      a        double precision(n,n) 
 *               -on exit, a  contains the transformed upper 
 *               triangular form of a.   (see comments below) 
 *      x        double precision(n,n) 
 *               -the solution matrix 
 *      u        double precision(n,n) 
 *               - u  contains the orthogonal matrix which was 
 *               used to reduce  a  to upper triangular form 
 *      ierr    int 
 *              -error indicator 
 *              ierr = 0        successful return 
 *              ierr = 1        a  has reciprocal eigenvalues 
 *              ierr = 2        a  cannot be reduced to triangular form 
 *       wrk        double precision(n) 
 * 
 * 
 *               control systems research group, dept eecs, kingston 
 *               polytechnic, penrhyn road, kingston-upon-thames, u.k. 
 * 
 *               note that the contents of  a  are overwritten by 
 *               this routine by the triangularised form of  a. 
 *               if required, a  can be re-formed from the matrix 
 *               product u' * a * u. this is not done by the routine 
 *               because the factored form of  a  may be required by 
 *               further routines. 
 * 
 */

static int c__1 = 1;
static int c__11 = 11;

int
nsp_ctrlpack_lybad (int *n, double *a, int *na, double *c__, double *x,
		    double *u, double *eps, double *wrk, int *mode, int *ierr)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, x_dim1, x_offset, u_dim1, u_offset,
    i__1, i__2, i__3;

  /* Local variables */
  int i__, j;
  double t[1];

  /* Parameter adjustments */
  --wrk;
  u_dim1 = *na;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  x_dim1 = *na;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  c_dim1 = *na;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  if (*mode == 1)
    {
      goto L10;
    }
  nsp_ctrlpack_orthes (na, n, &c__1, n, &a[a_offset], &wrk[1]);
  nsp_ctrlpack_ortran (na, n, &c__1, n, &a[a_offset], &wrk[1], &u[u_offset]);
  nsp_ctrlpack_hqror2 (na, n, &c__1, n, &a[a_offset], t, t, &u[u_offset],
		       ierr, &c__11);
  if (*ierr != 0)
    {
      goto L140;
    }
  /* 
   */
 L10:
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  x[i__ + j * x_dim1] = c__[i__ + j * c_dim1];
	  /* L15: */
	}
      x[j + j * x_dim1] *= .5;
      /* L20: */
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  i__3 = *n - i__ + 1;
	  wrk[j] =
	    C2F (ddot) (&i__3, &x[i__ + i__ * x_dim1], na,
			&u[i__ + j * u_dim1], &c__1);
	  /* L35: */
	}
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  x[i__ + j * x_dim1] = wrk[j];
	  /* L40: */
	}
    }
  i__2 = *n;
  for (j = 1; j <= i__2; ++j)
    {
      i__1 = *n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  wrk[i__] =
	    C2F (ddot) (n, &u[i__ * u_dim1 + 1], &c__1,
			&x[j * x_dim1 + 1], &c__1);
	  /* L55: */
	}
      i__1 = *n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  x[i__ + j * x_dim1] = wrk[i__];
	  /* L60: */
	}
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *n;
      for (j = i__; j <= i__2; ++j)
	{
	  x[i__ + j * x_dim1] += x[j + i__ * x_dim1];
	  x[j + i__ * x_dim1] = x[i__ + j * x_dim1];
	  /* L70: */
	}
    }
  nsp_ctrlpack_lydsr (n, &a[a_offset], na, &x[x_offset], ierr);
  if (*ierr != 0)
    {
      return 0;
    }
  i__2 = *n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      x[i__ + i__ * x_dim1] *= .5;
      /* L80: */
    }
  i__2 = *n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__3 = *n - i__ + 1;
	  wrk[j] =
	    C2F (ddot) (&i__3, &x[i__ + i__ * x_dim1], na,
			&u[j + i__ * u_dim1], na);
	  /* L95: */
	}
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  x[i__ + j * x_dim1] = wrk[j];
	  /* L100: */
	}
    }
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  wrk[i__] =
	    C2F (ddot) (n, &u[i__ + u_dim1], na, &x[j * x_dim1 + 1], &c__1);
	  /* L115: */
	}
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  x[i__ + j * x_dim1] = wrk[i__];
	  /* L120: */
	}
    }
  i__2 = *n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      i__1 = *n;
      for (j = i__; j <= i__1; ++j)
	{
	  x[i__ + j * x_dim1] += x[j + i__ * x_dim1];
	  x[j + i__ * x_dim1] = x[i__ + j * x_dim1];
	  /* L130: */
	}
    }
  /* 
   */
  goto L150;
 L140:
  *ierr = 2;
 L150:
  return 0;
}				/* lybad_ */
