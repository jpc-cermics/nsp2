/* sybad.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* Table of constant values */

static int c__1 = 1;
static int c__11 = 11;

int
nsp_ctrlpack_sybad (int *n, int *m, double *a, int *na, double *b, int *nb,
		    double *c__, int *nc, double *u, double *v, double *eps,
		    double *wrk, int *mode, int *ierr)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, u_dim1, u_offset, b_dim1, b_offset,
    v_dim1, v_offset, i__1, i__2;

  /* Local variables */
  int i__, j;
  double t;
  double tt[1];

  /* 
   * 
   *!purpose 
   * 
   *       to solve the double precision matrix equation 
   *              a*x*b - x = c 
   * 
   *! calling sequence 
   *      subroutine sybad(n,m,a,na,b,nb,c,nc,u,v,eps,wrk,mode,ierr) 
   * 
   *       int n,na,mode,ierr 
   *       double precision a(na,n),c(nc,m),u(na,n),wrk(Max(n,m)) 
   *       double precision b(nb,m),v(nb,m) 
   * 
   *arguments in 
   * 
   *      n        int 
   *               -the dimension of a. 
   * 
   *      m        imteger 
   *               -the dimension of b. 
   * 
   *      a        double precision(n,n) 
   *               -the coefficient matrix  a  of the equation. on 
   *               exit, a  is overwritten, but see  comments  below. 
   * 
   *      na       int 
   *               -the declared first dimension of  a and  u 
   * 
   *      b        double precision(m,m) 
   *               -the coefficient matrix  b  of the equation. on 
   *               exit, b  is overwritten, but see  comments  below. 
   * 
   *      nb       int 
   *               -the declared first dimension of  b and  v 
   * 
   *      c        double precision(n,n) 
   *               -the coefficient matrix  c  of the equation. 
   * 
   *      nc       int 
   *               -the declared first dimension of  c 
   * 
   *      mode     int 
   * 
   *               - mode = 0  if  a  has not already been reduced to 
   *                               upper triangular form 
   * 
   *               - mode = 1  if  a  has been reduced to triangular form 
   *                        by (e.g.) a previous call to this routine 
   * 
   *arguments out 
   * 
   *      a        double precision(n,n) 
   *               -on exit, a  contains the transformed upper 
   *               triangular form of a.   (see comments below) 
   * 
   *      b        double precision(n,n) 
   *               -on exit, b  contains the transformed lower 
   *               triangular form of b.   (see comments below) 
   * 
   *      c        double precision(n,m) 
   *               -the solution matrix 
   * 
   *      u        double precision(n,n) 
   *               - u  contains the orthogonal matrix which was 
   *               used to reduce  a  to upper triangular form 
   * 
   *      v        double precision(m,m) 
   *               - v  contains the orthogonal matrix which was 
   *               used to reduce  b  to lower triangular form 
   * 
   *      ierr    int 
   *              -error indicator 
   * 
   *              ierr = 0        successful return 
   * 
   *              ierr = 1        a  has reciprocal eigenvalues 
   * 
   *              ierr = 2        a  cannot be reduced to triangular form 
   * 
   *working space 
   * 
   *       wrk        double precision (Max(n,m)) 
   * 
   *!originator 
   * 
   *    Serge Steer Inria 1987 
   *    Copyright SLICOT 
   *!comments 
   *               note that the contents of  a  are overwritten by 
   *               this routine by the triangularised form of  a. 
   *               if required, a  can be re-formed from the matrix 
   *               product u' * a * u. this is not done by the routine 
   *               because the factored form of  a  may be required by 
   *               further routines. 
   * 
   *!method 
   * 
   *       this routine is a modification of the subroutine d2lyb, 
   *       written and discussed by a.y.barraud (see reference). 
   * 
   *!reference 
   * 
   *       a.y.barraud 
   *       "a numerical algorithm to solve  a' * x * a  -  x  =  q  ", 
   *       ieee trans. automat. control, vol. ac-22, 1977, pp 883-885 
   * 
   *!auxiliary routines 
   * 
   *      ddot (blas) 
   *      orthes,ortran (eispack) 
   *      sgefa,sgesl   (linpack) 
   * 
   *! 
   * 
   * 
   *       internal variables: 
   * 
   * 
   */
  /* Parameter adjustments */
  u_dim1 = *na;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  v_dim1 = *nb;
  v_offset = v_dim1 + 1;
  v -= v_offset;
  b_dim1 = *nb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  c_dim1 = *nc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  --wrk;

  /* Function Body */
  if (*mode == 1)
    {
      goto L30;
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = i__;
      for (j = 1; j <= i__2; ++j)
	{
	  t = a[i__ + j * a_dim1];
	  a[i__ + j * a_dim1] = a[j + i__ * a_dim1];
	  a[j + i__ * a_dim1] = t;
	  /* L10: */
	}
    }
  nsp_ctrlpack_orthes (na, n, &c__1, n, &a[a_offset], &wrk[1]);
  nsp_ctrlpack_ortran (na, n, &c__1, n, &a[a_offset], &wrk[1], &u[u_offset]);
  nsp_ctrlpack_hqror2 (na, n, &c__1, n, &a[a_offset], tt, tt, &u[u_offset],
		       ierr, &c__11);
  if (*ierr != 0)
    {
      goto L140;
    }
  nsp_ctrlpack_orthes (nb, m, &c__1, m, &b[b_offset], &wrk[1]);
  nsp_ctrlpack_ortran (nb, m, &c__1, m, &b[b_offset], &wrk[1], &v[v_offset]);
  nsp_ctrlpack_hqror2 (nb, m, &c__1, m, &b[b_offset], tt, tt, &v[v_offset],
		       ierr, &c__11);
  if (*ierr != 0)
    {
      goto L140;
    }
  /* 
   */
 L30:
  i__2 = *n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      i__1 = *m;
      for (j = 1; j <= i__1; ++j)
	{
	  wrk[j] =
	    C2F (ddot) (m, &c__[i__ + c_dim1], nc, &v[j * v_dim1 + 1], &c__1);
	  /* L35: */
	}
      i__1 = *m;
      for (j = 1; j <= i__1; ++j)
	{
	  c__[i__ + j * c_dim1] = wrk[j];
	  /* L40: */
	}
    }
  i__1 = *m;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  wrk[i__] =
	    C2F (ddot) (n, &u[i__ * u_dim1 + 1], &c__1,
			&c__[j * c_dim1 + 1], &c__1);
	  /* L55: */
	}
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  c__[i__ + j * c_dim1] = wrk[i__];
	  /* L60: */
	}
    }
  /* 
   */
  nsp_ctrlpack_sydsr (n, m, &a[a_offset], na, &b[b_offset], nb,
		      &c__[c_offset], nc, ierr);
  if (*ierr != 0)
    {
      return 0;
    }
  /* 
   */
  i__2 = *n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      i__1 = *m;
      for (j = 1; j <= i__1; ++j)
	{
	  wrk[j] = C2F (ddot) (m, &c__[i__ + c_dim1], nc, &v[j + v_dim1], nb);
	  /* L95: */
	}
      i__1 = *m;
      for (j = 1; j <= i__1; ++j)
	{
	  c__[i__ + j * c_dim1] = wrk[j];
	  /* L100: */
	}
    }
  i__1 = *m;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  wrk[i__] =
	    C2F (ddot) (n, &u[i__ + u_dim1], na, &c__[j * c_dim1 + 1], &c__1);
	  /* L115: */
	}
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  c__[i__ + j * c_dim1] = wrk[i__];
	  /* L120: */
	}
    }
  /* 
   */
  goto L150;
 L140:
  *ierr = 2;
 L150:
  return 0;
}				/* sybad_ */
