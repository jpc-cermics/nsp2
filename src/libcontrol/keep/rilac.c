/* rilac.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* Table of constant values */

static int c__1 = 1;
static int c__11 = 11;

int
nsp_ctrlpack_rilac (int *n, int *nn, double *a, int *na, double *c__,
		    double *d__, double *rcond, double *x, double *w,
		    int *nnw, double *z__, double *eps, int *iwrk,
		    double *wrk1, double *wrk2, int *ierr)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, d_dim1, d_offset, x_dim1, x_offset,
    w_dim1, w_offset, z_dim1, z_offset, i__1, i__2;

  /* Local variables */
  int fail;
  int ndim;
  int i__, j;
  double t[1];
  int ni, nj;
  int igh, low;

  /*!purpose 
   * 
   *       to solve the continuous time algebraic equation 
   * 
   *               trans(a)*x + x*a + c - x*d*x  =  0 
   * 
   *       where  trans(a)  denotes the transpose of  a . 
   * 
   *!method 
   * 
   *       the method used is laub's variant of the hamiltonian - 
   *       eigenvector approach (schur method). 
   * 
   *!reference 
   * 
   *       a.j. laub 
   *       a schur method for solving algebraic riccati equations 
   *       ieee trans. automat. control, vol. ac-25, 1980. 
   * 
   *! auxiliary routines 
   * 
   *      orthes,ortran,balanc,balbak (eispack) 
   *      dgeco,dgesl (linpack) 
   *      hqror2,inva,exchgn,qrstep 
   * 
   *! calling sequence 
   *       subroutine rilac(n,nn,a,na,c,d,rcond,x,w,nnw,z, 
   *   +                iwrk,wrk1,wrk2,ierr) 
   * 
   *       int n,nn,na,nnw,iwrk(nn),ierr 
   *       double precision a(na,n),c(na,n),d(na,n) 
   *       double precision rcond,x(na,n),w(nnw,nn),z(nnw,nn) 
   *       double precision wrk1(nn),wrk2(nn) 
   * 
   *arguments in 
   * 
   *      n       int 
   *              -the order of a,c,d and x 
   * 
   *      na      int 
   *              -the declared first dimension of a,c,d and x 
   * 
   *      nn      int 
   *              -the order of w and z 
   *                   nn = n + n 
   * 
   *      nnw     int 
   *              -the declared first dimension of w and z 
   * 
   * 
   *      a       double precision(n,n) 
   * 
   *      c       double precision(n,n) 
   * 
   *      d       double precision(n,n) 
   * 
   *arguments out 
   * 
   *      x       double precision(n,n) 
   *              - x  contains the solution matrix 
   * 
   *      w       double precision(nn,nn) 
   *              - w  contains the ordered real upper-triangular 
   *              form of the hamiltonian matrix 
   * 
   *      z       double precision(nn,nn) 
   *              - z  contains the transformation matrix which 
   *              reduces the hamiltonian matrix to the ordered 
   *              real upper-triangular form 
   * 
   *      rcond   double precision 
   *              - rcond  contains an estimate of the reciprocal 
   *              condition of the  n-th order system of algebraic 
   *              equations from which the solution matrix is obtained 
   * 
   *      ierr    int 
   *              -error indicator set on exit 
   * 
   *              ierr  =  0       successful return 
   * 
   *              ierr  =  1       the real upper triangular form of 
   *                               the hamiltonian matrix cannot be 
   *                               appropriately ordered 
   * 
   *              ierr  =  2       the hamiltonian matrix has less than n 
   *                               eigenvalues with negative real parts 
   * 
   *              ierr  =  3       the  n-th order system of linear 
   *                               algebraic equations, from which the 
   *                               solution matrix would be obtained, is 
   *                               singular to working precision 
   * 
   *              ierr  =  4       the hamiltonian matrix cannot be 
   *                               reduced to upper-triangular form 
   * 
   *working space 
   * 
   *      iwrk    int(nn) 
   * 
   *      wrk1    double precision(nn) 
   * 
   *      wrk2    double precision(nn) 
   * 
   *!originator 
   * 
   *               control systems research group, dept. eecs, kingston 
   *               polytechnic, penrhyn rd.,kingston-upon-thames, england. 
   * 
   *! comments 
   *               if there is a shortage of storage space, then the 
   *               matrices  c  and  x  can share the same locations, 
   *               but this will, of course, result in the loss of  c. 
   * 
   ******************************************************************** 
   * 
   * 
   *       local declarations: 
   * 
   * 
   * 
   *        eps is a machine dependent parameter specifying 
   *       the relative precision of realing point arithmetic. 
   * 
   *       initialise the hamiltonian matrix associated with the problem 
   * 
   */
  /* Parameter adjustments */
  --wrk2;
  --wrk1;
  --iwrk;
  x_dim1 = *na;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  d_dim1 = *na;
  d_offset = d_dim1 + 1;
  d__ -= d_offset;
  c_dim1 = *na;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  z_dim1 = *nnw;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  w_dim1 = *nnw;
  w_offset = w_dim1 + 1;
  w -= w_offset;

  /* Function Body */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      nj = *n + j;
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ni = *n + i__;
	  w[i__ + j * w_dim1] = a[i__ + j * a_dim1];
	  w[ni + j * w_dim1] = -c__[i__ + j * c_dim1];
	  w[i__ + nj * w_dim1] = -d__[i__ + j * d_dim1];
	  w[ni + nj * w_dim1] = -a[j + i__ * a_dim1];
	  /* L10: */
	}
    }
  /* 
   */
  nsp_ctrlpack_balanc (nnw, nn, &w[w_offset], &low, &igh, &wrk1[1]);
  /* 
   */
  nsp_ctrlpack_orthes (nn, nn, &c__1, nn, &w[w_offset], &wrk2[1]);
  nsp_ctrlpack_ortran (nn, nn, &c__1, nn, &w[w_offset], &wrk2[1],
		       &z__[z_offset]);
  nsp_ctrlpack_hqror2 (nn, nn, &c__1, nn, &w[w_offset], t, t, &z__[z_offset],
		       ierr, &c__11);
  if (*ierr != 0)
    {
      goto L70;
    }
  nsp_ctrlpack_inva (nn, nn, &w[w_offset], &z__[z_offset], nsp_ctrlpack_folhp,
		     eps, &ndim, &fail, &iwrk[1]);
  /* 
   */
  if (*ierr != 0)
    {
      goto L40;
    }
  if (ndim != *n)
    {
      goto L50;
    }
  /* 
   */
  nsp_ctrlpack_balbak (nnw, nn, &low, &igh, &wrk1[1], nn, &z__[z_offset]);
  /* 
   * 
   */
  nsp_ctrlpack_dgeco (&z__[z_offset], nnw, n, &iwrk[1], rcond, &wrk1[1]);
  if (*rcond < *eps)
    {
      goto L60;
    }
  /* 
   */
  i__2 = *n;
  for (j = 1; j <= i__2; ++j)
    {
      nj = *n + j;
      i__1 = *n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  x[i__ + j * x_dim1] = z__[nj + i__ * z_dim1];
	  /* L20: */
	}
      nsp_ctrlpack_dgesl (&z__[z_offset], nnw, n, &iwrk[1],
			  &x[j * x_dim1 + 1], &c__1);
      /* L30: */
    }
  goto L100;
  /* 
   */
 L40:
  *ierr = 1;
  goto L100;
  /* 
   */
 L50:
  *ierr = 2;
  goto L100;
  /* 
   */
 L60:
  *ierr = 3;
  goto L100;
  /* 
   */
 L70:
  *ierr = 4;
  goto L100;
  /* 
   */
 L100:
  return 0;
}				/* rilac_ */
