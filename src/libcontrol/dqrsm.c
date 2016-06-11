/* dqrsm.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* 
 *
 *    sqrsm is a subroutine to compute least squares solutions 
 *    to the system 
 * 
 *    (1)               x * b = y, 
 * 
 *    which may be either under-determined or over-determined. 
 *    the relative machine precision eps is used as a tolerance 
 *    to limit the columns of x used in computing the solution. 
 *    in effect, a set of columns with a condition number 
 *    approximately rounded by 1/eps is used, the other 
 *    components of b being set to zero 
 *    if n.eq.1 and nc.gt.1 the elements in the nc-th column of b 
 *    are set to one). 
 *
 *    subroutine dqrsm(x,ldx,n,p,y,ldy,nc,b,ldb,k,jpvt,qraux,work) 
 * 
 *    on entry 
 *       x     real(ldx,p), where ldx.ge.n 
 *             x contains the nxp coefficient matrix of 
 *             the system (1), x is destroyed by sqrsm. 
 *       ldx   int 
 *             ldx is the leading dimension of the array x. 
 *       n     int 
 *             n is the number of rows of the matrix x. 
 *       p     int 
 *             p is the number of columns of the matrix x. 
 *       y     real(ldy,nc) 
 *             y contains the right hand side of the system(1). 
 *       ldy   int 
 *             ldy is the leading dimension of the array y. 
 *       nc    int 
 *             nc is the number of columns of the matrix y. 
 *       jpvt  int(p) 
 *             jpvt is an int array used by sqrdc. 
 *       qraux real(p) 
 *             qraux is an array used by sqrdc and sqrsl 
 *       work  real(p) 
 *             work is a array used by sqrdc. 
 *    on return 
 *       x     x contains the output array from sqrdc. 
 *       b     real(ldb,nc) 
 *             b contains the solution matrix. components 
 *             corresponding io columns not used are set to zero 
 *             (if n.eq.1 and nc.gt.1 the elements in the nc-th 
 *             column of b are set to one). 
 *       ldb   int 
 *       k     int 
 *             k contains the number of columns used in the 
 *             solutions. 
 *       jpvt  contains the pivot information from sqrdc. 
 *       qraux contains the array output by sqrdc. 
 * 
 *    on return the arrays x, jpvt and qraux contain the 
 *    usual output from dqrdc, so that the qr decomposition 
 *    of x with pivoting is fully available to the user. 
 *    in particular, columns jpvt(1), jpvt(2),...,jpvt(k) 
 *    were used in the solution, and the condition number 
 *    associated with those columns is estimated by 
 *    Abs(x(1,1)/x(k,k)). 
 *
 *    this subroutine is a modification of the example program sqrst, 
 *    given in the linpack users' guide: 
 *    dongarra j.j., j.r.bunch, c.b.moler and g.w.stewart. 
 *    linpack users' guide. siam, philadelphia, 1979. 
 */

static int c__1 = 1;
static int c__100 = 100;

int
nsp_ctrlpack_dqrsm (double *x, int *ldx, int *n, int *p, double *y, int *ldy,
		    int *nc, double *b, int *ldb, int *k, int *jpvt,
		    double *qraux, double *work)
{
  int x_dim1, x_offset, y_dim1, y_offset, b_dim1, b_offset, i__1, i__2;
  double d__1, d__2;
  int info, j, l, m;
  double t;
  int kk;
  double tt[1];
  int np1=0;

  /* Parameter adjustments */
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  y_dim1 = *ldy;
  y_offset = y_dim1 + 1;
  y -= y_offset;
  b_dim1 = *ldb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  --jpvt;
  --qraux;
  --work;

  /* Function Body */
  i__1 = *p;
  for (j = 1; j <= i__1; ++j)
    {
      jpvt[j] = 0;
      /* L10: */
    }
  /* 
   *    reduce x. 
   * 
   */
  nsp_ctrlpack_dqrdc (&x[x_offset], ldx, n, p, &qraux[1], &jpvt[1], &work[1],
		      &c__1);
  /* 
   *    determine which columns to use. 
   * 
   */
  *k = 0;
  m = Min (*n, *p);
  i__1 = m;
  for (kk = 1; kk <= i__1; ++kk)
    {
      t = (d__1 = x[x_dim1 + 1], Abs (d__1)) + (d__2 =
						x[kk + kk * x_dim1],
						Abs (d__2));
      if (t == (d__1 = x[x_dim1 + 1], Abs (d__1)))
	{
	  goto L30;
	}
      *k = kk;
      /* L20: */
    }
 L30:
  /* 
   *    solve the least squares problem. 
   * 
   */
  if (*k == 0)
    {
      goto L160;
    }
  if (*n >= *p || *n > 1 || *nc == 1)
    {
      goto L60;
    }
  np1 = *n + 1;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *p;
      for (kk = np1; kk <= i__2; ++kk)
	{
	  y[j + *nc * y_dim1] -= x[j + kk * x_dim1];
	  /* L40: */
	}
      /* L50: */
    }
 L60:
  i__1 = *nc;
  for (l = 1; l <= i__1; ++l)
    {
      nsp_ctrlpack_dqrsl (&x[x_offset], ldx, n, k, &qraux[1],
			  &y[l * y_dim1 + 1], tt, &y[l * y_dim1 + 1],
			  &b[l * b_dim1 + 1], tt, tt, &c__100, &info);
      /* L70: */
    }
  /* 
   *   set the unused components of b to zero and initialize jpvt 
   *   for unscrambling. 
   * 
   */
  i__1 = *p;
  for (j = 1; j <= i__1; ++j)
    {
      jpvt[j] = -jpvt[j];
      if (j <= *k)
	{
	  goto L90;
	}
      i__2 = *nc;
      for (l = 1; l <= i__2; ++l)
	{
	  b[j + l * b_dim1] = 0.;
	  /* L80: */
	}
    L90:
      ;
    }
  if (*n != 1 || *nc <= 1 || *p <= *n)
    {
      goto L110;
    }
  /* 
   *   if n.eq.1 and nc.gt.1 set the elements in the nc-th 
   *   column of b to one. 
   * 
   */
  i__1 = *p;
  for (j = np1; j <= i__1; ++j)
    {
      b[j + *nc * b_dim1] = 1.;
      /* L100: */
    }
 L110:
  /* 
   *   unscramble the solution. 
   * 
   */
  i__1 = *p;
  for (j = 1; j <= i__1; ++j)
    {
      if (jpvt[j] > 0)
	{
	  goto L150;
	}
      kk = -jpvt[j];
      jpvt[j] = kk;
    L120:
      if (kk == j)
	{
	  goto L140;
	}
      i__2 = *nc;
      for (l = 1; l <= i__2; ++l)
	{
	  t = b[j + l * b_dim1];
	  b[j + l * b_dim1] = b[kk + l * b_dim1];
	  b[kk + l * b_dim1] = t;
	  /* L130: */
	}
      jpvt[kk] = -jpvt[kk];
      kk = jpvt[kk];
      goto L120;
    L140:
    L150:
      ;
    }
 L160:
  return 0;
}				/* dqrsm_ */
