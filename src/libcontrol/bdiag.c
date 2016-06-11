/* bdiag.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/* 
 *  dbdiag reduces a matrix a to block diagonal form by first 
 *  reducing it to quasi-triangular form by hqror2 and then by 
 *  solving the matrix equation -a11*p+p*a22=a12 to introduce zeros 
 *  above the diagonal. 
 *  right transformation is factored : p*d*u*y ;where: 
 *    p is a permutation matrix and d positive diagonal matrix 
 *    u is orthogonal and y block upper triangular with identity 
 *    blocks on the diagonal 
 * 
 *    subroutine bdiag(lda,n,a,epsshr,rmax,er,ei,bs,x,xi,scale,job,fail) 
 * 
 *    int lda, n,  bs, job 
 *    double precision a,er,ei,x,xi,rmax,epsshr,scale 
 *    dimension a(lda,n),x(lda,n),xi(lda,n),er(n),ei(n),bs(n) 
 *    dimension scale(n) 
 *    int fail 
 * 
 *  starred parameters are altered by the subroutine 
 * *a        an array that initially contains the m x n matrix 
 *           to be reduced. on return,  see job 
 *  lda      the leading dimension of array a. and array x,xi. 
 *  n        the order of the matrices a,x,xi 
 *  epsshr   the minimal conditionnement allowed for linear sytems 
 *  rmax     the maximum size allowed for any element of the 
 *           transformations. 
 * *er       a singly subscripted real array containing the real 
 *           parts of the eigenvalues. 
 * *ei       a singly subscripted real array containg the imaginary 
 *           parts of the eigenvalues. 
 * *bs       a singly subscripted int array that contains block 
 *           structure information.  if there is a block of order 
 *           k starting at a(l,l) in the output matrix a, then 
 *           bs(l) contains the positive int k, bs(l+1) contains 
 *           -(k-1), bs(la+2) = -(k-2), ..., bs(l+k-1) = -1. 
 *           thus a positive int in the l-th entry of bs 
 *           indicates a new block of order bs(l) starting at a(l,l). 
 * *x        contains,  either right reducing transformation u*y, 
 *           either orthogonal tranformation u (see job) 
 * *xi       xi contains the inverse reducing matrix transformation 
 *              or y matrix (see job) 
 * *scale    contains the scale factor and definitions of p size(n) 
 *  job      int parametre specifying outputed transformations 
 *           job=0 : a contains block diagonal form 
 *                   x right transformation 
 *                   xi dummy variable 
 *           job=1:like job=0 and xi contain x**-1 
 *           job=2 a contains block diagonal form 
 *                 x contains u and xi contain y 
 *           job=3: a contains: 
 *                     -block diagonal form in the diagonal blocks 
 *                     -a factorisation of y in the upper triangular 
 *                  x contains u 
 *                  xi dummy 
 * *fail     a int variable which is false on normal return and 
 *           true if there is any error in bdiag. 
 */

static int c__1 = 1;
static int c__21 = 21;
static int c__2 = 2;
static int c__0 = 0;

int
nsp_ctrlpack_bdiag (int *lda, int *n, double *a, double *epsshr, double *rmax,
		    double *er, double *ei, int *bs, double *x, double *xi,
		    double *scale, int *job, int *fail)
{
  /* Initialized data */

  static double zero = 0.;
  static double one = 1.;
  static double mone = -1.;

  /* System generated locals */
  int a_dim1, a_offset, x_dim1, x_offset, xi_dim1, xi_offset, i__1, i__2,
    i__3;
  double d__1, d__2;

  /* Local variables */
  int l11pi;
  int ierr;
  double temp, c__, d__;
  int i__, j, k, l;
  int fails;
  double e1, e2;
  int l11, l22;
  int ii;
  int nk;
  int km1, km2;
  int da11, da22;
  int igh;
  double cav;
  /* int ino; */
  double eps, rav;
  int low, l22m1;

  /* Parameter adjustments */
  --scale;
  xi_dim1 = *lda;
  xi_offset = xi_dim1 + 1;
  xi -= xi_offset;
  x_dim1 = *lda;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --bs;
  --ei;
  --er;
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  /* 
   * 
   */
  *fail = FALSE;
  fails = TRUE;
  /* ino = -1;*/
  /* 
   *    compute eps the l1 norm of the a matrix 
   * 
   */
  eps = 0.;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      temp = 0.;
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  temp += (d__1 = a[i__ + j * a_dim1], Abs (d__1));
	  /* L10: */
	}
      eps = Max (eps, temp);
      /* L11: */
    }
  if (eps == 0.)
    {
      eps = 1.;
    }
  eps = C2F (dlamch) ("p", 1L) * eps;
  /* 
   *    convert a to upper hessenberg form. 
   * 
   */
  nsp_ctrlpack_balanc (lda, n, &a[a_offset], &low, &igh, &scale[1]);
  nsp_ctrlpack_orthes (lda, n, &low, &igh, &a[a_offset], &er[1]);
  nsp_ctrlpack_ortran (lda, n, &low, &igh, &a[a_offset], &er[1],
		       &x[x_offset]);
  /* 
   *    convert a to quasi-upper triangular form by qr method. 
   * 
   */
  nsp_ctrlpack_hqror2 (lda, n, &c__1, n, &a[a_offset], &er[1], &ei[1],
		       &x[x_offset], &ierr, &c__21);
  /* 
   *    check to see if hqror2 failed in computing any eigenvalue 
   * 
   * 
   */
  if (ierr > 1)
    {
      goto L600;
    }
  /* 
   *    reduce a to block diagonal form 
   * 
   * 
   *    segment a into 4 matrices: a11, a da11 x da11 block 
   *    whose (1,1)-element is at a(l11,l11))  a22, a da22 x da22 
   *    block whose (1,1)-element is at a(l22,l22)) a12, 
   *    a da11 x da22 block whose (1,1)-element is at a(l11,l22)) 
   *    and a21, a da22 x da11 block = 0 whose (1,1)- 
   *    element is at a(l22,l11). 
   * 
   * 
   * 
   *    this loop uses l11 as loop index and splits off a block 
   *    starting at a(l11,l11). 
   * 
   * 
   */
  l11 = 1;
 L40:
  if (l11 > *n)
    {
      goto L350;
    }
  l22 = l11;
  /* 
   *      this loop uses da11 as loop variable and attempts to split 
   *      off a block of size da11 starting at a(l11,l11) 
   * 
   */
 L50:
  if (l22 != l11)
    {
      goto L60;
    }
  da11 = 1;
  if (l11 == *n)
    {
      goto L51;
    }
  if ((d__1 = a[l11 + 1 + l11 * a_dim1], Abs (d__1)) > eps)
    {
      da11 = 2;
    }
 L51:
  l22 = l11 + da11;
  l22m1 = l22 - 1;
  goto L240;
 L60:
  /* 
   * 
   *          compute the average of the eigenvalues in a11 
   * 
   */
  rav = zero;
  cav = zero;
  i__1 = l22m1;
  for (i__ = l11; i__ <= i__1; ++i__)
    {
      rav += er[i__];
      cav += (d__1 = ei[i__], Abs (d__1));
      /* L70: */
    }
  rav /= (double) da11;
  cav /= (double) da11;
  /* 
   *          loop on eigenvalues of a22 to find the one closest to the av 
   * 
   *Computing 2nd power 
   */
  d__1 = rav - er[l22];
  /*Computing 2nd power 
   */
  d__2 = cav - ei[l22];
  d__ = d__1 * d__1 + d__2 * d__2;
  k = l22;
  l = l22 + 1;
  if (l22 == *n)
    {
      goto L71;
    }
  if ((d__1 = a[l22 + 1 + l22 * a_dim1], Abs (d__1)) > eps)
    {
      l = l22 + 2;
    }
 L71:
 L80:
  if (l > *n)
    {
      goto L100;
    }
  /*Computing 2nd power 
   */
  d__1 = rav - er[l];
  /*Computing 2nd power 
   */
  d__2 = cav - ei[l];
  c__ = d__1 * d__1 + d__2 * d__2;
  if (c__ >= d__)
    {
      goto L90;
    }
  k = l;
  d__ = c__;
 L90:
  ++l;
  if (l > *n)
    {
      goto L100;
    }
  if ((d__1 = a[l + (l - 1) * a_dim1], Abs (d__1)) > eps)
    {
      ++l;
    }
  goto L80;
 L100:
  /* 
   * 
   *          loop to move the eigenvalue just located 
   *          into first position of block a22. 
   * 
   */
  if (k == *n)
    {
      goto L105;
    }
  if ((d__1 = a[k + 1 + k * a_dim1], Abs (d__1)) > eps)
    {
      goto L150;
    }
  /* 
   *            the block we're moving to add to a11 is a 1 x 1 
   * 
   */
 L105:
  nk = 1;
 L110:
  if (k == l22)
    {
      goto L230;
    }
  km1 = k - 1;
  if ((d__1 = a[km1 + (k - 2) * a_dim1], Abs (d__1)) < eps)
    {
      goto L140;
    }
  /* 
   *            we're swapping the closest block with a 2 x 2 
   * 
   */
  km2 = k - 2;
  nsp_ctrlpack_exch (lda, n, &a[a_offset], &x[x_offset], &km2, &c__2, &c__1);
  /* 
   *            try to split this block into 2 real eigenvalues 
   * 
   */
  nsp_ctrlpack_split (&a[a_offset], &x[x_offset], n, &km1, &e1, &e2, lda,
		      lda);
  if (a[k + km1 * a_dim1] == zero)
    {
      goto L120;
    }
  /* 
   *            block is still complex. 
   * 
   */
  er[km2] = er[k];
  ei[km2] = zero;
  er[k] = e1;
  er[km1] = e1;
  ei[km1] = e2;
  ei[k] = -e2;
  goto L130;
  /* 
   *            complex block split into two real eigenvalues. 
   * 
   */
 L120:
  er[km2] = er[k];
  er[km1] = e1;
  er[k] = e2;
  ei[km2] = zero;
  ei[km1] = zero;
 L130:
  k = km2;
  if (k <= l22)
    {
      goto L230;
    }
  goto L110;
  /* 
   * 
   *            we're swapping the closest block with a 1 x 1. 
   * 
   */
 L140:
  nsp_ctrlpack_exch (lda, n, &a[a_offset], &x[x_offset], &km1, &c__1, &c__1);
  temp = er[k];
  er[k] = er[km1];
  er[km1] = temp;
  k = km1;
  if (k <= l22)
    {
      goto L230;
    }
  goto L110;
  /* 
   *            the block we're moving to add to a11 is a 2 x 2. 
   * 
   */
 L150:
  nk = 2;
 L160:
  if (k == l22)
    {
      goto L230;
    }
  km1 = k - 1;
  if ((d__1 = a[km1 + (k - 2) * a_dim1], Abs (d__1)) < eps)
    {
      goto L190;
    }
  /* 
   *            we're swapping the closest block with a 2 x 2 block. 
   * 
   */
  km2 = k - 2;
  nsp_ctrlpack_exch (lda, n, &a[a_offset], &x[x_offset], &km2, &c__2, &c__2);
  /* 
   *            try to split swapped block into two reals. 
   * 
   */
  nsp_ctrlpack_split (&a[a_offset], &x[x_offset], n, &k, &e1, &e2, lda, lda);
  er[km2] = er[k];
  er[km1] = er[k + 1];
  ei[km2] = ei[k];
  ei[km1] = ei[k + 1];
  if (a[k + 1 + k * a_dim1] == zero)
    {
      goto L170;
    }
  /* 
   *            still complex block. 
   * 
   */
  er[k] = e1;
  er[k + 1] = e1;
  ei[k] = e2;
  ei[k + 1] = -e2;
  goto L180;
  /* 
   *            two real roots. 
   * 
   */
 L170:
  er[k] = e1;
  er[k + 1] = e2;
  ei[k] = zero;
  ei[k + 1] = zero;
 L180:
  k = km2;
  if (k == l22)
    {
      goto L210;
    }
  goto L160;
  /* 
   *            we're swapping the closest block with a 1 x 1. 
   * 
   */
 L190:
  nsp_ctrlpack_exch (lda, n, &a[a_offset], &x[x_offset], &km1, &c__1, &c__2);
  er[k + 1] = er[km1];
  er[km1] = er[k];
  ei[km1] = ei[k];
  ei[k] = ei[k + 1];
  ei[k + 1] = zero;
  goto L200;
  /* 
   */
 L200:
  k = km1;
  if (k == l22)
    {
      goto L210;
    }
  goto L160;
  /* 
   *            try to split relocated complex block. 
   * 
   */
 L210:
  nsp_ctrlpack_split (&a[a_offset], &x[x_offset], n, &k, &e1, &e2, lda, lda);
  if (a[k + 1 + k * a_dim1] == zero)
    {
      goto L220;
    }
  /* 
   *            still complex. 
   * 
   */
  er[k] = e1;
  er[k + 1] = e1;
  ei[k] = e2;
  ei[k + 1] = -e2;
  goto L230;
  /* 
   *            split into two real eigenvalues. 
   * 
   */
 L220:
  er[k] = e1;
  er[k + 1] = e2;
  ei[k] = zero;
  ei[k + 1] = zero;
  /* 
   */
 L230:
  da11 += nk;
  l22 = l11 + da11;
  l22m1 = l22 - 1;
 L240:
  if (l22 > *n)
    {
      goto L290;
    }
  /* 
   *      attempt to split off a block of size da11. 
   * 
   */
  da22 = *n - l22 + 1;
  /* 
   *      save a12 in its transpose form in block a21. 
   * 
   */
  i__1 = l22m1;
  for (j = l11; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = l22; i__ <= i__2; ++i__)
	{
	  a[i__ + j * a_dim1] = a[j + i__ * a_dim1];
	  /* L250: */
	}
      /* L260: */
    }
  /* 
   * 
   *      convert a11 to lower quasi-triangular and multiply it by -1 and 
   *      a12 appropriately (for solving -a11*p+p*a22=a12). 
   * 
   */
  nsp_calpack_dad (&a[a_offset], lda, &l11, &l22m1, &l11, n, &one, &c__0);
  nsp_calpack_dad (&a[a_offset], lda, &l11, &l22m1, &l11, &l22m1, &mone,
		   &c__1);
  /* 
   *      solve -a11*p + p*a22 = a12. 
   * 
   */
  nsp_ctrlpack_shrslv (&a[l11 + l11 * a_dim1], &a[l22 + l22 * a_dim1],
		       &a[l11 + l22 * a_dim1], &da11, &da22, lda, lda, lda,
		       &eps, epsshr, rmax, &fails);
  if (!fails)
    {
      goto L290;
    }
  /* 
   *      change a11 back to upper quasi-triangular. 
   * 
   */
  nsp_calpack_dad (&a[a_offset], lda, &l11, &l22m1, &l11, &l22m1, &one,
		   &c__1);
  nsp_calpack_dad (&a[a_offset], lda, &l11, &l22m1, &l11, &l22m1, &mone,
		   &c__0);
  /* 
   *      was unable to solve for p - try again 
   * 
   * 
   *      move saved a12 back into its correct position. 
   * 
   */
  i__1 = l22m1;
  for (j = l11; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = l22; i__ <= i__2; ++i__)
	{
	  a[j + i__ * a_dim1] = a[i__ + j * a_dim1];
	  a[i__ + j * a_dim1] = zero;
	  /* L270: */
	}
      /* L280: */
    }
  /* 
   * 
   */
  goto L50;
 L290:
  /* 
   *    change solution to p to proper form. 
   * 
   */
  if (l22 > *n)
    {
      goto L300;
    }
  nsp_calpack_dad (&a[a_offset], lda, &l11, &l22m1, &l11, n, &one, &c__0);
  nsp_calpack_dad (&a[a_offset], lda, &l11, &l22m1, &l11, &l22m1, &mone,
		   &c__1);
  /* 
   *    store block size in array bs. 
   * 
   */
 L300:
  bs[l11] = da11;
  j = da11 - 1;
  if (j == 0)
    {
      goto L320;
    }
  i__1 = j;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      l11pi = l11 + i__;
      bs[l11pi] = -(da11 - i__);
      /* L310: */
    }
 L320:
  l11 = l22;
  goto L40;
 L350:
  *fail = FALSE;
  /* 
   *    set transformations matrices as required 
   * 
   */
  if (*job == 3)
    {
      return 0;
    }
  /* 
   *compute inverse transformation 
   */
  if (*job != 1)
    {
      goto L450;
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  xi[i__ + j * xi_dim1] = x[j + i__ * x_dim1];
	  /* L410: */
	}
    }
  l22 = 1;
 L420:
  l11 = l22;
  l22 = l11 + bs[l11];
  if (l22 > *n)
    {
      goto L431;
    }
  l22m1 = l22 - 1;
  i__2 = l22m1;
  for (i__ = l11; i__ <= i__2; ++i__)
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__3 = *n - l22m1;
	  xi[i__ + j * xi_dim1] -=
	    C2F (ddot) (&i__3, &a[i__ + l22 * a_dim1], lda,
			&xi[l22 + j * xi_dim1], &c__1);
	  /* L430: */
	}
    }
  goto L420;
  /*in-lines back-tranfc in-lines right transformations of xi 
   */
 L431:
  if (igh != low)
    {
      i__1 = igh;
      for (j = low; j <= i__1; ++j)
	{
	  temp = 1. / scale[j];
	  i__2 = *n;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      xi[i__ + j * xi_dim1] *= temp;
	      /* L434: */
	    }
	  /* L435: */
	}
    }
  i__1 = *n;
  for (ii = 1; ii <= i__1; ++ii)
    {
      i__ = ii;
      if (i__ >= low && i__ <= igh)
	{
	  goto L445;
	}
      if (i__ < low)
	{
	  i__ = low - ii;
	}
      k = (int) scale[i__];
      if (k == i__)
	{
	  goto L445;
	}
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  temp = xi[j + i__ * xi_dim1];
	  xi[j + i__ * xi_dim1] = xi[j + k * xi_dim1];
	  xi[j + k * xi_dim1] = temp;
	  /* L444: */
	}
    L445:
      ;
    }
  /* 
   */
 L450:
  if (*job == 2)
    {
      goto L500;
    }
  /*compute right transformation 
   */
  l22 = 1;
 L460:
  l11 = l22;
  l22 = l11 + bs[l11];
  if (l22 > *n)
    {
      goto L480;
    }
  i__1 = *n;
  for (j = l22; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  i__3 = l22 - l11;
	  x[i__ + j * x_dim1] +=
	    C2F (ddot) (&i__3, &x[i__ + l11 * x_dim1], lda,
			&a[l11 + j * a_dim1], &c__1);
	  /* L470: */
	}
    }
  goto L460;
  /* 
   */
 L480:
  nsp_ctrlpack_balbak (lda, n, &low, &igh, &scale[1], n, &x[x_offset]);
  goto L550;
  /* 
   *extract non orthogonal tranformation from a 
   */
 L500:
  i__2 = *n;
  for (j = 1; j <= i__2; ++j)
    {
      nsp_dset (n, &zero, &xi[j * xi_dim1 + 1], &c__1);
      /* L510: */
    }
  i__2 = *lda + 1;
  nsp_dset (n, &one, &xi[xi_dim1 + 1], &i__2);
  l22 = 1;
 L520:
  l11 = l22;
  if (l11 > *n)
    {
      goto L550;
    }
  l22 = l11 + bs[l11];
  i__2 = *n;
  for (j = l22; j <= i__2; ++j)
    {
      i__1 = *n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  i__3 = l22 - l11;
	  xi[i__ + j * xi_dim1] +=
	    C2F (ddot) (&i__3, &xi[i__ + l11 * xi_dim1], lda,
			&a[l11 + j * a_dim1], &c__1);
	  /* L530: */
	}
    }
  goto L520;
  /* 
   *set zeros in the matrix a 
   */
 L550:
  l11 = 1;
 L560:
  l22 = l11 + bs[l11];
  if (l22 > *n)
    {
      return 0;
    }
  l22m1 = l22 - 1;
  i__1 = l22m1;
  for (j = l11; j <= i__1; ++j)
    {
      i__2 = *n - l22m1;
      nsp_dset (&i__2, &zero, &a[j + l22 * a_dim1], lda);
      i__2 = *n - l22m1;
      nsp_dset (&i__2, &zero, &a[l22 + j * a_dim1], &c__1);
      /* L570: */
    }
  l11 = l22;
  goto L560;
  /* 
   *    error return. 
   * 
   */
 L600:
  *fail = TRUE;
  /* 
   */
  return 0;
}				/* bdiag_ */
