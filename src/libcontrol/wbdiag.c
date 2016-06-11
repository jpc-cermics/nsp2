/* wbdiag.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/* Table of constant values */

static int c__1 = 1;
static int c__11 = 11;
static int c__0 = 0;

int
nsp_ctrlpack_wbdiag (int *lda, int *n, double *ar, double *ai, double *rmax,
		     double *er, double *ei, int *bs, double *xr, double *xi,
		     double *yr, double *yi, double *scale, int *job,
		     int *fail)
{
  /* Initialized data */

  static double zero = 0.;
  static double one = 1.;
  static double mone = -1.;

  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, xr_dim1, xr_offset, xi_dim1,
    xi_offset, yr_dim1, yr_offset, yi_dim1, yi_offset, i__1, i__2, i__3, i__4;
  double d__1, d__2;

  /* Local variables */
  int l11pi;
  double temp, c__, d__;
  int i__, j, k, l;
  int fails;
  int l11, l22, ii;
  int km1;
  int da11, da22;
  int igh;
  double cav, eps, rav;
  int err, low, l22m1;

  /* 
   *!purpose 
   *  wbdiag reduces a matrix a to block diagonal form by first 
   *  reducing it to triangular form by comqr3 and then by 
   *  solving the matrix equation -a11*p+p*a22=a12 to introduce zeros 
   *  above the diagonal. 
   *  right transformation is factored : p*d*u*y ;where: 
   *    p is a permutation matrix and d positive diagonal matrix, 
   *    p and d are given by scale 
   *    u is orthogonal and y block upper triangular with identity 
   *    blocks on the diagonal 
   * 
   *!calling sequence 
   * 
   *    subroutine wbdiag(lda,n,ar,ai,rmaxr,er,ei,bs,xr,xi, 
   *   * yr,yi,scale,job,fail) 
   * 
   *    int lda, n, bs, job 
   *    double precision ar,ai,er,ei,xr,xi,yr,yi,rmax,scale(n) 
   *    dimension ar(lda,n),ai(lda,n) 
   *    dimension xr(lda,n),xi(lda,n),yr(lda,n),yi(lda,n), 
   *              er(n),ei(n),bs(n) 
   *    int fail 
   * 
   *  starred parameters are altered by the subroutine 
   * 
   * 
   * *ar,ai    an array that initially contains the n x n matrix 
   *           to be reduced. on return,  see job 
   * 
   *  lda      the leading dimension of array a. and array x,y. 
   * 
   *  n        the order of the matrices a,x,y 
   * 
   *  rmax     the maximum size allowed for any element of the 
   *           transformations. 
   * 
   * *er       a singly subscripted real array containing the real 
   *           parts of the eigenvalues. 
   * 
   * *ei       a singly subscripted real array containg the imaginary 
   *           parts of the eigenvalues. 
   * 
   * *bs       a singly subscripted int array that contains block 
   *           structure information.  if there is a block of order 
   *           k starting at a(l,l) in the output matrix a, then 
   *           bs(l) contains the positive int k, bs(l+1) contains 
   *           -(k-1), bs(l+2) = -(k-2), ..., bs(l+k-1) = -1. 
   *           thus a positive int in the l-th entry of bs 
   *           indicates a new block of order bs(l) starting at a(l,l). 
   * 
   * *xr,xi    contains,  either right reducing transformation u*y, 
   *           either orthogonal tranformation u (see job) 
   * 
   * *yr,yi    contains the inverse reducing matrix transformation 
   *              or y matrix (see job) 
   * 
   * *scale    contains the scale factor and definitions of p and d 
   *           size(n) 
   * 
   *  job      int parametre specifying outputed transformations 
   *           job=0 : a contains block diagonal form 
   *                   x right transformation 
   *                   y dummy variable 
   *           job=1:like job=0 and y contain x**-1 
   *           job=2 a contains block diagonal form 
   *                 x contains u and y contain y 
   *           job=3: a contains: 
   *                     -block diagonal form in the diagonal blocks 
   *                     -a factorisation of y in the upper triangular 
   *                  x contains u 
   *                  y dummy 
   * *fail     a int variable which is false on normal return and 
   *           true if there is any error in wbdiag. 
   * 
   * 
   *!auxiliary routines 
   *    corth cortr comqr3 cbal balbak (eispack) 
   *    wexchn  (eispack.extensions) 
   *    dset ddot (blas) 
   *    wshrsl dad 
   * 
   *! 
   *    Copyright INRIA 
   * 
   * 
   *     character*100 cw 
   *     int iw(200) 
   */
  /* Parameter adjustments */
  --scale;
  yi_dim1 = *lda;
  yi_offset = yi_dim1 + 1;
  yi -= yi_offset;
  yr_dim1 = *lda;
  yr_offset = yr_dim1 + 1;
  yr -= yr_offset;
  xi_dim1 = *lda;
  xi_offset = xi_dim1 + 1;
  xi -= xi_offset;
  xr_dim1 = *lda;
  xr_offset = xr_dim1 + 1;
  xr -= xr_offset;
  --bs;
  --ei;
  --er;
  ai_dim1 = *lda;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *lda;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;

  /* Function Body */
  /* 
   * 
   */
  *fail = TRUE;
  /* 
   *    compute l1 norm of a 
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
	  temp = temp + (d__1 = ar[i__ + j * ar_dim1], Abs (d__1)) + (d__2 =
								      ai[i__ +
									 j *
									 ai_dim1],
								      Abs
								      (d__2));
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
  nsp_ctrlpack_cbal (lda, n, &ar[ar_offset], &ai[ai_offset], &low, &igh,
		     &scale[1]);
  nsp_ctrlpack_corth (lda, n, &c__1, n, &ar[ar_offset], &ai[ai_offset],
		      &er[1], &ei[1]);
  nsp_ctrlpack_cortr (lda, n, &c__1, n, &ar[ar_offset], &ai[ai_offset],
		      &er[1], &ei[1], &xr[xr_offset], &xi[xi_offset]);
  /* 
   *    convert a to upper triangular form by qr method. 
   * 
   */
  nsp_ctrlpack_comqr3 (lda, n, &c__1, n, &ar[ar_offset], &ai[ai_offset],
		       &er[1], &ei[1], &xr[xr_offset], &xi[xi_offset], &err,
		       &c__11);
  /* 
   *    check to see if comqr3 failed in computing any eigenvalue 
   * 
   * 
   */
  if (err > 1)
    {
      goto L600;
    }
  /* 
   *    reduce a to block diagonal form 
   * 
   *    segment a into 4 matrices: a11, a 1 x 1 block 
   *    whose (1,1)-element is at a(l11,l11))  a22, a 1 x 1 
   *    block whose (1,1)-element is at a(l22,l22)) a12, 
   *    a 1 x 1 block whose (1,1)-element is at a(l11,l22)) 
   *    and a21, a 1 x 1 block = 0 whose (1,1)- 
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
  /*     call wmdsp(ar,ai,n,n,n,10,1,80,6,cw,iw) 
   */
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
  l22 = l11 + 1;
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
  goto L80;
 L100:
  /* 
   * 
   *          loop to move the eigenvalue just located 
   *          into first position of block a22. 
   * 
   * 
   *            the block we're moving to add to a11 is a 1 x 1 
   * 
   */
 L110:
  if (k == l22)
    {
      goto L230;
    }
  km1 = k - 1;
  nsp_ctrlpack_wexchn (&ar[ar_offset], &ai[ai_offset], &xr[xr_offset],
		       &xi[xi_offset], n, &km1, fail, lda, lda);
  if (*fail)
    {
      goto L600;
    }
  temp = er[k];
  er[k] = er[km1];
  er[km1] = temp;
  temp = ei[k];
  ei[k] = ei[km1];
  ei[km1] = temp;
  k = km1;
  if (k <= l22)
    {
      goto L230;
    }
  goto L110;
  /* 
   */
 L230:
  ++da11;
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
	  ar[i__ + j * ar_dim1] = ar[j + i__ * ar_dim1];
	  ai[i__ + j * ai_dim1] = ai[j + i__ * ai_dim1];
	  /* L250: */
	}
      /* L260: */
    }
  /* 
   * 
   *      convert a11 to lower quasi-triangular and multiply it by -1 and 
   *      a12 appropriately (for solving -a11*p+p*a22=a12). 
   * 
   *     write(6,'(''da11='',i2,''da22='',i2)') da11,da22 
   *     write(6,'(''a'')') 
   *     call wmdsp(ar,ai,n,n,n,10,1,80,6,cw,iw) 
   */
  nsp_calpack_dad (&ar[ar_offset], lda, &l11, &l22m1, &l11, n, &one, &c__0);
  nsp_calpack_dad (&ar[ar_offset], lda, &l11, &l22m1, &l11, &l22m1, &mone,
		   &c__1);
  nsp_calpack_dad (&ai[ai_offset], lda, &l11, &l22m1, &l11, n, &one, &c__0);
  nsp_calpack_dad (&ai[ai_offset], lda, &l11, &l22m1, &l11, &l22m1, &mone,
		   &c__1);
  /* 
   *      solve -a11*p + p*a22 = a12. 
   * 
   */
  nsp_ctrlpack_wshrsl (&ar[l11 + l11 * ar_dim1], &ai[l11 + l11 * ai_dim1],
		       &ar[l22 + l22 * ar_dim1], &ai[l22 + l22 * ai_dim1],
		       &ar[l11 + l22 * ar_dim1], &ai[l11 + l22 * ai_dim1],
		       &da11, &da22, lda, lda, lda, &eps, rmax, &fails);
  if (!fails)
    {
      goto L290;
    }
  /* 
   *      change a11 back to upper quasi-triangular. 
   * 
   */
  nsp_calpack_dad (&ar[ar_offset], lda, &l11, &l22m1, &l11, &l22m1, &one,
		   &c__1);
  nsp_calpack_dad (&ar[ar_offset], lda, &l11, &l22m1, &l11, &l22m1, &mone,
		   &c__0);
  nsp_calpack_dad (&ai[ai_offset], lda, &l11, &l22m1, &l11, &l22m1, &one,
		   &c__1);
  nsp_calpack_dad (&ai[ai_offset], lda, &l11, &l22m1, &l11, &l22m1, &mone,
		   &c__0);
  /*     write(6,'(''failed a'')') 
   *     call wmdsp(ar,ai,n,n,n,10,1,80,6,cw,iw) 
   * 
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
	  ar[j + i__ * ar_dim1] = ar[i__ + j * ar_dim1];
	  ar[i__ + j * ar_dim1] = zero;
	  ai[j + i__ * ai_dim1] = ai[i__ + j * ai_dim1];
	  ai[i__ + j * ai_dim1] = zero;
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
  nsp_calpack_dad (&ar[ar_offset], lda, &l11, &l22m1, &l11, n, &one, &c__0);
  nsp_calpack_dad (&ar[ar_offset], lda, &l11, &l22m1, &l11, &l22m1, &mone,
		   &c__1);
  nsp_calpack_dad (&ai[ai_offset], lda, &l11, &l22m1, &l11, n, &one, &c__0);
  nsp_calpack_dad (&ai[ai_offset], lda, &l11, &l22m1, &l11, &l22m1, &mone,
		   &c__1);
  /*     write(6,'(''not failed a'')') 
   *     call wmdsp(ar,ai,n,n,n,10,1,80,6,cw,iw) 
   * 
   * 
   *    store block size in array da11s. 
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
	  yr[i__ + j * yr_dim1] = xr[j + i__ * xr_dim1];
	  yi[i__ + j * yi_dim1] = -xi[j + i__ * xi_dim1];
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
	  i__4 = *n - l22m1;
	  yr[i__ + j * yr_dim1] =
	    yr[i__ + j * yr_dim1] - C2F (ddot) (&i__3,
						&ar[i__ +
						    l22 * ar_dim1],
						lda,
						&yr[l22 + j * yr_dim1],
						&c__1) +
	    C2F (ddot) (&i__4, &ai[i__ + l22 * ai_dim1], lda,
			&yi[l22 + j * yi_dim1], &c__1);
	  i__3 = *n - l22m1;
	  i__4 = *n - l22m1;
	  yi[i__ + j * yi_dim1] =
	    yi[i__ + j * yi_dim1] - C2F (ddot) (&i__3,
						&ar[i__ +
						    l22 * ar_dim1],
						lda,
						&yi[l22 + j * yi_dim1],
						&c__1) -
	    C2F (ddot) (&i__4, &ai[i__ + l22 * ai_dim1], lda,
			&yr[l22 + j * yr_dim1], &c__1);
	  /* L430: */
	}
    }
  goto L420;
  /* 
   *in-lines back-tranfc in-lines right transformations of xi 
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
	      yr[i__ + j * yr_dim1] *= temp;
	      yi[i__ + j * yi_dim1] *= temp;
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
	  temp = yr[j + i__ * yr_dim1];
	  yr[j + i__ * yr_dim1] = yr[j + k * yr_dim1];
	  yr[j + k * yr_dim1] = temp;
	  temp = yi[j + i__ * yi_dim1];
	  yi[j + i__ * yi_dim1] = yi[j + k * yi_dim1];
	  yi[j + k * yi_dim1] = temp;
	  /* L444: */
	}
    L445:
      ;
    }
  /* 
   * 
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
	  i__4 = l22 - l11;
	  xr[i__ + j * xr_dim1] =
	    xr[i__ + j * xr_dim1] + C2F (ddot) (&i__3,
						&xr[i__ +
						    l11 * xr_dim1],
						lda,
						&ar[l11 + j * ar_dim1],
						&c__1) -
	    C2F (ddot) (&i__4, &xi[i__ + l11 * xi_dim1], lda,
			&ai[l11 + j * ai_dim1], &c__1);
	  i__3 = l22 - l11;
	  i__4 = l22 - l11;
	  xi[i__ + j * xi_dim1] =
	    xi[i__ + j * xi_dim1] + C2F (ddot) (&i__3,
						&xr[i__ +
						    l11 * xr_dim1],
						lda,
						&ai[l11 + j * ai_dim1],
						&c__1) +
	    C2F (ddot) (&i__4, &xi[i__ + l11 * xi_dim1], lda,
			&ar[l11 + j * ar_dim1], &c__1);
	  /* L470: */
	}
    }
  goto L460;
  /* 
   */
 L480:
  nsp_ctrlpack_balbak (lda, n, &low, &igh, &scale[1], n, &xr[xr_offset]);
  nsp_ctrlpack_balbak (lda, n, &low, &igh, &scale[1], n, &xi[xi_offset]);
  goto L550;
  /* 
   *extract non orthogonal tranformation from a 
   */
 L500:
  i__2 = *n;
  for (j = 1; j <= i__2; ++j)
    {
      nsp_dset (n, &zero, &yr[j * yr_dim1 + 1], &c__1);
      nsp_dset (n, &zero, &yi[j * yi_dim1 + 1], &c__1);
      /* L510: */
    }
  i__2 = *lda + 1;
  nsp_dset (n, &one, &yr[yr_dim1 + 1], &i__2);
  i__2 = *lda + 1;
  nsp_dset (n, &one, &yi[yi_dim1 + 1], &i__2);
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
	  i__4 = l22 - l11;
	  yr[i__ + j * yr_dim1] =
	    yr[i__ + j * yr_dim1] + C2F (ddot) (&i__3,
						&yr[i__ +
						    l11 * yr_dim1],
						lda,
						&ar[l11 + j * ar_dim1],
						&c__1) -
	    C2F (ddot) (&i__4, &yi[i__ + l11 * yi_dim1], lda,
			&ai[l11 + j * ai_dim1], &c__1);
	  i__3 = l22 - l11;
	  i__4 = l22 - l11;
	  yi[i__ + j * yi_dim1] =
	    yi[i__ + j * yi_dim1] + C2F (ddot) (&i__3,
						&yr[i__ +
						    l11 * yr_dim1],
						lda,
						&ai[l11 + j * ai_dim1],
						&c__1) +
	    C2F (ddot) (&i__4, &yi[i__ + l11 * yi_dim1], lda,
			&ar[l11 + j * ar_dim1], &c__1);
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
      nsp_dset (&i__2, &zero, &ar[j + l22 * ar_dim1], lda);
      i__2 = *n - l22m1;
      nsp_dset (&i__2, &zero, &ar[l22 + j * ar_dim1], &c__1);
      i__2 = *n - l22m1;
      nsp_dset (&i__2, &zero, &ai[j + l22 * ai_dim1], lda);
      i__2 = *n - l22m1;
      nsp_dset (&i__2, &zero, &ai[l22 + j * ai_dim1], &c__1);
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
  return 0;
}				/* wbdiag_ */
