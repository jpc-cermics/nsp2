/* hhdml.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*
 *       to pre- or post-multiply a specified block of matrix a by the 
 *       orthogonal matrix q (or its transpose), where q is the 
 *       product of householder transformations which are stored as by 
 *       linpack routine dqrdc in arrays x and qraux. 
 * 
 *       the block of a to be transformed is the (nrowbl x ncolbl) one 
 *       with offset (ioff,joff), ie with first (top left) element 
 *       (ioff + 1,joff + 1).  this is operated on by the orthogonal 
 *       (ndimq x ndimq) q = h(1) * ... * h(ktrans) or its transpose, 
 *       where ndimq equals nrowbl for pre-multiplication and ncolbl 
 *       for post-multiplication.  each householder transformation 
 *       h(l) is completely described by the sub-vector stored in the 
 *       l-th element of qraux and the sub-diagonal part of the l-th 
 *       column of the (ndimq x ktrans) x.  note finally that ktrans 
 *       .le. ndimq. 
 * 
 *       dongarra, j.j. et al 
 *       "linpack users' guide" 
 *       siam, 1979.  (chapter 9) 
 * 
 *       subroutine hhdml(ktrans,nrowa,ncola,ioff,joff,nrowbl,ncolbl, 
 *   1                    x,nx,qraux,a,na,mode,ierr) 
 * 
 *       int ktrans,nrowa,ncola,ioff,joff,nrowbl,ncolbl,nx,na 
 *       int mode,ierr 
 *       double precision x(nx,ktrans),qraux(ktrans),a(na,ncola) 
 * 
 *arguments in 
 *      ktrans   int 
 *               -the number of householder transformations making up 
 *               q; declared first dimension of qraux and second 
 *               dimension of x 
 *      nrowa    int 
 *               -the number of rows of matrix a 
 *      ncola    int 
 *               -the number of columns of matrix a 
 *      ioff     int 
 *               -the row offset of the specified block of a 
 *      joff     int 
 *               -the column offset of the specified block of a 
 *      nrowbl   int 
 *               -the number of rows of the specified block of a 
 *      ncolbl   int 
 *               -the number of columns of the specified block of a 
 *      x        double precision(ndimq,ktrans) 
 *               -the matrix containing in its sub-diagonal part most 
 *               of the information necessary to construct q 
 *      nx       int 
 *               -the declared first dimension of x.  note that 
 *               nx .ge. ndimq .ge. ktrans 
 *      qraux    double precision(ktrans) 
 *               -the remaining information necessary to construct q 
 *      a        double precision(nrowa,ncola) 
 *               -the matrix of which a specified block is to be 
 *               transformed.  note that this block is overwritten 
 *               here 
 *      na       int 
 *               -the declared first dimension of a.  note that 
 *               na .ge. nrowa 
 *      mode     int 
 *               -mode is a two-digit non-negative int: its units 
 *               digit is 0 if q is to be applied and non-zero if 
 *               qtrans is, and its tens digit is 0 for post-multipli- 
 *               cation and non-zero for pre-multiplication 
 *arguments out 
 *      a        double precision(nrowa,ncola) 
 *               -the given matrix with specified block transformed 
 *      ierr     int 
 *               -error indicator 
 *               ierr = 0        successful return 
 *               ierr = 1        nrowa .lt. (ioff + nrowbl) 
 *               ierr = 2        ncola .lt. (joff + ncolbl) 
 *               ierr = 3        ndimq does not lie in the interval 
 *                               ktrans, nx 
 *               t.w.c.williams, control systems research group, 
 *               kingston polytechnic, march 16 1982 
 */

int
nsp_ctrlpack_hhdml (int *ktrans, int *nrowa, int *ncola, int *ioff, int *joff,
		    int *nrowbl, int *ncolbl, double *x, int *nx,
		    double *qraux, double *a, int *na, int *mode, int *ierr)
{
  /* System generated locals */
  int x_dim1, x_offset, a_dim1, a_offset, i__1, i__2, i__3;

  /* Local variables */
  double diag;
  int ipre;
  double temp;
  int iback, i__, j, k, l, ndimq, lstep, ia, ja, itrans;
  double tau;

  /* Parameter adjustments */
  --qraux;
  x_dim1 = *nx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  *ierr = 0;
  /* 
   */
  if (*ioff + *nrowbl <= *nrowa)
    {
      goto L10;
    }
  /* 
   */
  *ierr = 1;
  goto L150;
  /* 
   */
 L10:
  if (*joff + *ncolbl <= *ncola)
    {
      goto L20;
    }
  /* 
   */
  *ierr = 2;
  goto L150;
  /* 
   *    itrans units digit of mode: 0 iff non-transposed q to be used 
   * 
   */
 L20:
  itrans = *mode % 10;
  /* 
   *    ipre 10 * (tens digit of mode): 0 iff post-multiplying ablk 
   * 
   */
  ipre = *mode - itrans;
  /* 
   */
  ndimq = *ncolbl;
  if (ipre != 0)
    {
      ndimq = *nrowbl;
    }
  if (*ktrans <= ndimq && ndimq <= *nx)
    {
      goto L30;
    }
  /* 
   */
  *ierr = 3;
  goto L150;
  /* 
   *    iback 1 iff precisely one of itrans, ipre .ne. 0, ie iff the 
   *    householder transformations h(l) are applied in descending order 
   * 
   */
 L30:
  iback = 0;
  if (itrans != 0)
    {
      iback = 1;
    }
  if (ipre != 0)
    {
      ++iback;
    }
  /* 
   */
  if (iback == 1)
    {
      goto L40;
    }
  /* 
   *    initialization for h(l) applied in ascending order 
   * 
   */
  l = 1;
  lstep = 1;
  goto L50;
  /* 
   *    initialization for h(l) applied in descending order 
   * 
   */
 L40:
  l = *ktrans;
  lstep = -1;
  /* 
   */
 L50:
  if (ipre == 0)
    {
      goto L100;
    }
  /* 
   *    pre-multiply appropriate block of a by h(l) in correct order 
   * 
   */
  i__1 = *ktrans;
  for (k = 1; k <= i__1; ++k)
    {
      diag = qraux[l];
      if (diag == 0.)
	{
	  goto L90;
	}
      temp = x[l + l * x_dim1];
      x[l + l * x_dim1] = diag;
      /* 
       *       operate on a one column at a time 
       * 
       */
      i__2 = *ncolbl;
      for (j = 1; j <= i__2; ++j)
	{
	  ja = *joff + j;
	  tau = 0.;
	  i__3 = *nrowbl;
	  for (i__ = l; i__ <= i__3; ++i__)
	    {
	      ia = *ioff + i__;
	      /* L60: */
	      tau += x[i__ + l * x_dim1] * a[ia + ja * a_dim1];
	    }
	  tau /= diag;
	  i__3 = *nrowbl;
	  for (i__ = l; i__ <= i__3; ++i__)
	    {
	      ia = *ioff + i__;
	      /* L70: */
	      a[ia + ja * a_dim1] -= tau * x[i__ + l * x_dim1];
	    }
	  /* 
	   */
	  /* L80: */
	}
      /* 
       */
      x[l + l * x_dim1] = temp;
    L90:
      l += lstep;
    }
  goto L150;
  /* 
   *    post-multiply appropriate block of a by h(l) in correct order 
   * 
   */
 L100:
  i__1 = *ktrans;
  for (k = 1; k <= i__1; ++k)
    {
      diag = qraux[l];
      if (diag == 0.)
	{
	  goto L140;
	}
      temp = x[l + l * x_dim1];
      x[l + l * x_dim1] = diag;
      /* 
       *       operate on a one row at a time 
       * 
       */
      i__2 = *nrowbl;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ia = *ioff + i__;
	  tau = 0.;
	  i__3 = *ncolbl;
	  for (j = l; j <= i__3; ++j)
	    {
	      ja = *joff + j;
	      /* L110: */
	      tau += a[ia + ja * a_dim1] * x[j + l * x_dim1];
	    }
	  tau /= diag;
	  i__3 = *ncolbl;
	  for (j = l; j <= i__3; ++j)
	    {
	      ja = *joff + j;
	      /* L120: */
	      a[ia + ja * a_dim1] -= tau * x[j + l * x_dim1];
	    }
	  /* 
	   */
	  /* L130: */
	}
      /* 
       */
      x[l + l * x_dim1] = temp;
    L140:
      l += lstep;
    }
  /* 
   */
 L150:
  /* 
   */
  return 0;
}				/* hhdml_ */
