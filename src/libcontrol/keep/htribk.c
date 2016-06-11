/* htribk.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* 
 *    this subroutine forms the eigenvectors of a complex hermitian 
 *    matrix by back transforming those of the corresponding 
 *    real symmetric tridiagonal matrix determined by  htridi. 
 *
 *    subroutine htribk(nm,n,ar,ai,tau,m,zr,zi) 
 * 
 *    int i,j,k,l,m,n,nm 
 *    double precision ar(nm,n),ai(nm,n),tau(2,n),zr(nm,m),zi(nm,m) 
 *    double precision h,s,si 
 * 
 *    on input: 
 *       nm must be set to the row dimension of two-dimensional 
 *         array parameters as declared in the calling program 
 *         dimension statement; 
 *       n is the order of the matrix; 
 *       ar and ai contain information about the unitary trans- 
 *         formations used in the reduction by  htridi  in their 
 *         full lower triangles except for the diagonal of ar; 
 *       tau contains further information about the transformations; 
 *       m is the number of eigenvectors to be back transformed; 
 *       zr contains the eigenvectors to be back transformed 
 *         in its first m columns. 
 *    on output: 
 *       zr and zi contain the real and imaginary parts, 
 *         respectively, of the transformed eigenvectors 
 *         in their first m columns. 
 *    note that the last component of each returned vector 
 *    is real and that vector euclidean norms are preserved. 
 * 
 *    this subroutine is a translation of a complex analogue of 
 *    the algol procedure trbak1, num. math. 11, 181-195(1968) 
 *    by martin, reinsch, and wilkinson. 
 *    handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). 
 * 
 *    questions and comments should be directed to b. s. garbow, 
 *    applied mathematics division, argonne national laboratory 
 */

int
nsp_ctrlpack_htribk (int *nm, int *n, double *ar, double *ai, double *tau,
		     int *m, double *zr, double *zi)
{
  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, zr_dim1, zr_offset, zi_dim1,
    zi_offset, i__1, i__2, i__3;

  /* Local variables */
  double h__;
  int i__, j, k, l;
  double s, si;

  /* Parameter adjustments */
  tau -= 3;
  ai_dim1 = *nm;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *nm;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;
  zi_dim1 = *nm;
  zi_offset = zi_dim1 + 1;
  zi -= zi_offset;
  zr_dim1 = *nm;
  zr_offset = zr_dim1 + 1;
  zr -= zr_offset;

  /* Function Body */
  if (*m == 0)
    {
      goto L200;
    }
  /*    :::::::::: transform the eigenvectors of the real symmetric 
   *               tridiagonal matrix to those of the hermitian 
   *               tridiagonal matrix. :::::::::: 
   */
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      /* 
       */
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  zi[k + j * zi_dim1] = -zr[k + j * zr_dim1] * tau[(k << 1) + 2];
	  zr[k + j * zr_dim1] *= tau[(k << 1) + 1];
	  /* L50: */
	}
    }
  /* 
   */
  if (*n == 1)
    {
      goto L200;
    }
  /*    :::::::::: recover and apply the householder matrices :::::::::: 
   */
  i__2 = *n;
  for (i__ = 2; i__ <= i__2; ++i__)
    {
      l = i__ - 1;
      h__ = ai[i__ + i__ * ai_dim1];
      if (h__ == 0.)
	{
	  goto L140;
	}
      /* 
       */
      i__1 = *m;
      for (j = 1; j <= i__1; ++j)
	{
	  s = 0.;
	  si = 0.;
	  /* 
	   */
	  i__3 = l;
	  for (k = 1; k <= i__3; ++k)
	    {
	      s =
		s + ar[i__ + k * ar_dim1] * zr[k + j * zr_dim1] - ai[i__ +
								     k *
								     ai_dim1]
		* zi[k + j * zi_dim1];
	      si =
		si + ar[i__ + k * ar_dim1] * zi[k + j * zi_dim1] + ai[i__ +
								      k *
								      ai_dim1]
		* zr[k + j * zr_dim1];
	      /* L110: */
	    }
	  /*    :::::::::: double divisions avoid possible underflow :::::::::: 
	   */
	  s = s / h__ / h__;
	  si = si / h__ / h__;
	  /* 
	   */
	  i__3 = l;
	  for (k = 1; k <= i__3; ++k)
	    {
	      zr[k + j * zr_dim1] =
		zr[k + j * zr_dim1] - s * ar[i__ + k * ar_dim1] -
		si * ai[i__ + k * ai_dim1];
	      zi[k + j * zi_dim1] =
		zi[k + j * zi_dim1] - si * ar[i__ + k * ar_dim1] +
		s * ai[i__ + k * ai_dim1];
	      /* L120: */
	    }
	  /* 
	   */
	  /* L130: */
	}
      /* 
       */
    L140:
      ;
    }
  /* 
   */
 L200:
  return 0;
  /*    :::::::::: last card of htribk :::::::::: 
   */
}				/* htribk_ */
