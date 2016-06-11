/* reduc2.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

int
nsp_ctrlpack_reduc2 (int *n, int *ma, double *a, int *mb, double *b, int *low,
		     int *igh, double *cscale, double *wk)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, i__1, i__2;

  /* Local variables */
  double f;
  int i__, j, k, l, m, iflow, ii, is, ip1, jp1, lm1;

  /* 
   *    *****parameters: 
   * 
   *    *****local variables: 
   * 
   *    *****functions: 
   *    none 
   * 
   *    *****subroutines called: 
   *    none 
   * 
   *    --------------------------------------------------------------- 
   * 
   *    *****purpose: 
   *    this subroutine reduces, if possible, the order of the 
   *    generalized eigenvalue problem a*x = (lambda)*b*x by permuting 
   *    the rows and columns of a and b so that they each have the 
   *    form 
   *                      u  x  y 
   *                      0  c  z 
   *                      0  0  r 
   * 
   *    where u and r are upper triangular and c, x, y, and z are 
   *    arbitrary.  thus, the isolated eigenvalues corresponding to 
   *    the triangular matrices are obtained by a division, leaving 
   *    only eigenvalues corresponding to the center matrices to be 
   *    computed. 
   *    ref.:  ward, r. c., balancing the generalized eigenvalue 
   *    problem, siam j. sci. stat. comput., vol. 2, no. 2, june 1981, 
   *    141-152. 
   * 
   *    *****parameter description: 
   * 
   *    on input: 
   * 
   *      ma,mb   int 
   *              row dimensions of the arrays containing matrices 
   *              a and b respectively, as declared in the main calling 
   *              program dimension statement; 
   * 
   *      n       int 
   *              order of the matrices a and b; 
   * 
   *      a       real(ma,n) 
   *              contains the a matrix of the generalized eigenproblem 
   *              defined above; 
   * 
   *      b       real(mb,n) 
   *              contains the b matrix of the generalized eigenproblem 
   *              defined above. 
   * 
   *    on output: 
   * 
   *      a,b     contain the permuted a and b matrices; 
   * 
   *      low     int 
   *              beginning -1 of the submatrices of a and b 
   *              containing the non-isolated eigenvalues; 
   * 
   *      igh     int 
   *              ending -1 of the submatrices of a and b 
   *              containing the non-isolated eigenvalues.  if 
   *              igh = 1 (low = 1 also), the permuted a and b 
   *              matrices are upper triangular; 
   * 
   *      cscale  real(n) 
   *              contains the required column permutations in its 
   *              first low-1 and its igh+1 through n locations; 
   * 
   *      wk      real(n) 
   *              contains the required row permutations in its first 
   *              low-1 and its igh+1 through n locations. 
   * 
   *    *****algorithm notes: 
   *    none 
   * 
   *    *****history: 
   *    written by r. c. ward....... 
   * 
   *    --------------------------------------------------------------- 
   * 
   */
  /* Parameter adjustments */
  --wk;
  --cscale;
  a_dim1 = *ma;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  b_dim1 = *mb;
  b_offset = b_dim1 + 1;
  b -= b_offset;

  /* Function Body */
  k = 1;
  l = *n;
  goto L20;
  /* 
   *    find row with one nonzero in columns 1 through l 
   * 
   */
 L10:
  l = lm1;
  if (l != 1)
    {
      goto L20;
    }
  wk[1] = 1.;
  cscale[1] = 1.;
  goto L200;
 L20:
  lm1 = l - 1;
  i__1 = l;
  for (ii = 1; ii <= i__1; ++ii)
    {
      i__ = l + 1 - ii;
      i__2 = lm1;
      for (j = 1; j <= i__2; ++j)
	{
	  jp1 = j + 1;
	  if (a[i__ + j * a_dim1] != 0. || b[i__ + j * b_dim1] != 0.)
	    {
	      goto L40;
	    }
	  /* L30: */
	}
      j = l;
      goto L60;
    L40:
      i__2 = l;
      for (j = jp1; j <= i__2; ++j)
	{
	  if (a[i__ + j * a_dim1] != 0. || b[i__ + j * b_dim1] != 0.)
	    {
	      goto L70;
	    }
	  /* L50: */
	}
      j = jp1 - 1;
    L60:
      m = l;
      iflow = 1;
      goto L150;
    L70:
      ;
    }
  goto L90;
  /* 
   *    find column with one nonzero in rows k through n 
   * 
   */
 L80:
  ++k;
 L90:
  i__1 = l;
  for (j = k; j <= i__1; ++j)
    {
      i__2 = lm1;
      for (i__ = k; i__ <= i__2; ++i__)
	{
	  ip1 = i__ + 1;
	  if (a[i__ + j * a_dim1] != 0. || b[i__ + j * b_dim1] != 0.)
	    {
	      goto L110;
	    }
	  /* L100: */
	}
      i__ = l;
      goto L130;
    L110:
      i__2 = l;
      for (i__ = ip1; i__ <= i__2; ++i__)
	{
	  if (a[i__ + j * a_dim1] != 0. || b[i__ + j * b_dim1] != 0.)
	    {
	      goto L140;
	    }
	  /* L120: */
	}
      i__ = ip1 - 1;
    L130:
      m = k;
      iflow = 2;
      goto L150;
    L140:
      ;
    }
  goto L200;
  /* 
   *    permute rows m and i 
   * 
   */
 L150:
  wk[m] = (double) i__;
  if (i__ == m)
    {
      goto L170;
    }
  i__1 = *n;
  for (is = k; is <= i__1; ++is)
    {
      f = a[i__ + is * a_dim1];
      a[i__ + is * a_dim1] = a[m + is * a_dim1];
      a[m + is * a_dim1] = f;
      f = b[i__ + is * b_dim1];
      b[i__ + is * b_dim1] = b[m + is * b_dim1];
      b[m + is * b_dim1] = f;
      /* L160: */
    }
  /* 
   *    permute columns m and j 
   * 
   */
 L170:
  cscale[m] = (double) j;
  if (j == m)
    {
      goto L190;
    }
  i__1 = l;
  for (is = 1; is <= i__1; ++is)
    {
      f = a[is + j * a_dim1];
      a[is + j * a_dim1] = a[is + m * a_dim1];
      a[is + m * a_dim1] = f;
      f = b[is + j * b_dim1];
      b[is + j * b_dim1] = b[is + m * b_dim1];
      b[is + m * b_dim1] = f;
      /* L180: */
    }
 L190:
  switch (iflow)
    {
    case 1:
      goto L10;
    case 2:
      goto L80;
    }
 L200:
  *low = k;
  *igh = l;
  return 0;
  /* 
   *    last line of reduc2 
   * 
   */
}				/* reduc2_ */
