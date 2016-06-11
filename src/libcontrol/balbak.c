/* balbak.f -- translated by f2c (version 19961017). */

#include "ctrlpack.h"

/* 
 *! purpose 
 * 
 *    this subroutine forms the eigenvectors of a real general 
 *    matrix by back transforming those of the corresponding 
 *    balanced matrix determined by  balanc. 
 * 
 *! calling sequence 
 * 
 *     subroutine balbak(nm,n,low,igh,scale,m,z) 
 * 
 *    on input: 
 * 
 *       nm must be set to the row dimension of two-dimensional 
 *         array parameters as declared in the calling program 
 *         dimension statement; 
 * 
 *       n is the order of the matrix; 
 * 
 *       low and igh are ints determined by  balanc; 
 * 
 *       scale contains information determining the permutations 
 *         and scaling factors used by  balanc; 
 * 
 *       m is the number of columns of z to be back transformed; 
 * 
 *       z contains the real and imaginary parts of the eigen- 
 *         vectors to be back transformed in its first m columns. 
 * 
 *    on output: 
 * 
 *       z contains the real and imaginary parts of the 
 *         transformed eigenvectors in its first m columns. 
 * 
 *! originator 
 * 
 *    this subroutine is a translation of the algol procedure balbak, 
 *    num. math. 13, 293-304(1969) by parlett and reinsch. 
 *    handbook for auto. comp., vol.ii-linear algebra, 315-326(1971). 
 *! 
 *    questions and comments should be directed to b. s. garbow, 
 *    applied mathematics division, argonne national laboratory 
 * 
 *    ------------------------------------------------------------------ 
 * 
 */

int
nsp_ctrlpack_balbak (int *nm, int *n, int *low, int *igh, double *scale,
		     int *m, double *z__)
{
  /* System generated locals */
  int z_dim1, z_offset, i__1, i__2;

  /* Local variables */
  int i__, j, k;
  double s;
  int ii;

  /* Parameter adjustments */
  --scale;
  z_dim1 = *nm;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;

  /* Function Body */
  if (*m == 0)
    {
      goto L200;
    }
  if (*igh == *low)
    {
      goto L120;
    }
  /* 
   */
  i__1 = *igh;
  for (i__ = *low; i__ <= i__1; ++i__)
    {
      s = scale[i__];
      /*    :::::::::: left hand eigenvectors are back transformed 
       *               if the foregoing statement is replaced by 
       *               s=1.0d+0/scale(i). :::::::::: 
       */
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  /* L100: */
	  z__[i__ + j * z_dim1] *= s;
	}
      /* 
       */
      /* L110: */
    }
  /*    ::::::::: for i=low-1 step -1 until 1, 
   *              igh+1 step 1 until n do -- :::::::::: 
   */
 L120:
  i__1 = *n;
  for (ii = 1; ii <= i__1; ++ii)
    {
      i__ = ii;
      if (i__ >= *low && i__ <= *igh)
	{
	  goto L140;
	}
      if (i__ < *low)
	{
	  i__ = *low - ii;
	}
      k = (int) scale[i__];
      if (k == i__)
	{
	  goto L140;
	}
      /* 
       */
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  s = z__[i__ + j * z_dim1];
	  z__[i__ + j * z_dim1] = z__[k + j * z_dim1];
	  z__[k + j * z_dim1] = s;
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
}				/* balbak_ */
