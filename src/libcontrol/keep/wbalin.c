/* wbalin.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*/MEMBR ADD NAME=WBALIN,SSI=0 
 *    Copyright INRIA 
 */
int
nsp_ctrlpack_wbalin (int *max__, int *n, int *low, int *igh, double *scale,
		     double *ear, double *eai)
{
  /* System generated locals */
  int ear_dim1, ear_offset, eai_dim1, eai_offset, i__1, i__2;

  /* Local variables */
  int i__, j, k;
  double s;
  int ii;

  /* 
   *!purpose 
   *               - performs the inverse transformation of that 
   *                           done in subroutine cbal 
   * 
   *!calling sequence 
   * 
   *     subroutine wbalin(max , n , low , igh , scale , ear , eai) 
   *          s  max       - maximum row dimension of ea 
   *             n         - order of ea 
   *             low       - int determined by balanc or balanx 
   *             igh       - int determined by balanc or balanx 
   *             scale(n)  - contains information determining the 
   *                           permutations and scaling factors used 
   *                           by balanc or balanx 
   *             ea(max,n) - contains the matrix to be transformed 
   * 
   *! 
   * 
   ***** 
   */
  /* Parameter adjustments */
  eai_dim1 = *max__;
  eai_offset = eai_dim1 + 1;
  eai -= eai_offset;
  ear_dim1 = *max__;
  ear_offset = ear_dim1 + 1;
  ear -= ear_offset;
  --scale;

  /* Function Body */
  if (*igh == *low)
    {
      goto L50;
    }
  /***** 
   *    remove scaling from rows and columns 
   ***** 
   */
  i__1 = *igh;
  for (i__ = *low; i__ <= i__1; ++i__)
    {
      s = scale[i__];
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  ear[i__ + j * ear_dim1] = s * ear[i__ + j * ear_dim1];
	  eai[i__ + j * eai_dim1] = s * eai[i__ + j * eai_dim1];
	  /* L10: */
	}
      /* L20: */
    }
  i__1 = *igh;
  for (j = *low; j <= i__1; ++j)
    {
      s = 1. / scale[j];
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ear[i__ + j * ear_dim1] *= s;
	  eai[i__ + j * eai_dim1] *= s;
	  /* L30: */
	}
      /* L40: */
    }
  /***** 
   *    re-permute rows and columns 
   ***** 
   */
 L50:
  i__1 = *n;
  for (ii = 1; ii <= i__1; ++ii)
    {
      i__ = ii;
      if (i__ >= *low && i__ <= *igh)
	{
	  goto L80;
	}
      if (i__ < *low)
	{
	  i__ = *low - ii;
	}
      k = (int) scale[i__];
      if (k == i__)
	{
	  goto L80;
	}
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  s = ear[i__ + j * ear_dim1];
	  ear[i__ + j * ear_dim1] = ear[k + j * ear_dim1];
	  ear[k + j * ear_dim1] = s;
	  s = eai[i__ + j * eai_dim1];
	  eai[i__ + j * eai_dim1] = eai[k + j * eai_dim1];
	  eai[k + j * eai_dim1] = s;
	  /* L60: */
	}
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  s = ear[j + i__ * ear_dim1];
	  ear[j + i__ * ear_dim1] = ear[j + k * ear_dim1];
	  ear[j + k * ear_dim1] = s;
	  s = eai[j + i__ * eai_dim1];
	  eai[j + i__ * eai_dim1] = eai[j + k * eai_dim1];
	  eai[j + k * eai_dim1] = s;
	  /* L70: */
	}
    L80:
      ;
    }
  return 0;
}				/* wbalin_ */
