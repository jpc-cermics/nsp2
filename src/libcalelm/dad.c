
#include "calpack.h"

  /* 
   *    dad compute the matrix product a=d*a or a=a*d 
   *    where d is the matrix with ones on the anti- 
   *    diagonal and a the input matrix. it also 
   *    multiplies each element of the product with 
   *    the constant r 
   * 
   *    subroutine dad(a, na, i1, i2, j1, j2, r, isw) 
   *    int i1, i2, j1, j2, na, isw 
   *    double precision a, r 
   *    dimension a(na,*) 
   * 
   *    a : input matrix 
   *    na: leading dimension of a 
   *    i1,i2 : the first and the last rows of a to be considered 
   *    j1,j2 : the first and the last columns of a to be considered 
   *    r: constant factor 
   *    isw: parameter specifying the product to be done 
   *         isw=0 : d*a 
   *         isw=1 : a*d 
   *    Copyright INRIA 
   */

int
nsp_calpack_dad (double *a, int *na, int *i1, int *i2, int *j1, int *j2,
		 double *r__, int *isw)
{
  int a_dim1, a_offset, i__1, i__2;
  double temp;
  int i__, j, i1i, i2i, j1j, j2j, ip1, jp1, ncd2, nrd2;
  
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  
  if (*isw == 1)
    {
      goto L60;
    }
  if (*i1 == *i2)
    {
      goto L40;
    }
  /* 
   */
  nrd2 = (int) ((double) ((*i2 - *i1 + 1) / 2));
  i__1 = *j2;
  for (j = *j1; j <= i__1; ++j)
    {
      i__2 = nrd2;
      for (ip1 = 1; ip1 <= i__2; ++ip1)
	{
	  i__ = ip1 - 1;
	  i1i = *i1 + i__;
	  i2i = *i2 - i__;
	  temp = a[i1i + j * a_dim1];
	  a[i1i + j * a_dim1] = a[i2i + j * a_dim1] * *r__;
	  a[i2i + j * a_dim1] = temp * *r__;
	  /* L10: */
	}
      /* L20: */
    }
  if ((*i2 - *i1) % 2 == 1)
    {
      return 0;
    }
  i__ = *i1 + nrd2;
  i__1 = *j2;
  for (j = *j1; j <= i__1; ++j)
    {
      a[i__ + j * a_dim1] *= *r__;
      /* L30: */
    }
  return 0;
L40:
  i__1 = *j2;
  for (j = *j1; j <= i__1; ++j)
    {
      a[*i1 + j * a_dim1] *= *r__;
      /* L50: */
    }
  return 0;
  /* 
   *        computes the product ad where d is as above. 
   */
L60:
  if (*j1 == *j2)
    {
      goto L100;
    }
  ncd2 = (int) ((double) ((*j2 - *j1 + 1) / 2));
  i__1 = ncd2;
  for (jp1 = 1; jp1 <= i__1; ++jp1)
    {
      j = jp1 - 1;
      i__2 = *i2;
      for (i__ = *i1; i__ <= i__2; ++i__)
	{
	  j1j = *j1 + j;
	  j2j = *j2 - j;
	  temp = a[i__ + j1j * a_dim1];
	  a[i__ + j1j * a_dim1] = a[i__ + j2j * a_dim1] * *r__;
	  a[i__ + j2j * a_dim1] = temp * *r__;
	  /* L70: */
	}
      /* L80: */
    }
  if ((*j2 - *j1) % 2 == 1)
    {
      return 0;
    }
  j = *j1 + ncd2;
  i__1 = *i2;
  for (i__ = *i1; i__ <= i__1; ++i__)
    {
      a[i__ + j * a_dim1] *= *r__;
      /* L90: */
    }
  return 0;
L100:
  i__1 = *i2;
  for (i__ = *i1; i__ <= i__1; ++i__)
    {
      a[i__ + *j1 * a_dim1] *= *r__;
      /* L110: */
    }
  return 0;
}
