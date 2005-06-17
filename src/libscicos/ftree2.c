#include "scicos_block.h"

extern int scicos_isort();

int scicos_ftree2 (int *vec, int *nb, int *deput, int *outoin, int *outoinptr,
		   int *ord, int *nord, int *ok)
{
  int i__1, i__2, i__3;
  int fini, i__, j, k, m;
  int ii, mm;
  /*     Copyright INRIA */
  /* make sure nb > 0 */
  /* Parameter adjustments */
  --ord;
  --outoinptr;
  --outoin;
  --deput;
  --vec;
  /* Function Body */
  *ok = 1;
  i__1 = *nb + 2;
  for (j = 1; j <= i__1; ++j)
    {
      fini = 1;
      i__2 = *nb;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  if (vec[i__] == j - 1)
	    {
	      if (j == *nb + 2)
		{
		  *ok = 0;
		  *nord = 0;
		  return 0;
		}
	      if (outoinptr[i__ + 1] - outoinptr[i__] != 0)
		{
		  i__3 = outoinptr[i__ + 1] - 1;
		  for (k = outoinptr[i__]; k <= i__3; ++k)
		    {
		      ii = outoin[k];
		      if (vec[ii] > -1 && deput[ii] == 1)
			{
			  fini = 0;
			  vec[ii] = j;
			}
		      /* L60: */
		    }
		}
	    }
	  /* L100: */
	}
      if (fini == 1)
	{
	  goto L200;
	}
      /* L150: */
    }
L200:
  i__1 = *nb;
  for (m = 1; m <= i__1; ++m)
    {
      vec[m] = -vec[m];
      /* L202: */
    }
  scicos_isort (&vec[1], nb, &ord[1]);
  i__1 = *nb;
  for (m = 1; m <= i__1; ++m)
    {
      if (vec[m] < 1)
	{
	  if (m == 1)
	    {
	      *nord = *nb;
	      return 0;
	    }
	  else
	    {
	      *nord = *nb - m + 1;
	      i__2 = *nord;
	      for (mm = 1; mm <= i__2; ++mm)
		{
		  ord[mm] = ord[mm + *nb - *nord];
		  /* L250: */
		}
	      return 0;
	    }
	}
      /* L300: */
    }
  *nord = 0;
  return 0;
}			

