#include "scicos_block.h"

int ftree4scicos_ (int *vec, int *nb, int *nd, int *nnd, int *typr, int *outoin,
		   int *outoinptr, int *r1, int *r2, int *nr)
{
  int i__1, i__2, i__3;
  int fini, nprt, i__, j, k, ii;
  /*     Copyright INRIA */
  /* make sure nb > 0 */
  /* Parameter adjustments */
  --r2;
  --r1;
  --outoinptr;
  --outoin;
  --typr;
  --nd;
  --vec;

  /* Function Body */
  *nr = 0;
  i__1 = *nb - 1;
  for (j = 1; j <= i__1; ++j)
    {
      fini = 1;
      i__2 = *nb;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  if (vec[i__] > -1)
	    {
	      if (outoinptr[i__ + 1] - outoinptr[i__] != 0)
		{
		  i__3 = outoinptr[i__ + 1] - 1;
		  for (k = outoinptr[i__]; k <= i__3; ++k)
		    {
		      ii = outoin[k];
		      if (typr[ii] == 1)
			{
			  nprt = outoin[k + outoinptr[*nb + 1] - 1];
			  if (nd[nprt + 1 + (ii - 1) * *nnd] == 0)
			    {
			      ++(*nr);
			      r1[*nr] = ii;
			      r2[*nr] = nprt;
			      fini = 0;
			      vec[ii] = 0;
			      nd[nprt + 1 + (ii - 1) * *nnd] = 1;
			    }
			}
		    }
		}
	    }
	}
      if (fini == 1)
	{
	  goto L200;
	}
    }
 L200:
  return 0;
}	

