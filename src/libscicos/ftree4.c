#include "scicos_block.h"

int ftree4scicos_ (int *vec, int *nb, int *nd, int *nnd, int *typr, int *outoin,
		   int *outoinptr, int *r1, int *r2, int *nr)
{
  int i1, i2, i3;
  int fini, nprt, i, j, k, ii;
  --r2;
  --r1;
  --outoinptr;
  --outoin;
  --typr;
  --nd;
  --vec;
  *nr = 0;
  i1 = *nb - 1;
  for (j = 1; j <= i1 ; ++j)
    {
      fini = 1;
      i2 = *nb;
      for (i = 1; i <= i2 ; ++i)
	{
	  if (vec[i] > -1)
	    {
	      if (outoinptr[i + 1] - outoinptr[i] != 0)
		{
		  i3 = outoinptr[i + 1] - 1;
		  for (k = outoinptr[i  ]; k <= i3; ++k)
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
      if (fini == 1) return ;
    }
}	

