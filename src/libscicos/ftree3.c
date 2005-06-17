#include "scicos_block.h"

extern int scicos_isort();

int scicos_ftree3(int *vec, int *nb, int *deput, int *typl, int *bexe,
		   int *boptr, int *blnk, int *blptr, int *kk, int *ord,
		   int *nord, int *ok)
{
  int i__1, i__2, i__3;
  int fini, i__, j, m;
  int ii, mm, nkk;
  /*     Copyright INRIA */
  /*     make sure nb > 0 */
  /* Parameter adjustments */
  --ord;
  --kk;
  --blptr;
  --blnk;
  --boptr;
  --bexe;
  --typl;
  --deput;
  --vec;

  /* Function Body */
  *ok = 1;
  i__1 = *nb;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (vec[i__] == 0 && typl[i__] == 1)
	{
	  vec[i__] = 1;
	}
    }
  i__1 = *nb + 2;
  for (j = 1; j <= i__1; ++j)
    {
      fini = 1;
      if (j == *nb + 2)
	{
	  *ok = 0;
	  *nord = 0;
	  return 0;
	}
      i__2 = *nb;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  if (vec[i__] > -1 && typl[i__] != -1)
	    {
	      if (typl[i__] == 1)
		{
		  nkk = boptr[i__ + 1] - boptr[i__];
		  if (nkk != 0)
		    {
		      i__3 = nkk;
		      for (m = 1; m <= i__3; ++m)
			{
			  ii = bexe[m + boptr[i__] - 1];
			  if (typl[ii] == 1)
			    {
			      if (vec[ii] < vec[i__] + 2)
				{
				  fini = 0;
				  vec[ii] = vec[i__] + 2;
				}
			    }
			  else
			    {
			      if (vec[ii] < vec[i__] + 1)
				{
				  fini = 0;
				  vec[ii] = vec[i__] + 1;
				}
			    }
			}
		    }
		}
	      else
		{
		  nkk = blptr[i__ + 1] - blptr[i__];
		  if (nkk != 0)
		    {
		      i__3 = nkk;
		      for (m = 1; m <= i__3; ++m)
			{
			  ii = blnk[m + blptr[i__] - 1];
			  if (vec[ii] > -1
			      && (deput[ii] == 1 || typl[ii] == 1))
			    {
			      if (vec[ii] < vec[i__])
				{
				  fini = 0;
				  vec[ii] = vec[i__];
				}
			    }
			  /* L60: */
			}
		    }
		}
	    }
	  /* L100: */
	}
      if (fini == 1)
	{
	  goto L200;
	}
      /*          write(6,'(      "vec"  ,e10.3,"flag=",i1           )') t,flag */
      /* L150: */
    }
  /*     loop J finished */
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


