#include "nsp/machine.h"
#include "nsp/math.h"
#include "nsp/gsort-p.h"
#include "scicos_block.h"


/* 
 */

void scicos_isort(int *a,int *na, int *index)
{
  return nsp_qsort_int(a,index,TRUE,*na,'d');
}


/*
 * nb: number of regular blocks 
 * vec: int vector of size nb 
 * in: int vector 
 * depu: int vector, first column of dep_ut 
 * outptr: int vector 
 * cmat: int vector 
 * kk: int work area of size nb 
 * 
 * outputs: 
 * ok: int 
 * ord: int vector of size nord (=<nb) 
 * nord 
 * Copyright INRIA 
*/

int scicos_sctree (int *nb, int *vec, int *in, int *depu, int *outptr, int *cmat,
		   int *ord, int *nord, int *ok, int *kk)
{

  int i__1, i__2, i__3;
  int fini;
  int i__, j, l;
  int ii, lkk;

  --kk;
  --vec;
  --in;
  --depu;
  --outptr;
  --cmat;
  --ord;
  *ok = 1;
  i__1 = *nb + 2;
  for (j = 1; j <= i__1; ++j)
    {
      fini = TRUE;
      i__2 = *nb;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  if (vec[i__] == j - 1)
	    {
	      if (j == *nb + 2)
		{
		  *ok = 0;
		  return 0;
		}
	      lkk = 0;
	      i__3 = outptr[i__ + 1] - 1;
	      for (l = outptr[i__]; l <= i__3; ++l)
		{
		  ii = in[cmat[l]];
		  if (depu[ii] == 1)
		    {
		      ++lkk;
		      kk[lkk] = ii;
		    }
		  /* L40: */
		}
	      if (lkk > 0)
		{
		  fini = FALSE;
		  i__3 = lkk;
		  for (l = 1; l <= i__3; ++l)
		    {
		      vec[kk[l]] = j;
		      /* L45: */
		    }
		}
	    }
	  /* L50: */
	}
      if (fini)
	{
	  goto L65;
	}
      /* L60: */
    }
 L65:
  i__1 = *nb;
  for (l = 1; l <= i__1; ++l)
    {
      kk[l] = -vec[l];
      /* L70: */
    }
  scicos_isort (&kk[1], nb, &ord[1]);
  *nord = 0;
  i__1 = *nb;
  for (l = 1; l <= i__1; ++l)
    {
      if (kk[l] != 1 && outptr[ord[l] + 1] - outptr[ord[l]] != 0)
	{
	  ++(*nord);
	  ord[*nord] = ord[l];
	}
      /* L80: */
    }
  return 0;
}				
