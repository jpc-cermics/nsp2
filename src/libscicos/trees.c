/*     Copyright INRIA */

#include "nsp/machine.h"
#include "nsp/math.h"
#include "nsp/gsort-p.h"
#include "scicos_block.h"

/* 
 * 
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


/* make sure nb > 0 */
/* Parameter adjustments */


int scicos_ftree2 (int *vec, int *nb, int *deput, int *outoin, int *outoinptr,
		   int *ord, int *nord, int *ok)
{
  int i__1, i__2, i__3;
  int fini, i__, j, k, m;
  int ii, mm;
  --ord;
  --outoinptr;
  --outoin;
  --deput;
  --vec;

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


int scicos_ftree3(int *vec, int *nb, int *deput, int *typl, int *bexe,
		   int *boptr, int *blnk, int *blptr, int *kk, int *ord,
		   int *nord, int *ok)
{
  int i__1, i__2, i__3;
  int fini, i__, j, m;
  int ii, mm, nkk;
  --ord;
  --kk;
  --blptr;
  --blnk;
  --boptr;
  --bexe;
  --typl;
  --deput;
  --vec;

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



int scicos_ftree4(int *vec, int *nb, int *nd, int *nnd, int *typr, int *outoin,
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
      if (fini == 1) return OK;
    }
  return OK;
}	


