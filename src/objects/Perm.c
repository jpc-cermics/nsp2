/* Nsp
 * Copyright (C) 1998-2006 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <math.h>

#include "nsp/object.h"

/**
 * dperm:
 * @A: array to be permuted 
 * @nv: size of @ind 
 * @ind: array of integer which codes a permutation 
 * 
 * performs the permutation given by @ind of size @nv on array @A 
 * Note that permutation is given by a function [1,nv]-->[1,nv] 
 * (i.e first indice is zero)  
 * @ind is left unchanged at the end of execution 
 * 
 **/

void  C2F(dperm)(double *A,int *nv, int *ind)
{
  double x;
  int i,i0,i1;
  i0=0; i=i0; x=A[0];
  while (1) 
    {
      if ( ind[i]-1 == i0) 
	{ 
	  ind[i]=-ind[i]-1;
	  A[i]=x;
	  i1=-1;
	  while (1) 
	    {
	      i1=i1+1;
	      if (i1 >= *nv) goto end;
	      if (ind[i1] < 0) continue;
	      i0=i1; i=i0; x=A[i0];
	    }
	}
      else
	{
	  A[i]=A[ind[i]-1];
	  i1=ind[i]-1;
	  ind[i]=-ind[i]-1;
	  i=i1;
	}
    }
 end:
  for ( i = 0 ; i < *nv ; i++) 
    ind[i]=-ind[i]-1;
}



/**
 * iperm:
 * @A: array to be permuted 
 * @nv: size of @ind 
 * @ind: array of integer which codes a permutation 
 * 
 * performs the permutation given by @ind of size @nv on array @A 
 * Note that permutation is given by a function [1,nv]-->[1,nv] 
 * (i.e first indice is zero)  
 * @ind is left unchanged at the end of execution 
 * 
 * 
 **/

void  C2F(iperm)(int *A,int *nv, int *ind)
{
  int x;
  int i,i0,i1;
  i0=0; i=i0; x=A[0];
  while (1) 
    {
      if ( ind[i]-1 == i0) 
	{ 
	  ind[i]=-ind[i]-1;
	  A[i]=x;
	  i1=-1;
	  while (1) 
	    {
	      i1=i1+1;
	      if (i1 >= *nv) goto end;
	      if (ind[i1] < 0) continue;
	      i0=i1; i=i0; x=A[i0];
	    }
	}
      else
	{
	  A[i]=A[ind[i]-1];
	  i1=ind[i]-1;
	  ind[i]=-ind[i]-1;
	  i=i1;
	}
    }
 end:
  for ( i = 0 ; i < *nv ; i++) 
    ind[i]=-ind[i]-1;
}


/**
 * zperm:
 * @A: array to be permuted 
 * @nv: size of @ind 
 * @ind: array of integer which codes a permutation 
 * 
 * performs the permutation given by @ind of size @nv on array @A 
 * Note that permutation is given by a function [1,nv]-->[1,nv] 
 * (i.e first indice is zero)  
 * @ind is left unchanged at the end of execution 
 * 
 **/

void  C2F(zperm)(doubleC *A,int *nv, int *ind)
{
  doubleC x;
  int i,i0,i1;
  i0=0; i=i0; x=A[0];
  while (1) 
    {
      if ( ind[i]-1 == i0) 
	{ 
	  ind[i]=-ind[i]-1;
	  A[i]=x;
	  i1=-1;
	  while (1) 
	    {
	      i1=i1+1;
	      if (i1 >= *nv) goto end;
	      if (ind[i1] < 0) continue;
	      i0=i1; i=i0; x=A[i0];
	    }
	}
      else
	{
	  A[i]=A[ind[i]-1];
	  i1=ind[i]-1;
	  ind[i]=-ind[i]-1;
	  i=i1;
	}
    }
 end:
  for ( i = 0 ; i < *nv ; i++) 
    ind[i]=-ind[i]-1;
}



#ifdef TEST 
int main()
{
  int n=4;
  double X[4]={ 1,2,3,4};
  int perm[4]={ 2,3,4,1};
  dperm(X,&n,perm);
  return 0;
}

#endif 
