#include <stddef.h> 
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include "nsp/object.h"
#include "maxplus.h" 

static int Karp(const NspMatrix *IJ,const NspMatrix *A,int nnodes, int entry, double *u);

#define MAXPLUS_EPSILON -HUGE_VAL
#define INF HUGE_VAL

/**
 * maxplus_matrix_karp:
 * @A: 
 * @entry 
 * 
 * 
 * Return value: 
 **/

int maxplus_matrix_karp(NspMatrix *A,int entry,double *res)
{
  int i,j;
  NspMatrix *IJ; 
  /* 
   * compute [ij,A,s]=spget(A) for a full matrix 
   * we store indices starting from 1 to be compatible with nsp 
   */ 
  if (( IJ = nsp_matrix_create(NVOID,'r',A->mn,2)) == NULLMAT) return FAIL;
  for ( i = 0 ; i < A->m ; i++) 
    {
      for ( j = 0 ; j < A->m ; j++) 
	{
	  IJ->R[i+A->m*j] = i+1;
	  IJ->R[i+A->m*j+IJ->m] = j+1;
	}
    }
  return Karp(IJ,A,A->m,entry,res);
}

/**
 * maxplus_spmatrix_karp:
 * @A: 
 * @entry 
 * 
 * 
 * Return value: 
 **/

int maxplus_spmatrix_karp(NspSpMatrix *Sp,int entry,double *res) 
{
  NspMatrix *IJ, *A;
  if ( nsp_spmatrix_get(Sp, &IJ, &A)== FAIL) return FAIL;
  return  Karp(IJ,A,Sp->m,entry,res);
}


/*  
 * Computes the entry ``entry'' of the cycle time
 *   of the sparse matrix (ij,A,nnodes,narcs)
 *      the coding is defined as in 
 *      [ij,A,s]=spget(As) ; nnodes = s(1); narcs = size(A,'r');
 *      thus ij is a [narcs,2] matrix A a narcs vector 
 *                   nnodes: the number of rows of sparse matrix As.
 *   using Karp's algorithm. Result in u 
 * 
 *  Requires : O(nnodes^2) SPACE
 *             O(nnodes*narcs) TIME
 *  The input must be a max-plus matrix with at
 *  least one finite entry per row.
 *  
 * Warning: 
 *   IJ uses indices starting at 1 and is a  narcs x 2 matrix 
 *   A is a matrix such that mxn == narcs (i.e A can be used 
 *       as a narcsx1 matrix)
 *   
 *  
 */

#define ij(n) (((int) IJ->R[n])-1)

static int Karp(const NspMatrix *IJ,const NspMatrix *A,int nnodes, int entry, double *u)
{ 
  NspMatrix *V;
  int narcs = IJ->m; 
  int i,j,k;
  double v;
  int SIZE,SMALLSIZE,jnnodes,newjnnodes;
  SMALLSIZE=nnodes*nnodes;
  SIZE=SMALLSIZE+nnodes;

  if (( V = nsp_matrix_create(NVOID,'r',SIZE,1)) == NULLMAT) 
    {
      Scierror("Error: you may try Karp_Big, which is twice slower\n");
      Scierror("\tbut only requires a linear space\n");
      return FAIL;
    }
  for (i=0; i<SIZE; i++)
    {
      V->R[i]=MAXPLUS_EPSILON;  
    }
  if ( entry < 0 || entry >= SIZE )
    {
      Scierror("Error: entry=%d is incorrect in function karp\n",entry+1);
      return FAIL;
    }
  V->R[entry]=0;
  jnnodes=0;
  for (j=2;j<=nnodes+1;j++)
    { 
      newjnnodes=jnnodes+nnodes;
      for (i=0;i<narcs; i++)
	{
	  V->R[ij(i+narcs) + newjnnodes]=
	    Max(A->R[i]+V->R[ij(i)+jnnodes],V->R[ij(i+narcs)+newjnnodes]);
	}
      jnnodes=newjnnodes;
    }
  *u=MAXPLUS_EPSILON;
  for (i=0;i<nnodes;i++)
    { 
      if (V->R[SMALLSIZE+i] != MAXPLUS_EPSILON) 
	{ 
	  v=INF;
      	  for (k=0; k<nnodes; k++)
	    {
	      v=Min(v,(V->R[SMALLSIZE+ i]-V->R[nnodes*k+i])/(nnodes-k));
	    }
	  *u=Max(*u,v);
	}
    }
  nsp_matrix_destroy(V);
  return OK;
}


/* 
 * Karp algorithm for very large matrices. Twice slower than
 * the original Karp's algorithm above, but requires only O(nnodes) space 
 */

void Karp_Big(int *ij,double *A,int nnodes, int narcs, int entry, double *u)
{  
  double *V,*W,*LAST,*TABLE;
  int i,j;
  V=(double *)calloc(nnodes,sizeof(double));
  W=(double *)calloc(nnodes,sizeof(double));
  LAST=(double *)calloc(nnodes,sizeof(double));
  TABLE=(double *)calloc(nnodes,sizeof(double));
  if ( (V==NULL) || (W==NULL) || (LAST==NULL) || (TABLE==NULL))
    {
      printf("Error in Karp_Big... memory allocation failed...\n");
      exit(1);
    }
  for (i=0; i<nnodes; i++)
    {
      V[i]=MAXPLUS_EPSILON;  
      W[i]=MAXPLUS_EPSILON;
    }
  V[entry]=0;
  for (j=0;j<nnodes;j++)
    { 
      for (i=0;i<narcs; i++)
	{
	  W[ij[i*2+1]]=Max(A[i]+V[ij[i*2]],W[ij[i*2+1]]);
	}
      /* copying the new value */
      for (i=0; i<nnodes; i++)
	{
	  V[i]=W[i];
	  W[i]=MAXPLUS_EPSILON;
	}
    }
  /* yA^n has been computed */
  /* We initialize the table */ 
  for (i=0; i<nnodes; i++)
    {
      LAST[i]=V[i];
      V[i]=MAXPLUS_EPSILON;
      TABLE[i]=INF;
    }
  V[entry]=0;
  if (LAST[entry] != MAXPLUS_EPSILON)
    TABLE[entry] =LAST[entry]/nnodes;
  /* we compute the sequence yA^k, k< =n a second time, and
   * generate the vector of differences Min_k (yA^k-yA^n) on the fly.
   * This is the price to pay (in time) 
   * to perform the computations in linear space
   */
  for (j=1;j<nnodes;j++)
    { 
      for (i=0;i<narcs; i++)
	{
	  W[ij[i*2+1]]=Max(A[i]+V[ij[i*2]],W[ij[i*2+1]]);
	}
      /* copying the new value */
      for (i=0; i<nnodes; i++)
	{
	  V[i]=W[i];
	  W[i]=MAXPLUS_EPSILON;
	  if (LAST[i] !=MAXPLUS_EPSILON)
	    {
	      TABLE[i]=Min(TABLE[i],(LAST[i]-V[i])/(nnodes -j));
	    }
	}
    }
  
  *u=MAXPLUS_EPSILON;
  for (i=0;i<nnodes;i++)
    {
      if (LAST[i] !=MAXPLUS_EPSILON )
	{
	  *u=Max(*u,TABLE[i]);
	}
    }
  free(TABLE);
  free(LAST);
  free(W);
  free(V);
}





