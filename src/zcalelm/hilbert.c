/*********************************************************************
 * This Software is ( Copyright ENPC 1998 )                          *
 * Jean-Philippe Chancelier Enpc/Cergrene                            *
 *********************************************************************/

#include "nsp/machine.h" 

/******************************************************
 * Order n Hilbert matrix and its inverse 
 * 
 * a(i,j)= 1/(i+j-1) 
 ******************************************************/

void nsp_hilbert_matrix(double *a,int n)
{
  int i,j;
  for ( i= 0 ; i < n ; i++) 
    {
      a[i*n+i]= 1.0/((double) 2*i+1);
      for ( j = 0 ; j < i ; j++) 
	a[j+n*i]=a[i+(n)*j] = 1.0/((double) i+j+1);
    }
}


void nsp_hilbert_inverse_matrix(double *a,int n)
{
  int i,j;
  double p = (double) n,r;
  for ( i = 0 ; i < n ; i++)
    {
      if (i != 0 ) p *= ((double) (n-i)*(n+i))/(i*i);
      r = p*p;
      a[i+n*i] = r/(2*(i+1)-1);
      for ( j = i+1 ; j < n ; j++)
	{
	  r *= - ((double)(n-j)*(n+j))/(j*j);
	  a[j+n*i] = a[i+n*j] = r/(i+j+1);
	}
    }
}






