/*********************************************************************
 * This Software is ( Copyright ENPC 1998 )                          *
 * Jean-Philippe Chancelier Enpc/Cergrene                            *
 *********************************************************************/

#include "nsp/machine.h" 

/******************************************************
 * Order n Franck Matrix and its inverse 
 * 
 * a(i,j)=n-j+1   si i <= j , 
 * a(j,j-1)=n-j , a(i,j)=0 if  i > j+1 
 * 
 * a is a naxn Matrix 
 * job = 0 : franck Matrix 
 * job = 1 : inverse
 ******************************************************/

void nsp_franck_matrix(double *a,int n)
{
  int i,j;
  for ( i= 0 ; i < n ; i++) 
    {
      for ( j = 0 ; j < i-1 ; j++) a[i+(n)*j]=0.00;
      for ( j = i ; j < n ; j++)  a[i+(n)*j]=(double)(n-j);
      if ( i > 0) a[i+(n)*(i-1)]= (double)(n-i);
    }
}

void nsp_franck_inverse_matrix(double *a,int n)
{
  int i,j;
  a[0]=1.00;
  if( n == 1 ) return ;
  for ( i=0 ; i < n-1 ; i++)
    a[i+(n)*(i+1)] = -1.00;
  for ( i=1 ; i < n ; i++)
    a[i+(n)*i]=(double) (n-i+1);
  for (i = 1 ; i < n ; i++ )
    for ( j =0 ; j < i ; j++)
      a[i+(n)*j]= (n-1-i+1)*(-a[i-1+(n)*j]);
  for ( i = 0 ; i < n ; i++ ) 
    for ( j = i+2 ; j < n ; j++ )
      a[i+(n)*j]=0.00;
}

