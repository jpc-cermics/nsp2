/*********************************************************************
 * This Software is ( Copyright ENPC 1998 )                          *
 * Jean-Philippe Chancelier Enpc/Cergrene                            *
 *********************************************************************/

#include "nsp/object.h"

/********************************************************************
 * dset : dy[incy*k] =dx , k= 0, *n-1 
 ********************************************************************/

int C2F(dset)(const int *n,const double * dx,double * dy,const int * incy)
{
  int i,iy;
  if (*n <= 0) {  return 0;  }
  iy = 0;
  if (*incy < 0) {  iy = (-(*n) + 1) * *incy ; } 
  for (i = 0 ; i < *n ; ++i) 
    {
      dy[iy] = *dx;
      iy += *incy;
  }
  return 0;
} 

/********************************************************************
 * dadd  :  dy[incy*k ] += dx[incx*k]  k=0,*n-1
 ********************************************************************/

int C2F(dadd)(n, dx, incx, dy, incy)
     int *n;
     double *dx;
     int *incx;
     double *dy;
     int *incy;
{
  static int i, ix, iy;
  if (*n <= 0) { return 0;  }
  if (*incx == 1 && *incy == 1) 
    {
      for (i = 0 ; i < *n ; ++i) 
	{
	  dy[i] += dx[i];
	}
      return 0;
    }
  else 
    {
      /* code for unequal increments or equal increments */
      /* not equal to 1 */
      ix = iy = 0;
      if (*incx < 0) { ix = (-(*n) + 1) * *incx ;} 
      if (*incy < 0) { iy = (-(*n) + 1) * *incy ;}
      for ( i = 0; i < *n ; i++ ) {
	dy[iy] += dx[ix] ;
	ix += *incx;
	iy += *incy;
      }
    }
  return 0;
}

/********************************************************************
 * dsub  :  dy[incy*k ] -= dx[incx*k]  k=0,*n-1
 ********************************************************************/

int C2F(dsub)(n, dx, incx, dy, incy)
     int *n;
     double *dx;
     int *incx;
     double *dy;
     int *incy;
{
  static int i, ix, iy;
  if (*n <= 0) { return 0;  }
  if (*incx == 1 && *incy == 1) 
    {
      for (i = 0 ; i < *n ; ++i) 
	{
	  dy[i] -= dx[i] ;
	}
      return 0;
    }
  else 
    {
      /* code for unequal increments or equal increments */
      /* not equal to 1 */
      ix = iy = 0;
      if (*incx < 0) { ix = (-(*n) + 1) * *incx ;} 
      if (*incy < 0) { iy = (-(*n) + 1) * *incy ;}
      for ( i = 0; i < *n ; i++ ) {
	dy[iy] -= dx[ix];
	ix += *incx;
	iy += *incy;
      }
    }
  return 0;
}


/********************************************************************
 * dsum   :  sum(dx[incx*k], k=0,*n-1)
 ********************************************************************/


double C2F(dsum)( n, dx, incx)
     int *n;
     double *dx;
     int *incx;
{
  double d=0.00;
  int i;
  if( *n <=0 || *incx <= 0) return d;
  if (*incx == 1)
    {
      int m = *n % 6 ;
      for ( i = 0 ; i < m ; i++)
	{
	  d += dx[i];
	}
      for ( i= m ; i < *n ; i+=6 )
	{
	  d += dx[i] + dx[i+1] + dx[i+2] + 
	    dx[i+3] + dx[i+4] + dx[i+5];
	}
    }
  else
    {
      int nincx = *n * *incx;
      for ( i = 0 ; i < nincx ; i += *incx ) d  += dx[i];
    }
  return d;
}


/********************************************************************
 * dvmul  :  dy[incy*k ] *= dx[incx*k]  k=0,*n-1
 ********************************************************************/

int C2F(dvmul)(n, dx, incx, dy, incy)
     int *n;
     double *dx;
     int *incx;
     double *dy;
     int *incy;
{
  static int i, ix, iy;
  if (*n <= 0) { return 0;  }
  if (*incx == 1 && *incy == 1) 
    {
      for (i = 0 ; i < *n ; ++i) 
	{
	  dy[i] *= dx[i];
	}
      return 0;
    }
  else 
    {
      /* code for unequal increments or equal increments */
      /* not equal to 1 */
      ix = iy = 0;
      if (*incx < 0) { ix = (-(*n) + 1) * *incx ;} 
      if (*incy < 0) { iy = (-(*n) + 1) * *incy ;}
      for ( i = 0; i < *n ; i++ ) {
	dy[iy]  *=  dx[ix];
	ix += *incx;
	iy += *incy;
      }
    }
  return 0;
}







