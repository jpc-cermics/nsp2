/*------------------------------------------------------------------------
 *    Copyright (C) 1998-2003 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

#include "nsp/object.h"
#include "nsp/calelm.h"

/********************************************************************
 * dzcopy : copies a double precision x, to a complex vector, zy. 
 *     x becomes the real part of y the imag part is set to zero .
 ********************************************************************/

int C2F(dzcopy)(const int *n,const double * zx,const int * incx, doubleC * zy,const int * incy)
{
  static int i, ix, iy;
  if (*n <= 0) { return 0;  }
  if (*incx == 1 && *incy == 1) 
    {
      for (i = 0 ; i < *n ; ++i) 
	{
	  zy[i].r = zx[i] , zy[i].i = 0.0;
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
	zy[iy].r = zx[ix], zy[iy].i = 0.00;
	ix += *incx;
	iy += *incy;
      }
    }
  return 0;
} 


/********************************************************************
 * dzscal : scales a complex vector by a constant.
 *          similar to zscal 
 ********************************************************************/

int C2F(dzscal) (int *n, double *da, doubleC *zx, int *incx) 
{
  int i,ix;
  if (*n <= 0 || *incx <= 0) return 0;
  if (*incx == 1) 
    {
      /*        code for increment equal to 1 */
      for (i = 0 ; i < *n ; ++i) 
	{
	  zx[i].r *= *da ; zx[i].i *= *da;
	}
    }
  else
    {
      /*        code for increment not equal to 1 */
      ix = 0 ;
      for (i = 0 ; i < *n ; ++i) 
	{
	  zx[ix].r *= *da , zx[ix].i *= *da ;
	  ix += *incx;
	}
    }
  return 0;
} 

/********************************************************************
 * dzset : zy[incy*k] =dx , k= 0, *n-1 
 ********************************************************************/

int C2F(dzset) (int *n, double *dx, doubleC *zy, int *incy)
{
  int i,iy;
  if (*n <= 0) {  return 0;  }
  iy = 0;
  if (*incy < 0) {  iy = (-(*n) + 1) * *incy ; } 
  for (i = 0 ; i < *n ; ++i) 
    {
      zy[iy].r = *dx , zy[iy].i = 0.0;
      iy += *incy;
  }
  return 0;
} 

/********************************************************************
 * zadd  :  zy[incy*k ] += zx[incx*k]  k=0,*n-1
 ********************************************************************/

extern int C2F(zadd) (int *n, doubleC *zx, int *incx, doubleC *zy, int *incy)
{
  static int i, ix, iy;
  if (*n <= 0) { return 0;  }
  if (*incx == 1 && *incy == 1) 
    {
      for (i = 0 ; i < *n ; ++i) 
	{
	  zy[i].r += zx[i].r , zy[i].i += zx[i].i;
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
	zy[iy].r += zx[ix].r , zy[iy].i += zx[ix].i;
	ix += *incx;
	iy += *incy;
      }
    }
  return 0;
}

/********************************************************************
 * zasum  : sum zx[incx*k]  k=0,*n-1
 *        could use pythag to replace myzabs 
 ********************************************************************/

double myzabs(double real,double imag)
{
  double temp;
  if(real < 0) real = -real;
  if(imag < 0) imag = -imag;
  if(imag > real){
	temp = real;
	real = imag;
	imag = temp;
  }
  if((real+imag) == real)
    return(real);
  temp = imag/real;
  temp = real*sqrt(1.0 + temp*temp);  /*overflow!!*/
  return(temp);
}

double C2F(zasum) (int *n, doubleC *zx, int *incx) 
{
  int i;
  double dasum = 0.00;
  if( *n <=0 || *incx <= 0) return dasum;
  if (*incx == 1)
    {
      int m = *n % 6 ;
      for ( i = 0 ; i < m ; i++)
	dasum += myzabs(zx[i].r,zx[i].i);
      for ( i= m ; i < *n ; i+=6 )
	{
	  dasum += myzabs(zx[i].r,zx[i].i) + 
	    myzabs(zx[i+1].r,zx[i+1].i) + 
	    myzabs(zx[i+2].r,zx[i+2].i) + 
	    myzabs(zx[i+3].r,zx[i+3].i) + 
	    myzabs(zx[i+4].r,zx[i+4].i) + 
	    myzabs(zx[i+5].r,zx[i+5].i);
	}
    }
  else
    {
      int nincx = *n * *incx;
      for ( i = 0 ; i < nincx ; i += *incx )
        dasum += myzabs(zx[i].r,zx[i].i);
    }
  return dasum;
}


/********************************************************************
 * zsub  :  zy[incy*k ] -= zx[incx*k]  k=0,*n-1
 ********************************************************************/

int C2F(zsub) (int *n, doubleC *zx, int *incx, doubleC *zy, int *incy)
{
  static int i, ix, iy;
  if (*n <= 0) { return 0;  }
  if (*incx == 1 && *incy == 1) 
    {
      for (i = 0 ; i < *n ; ++i) 
	{
	  zy[i].r -= zx[i].r , zy[i].i -= zx[i].i;
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
	zy[iy].r -= zx[ix].r , zy[iy].i -= zx[ix].i;
	ix += *incx;
	iy += *incy;
      }
    }
  return 0;
}


/********************************************************************
 * zsum   :  sum(zx[incx*k], k=0,*n-1)
 ********************************************************************/

void C2F(zsum) (doubleC *ret_val, int *n, doubleC *zx, int *incx)
{
  int i;
  ret_val->r = 0.0;   ret_val->i = 0.0;
  if( *n <=0 || *incx <= 0) return ;
  if (*incx == 1)
    {
      int m = *n % 6 ;
      for ( i = 0 ; i < m ; i++)
	{
	  ret_val->r += zx[i].r;
	  ret_val->i += zx[i].i;
	}
      for ( i= m ; i < *n ; i+=6 )
	{
	  ret_val->r += zx[i].r + zx[i+1].r + zx[i+2].r + 
	    zx[i+3].r + zx[i+4].r + zx[i+5].r;
	  ret_val->i += zx[i].i + zx[i+1].i + zx[i+2].i + 
	    zx[i+3].i + zx[i+4].i + zx[i+5].i;
	}
    }
  else
    {
      int nincx = *n * *incx;
      for ( i = 0 ; i < nincx ; i += *incx )
        {
	  ret_val->r  += zx[i].r;
	  ret_val->i  += zx[i].i;
	}
    }
}


/********************************************************************
 * zvmul  :  zy[incy*k ] *= zx[incx*k]  k=0,*n-1
 ********************************************************************/

int C2F(zvmul) (int *n, doubleC *zx, int *incx, doubleC *zy, int *incy)
{
  double x;
  static int i, ix, iy;
  if (*n <= 0) { return 0;  }
  if (*incx == 1 && *incy == 1) 
    {
      for (i = 0 ; i < *n ; ++i) 
	{
	  x= zy[i].r * zx[i].r - zy[i].i * zx[i].i;
	  zy[i].i = zy[i].r * zx[i].i + zy[i].i * zx[i].r;
	  zy[i].r = x ;
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
	x= zy[iy].r * zx[ix].r - zy[iy].i * zx[ix].i;
	zy[iy].i = zy[iy].r * zx[ix].i + zy[iy].i * zx[ix].r;
	zy[iy].r = x ;
	ix += *incx;
	iy += *incy;
      }
    }
  return 0;
}


