/*------------------------------------------------------------------------
 *    Copyright (C) 1998-2004 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

#include <string.h>
#include "nsp/math.h"
#include "nsp/object.h"
#include "nsp/matutil.h"


/*
 * A set of utilities used in matrix operations
 */

/**
 * nsp_double2int:
 * @n:  array size 
 * @dx: a pointer to an array of double 
 * @ix: a pointer to an array of int
 * 
 * converts an array of @n double precision numbers 
 * in an array of integers. The two arrays can start at 
 * the same address or be disjoints
 * 
 * 
 * Return value: unused
 **/

int nsp_double2int(int *n, double *dx, int *ix)
{
  int i;
  for (i = 0 ; i < *n ; ++i)  ix[i] = (int) dx[i];
  return 0;
} 


/**
 * nsp_float2int:
 * @n:  array size 
 * @dx: a pointer to an array of floats
 * @ix: a pointer to an array of int
 * 
 * converts an array of @n float numbers 
 * in an array of integers. The two arrays can start at 
 * the same address or be disjoints
 * 
 * Return value: unused 
 **/

int nsp_float2int(int *n, float *dx, int *ix)
{
  int i;
  for (i = 0 ; i < *n ; ++i)  ix[i] = (int) dx[i];
  return 0;
} 


/**
 * nsp_double2float:
 * @n:  array size 
 * @dx: a pointer to an array of double 
 * @rx: a pointer to an array of float 
 * 
 * converts an array of @n double precision numbers 
 * in an array of foat numbers. The two arrays can start at 
 * the same address or be disjoints
 * 
 * Return value: unused 
 **/

int nsp_double2float(int *n, double *dx, float *rx)
{
  int i;
  for (i = 0 ; i < *n ; ++i)  rx[i] = (float) dx[i];
  return 0;
} 


/**
 * nsp_int2double:
 * @n: array size 
 * @idx: a pointer to an array of int
 * @incx: increment to use for the int array 
 * @dy:  a pointer to an array of double 
 * @incy: increment to use for the double array 
 * 
 * copy an int array @idx to a double array @dy
 * when @idx and @dy points to the same area -1 increments must be used 
 * to properly perform the conversion.
 * 
 * Return value: 
 **/

int nsp_int2double(int *n, int *idx, int *incx, double *dy, int *incy)
{
  static int i, ix, iy;
  if (*n <= 0) { return 0;  }
  if (*incx == 1 && *incy == 1) 
    {
      for (i = 0 ; i < *n ; ++i) dy[i] = (double) idx[i];
    }
  else 
    {
      /* code for unequal increments or equal increments */
      /* not equal to 1 */
      ix = iy = 0;
      if (*incx < 0) { ix = (-(*n) + 1) * *incx ;} 
      if (*incy < 0) { iy = (-(*n) + 1) * *incy ;}
      for ( i = 0; i < *n ; i++ ) {
	dy[iy] = (double) idx[ix];
	ix += *incx;
	iy += *incy;
      }
    }
  return 0;
} 


/**
 * nsp_int2float:
 * @n: array size 
 * @idx: a pointer to an array of int
 * @incx: increment to use for the int array 
 * @dy:  a pointer to an array of float
 * @incy: increment to use for the float array 
 * 
 * int2float : copies an int array idx to a float array dy
 * when idx and dy points to the same area -1 increments must be used 
 * to properly perform the conversion
 * 
 * 
 * Return value: 
 **/

int nsp_int2float(int *n, int *idx, int *incx, float *dy, int *incy)
{
  static int i, ix, iy;
  if (*n <= 0) { return 0;  }
  if (*incx == 1 && *incy == 1) 
    {
      for (i = 0 ; i < *n ; ++i) dy[i] = (float) idx[i];
    }
  else 
    {
      /* code for unequal increments or equal increments */
      /* not equal to 1 */
      ix = iy = 0;
      if (*incx < 0) { ix = (-(*n) + 1) * *incx ;} 
      if (*incy < 0) { iy = (-(*n) + 1) * *incy ;}
      for ( i = 0; i < *n ; i++ ) {
	dy[iy] = (float) idx[ix];
	ix += *incx;
	iy += *incy;
      }
    }
  return 0;
} 



/**
 * nsp_float2double:
 * @n:  array size 
 * @rdx: a pointer to an array of float
 * @incx:  increment to use for the float array 
 * @dy:  a pointer to an array of double 
 * @incy:  increment to use for the double array 
 * 
 * float2double : copies a float array rdx to a double array dy
 * when rdx and dy points to the same area -1 increments must be used 
 * to properly perform the conversion
 * 
 * Return value: 
 **/

int nsp_float2double(int *n, float *rdx, int *incx, double *dy, int *incy)
{
  static int i, ix, iy;
  if (*n <= 0) { return 0;  }
  if (*incx == 1 && *incy == 1) 
    {
      for (i = 0 ; i < *n ; ++i) dy[i] = (double) rdx[i];
    }
  else 
    {
      /* code for unequal increments or equal increments */
      /* not equal to 1 */
      ix = iy = 0;
      if (*incx < 0) { ix = (-(*n) + 1) * *incx ;} 
      if (*incy < 0) { iy = (-(*n) + 1) * *incy ;}
      for ( i = 0; i < *n ; i++ ) {
	dy[iy] = (double) rdx[ix];
	ix += *incx;
	iy += *incy;
      }
    }
  return 0;
} 


#define TO_TYPE_CONVERT(x,n,Type) { Type *val = (Type *) x ; \
   for ( i=0; i < n; i++) { *val++ = (Type) (*x++) ;} \
}

/**
 * nsp_convert_double_to_type:
 * @x: the double array to be in place converted 
 * @n: the array size 
 * @type: a character which gives the conversion target
 * 
 * in place conversion of a double array to a given type 
 * for which sizeof(type) <= sizeof(double)
 * @type can be  "i", "l", "s", "c", "d", "f", "ui", "ul", "us", "u ", "uc"
 * which stands respectively for int, long, short, char, double, float, unsigned int
 * , unsigned long, unsigned short, unsigned int and insigned char.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_convert_double_to_type(double *x,int n,const char *type)
{ 
  int i; 
  if ( strlen(type) ==  0) return FAIL;

  switch ( type[0] )
    {
    case 'i' : TO_TYPE_CONVERT(x,n,int);       break;
    case 'l' : TO_TYPE_CONVERT(x,n,long int);      break;
    case 's' : TO_TYPE_CONVERT(x,n,short);     break;
    case 'c' : TO_TYPE_CONVERT(x,n,char) ;                 break;
    case 'd' : /* TO_TYPE_CONVERT(x,n,double);*/    break;
    case 'f' : TO_TYPE_CONVERT(x,n,float);     break;
    case 'u' :
      if ( strlen(type) <=  1 ) return FAIL;
      switch ( type[1] )
	{
	case 'i' :  TO_TYPE_CONVERT(x,n,unsigned int); break;
	case 'l' :  TO_TYPE_CONVERT(x,n,unsigned long int); break;
	case 's' :  TO_TYPE_CONVERT(x,n,unsigned short); break;
	case ' ' :  TO_TYPE_CONVERT(x,n,unsigned int); break;
	case 'c' :  TO_TYPE_CONVERT(x,n,unsigned char); break;
	default :  return FAIL; break;
	}
      break; 
    default : 
      return FAIL; break;
    }
  return OK;
}


		
#define DOUBLE_CONVERT(x,n,Type) {double *xf = &x[n-1]; \
 Type *tf = &(((Type *) x )[n-1]); \
 for ( i=0; i < n; i++) { *xf-- = (double ) (*tf--);} \
}

/**
 * nsp_convert_type_to_double:
 * @x: the double array to be in place converted 
 * @n: the array size 
 * @type: a character which gives the conversion target
 * 
 * in place conversion of an array of double (of size @n)
 * which is filled with an array of type @type. 
 * This function is mainly used to recover a double array 
 * which has been converted to @type through the use of 
 * #nsp_convert_double_to_type.
 * @type can be  "i", "l", "s", "c", "d", "f", "ui", "ul", "us", "u ", "uc"
 * which stands respectively for int, long, short, char, double, float, unsigned int
 * , unsigned long, unsigned short, unsigned int and insigned char.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_convert_type_to_double(double *x,int n,const char *type)
{ 
  int i; 
  if ( strlen(type) ==  0) return FAIL;

  switch ( type[0] )
    {
    case 'i' : DOUBLE_CONVERT(x,n,int);       break;
    case 'l' : DOUBLE_CONVERT(x,n,long int);      break;
    case 's' : DOUBLE_CONVERT(x,n,short);     break;
    case 'c' : DOUBLE_CONVERT(x,n,char) ;                 break;
    case 'd' : /*DOUBLE_CONVERT(x,n,double);*/    break;
    case 'f' : DOUBLE_CONVERT(x,n,float);     break;
    case 'u' :
      if ( strlen(type) <=  1 ) return FAIL;
      switch ( type[1] )
	{
	case 'i' :  DOUBLE_CONVERT(x,n,unsigned int); break;
	case 'l' :  DOUBLE_CONVERT(x,n,unsigned long int); break;
	case 's' :  DOUBLE_CONVERT(x,n,unsigned short); break;
	case ' ' :  DOUBLE_CONVERT(x,n,unsigned int); break;
	case 'c' :  DOUBLE_CONVERT(x,n,unsigned char); break;
	default :  return FAIL; break;
	}
      break; 
    default : 
      return FAIL; break;
    }
  return OK;
}




/**
 * nsp_dset:
 * @n: number of affectation to be performed
 * @dx: a double given by a pointer 
 * @dy: an array of double 
 * @incy: increment for array indices 
 * 
 * dy[incy*k] = *dx , k= 0, *n-1 
 * 
 * 
 * Return value: unused 
 **/

int nsp_dset(const int *n,const double * dx,double * dy,const int * incy)
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


/**
 * nsp_dadd:
 * @n: number of += operation to perform
 * @dx: array of double to be added 
 * @incx: increment for @dx array indices
 * @dy: array of double in which addition is performed
 * @incy: increment for @dy array indices
 * 
 * dy[incy*k ] += dx[incx*k]  k=0,*n-1
 * 
 * Return value: unused 
 **/

int nsp_dadd(int *n, double *dx, int *incx, double *dy, int *incy)
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


/**
 * nsp_dsub:
 * @n: number of -= operation to perform
 * @dx: array of double to be substracted 
 * @incx: increment for @dx array indices
 * @dy: array of double in which addition is performed
 * @incy: increment for @dy array indices
 * 
 * dy[incy*k ] -= dx[incx*k]  k=0,*n-1
 * 
 * Return value: 
 **/

int nsp_dsub(int *n, double *dx, int *incx, double *dy, int *incy)
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


/**
 * nsp_dsum:
 * @n: a max indice 
 * @dx: an array of double
 * @incx:  increment for array @dx indices
 * 
 *  sum(dx[incx*k], k=0,*n-1)
 * 
 * Return value: the calculated sum 
 **/

double nsp_dsum(int *n, double *dx, int *incx)
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



/**
 * nsp_dvmul:
 * @n: number of *= operation to perform
 * @dx: array of double
 * @incx: increment for @dx array indices
 * @dy: array of double which is multiplied
 * @incy: increment for @dy array indices
 * 
 * dy[incy*k ] *= dx[incx*k]  k=0,*n-1
 * 
 * Return value: 
 **/

int nsp_dvmul(int *n, double *dx, int *incx, double *dy, int *incy)
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



/**
 * nsp_icopy:
 * @n: number of int to copy 
 * @idx: copy from @idx int array 
 * @incx: increment to use for @idx int array 
 * @idy: copy tp @idy int array 
 * @incy:  increment to use for @idy int array 
 * 
 * copies an int array idx, to an int array idy. 
 * 
 * Return value: 
 **/

int nsp_icopy(const int *n,const int *idx,const int *incx, int *idy,
		const int *incy)
{
  static int i, ix, iy;
  if (*n <= 0) { return 0;  }
  if (*incx == 1 && *incy == 1) 
    {
      for (i = 0 ; i < *n ; ++i) 
	{
	  idy[i] = idx[i];
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
	idy[iy] = idx[ix];
	ix += *incx;
	iy += *incy;
      }
    }
  return 0;
} 


/**
 * nsp_iset:
 * @n: number of int to copy 
 * @idx: an int pointer 
 * @idy: int array 
 * @incy: increment to use for @idy int array 
 * 
 *  idy[incy*k] =dx , k= 0, *n-1 
 * 
 * Return value: 
 **/

int nsp_iset(int *n, int *idx, int *idy, int *incy)
{
  int i,iy;
  if (*n <= 0) {  return 0;  }
  iy = 0;
  if (*incy < 0) {  iy = (-(*n) + 1) * *incy ; } 
  for (i = 0 ; i < *n ; ++i) 
    {
      idy[iy] = *idx;
      iy += *incy;
    }
  return 0;
} 


/**
 * nsp_iadd:
 * @n: number of addition to perform 
 * @ival: an int pointer 
 * @idy: int array 
 * @incy: increment to use for @idy int array 
 * 
 *  iadd  :  idy[k] += ival  k=0,*incy,*n
 * 
 * Return value: 
 **/

int nsp_iadd(int *n, int *ival, int *idy, int *incy)
{
  static int i;
  if (*n <= 0) { return 0;  }
  for (i = 0 ; i < *n ; i+= *incy )
    idy[i] += *ival;
  return 0;
}



/**
 * nsp_dzcopy:
 * @n: number of copy to perform 
 * @zx: a double array 
 * @incx: increment to use for @zx double array 
 * @zy: a #doubleC array 
 * @incy: increment to use for @zy #doubleC array 
 * 
 * dzcopy : copies a double precision x, to a complex vector, zy. 
 *     x becomes the real part of y the imag part is set to zero .
 * 
 * 
 * Return value: 
 **/

int nsp_dzcopy(const int *n,const double * zx,const int * incx, doubleC * zy,const int * incy)
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


/**
 * nsp_dzscal:
 * @n: number of multiplication to perform 
 * @da: a pointer to a double 
 * @zx: a #doubleC array 
 * @incx: increment to use for @zx #doubleC array 
 * 
 * dzscal : scales a complex vector by a constant.
 *          similar to zscal 
 * 
 * Return value: 
 **/

int nsp_dzscal(int *n, double *da, doubleC *zx, int *incx) 
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


/**
 * nsp_dzset:
 * @n: number of affectation to perform 
 * @dx:  a pointer to a double 
 * @zy: a #doubleC array 
 * @incy: increment to use for @zy #doubleC array 
 * 
 * dzset : zy[incy*k] =dx , k= 0, *n-1 
 * 
 * Return value: 
 **/

int nsp_dzset(int *n, double *dx, doubleC *zy, int *incy)
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


/**
 * nsp_zadd:
 * @n: number of addition to perform 
 * @zx: a #doubleC array 
 * @incx: increment to use for @zx #doubleC array 
 * @zy:  a #doubleC array 
 * @incy: increment to use for @zy #doubleC array 
 * 
 * zadd  :  zy[incy*k ] += zx[incx*k]  k=0,*n-1
 * 
 * 
 * Return value: 
 **/

int nsp_zadd(int *n, doubleC *zx, int *incx, doubleC *zy, int *incy)
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


/**
 * myzabs:
 * @real: a double 
 * @imag: a double 
 * 
 * zasum  : sum zx[incx*k]  k=0,*n-1
 *        could use pythag to replace myzabs 
 * 
 * 
 * Return value: 
 **/

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

/**
 * nsp_zasum:
 * @n: number of addition to perform 
 * @zx:  a #doubleC array 
 * @incx: increment to use for @zx #doubleC array 
 * 
 * 
 * 
 * Return value: 
 **/

double nsp_zasum(int *n, doubleC *zx, int *incx) 
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


/**
 * nsp_zsub:
 * @n: number of substraction to perform 
 * @zx: a #doubleC array 
 * @incx: increment to use for @zx #doubleC array 
 * @zy: a #doubleC array 
 * @incy:  increment to use for @zy #doubleC array 
 * 
 * zsub  :  zy[incy*k ] -= zx[incx*k]  k=0,*n-1
 * 
 * 
 * Return value: 
 **/

int nsp_zsub(int *n, doubleC *zx, int *incx, doubleC *zy, int *incy)
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


/**
 * nsp_zsum:
 * @ret_val: 
 * @n: number of addition to perform 
 * @zx:  a #doubleC array 
 * @incx: increment to use for @zx #doubleC array 
 * 
 * zsum   :  sum(zx[incx*k], k=0,*n-1)
 * 
 **/

void nsp_zsum(doubleC *ret_val, int *n, doubleC *zx, int *incx)
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


/**
 * nsp_zvmul:
 * @n: number of multiplication to perform 
 * @zx: a #doubleC array 
 * @incx: increment to use for @zx #doubleC array 
 * @zy: a #doubleC array 
 * @incy: increment to use for @zy #doubleC array 
 * 
 * zvmul  :  zy[incy*k ] *= zx[incx*k]  k=0,*n-1
 * 
 * Return value: 
 **/

int nsp_zvmul(int *n, doubleC *zx, int *incx, doubleC *zy, int *incy)
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


/*
 * hand conversion of urand.f (scilab) to C language. 
 * This routines could be replaced by grand. 
 */

/**
 * nsp_urand:
 * @iy: an int pointer which value it points to is not to be changed between calls.
 * 
 *  urand is a uniform random number generator based  on  theory  and 
 *  suggestions  given  in  d.e. knuth (1969),  vol  2.   the integer  iy 
 *  should be initialized to an arbitrary integer prior to the first call 
 *  to urand.  the calling program should  not  alter  the  value  of  iy 
 *  between  subsequent calls to urand.  values of urand will be returned 
 *  in the interval (0,1). 
 * 
 *
 * Return value: a double 
 **/


#define ININT(x) ((x)>=0 ? floor(x + .5) : -floor(.5 - x))

double nsp_urand(int *iy)
{
  static int m2 = 0;
  static int itwo = 2;
  static double s, halfm;
  static int ia, ic, mic;

  double ret_val, z;
  int m;

  if ( m2 == 0) 
    {
      /*  if first entry, compute machine int word length */
      m = 1;
      while (1) 
	{
	  m2 = m;
	  m = itwo * m2;
	  if (m <= m2) break;
	}
      halfm = (double) m2;
  
      /*  compute multiplier and increment for linear congruential method */
      z = halfm * atan(1.0) / 8.;
      ia = (((int) ININT(z)) << 3) + 5;
      z = halfm * (.5 - sqrt(3.) / 6.);
      ic = (((int) ININT(z)) << 1) + 1;
      mic = m2 - ic + m2;

      /*  s is the scale factor for converting to floating point */

      s = .5 / halfm;

      /*  compute next random number */
    }

  *iy *= ia;
  /*  the following statement is for computers which do not allow */
  /*  int overflow on addition */
  if (*iy > mic) {
    *iy = *iy - m2 - m2;
  }
  *iy += ic;

  /*  the following statement is for computers where the */
  /*  word length for addition is greater than for multiplication */

  if (*iy / 2 > m2) {
    *iy = *iy - m2 - m2;
  }
  /*  the following statement is for computers where int */
  /*  overflow affects the sign bit */
  if (*iy < 0) {
    *iy = *iy + m2 + m2;
  }
  ret_val = (double) (*iy) * s;
  return ret_val;
} 


/*
 *  MAGIC SQUARE
 *  Programed by;  Shin, Kwon Young.
 *  http://user.chollian.net/~brainstm/MagicSquare.htm
 * 
 * A magic square is an arrangement of the numbers from 1 to n^2 (n-squared) in an nxn matrix, 
 * with each number occurring exactly once, and such that the sum of the entries of any row, 
 * any column, or any main diagonal is the same. It is not hard to show that this sum must 
 * be n(n^2+1)/2.
 * 
 */

#include <stdlib.h>

static void odd_num(double *m,int n);
static void even_num(double *m,int n);
static void swap_double(double *m,int n,int i1, int j1, int i2, int j2);

/**
 * nsp_magic_matrix_fill:
 * @m: an array of double 
 * @n: size of the matrix 
 * 
 * fills the array @m (of size @n x @n) which magic square values.
 * A magic square is an arrangement of the numbers from 1 to n^2 (n-squared) in an nxn matrix, 
 * with each number occurring exactly once, and such that the sum of the entries of any row, 
 * any column, or any main diagonal is the same. It is not hard to show that this sum must 
 * be n(n^2+1)/2.
 * 
 * 
 **/

void nsp_magic_matrix_fill(double *m,int n)
{
  if(n < 3 ) return ;
  if( n % 2) 
    odd_num(m,n);
  else 
    even_num(m,n);
}

static void odd_num(double *m,int n)
{
  int i,j,num=1;
  int nn=n*3/2;

  for(i=0; i < n; i++)
    for(j=0; j < n; j++)
      m[((j-i+nn)%n) + n*((i*2-j+n)%n)]=num++;
}

static void even_num(double *m,int n)
{
  int i,j,num=1;
  int nminus=n-1,nmiddle=n/2,nn=n*n+1;
  int osl=0;
  int switch_row[2];
  int last_switch_column;
  int first_block=(n-2)/4,second_block=nminus-first_block;
  int first_inside=n/4,second_inside=nminus-first_inside;

  for(j=0; j < n; j++)
    for(i=0; i < n; i++) {
      if(i >= first_inside && i <= second_inside && j >= first_inside && j <= second_inside)
	m[i+n*j]=num;
      else if((i > first_block && i < second_block) || (j > first_block && j < second_block))
	m[i+n*j]=nn-num;
      else m[i+n*j]=num;
      num++;
    }
  if(!(n%4)) return;

#if 0 
  switch_row[0]=random(nmiddle-1)+first_block+1;
  switch_row[1]=random(nmiddle-1);
  if(switch_row[1] >= first_block) switch_row[1]+=(nmiddle+1);
  last_switch_column=random(nmiddle-1);
  if(last_switch_column >= first_block) last_switch_column+=(nmiddle+1);
#else 
  switch_row[0]=nmiddle;
  switch_row[1]=0;
  last_switch_column=0;
#endif 

  for(i=0; i < nmiddle; i++) {
    if(i==first_block || i==second_block) {
      osl=1-osl;
      continue;
    }
    swap_double(m,n,second_block, i, second_block, nminus-i);
    swap_double(m,n,i, first_block, nminus-i, first_block);
    swap_double(m,n,i, second_block, nminus-i, second_block);
    swap_double(m,n,i, switch_row[osl], nminus-i, switch_row[osl]);
  }
  for(i=first_block+1; i < second_block; i++) {
    swap_double(m,n,first_block, i, second_block, i);
    swap_double(m,n,i, first_block, i, second_block);
  }
  swap_double(m,n,first_block, nmiddle, second_block, nmiddle);
  swap_double(m,n,last_switch_column, first_block, last_switch_column, second_block);
}

static void swap_double(double *m,int n,int i1, int j1, int i2, int j2)
{
  int k;
  k=m[i1+n*j1];
  m[i1+n*j1]=m[i2+n*j2];
  m[i2+n*j2]=k;
}



/**
 * nsp_franck_matrix:
 * @a: a pointer to a double array which is to be filled
 * @n: the matrix dimension (@nx@n)
 * 
 * returns the order @n Franck matrix. 
 * 
 * a(i,j)=n-j+1   si i <= j , 
 * a(j,j-1)=n-j , a(i,j)=0 if  i > j+1 
 * 
 **/

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

/**
 * nsp_franck_inverse_matrix:
 * @a: a pointer to a double array which is to be filled
 * @n: the matrix dimension (@nx@n)
 * 
 * returns the matrix inverse of order @n Franck matrix. 
 * 
 **/

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


/******************************************************
 * Order n Hilbert matrix and its inverse 
 * 
 * a(i,j)= 1/(i+j-1) 
 ******************************************************/

/**
 * nsp_hilbert_matrix:
 * @a: a pointer to a double array which is to be filled
 * @n: the matrix dimension (@nx@n)
 * 
 * returns the  order @n Hilbert matrix. 
 * a(i,j)= 1/(i+j-1) (i=1,..,n and j=1,..,n)
 * 
 **/

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


/**
 * nsp_hilbert_inverse_matrix:
 * @a: 
 * @n: 
 * 
 * returns the inverse of order @n Hilbert matrix. 
 * 
 **/

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






