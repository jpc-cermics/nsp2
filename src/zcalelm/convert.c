/*********************************************************************
 * This Software is ( Copyright ENPC 1998 )                          *
 * Jean-Philippe Chancelier Enpc/Cergrene                            *
 *********************************************************************/
#include <string.h>
#include "nsp/math.h"
#include "convert.h" 


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

