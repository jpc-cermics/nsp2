/*********************************************************************
 * This Software is ( Copyright ENPC 1998 )                          *
 * Jean-Philippe Chancelier Enpc/Cergrene                            *
 *********************************************************************/
#include <string.h>
#include "nsp/math.h"
#include "convert.h" 

/********************************************************************
 * double2int  ix[i]= int(dx[i])
 * ix and dx can start at the same adress or be disjoints
 ********************************************************************/

int C2F(double2int)(n, dx, ix )
     int *n,*ix;
     double *dx;
{
  int i;
  for (i = 0 ; i < *n ; ++i)  ix[i] = (int) dx[i];
  return 0;
} 

/********************************************************************
 * float2int  ix[i]= int(dx[i])
 * ix and dx can start at the same adress or be disjoints
 ********************************************************************/

int C2F(float2int)(n, dx, ix )
     int *n,*ix;
     float *dx;
{
  int i;
  for (i = 0 ; i < *n ; ++i)  ix[i] = (int) dx[i];
  return 0;
} 

/********************************************************************
 * double2float   rx[i]= real(dx[i])
 * rx and dx can start at the same adress or be disjoints
 ********************************************************************/

int C2F(double2float)(n, dx, rx )
     int *n; float *rx;
     double *dx;
{
  int i;
  for (i = 0 ; i < *n ; ++i)  rx[i] = (float) dx[i];
  return 0;
} 

/********************************************************************
 * int2double : copies an int array idx to a double array dy
 * when idx and dy points to the same area -1 increments must be used 
 * to properly perform the conversion
 ********************************************************************/

int C2F(int2double)(n, idx, incx, dy, incy)
     int *n,idx[],*incx;
     double *dy;
     int *incy;
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

/********************************************************************
 * int2float : copies an int array idx to a float array dy
 * when idx and dy points to the same area -1 increments must be used 
 * to properly perform the conversion
 ********************************************************************/

int C2F(int2float)(n, idx, incx, dy, incy)
     int *n,idx[],*incx;
     float *dy;
     int *incy;
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


/********************************************************************
 * float2double : copies a float array rdx to a double array dy
 * when rdx and dy points to the same area -1 increments must be used 
 * to properly perform the conversion
 ********************************************************************/

int C2F(float2double)(n, rdx, incx, dy, incy)
     int *n,*incx;
     float rdx[];
     double *dy;
     int *incy;
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


/**
 * nsp_convert_double_to_type
 *
 * on place conversion of a double array to a given type 
 * for which sizeof(type) <= sizeof(double)
 *
 */
		
#define TO_TYPE_CONVERT(x,n,Type) { Type *val = (Type *) x ; \
   for ( i=0; i < n; i++) { *val++ = (Type) (*x++) ;} \
}

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


/**
 * nsp_convert_type_to_double 
 *
 * on place conversion of a double array of size n 
 * which is filled with an array of type type. 
 *
 */
		
#define DOUBLE_CONVERT(x,n,Type) {double *xf = &x[n-1]; \
 Type *tf = &(((Type *) x )[n-1]); \
 for ( i=0; i < n; i++) { *xf-- = (double ) (*tf--);} \
}

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

