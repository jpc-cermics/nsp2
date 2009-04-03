/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
 * Copyright (C) 2008-2009 Bruno Pincon Esial/Iecn
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

#include <string.h>
#include "nsp/math.h"
#include "nsp/object.h"
#include "nsp/matutil.h"

/**
 * SECTION:matutil
 * @title: Matrix utilities
 * @short_description: A set of utilities used in matrix operations
 *
 * <para>
 * The following functions are used to convert given arrays on place 
 * from one type to an other type.
 * </para>
 **/

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
  int i, ix, iy;
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
  int i, ix, iy;
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
  int i, ix, iy;
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


#define TO_TYPE_CONVERT(x,n,Type) { Type *val = (Type *) x ;	\
    for ( i=0; i < n; i++) { *val++ = (Type) (*x++) ;}		\
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


		
#define DOUBLE_CONVERT(x,n,Type) {double *xf = &x[n-1];		\
    Type *tf = &(((Type *) x )[n-1]);				\
    for ( i=0; i < n; i++) { *xf-- = (double ) (*tf--);}	\
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

int nsp_dadd(const int n,const double *dx,const  int incx, double *dy,const  int incy)
{
  int i, ix, iy;
  if (n <= 0) { return 0;  }
  if (incx == 1 && incy == 1) 
    {
      for (i = 0 ; i < n ; ++i) 
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
      if (incx < 0) { ix = (-(n) + 1) * incx ;} 
      if (incy < 0) { iy = (-(n) + 1) * incy ;}
      for ( i = 0; i < n ; i++ ) {
	dy[iy] += dx[ix] ;
	ix += incx;
	iy += incy;
      }
    }
  return 0;
}

/**
 * nsp_dadd_maxplus:
 * @n: number of += operation to perform
 * @dx: array of double to be added 
 * @incx: increment for @dx array indices
 * @dy: array of double in which addition is performed
 * @incy: increment for @dy array indices
 * 
 * dy = dy + dx where +  returns -%inf when a or b is -%inf 
 * Return value: unused 
 **/

int nsp_dadd_maxplus(const int n, const double *dx, const  int incx, double *dy, const  int incy)
{
  int i, ix, iy;
  if (n <= 0) { return 0;  }
  if (incx == 1 && incy == 1) 
    {
      for (i = 0 ; i < n ; ++i) 
	{
	  if ( isinf(*dy) != -1  && isinf( *dx) != -1 ) 
	    {
	      *(dy++) += *(dx++);
	    }
	  else 
	    {
	      *(dy) = Min(*dy,*dx); dy++;dx++;
	    }
	}
      return 0;
    }
  else 
    {
      register double *y;
      register const double *x;
      /* code for unequal increments or equal increments */
      /* not equal to 1 */
      ix = iy = 0;
      if (incx < 0) { ix = (-(n) + 1) * incx ;} 
      if (incy < 0) { iy = (-(n) + 1) * incy ;}
      y= dy+iy;
      x= dx+ix;
      for ( i = 0; i < n ; i++ ) 
	{
	  if ( isinf( (*y) ) != -1  && isinf( (*x) ) != -1 ) 
	    {
	      *y += *x;
	    }
	  else 
	    {
	      *y = Min(*y,*x);
	    }
	  x += incx;
	  y += incy;
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

int nsp_dsub(const int n,const  double *dx,const int incx, double *dy,const  int incy)
{
  int i, ix, iy;
  if (n <= 0) { return 0;  }
  if (incx == 1 && incy == 1) 
    {
      for (i = 0 ; i < n ; ++i) 
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
      if (incx < 0) { ix = (-(n) + 1) * incx ;} 
      if (incy < 0) { iy = (-(n) + 1) * incy ;}
      for ( i = 0; i < n ; i++ ) {
	dy[iy] -= dx[ix];
	ix += incx;
	iy += incy;
      }
    }
  return 0;
}

/**
 * nsp_dsub_maxplus:
 * @n: number of -= operation to perform
 * @dx: array of double to be substracted 
 * @incx: increment for @dx array indices
 * @dy: array of double in which addition is performed
 * @incy: increment for @dy array indices
 * 
 * dy[incy*k ] -= dx[incx*k]  k=0,*n-1
 * except when dx[incx*k] = -%inf 
 * Return value: 
 **/

int nsp_dsub_maxplus(const int n,const double *dx,const int incx, double *dy,const int incy)
{
  int i, ix, iy;
  if (n <= 0) { return 0;  }
  if (incx == 1 && incy == 1) 
    {
      for (i = 0 ; i < n ; ++i) 
	{
	  if ( isinf( dx[i] ) != -1)  dy[i] -= dx[i] ;
	}
      return 0;
    }
  else 
    {
      /* code for unequal increments or equal increments */
      /* not equal to 1 */
      ix = iy = 0;
      if (incx < 0) { ix = (-(n) + 1) * incx ;} 
      if (incy < 0) { iy = (-(n) + 1) * incy ;}
      for ( i = 0; i < n ; i++ ) {
	if ( isinf( dx[ix] ) != -1) dy[iy] -= dx[ix];
	ix += incx;
	iy += incy;
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
 * nsp_dsumrows:
 * @x: an array m x n of double with column major order
 * @res: an array of size m with the result
 * @m, @n: sizes of the matrix @x
 * 
 *   res[i] = sum(x[i,j], j=0,n-1)
 * 
 * computes the sum of each row 
 **/
void nsp_dsumrows(double *x, double *res, int m, int n)
{
  int i, j, k, k2, k3, p;
  p = n % 3;

  for ( i = 0 ; i < m ; i++ ) 
    res[i] = 0.0;

  k = 0;
  for ( j = 0 ; j < p ; j++ )
    for ( i = 0 ; i < m ; i++, k++)
      res[i] += x[k];

  k2 = k + m;
  k3 = k2 + m;
  for ( j = p ; j < n ; j+=3, k+=2*m, k2+=2*m, k3+=2*m )
    for ( i = 0 ; i < m ; i++, k++, k2++, k3++)
      res[i] += x[k] + x[k2] + x[k3];
}

/**
 * nsp_zsumrows:
 * @x: an array m x n of complex with column major order
 * @res: an array of size m with the result
 * @m, @n: sizes of the matrix @x
 * 
 *   res[i] = sum(x[i,j], j=0,n-1)
 * 
 * computes the sum of each row 
 **/
void nsp_zsumrows(doubleC *x, doubleC *res, int m, int n)
{
  int i, j, k, k2, k3, p;
  p = n % 3;

  for ( i = 0 ; i < m ; i++ ) 
    { res[i].r = 0.0; res[i].i = 0.0; }

  k = 0;
  for ( j = 0 ; j < p ; j++ )
    for ( i = 0 ; i < m ; i++, k++)
      { 
	res[i].r += x[k].r; 
	res[i].i += x[k].i; 
      }

  k2 = k + m;
  k3 = k2 + m;
  for ( j = p ; j < n ; j+=3, k+=2*m, k2+=2*m, k3+=2*m )
    for ( i = 0 ; i < m ; i++, k++, k2++, k3++)
      { 
	res[i].r += x[k].r + x[k2].r + x[k3].r;
	res[i].i += x[k].i + x[k2].i + x[k3].i;
      }
}

/**
 * nsp_dprod:
 * @x: array of double
 * @n: number of components of @x
 * @incx: stride between 2 components of @x
 * 
 *  prod(x[incx*k], k=0,*n-1)
 * 
 * Return value: the calculated product
 **/
double nsp_dprod(double *x, int n, int incx)
{
  double d=1.0;
  int i;

  if (incx == 1)
    {
      int m = n % 6 ;
      for ( i = 0 ; i < m ; i++)
	d *= x[i];

      for ( i = m ; i < n ; i+=6 )
	d *= x[i] * x[i+1] * x[i+2] * x[i+3] * x[i+4] * x[i+5];
    }
  else
    for ( i = 0 ; i < n*incx ; i += incx ) d  *= x[i];

  return d;
}


/**
 * nsp_dprodrows:
 * @x: an array m x n of double with column major order
 * @res: an array of size m with the result
 * @m, @n: sizes of the matrix @x
 * 
 *   res[i] = prod(x[i,j], j=0,n-1)
 * 
 * computes the product of each row 
 **/
void nsp_dprodrows(double *x, double *res, int m, int n)
{
  int i, j, k, k2, k3, k4, p;
  p = n % 4;

  for ( i = 0 ; i < m ; i++ )
    res[i] = 1.0;

  k = 0;
  for ( j = 0 ; j < p ; j++ )
    for ( i = 0 ; i < m ; i++, k++)
      res[i] *= x[k];

  k2 = k + m;
  k3 = k2 + m;
  k4 = k3 + m;
  for ( j = p ; j < n ; j+=4, k+=3*m, k2+=3*m, k3+=3*m, k4+=3*m )
    for ( i = 0 ; i < m ; i++, k++, k2++, k3++, k4++)
      res[i] *= (x[k] * x[k2]) * (x[k3] * x[k4]);
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

int nsp_dvmul(const int n,const  double *dx,const  int incx, double *dy,const  int incy)
{
  int i, ix, iy;
  if (n <= 0) { return 0;  }
  if (incx == 1 && incy == 1) 
    {
      for (i = 0 ; i < n ; ++i) 
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
      if (incx < 0) { ix = (-(n) + 1) * incx ;} 
      if (incy < 0) { iy = (-(n) + 1) * incy ;}
      for ( i = 0; i < n ; i++ ) {
	dy[iy]  *=  dx[ix];
	ix += incx;
	iy += incy;
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
  int i, ix, iy;
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
  int i;
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
  int i, ix, iy;
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
  int i, ix, iy;
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
 * nsp_zadd_maxplus:
 * @n: number of addition to perform 
 * @zx: a #doubleC array 
 * @incx: increment to use for @zx #doubleC array 
 * @zy:  a #doubleC array 
 * @incy: increment to use for @zy #doubleC array 
 * 
 * dy = dy + dx where +  returns -%inf when a or b is -%inf 
 * Return value: unsused 
 **/


int nsp_zadd_maxplus(int *n, doubleC *zx, int *incx, doubleC *zy, int *incy)
{
  int i, ix, iy;
  if (*n <= 0) { return 0;  }
  if (*incx == 1 && *incy == 1) 
    {
      for (i = 0 ; i < *n ; ++i) 
	{
	  if ( isinf( (*zy).r)  != -1 && isinf( (*zx).r) != -1) 
	    {
	      (*zy).r += (*zx).r;
	    }
	  else 
	    {
	      (*zy).r = Min((*zy).r,(*zx).r);
	    }
	  if ( isinf( (*zy).i) != -1 && isinf( (*zx).i) != -1) 
	    {
	      (*zy).i += (*zx).i;
	    }
	  else 
	    {
	      (*zy).i = Min((*zy).i,(*zx).i);
	    }
	  zx++;zy++;
	}
      return 0;
    }
  else 
    {
      register doubleC *y,*x;
      /* code for unequal increments or equal increments */
      /* not equal to 1 */
      ix = iy = 0;
      if (*incx < 0) { ix = (-(*n) + 1) * *incx ;} 
      if (*incy < 0) { iy = (-(*n) + 1) * *incy ;}
      y= zy+iy;
      x= zx+ix;
      for ( i = 0; i < *n ; i++ ) 
	{
	  if ( isinf( (*y).r ) != -1 && isinf( (*x).r ) != -1 ) 
	    {
	      (*y).r += (*x).r;
	    }
	  else 
	    {
	      (*y).r = Min((*y).r,(*x).r);
	    }
	  if ( isinf( (*y).i ) != -1 && isinf( (*x).i ) != -1 ) 
	    {
	      (*y).i += (*x).i;
	    }
	  else 
	    {
	      (*y).i = Min((*y).i,(*x).i);
	    }
	  x += *incx;
	  y += *incy;
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
  int i, ix, iy;
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
 * nsp_zsub_maxplus:
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

int nsp_zsub_maxplus(int *n, doubleC *zx, int *incx, doubleC *zy, int *incy)
{
  int i, ix, iy;
  if (*n <= 0) { return 0;  }
  if (*incx == 1 && *incy == 1) 
    {
      for (i = 0 ; i < *n ; ++i) 
	{
	  if ( isinf( zx[i].r ) != -1) zy[i].r -= zx[i].r ;
	  if ( isinf( zx[i].i ) != -1) zy[i].i -= zx[i].i;
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
	if ( isinf(  zx[ix].r ) != -1)zy[iy].r -= zx[ix].r ;
	if ( isinf(  zx[ix].i ) != -1)zy[iy].i -= zx[ix].i;
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
  int i, ix, iy;
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

/**
 * nsp_dcross:
 * @A: (input) 3 x @n or @n x 3  double array
 * @B: (input) array with same dim than @A
 * @C: (output) array with same dims than @A and @B
 * @n: (input) first or second dim length of the arrays
 * @dim_flag: =1 for 3 x @n arrays and =2 for  @n x 3 arrays
 * 
 * compute (in the array @C) the cross product between the
 * respective "3d vectors" stored in @A or @B. Real version
 *  
 **/
void nsp_dcross(double *A, double *B, double *C, int n, int dim_flag)
{
  int k;

  if ( dim_flag == 1 )
    for ( k = 0 ; k < n ; k++, A+=3, B+=3, C+=3 )
      {
	C[0] = A[1]*B[2] - A[2]*B[1];
	C[1] = B[0]*A[2] - B[2]*A[0];
	C[2] = A[0]*B[1] - A[1]*B[0];
      }
  else /* dim_flag == 2 */
    {
      double *C1 = C + n, *C2 = C + 2*n, *A1 = A + n, *A2 = A + 2*n, *B1 = B + n, *B2 = B + 2*n;
      for ( k = 0 ; k < n ; k++ )
	{
	  C [k] = A1[k]*B2[k] - A2[k]*B1[k];
	  C1[k] = B [k]*A2[k] - B2[k]*A [k];
	  C2[k] = A [k]*B1[k] - A1[k]*B [k];
	}
    }
}

/**
 * nsp_zcross:
 * @A: (input) 3 x @n or @n x 3  double array
 * @B: (input) array with same dim than @A
 * @C: (output) array with same dims than @A and @B
 * @n: (input) first or second dim length of the arrays
 * @dim_flag: =1 for 3 x @n arrays and =2 for  @n x 3 arrays
 * 
 * compute (in the array @C) the cross product between the
 * respective "3d vectors" stored in @A or @B. Complex version
 *  
 **/
void nsp_zcross(doubleC *A, doubleC *B, doubleC *C, int n, int dim_flag)
{
  int k;

  if ( dim_flag == 1 )
    for ( k = 0 ; k < n ; k++, A+=3, B+=3, C+=3 )
      {
	C[0].r = (A[1].r*B[2].r - A[1].i*B[2].i) - (A[2].r*B[1].r - A[2].i*B[1].i);
	C[0].i = (A[1].r*B[2].i + A[1].i*B[2].r) - (A[2].r*B[1].i + A[2].i*B[1].r);
	C[1].r = (B[0].r*A[2].r - B[0].i*A[2].i) - (B[2].r*A[0].r - B[2].i*A[0].i);
	C[1].i = (B[0].r*A[2].i + B[0].i*A[2].r) - (B[2].r*A[0].i + B[2].i*A[0].r);
	C[2].r = (A[0].r*B[1].r - A[0].i*B[1].i) - (A[1].r*B[0].r - A[1].i*B[0].i);
	C[2].i = (A[0].r*B[1].i + A[0].i*B[1].r) - (A[1].r*B[0].i + A[1].i*B[0].r);
      }
  else /* dim_flag == 2 */
    {
      doubleC *C1 = C + n, *C2 = C + 2*n, *A1 = A + n, *A2 = A + 2*n, *B1 = B + n, *B2 = B + 2*n;
      for ( k = 0 ; k < n ; k++ )
	{
	  C [k].r = (A1[k].r*B2[k].r - A1[k].i*B2[k].i) - (A2[k].r*B1[k].r - A2[k].i*B1[k].i);
	  C [k].i = (A1[k].r*B2[k].i + A1[k].i*B2[k].r) - (A2[k].r*B1[k].i + A2[k].i*B1[k].r);
	  C1[k].r = (B [k].r*A2[k].r - B [k].i*A2[k].i) - (B2[k].r*A [k].r - B2[k].i*A [k].i);
	  C1[k].i = (B [k].r*A2[k].i + B [k].i*A2[k].r) - (B2[k].r*A [k].i + B2[k].i*A [k].r);
	  C2[k].r = (A [k].r*B1[k].r - A [k].i*B1[k].i) - (A1[k].r*B [k].r - A1[k].i*B [k].i);
	  C2[k].i = (A [k].r*B1[k].i + A [k].i*B1[k].r) - (A1[k].r*B [k].i + A1[k].i*B [k].r);
	}
    }
}

/**
 * nsp_dzdot:
 * @A: (input) vector of #double of size n
 * @B: (input) vectors of #doubleC of size n
 * @n: (input) length of the vectors @A and @B
 * 
 * an utility to complete the BLAS ddot and zdot functions, 
 * this one compute the scalar product A . B with a real A
 * and a complex B.
 *
 * return a #doubleC
 *  
 **/
doubleC nsp_dzdot(double *A, doubleC *B, int n)
{
  doubleC Z = { 0.0 , 0.0 };
  int k, p = n % 3;

  for ( k = 0 ; k < p ; k++ )
    {
      Z.r += A[k]*B[k].r;
      Z.i += A[k]*B[k].i;
    }
  for ( k = p ; k < n ; k+=3 )
    {
      Z.r += A[k]*B[k].r + A[k+1]*B[k+1].r + A[k+2]*B[k+2].r;
      Z.i += A[k]*B[k].i + A[k+1]*B[k+1].i + A[k+2]*B[k+2].i;
    }

  return Z;
}

/**
 * nsp_zddot:
 * @A: (input) vector of #double of size n
 * @B: (input) vectors of #doubleC of size n
 * @n: (input) length of the vectors @A and @B
 * 
 * an utility to complete the BLAS ddot and zdot functions, 
 * this one compute the scalar product A . B with a complex A
 * and a double B.
 *
 * return a #doubleC
 *  
 **/
doubleC nsp_zddot(doubleC *A, double *B, int n)
{
  doubleC Z = { 0.0 , 0.0 };
  int k, p = n % 3;

  for ( k = 0 ; k < p ; k++ )
    {
      Z.r += A[k].r*B[k];
      Z.i -= A[k].i*B[k];
    }
  for ( k = p ; k < n ; k+=3 )
    {
      Z.r += A[k].r*B[k] + A[k+1].r*B[k+1] + A[k+2].r*B[k+2];
      Z.i -= A[k].i*B[k] + A[k+1].i*B[k+1] + A[k+2].i*B[k+2];
    }
  
  return Z;
}

/**
 * nsp_rowdddot:
 * @A: (input) array of #double of dimensions m x n
 * @B: (input) array of #double of dimensions m x n
 * @Z: (output) vector of #double of length m
 * @m,@m: (input) dimensions sizes
 *
 * compute Z_i = sum(j=0,n-1) A_i_j * B_i_j, for 0<= i < m
 * 
 * an utility to complete the BLAS ddot and zdot functions.
 * These ones are not efficient to compute scalar product
 * of the respective rows of 2 matrices @A and @B (we 
 * can use m time ddot (with a increment of m) but this is slow). 
 * This function tries to be more efficient at least on my
 * machine.
 *  
 **/
void nsp_rowdddot(double *A, double *B, double *Z, int m, int n)
{
  int i, j, p = n % 3;
  double *A1, *A2, *B1, *B2;

  for ( i = 0 ; i < m ; i++ )
    Z[i] = 0.0;

  for ( j = 0; j < p ; j++, A+=m, B+=m )
    for ( i = 0 ; i < m ; i++)
      Z[i] += A[i]*B[i];

  A1 = A+m; A2 = A1+m;
  B1 = B+m; B2 = B1+m;
  for ( j = p ; j < n ; j+=3, A+=3*m, A1+=3*m, A2+=3*m, B+=3*m, B1+=3*m, B2+=3*m)
    for ( i = 0 ; i < m ; i++ )
      Z[i] += A[i]*B[i] + A1[i]*B1[i] + A2[i]*B2[i];
}

/**
 * nsp_rowdzdot:
 * @A: (input) array of #double of dimensions m x n
 * @B: (input) array of #doubleC of dimensions m x n
 * @Z: (output) vector of #doubleC of length m
 * @m,@m: (input) dimensions sizes
 *
 * compute Z_i = sum(j=0,n-1) A_i_j * B_i_j, for 0<= i < m
 * 
 * an utility to complete the BLAS ddot and zdot functions.
 * (see #nsp_rowdddot).
 *  
 **/
void nsp_rowdzdot(double *A, doubleC *B, doubleC *Z, int m, int n)
{
  int i, j, p = n % 3;
  double *A1, *A2;
  doubleC *B1, *B2, ZeroC = {0.0,0.0};;

  for ( i = 0 ; i < m ; i++ )
    Z[i] = ZeroC;

  for ( j = 0; j < p ; j++, A+=m, B+=m )
    for ( i = 0 ; i < m ; i++)
      {
	Z[i].r += A[i]*B[i].r; Z[i].i += A[i]*B[i].i;
      }

  A1 = A+m; A2 = A1+m;
  B1 = B+m; B2 = B1+m;
  for ( j = p ; j < n ; j+=3, A+=3*m, A1+=3*m, A2+=3*m, B+=3*m, B1+=3*m, B2+=3*m)
    for ( i = 0 ; i < m ; i++ )
      {
	Z[i].r += A[i]*B[i].r + A1[i]*B1[i].r + A2[i]*B2[i].r;
	Z[i].i += A[i]*B[i].i + A1[i]*B1[i].i + A2[i]*B2[i].i;
      }
}

/**
 * nsp_rowzddot:
 * @A: (input) array of #doubleC of dimensions m x n
 * @B: (input) array of #double of dimensions m x n
 * @Z: (output) vector of #doubleC of length m
 * @m,@m: (input) dimensions sizes
 *
 * compute Z_i = sum(j=0,n-1) conj(A_i_j) * B_i_j, for 0<= i < m
 * 
 * an utility to complete the BLAS ddot and zdot functions.
 * (see #nsp_rowdddot).
 *  
 **/
void nsp_rowzddot(doubleC *A, double *B, doubleC *Z, int m, int n)
{
  int i, j, p = n % 3;
  doubleC *A1, *A2, ZeroC = {0.0,0.0};
  double *B1, *B2;

  for ( i = 0 ; i < m ; i++ )
    Z[i] = ZeroC;

  for ( j = 0; j < p ; j++, A+=m, B+=m )
    for ( i = 0 ; i < m ; i++)
      {
	Z[i].r += A[i].r*B[i]; Z[i].i -= A[i].i*B[i];
      }

  A1 = A+m; A2 = A1+m;
  B1 = B+m; B2 = B1+m;
  for ( j = p ; j < n ; j+=3, A+=3*m, A1+=3*m, A2+=3*m, B+=3*m, B1+=3*m, B2+=3*m)
    for ( i = 0 ; i < m ; i++ )
      {
	Z[i].r += A[i].r*B[i] + A1[i].r*B1[i] + A2[i].r*B2[i];
	Z[i].i -= A[i].i*B[i] + A1[i].i*B1[i] + A2[i].i*B2[i];
      }
}

/**
 * nsp_rowzzdot:
 * @A: (input) array of #doubleC of dimensions m x n
 * @B: (input) array of #doubleC of dimensions m x n
 * @Z: (output) vector of #doubleC of length m
 * @m,@m: (input) dimensions sizes
 *
 * compute Z_i = sum(j=0,n-1) conj(A_i_j) * B_i_j, for 0<= i < m
 * 
 * an utility to complete the BLAS ddot and zdot functions.
 * (see #nsp_rowdddot).
 *  
 **/
void nsp_rowzzdot(doubleC *A, doubleC *B, doubleC *Z, int m, int n)
{
  int i, j, p = n % 3;
  doubleC *A1, *A2, *B1, *B2, ZeroC = {0.0,0.0};

  for ( i = 0 ; i < m ; i++ )
    Z[i] = ZeroC;

  for ( j = 0; j < p ; j++, A+=m, B+=m )
    for ( i = 0 ; i < m ; i++)
      {
	Z[i].r += (A[i].r*B[i].r + A[i].i*B[i].i); 
	Z[i].i += (A[i].r*B[i].i - A[i].i*B[i].r);
      }

  A1 = A+m; A2 = A1+m;
  B1 = B+m; B2 = B1+m;
  for ( j = p ; j < n ; j+=3, A+=3*m, A1+=3*m, A2+=3*m, B+=3*m, B1+=3*m, B2+=3*m)
    for ( i = 0 ; i < m ; i++ )
      {
	Z[i].r += (A[i].r*B[i].r + A[i].i*B[i].i) + (A1[i].r*B1[i].r + A1[i].i*B1[i].i) + (A2[i].r*B2[i].r + A2[i].i*B2[i].i); 
	Z[i].i += (A[i].r*B[i].i - A[i].i*B[i].r) + (A1[i].r*B1[i].i - A1[i].i*B1[i].r) + (A2[i].r*B2[i].i - A2[i].i*B2[i].r);
       }
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
 * fills @a with the order @n Franck matrix. 
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
 * fills @a with the matrix inverse of order @n Franck matrix. 
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
 * fills @a with the order @n Hilbert matrix. 
 * @a(i,j)= 1/(i+j-1) (i=1,..,n and j=1,..,n)
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
 * @a: a pointer to a double array which is to be filled
 * @n: the matrix dimension (@nx@n)
 * 
 * fills @a with the inverse of order @n Hilbert matrix.
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


/*------------------------------------------------------------------------
 * next functions are copyrighted 
 *    Copyright (c) 1997-2009 by Inria Lorraine.  All Rights Reserved 
 *    fleury - Jun 29, 1999: Created. 
 * modified by jpc for Nsp.
 *------------------------------------------------------------------------*/

/* utility */ 

static void swap(double* ptr1, double* ptr2, int size)
{
  double tmp;
  int i;
  for (i = 0; i < size; ++i) {
    tmp = ptr1[i];
    ptr1[i] = ptr2[i];
    ptr2[i] = tmp;
  }
}


/**
 * nsp_double2complex:
 * @tab: array of double to be converted 
 * @size: size of array 
 *
 *    Converts complex representation in @tab 
 *     [r_0, r_1,..., r_n, i_0, i_1,..., i_n]
 *    into [r_0, i_0, r_1, i_1, ..., r_n, i_n]
 *     Complexity O(nlogn) for this version. One can easly have
 *     O(nloglogn) by pruning the recursion. Next version wil take care 
 *     of the cache size.
 **/

/*     fleury - May 7, 1999: Created. */

void nsp_double2complex( double *tab, int size)
{
  int nb;
  if (size <= 1) {
    return;
  }
  nb = size / 2;
  if ( size % 2) {		
    /* si le nbr est impaire on "coupe" un
     * complexe en deux et donc il faut
     * reparer ce crime... 
     */
    swap(&(tab[nb]), &(tab[size + nb]), 1);
    swap(&tab[size - nb - 1], &tab[size], nb + 1); 
    nsp_double2complex(&tab[0], nb);
    nsp_double2complex(&tab[size + 1], nb);
  }
  else {
    swap(&tab[size - nb], &tab[size], nb); 
    nsp_double2complex(&tab[0], nb);
    nsp_double2complex(&tab[size], nb);
  }
}

/**
 * nsp_complex2double:
 * @tab: array of double to be converted 
 * @size: size of array 
 *
 *    Converts complex representation in @tab 
 *    [r_0, i_0, r_1, i_1, ..., r_n, i_n]
 *    into [r_0, r_1,..., r_n, i_0, i_1,..., i_n]
 *     Complexity O(nlogn) for this version. One can easly have
 *     O(nloglogn) by pruning the recursion. Next version wil take care 
 *     of the cache size.
 **/

/*     fleury - May 7, 1999: Created. */

void nsp_complex2double(double *tab, int size)
{
  int nb;
  
  if (size <= 1 ) {
    return;
  }
  nb = size / 2;
  if (size % 2) {		
    /* si le nbr est impaire on "coupe" un
     * complexe en deux et donc il faut
     * reparer ce crime... 
     */
    nsp_complex2double(&tab[0], nb);
    nsp_complex2double(&tab[size + 1], nb);
    swap(&(tab[size - 1]), &(tab[size]), 1);
    swap(&tab[size - nb - 1], &tab[size], nb + 1); 
  }
  else {
    nsp_complex2double(&tab[0], nb);
    nsp_complex2double(&tab[size], nb);
    swap(&tab[size - nb], &tab[size], nb); 
  }
} 

