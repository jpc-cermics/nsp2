/*********************************************************************
 * This Software is ( Copyright ENPC 1998 )                          *
 * Jean-Philippe Chancelier Enpc/Cergrene                            *
 *********************************************************************/

#include "nsp/object.h"
#include "nsp/calelm.h"

/********************************************************************
 * icopy : copies an int array idx, to an int array idy. 
 ********************************************************************/

int C2F(icopy) (const int *n,const int *idx,const int *incx, int *idy,
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

/********************************************************************
 * iset : idy[incy*k] =dx , k= 0, *n-1 
 ********************************************************************/

int C2F(iset) (int *n, int *idx, int *idy, int *incy)
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

/********************************************************************
 *  iadd  :  idy[k] += ival  k=0,*incy,*n
 ********************************************************************/

int C2F(iadd) (int *n, int *ival, int *idy, int *incy)
{
  static int i;
  if (*n <= 0) { return 0;  }
  for (i = 0 ; i < *n ; i+= *incy )
    idy[i] += *ival;
  return 0;
}





