/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
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

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "nsp/math.h"
#include "nsp/gsort.h"

/* swapcode for indices : indices are ints **/

/*
 * General sort routine for Scilab 
 * xI is the transmitted table to sort ( if table is int ) 
 * xD is the transmitted table to sort ( if table is double ) 
 * ind is the int table to store the permutation 
 *     (which is to be initialized and changed )
 * iflag == if 1 ind is to be computed if 0 ind is ignored 
 * m,n : matrix size 
 * type : the operation ( see the interface ) 
 * iord : "i" or "d" : increasind or decreasing sort 
 */


int C2F(gsort)(int *xI, double *xD, int *ind, int *iflag, int *m, int *n,nsp_const_string type,nsp_const_string iord)
{
  switch ( type[0])
    {
    case 'r' :  CNAME(ColSort,double)(xD,ind,*iflag,*m,*n,iord[0]);break;
    case 'c' :  CNAME(RowSort,double)(xD,ind,*iflag,*m,*n,iord[0]);break;
    case 'l' :  
      if ( type[1] == 'r' ) 
	CNAME(LexiRow,int)(xI,ind,*iflag,*m,*n,iord[0]);
      else
	CNAME(LexiCol,int)(xI,ind,*iflag,*m,*n,iord[0]);
      break;
    case 'i' : CNAME(GlobalSort,int)(xI,ind,*iflag,*m,*n,iord[0]);break;
    case 'g' : 
    default :  CNAME(GlobalSort,double)(xD,ind,*iflag,*m,*n,iord[0]);break;
    }
  return(0);
}

/*
 * just used to prevent warnings about unused functions 
 */

int C2F(gsort_uuuu)(xI,xD,ind,iflag,m,n,type,iord)
     int *xI,*ind;
     double *xD;
     int *m,*n,*iflag;
     char *type,*iord;
{
  switch ( type[0])
    {
    case 'r' :  CNAME(ColSort,int)(xI,ind,*iflag,*m,*n,iord[0]);break;
    case 'c' :  CNAME(RowSort,int)(xI,ind,*iflag,*m,*n,iord[0]);break;
    case 'l' :  
      if ( type[1] == 'r' ) 
	CNAME(LexiRow,double)((int *)xD,ind,*iflag,*m,*n,iord[0]);
      else
	CNAME(LexiCol,double)(xD,ind,*iflag,*m,*n,iord[0]);
      break;
    case 'g' : 
    default : CNAME(GlobalSort,int)((int *) xD,ind,*iflag,*m,*n,iord[0]);break;
    }
  return(0);
}

/*
 * General sort routine for Scilab 
 * The version for Scilab strings 
 * iflag == if 1 ind is to be computed if 0 ind is ignored 
 * m,n : matrix size 
 * type : the operation ( see the interface ) 
 * iord : 'i' or 'd' : increasind or decreasing sort 
 */
#ifndef TEST 

void C2F(gsorts)(char **data, int *ind, int *iflag, int *m, int *n,nsp_const_string type,nsp_const_string iord)
{
  switch ( type[0])
    {
    case 'r' :  CNAME(ColSort,char)(data,ind,*iflag,*m,*n,iord[0]);break;
    case 'c' :  CNAME(RowSort,char)(data,ind,*iflag,*m,*n,iord[0]);break;
    case 'l' :  
      if ( type[1] == 'r' ) 
	CNAME(LexiRow,char)((int **)data,ind,*iflag,*m,*n,iord[0]);
      else
	CNAME(LexiCol,char)(data,ind,*iflag,*m,*n,iord[0]);
      break;
    case 'g' : 
    default :  CNAME(GlobalSort,char)(data,ind,*iflag,*m,*n,iord[0]);break;
    }
}
#endif 

/*
 * TEST 
 */

#ifdef TEST 

void test_internal_gsort()
{
  printf("Test for char \n");
  CNAME(sorttest,char)() ;
  printf("Test for int \n");
  CNAME(sorttest,int)() ;
  printf("Test for double \n");
  CNAME(sorttest,double)() ;
}

main()
{
  test_internal_gsort()
  return(0);
}
#endif



