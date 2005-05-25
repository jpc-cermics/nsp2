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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nsp/matrix-in.h"
/* #include "PMatrix-IN.h" **/

extern void nsp_int2double(int *,int *,int *,double *,int *);

/*
 * Matrix ( used ar row vector 1xn ) -> 1x1 polymatrix filled 
 * with one polynom of degree n-1
 */

int int_mx2pmx(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P; NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A=GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if (( P=nsp_matrix_to_polynom(A))== NULLPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) P);
  return 1;
}

/*
 * Creation of a PMatrix 
 */

int int_pmxcreate(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  doubleC def ={0,0} ;
  NspPMatrix *P; 
  CheckRhs(1,3);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if (rhs == 3) 
    {
      if (GetScalarDouble(stack,1,&def.r) == FAIL) return RET_BUG;
    }
  if ( (P =nsp_pmatrix_create(NVOID,m1,n1,&def,(rhs==3)? 1: 0)) == NULLPMAT)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) P);
  return 1;
}









