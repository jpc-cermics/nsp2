/* Nsp
 * Copyright (C) 2007 Ramine Nikoukhah (Inria) 
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
 *
 * utilities copyrighted GPL in this version by Ramine Nikoukhah
 * translated to C and simplified by Jean-Philippe Chancelier 
 * 
 *--------------------------------------------------------------------------*/

#include "nsp/machine.h"
#include "nsp/object.h"
#include "nsp/blas.h"

/**
 * dmmul_scicos:
 * @a: array of double 
 * @na: int pointer
 * @b:  array of double 
 * @nb: int pointer
 * @c:  array of double 
 * @nc: int pointer
 * @l: int pointer 
 * @m: int pointer
 * @n: int pointer
 * 
 * computes the matrix product @c = @a*@b, where @c is (@l,@n), @a is 
 * (@l,@m) and @b is (@m,@n) by calling dgemm().
 * 
 * Returns: 0 
 **/

int dmmul_scicos(double *a, int *na, double *b, int *nb, double *c, int *nc, int *l, int *m, int *n)
{
  double c_b4 = 1.0, c_b5 = 0.0;
  C2F(dgemm)("n", "n", l, n, m, &c_b4, a, na,b, nb,&c_b5,c, nc, 1L, 1L);
  return 0;
} 

/**
 * dmmul1_scicos:
 * @a: array of double 
 * @na: int pointer
 * @b:  array of double 
 * @nb: int pointer
 * @c:  array of double 
 * @nc: int pointer
 * @l: int pointer 
 * @m: int pointer
 * @n: int pointer
 * 
 * computes the matrix product @c =@c +  @a*@b, where @c is (@l,@n), @a is 
 * (@l,@m) and @b is (@m,@n) by calling dgemm().
 * 
 * Returns: 0 
 **/

int dmmul1_scicos(double *a, int *na, double *b, int *nb, double *c, int *nc, int *l, int *m, int *n)
{
  /* 
  int i1=*n , i2=*l , i, j, ib=0, ic=0 , c1 = 1;
  for (j = 0 ; j < i1; ++j) 
    {
      for (i = 0; i < i2; ++i) {
	c[ic + i] += ddot_(m, &a[i], na, &b[ib], &c1);
      }
      ic += *nc;
      ib += *nb;
    }
  */
  double c_b4 = 1.0, c_b5 = 1.0;
  C2F(dgemm)("n", "n", l, n, m, &c_b4, a, na,b, nb,&c_b5,c, nc, 1L, 1L);
  return 0;
} 




