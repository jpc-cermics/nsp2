/* Nsp
 * Copyright (C) 2006 Bruno Pincon Esial/Iecn
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

#include "grand.h"
#include <math.h>

/**
 * rand_ndgauss:
 * @Mean: (input) a vector with @n components
 * @C: (input) @nx@n matrix (lower triangular factor of the covariance matrix)
 * @res: (output) random vector generated
 * @n: dimension of the nd gaussian distribution
 *
 * n-dim gaussian normal distribution generator N(Mean,Cov).
 * @C is obtained from the covariance matrix Cov with a call
 * to the lapack routine dpotrf.
 *
 * Algorithm :  res = C * x + Mean  where x is a vector
 *              with n independant samples from N(0,1)
 * 
 **/
void rand_ndgauss(double *Mean, double *C, double *res, int n)
{
  int i, j;
  double *col, u;

  /*  init res with Mean */
  for ( i = 0 ; i < n ; i++ )
     res[i] = Mean[i];

  /*  res += C * x  (at each j iteration, col point to 
   *  the beginning of the jth column of C)
   */
  for ( j = 0, col = C ; j < n ; j++, col+=n )
    {
      u = rand_nor_core();
      for ( i = j ; i < n ; i++ )
	res[i] += col[i]*u;
    }
}
