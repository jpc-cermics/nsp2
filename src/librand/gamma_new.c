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

/*    gamma generator as explained in "Marsaglia G and WW Tsang,
 *    A Simple Method for Generating Gamma Variables"
 *    ACM Transactions on math. Software, Vol 26, No 3, Sept 2000,
 *    pages 363-372"
 */

#include "grand.h"
#include <math.h>

double rand_gamma(double a)
{
  static double a_save = -1.0, d, c;
  double x, x2, u, v;

  if ( a >= 1.0 )
    {
      if ( a != a_save )
	{
	  d = a - 0.333333333333333333;
	  c = 1.0 / sqrt(9.0*d);
	}

      while ( 1 )
	{
	  do
	    {
	      x = rand_nor_core();
	      v = 1.0 + c*x;
	    }
	  while ( v <= 0.0 );
	  v = v*v*v;

	  u = rand_ranf();
	  x2 = x*x;
	  if ( u < 1.0 - 0.0331*x2*x2 )
	    break;

	  if ( log(u) < 0.5*x2 + d*(1.0 - v + log(v)) )
	    break;
	}
      return d*v;
    }

  else  /* case a < 1.0 */
    {
      return rand_gamma(a+1.0)*exp(-rand_exp_core()/a);
    }
}
