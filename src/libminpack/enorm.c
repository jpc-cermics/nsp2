/* 
 * Minpack Copyright Notice (1999) University of Chicago.  All rights reserved
 * 
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the
 * following conditions are met:
 * 
 * 1. Redistributions of source code must retain the above
 * copyright notice, this list of conditions and the following
 * disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials
 * provided with the distribution.
 * 
 * 3. The end-user documentation included with the
 * redistribution, if any, must include the following
 * acknowledgment:
 * 
 *    "This product includes software developed by the
 *    University of Chicago, as Operator of Argonne National
 *    Laboratory.
 * 
 * Alternately, this acknowledgment may appear in the software
 * itself, if and wherever such third-party acknowledgments
 * normally appear.
 * 
 * 4. WARRANTY DISCLAIMER. THE SOFTWARE IS SUPPLIED "AS IS"
 * WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT HOLDER, THE
 * UNITED STATES, THE UNITED STATES DEPARTMENT OF ENERGY, AND
 * THEIR EMPLOYEES: (1) DISCLAIM ANY WARRANTIES, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO ANY IMPLIED WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE
 * OR NON-INFRINGEMENT, (2) DO NOT ASSUME ANY LEGAL LIABILITY
 * OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR
 * USEFULNESS OF THE SOFTWARE, (3) DO NOT REPRESENT THAT USE OF
 * THE SOFTWARE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS, (4)
 * DO NOT WARRANT THAT THE SOFTWARE WILL FUNCTION
 * UNINTERRUPTED, THAT IT IS ERROR-FREE OR THAT ANY ERRORS WILL
 * BE CORRECTED.
 * 
 * 5. LIMITATION OF LIABILITY. IN NO EVENT WILL THE COPYRIGHT
 * HOLDER, THE UNITED STATES, THE UNITED STATES DEPARTMENT OF
 * ENERGY, OR THEIR EMPLOYEES: BE LIABLE FOR ANY INDIRECT,
 * INCIDENTAL, CONSEQUENTIAL, SPECIAL OR PUNITIVE DAMAGES OF
 * ANY KIND OR NATURE, INCLUDING BUT NOT LIMITED TO LOSS OF
 * PROFITS OR LOSS OF DATA, FOR ANY REASON WHATSOEVER, WHETHER
 * SUCH LIABILITY IS ASSERTED ON THE BASIS OF CONTRACT, TORT
 * (INCLUDING NEGLIGENCE OR STRICT LIABILITY), OR OTHERWISE,
 * EVEN IF ANY OF SAID PARTIES HAS BEEN WARNED OF THE
 * POSSIBILITY OF SUCH LOSS OR DAMAGES.
 */

	


/*     function enorm 
 *     given an n-vector x, this function calculates the 
 *     euclidean norm of x. 
 *     the euclidean norm is computed by accumulating the sum of 
 *     squares in three different sums. the sums of squares for the 
 *     small and large components are scaled so that no overflows 
 *     occur. non-destructive underflows are permitted. underflows 
 *     and overflows do not occur in the computation of the unscaled 
 *     sum of squares for the intermediate components. 
 *     the definitions of small, intermediate and large components 
 *     depend on two constants, rdwarf and rgiant. the main 
 *     restrictions on these constants are that rdwarf**2 not 
 *     underflow and rgiant**2 not overflow. the constants 
 *     given here are suitable for every known computer. 
 *     the function statement is 
 *       double precision function enorm(n,x) 
 *     where 
 *       n is a positive int input variable. 
 *       x is an input array of length n. 
 *     subprograms called 
 *       fortran-supplied ... dabs,dsqrt 
 *     argonne national laboratory. minpack project. march 1980. 
 *     burton s. Garbow, kenneth e. Hillstrom, jorge j. More 
 */

#include "minpack.h"

double minpack_enorm (int n,const  double *xx)
{
  const double one = 1.0, zero = 0.0;
  const double rdwarf = 3.834e-20;
  const double rgiant = 1.304e19;
  double d1, s1=zero, s2=zero, s3=zero, agiant, floatn=(double) (n), xabs, x1max=zero, x3max=zero;
  int  i;
  agiant = rgiant / floatn;

  for (i = 0 ; i < n; ++i)
    {
      xabs = Abs(xx[i]);
      if (xabs > rdwarf && xabs < agiant)
	{
	  /*           sum for intermediate components. */
	  d1 = xabs;
	  s2 += d1 * d1;
	  continue;
	}
      if (xabs <= rdwarf)
	{
	  /*              sum for small components. */
	  if (xabs <= x3max)
	    {
	      if (xabs != zero)
		{
		  d1 = xabs / x3max;
		  s3 += d1 * d1;
		}
	    }
	  else 
	    {
	      d1 = x3max / xabs;
	      s3 = one + s3 * (d1 * d1);
	      x3max = xabs;
	    }
	  continue;
	}
      /*              sum for large components. */
      if (xabs <= x1max)
	{
	  d1 = xabs / x1max;
	  s1 += d1 * d1;
	}
      else 
	{
	  d1 = x1max / xabs;
	  s1 = one + s1 * (d1 * d1);
	  x1max = xabs;
	}
    }

  /*     calculation of norm. */
  
  if (s1 == zero)
    {
      if (s2 == zero)
	{
	  return  x3max * sqrt (s3);
	}
      if (s2 >= x3max)
	{
	  return sqrt (s2 * (one + x3max / s2 * (x3max * s3)));
	}
      if (s2 < x3max)
	{
	  return  sqrt (x3max * (s2 / x3max + x3max * s3));
	}
    }
  return  x1max * sqrt (s1 + s2 / x1max / x1max);
}
