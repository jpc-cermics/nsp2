#include "minpack.h"

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

	


/*     subroutine fdjac1 
 *
 *     this subroutine computes a forward-difference approximation 
 *     to the n by n jacobian matrix associated with a specified 
 *     problem of n functions in n variables. if the jacobian has 
 *     a banded form, then function evaluations are saved by only 
 *     approximating the nonzero terms. 
 *
 *     the subroutine statement is 
 *
 *       subroutine fdjac1(fcn,n,x,fvec,fjac,ldfjac,iflag,ml,mu,epsfcn, 
 *                         wa1,wa2) 
 *
 *     where 
 *
 *       fcn is the name of the user-supplied subroutine which 
 *         calculates the functions. fcn must be declared 
 *         in an external statement in the user calling 
 *         program, and should be written as follows. 
 *
 *         subroutine fcn(n,x,fvec,iflag) 
 *         int n,iflag 
 *         double precision x(n),fvec(n) 
 *         ---------- 
 *         calculate the functions at x and 
 *         return this vector in fvec. 
 *         ---------- 
 *         return 
 *         end 
 *
 *         the value of iflag should not be changed by fcn unless 
 *         the user wants to terminate execution of fdjac1. 
 *         in this case set iflag to a negative int. 
 *
 *       n is a positive int input variable set to the number 
 *         of functions and variables. 
 *
 *       x is an input array of length n. 
 *
 *       fvec is an input array of length n which must contain the 
 *         functions evaluated at x. 
 *
 *       fjac is an output n by n array which contains the 
 *         approximation to the jacobian matrix evaluated at x. 
 *
 *       ldfjac is a positive int input variable not less than n 
 *         which specifies the leading dimension of the array fjac. 
 *
 *       iflag is an int variable which can be used to terminate 
 *         the execution of fdjac1. see description of fcn. 
 *
 *       ml is a nonnegative int input variable which specifies 
 *         the number of subdiagonals within the band of the 
 *         jacobian matrix. if the jacobian is not banded, set 
 *         ml to at least n - 1. 
 *
 *       epsfcn is an input variable used in determining a suitable 
 *         step length for the forward-difference approximation. this 
 *         approximation assumes that the relative errors in the 
 *         functions are of the order of epsfcn. if epsfcn is less 
 *         than the machine precision, it is assumed that the relative 
 *         errors in the functions are of the order of the machine 
 *         precision. 
 *
 *       mu is a nonnegative int input variable which specifies 
 *         the number of superdiagonals within the band of the 
 *         jacobian matrix. if the jacobian is not banded, set 
 *         mu to at least n - 1. 
 *
 *       wa1 and wa2 are work arrays of length n. if ml + mu + 1 is at 
 *         least n, then the jacobian is considered dense, and wa2 is 
 *         not referenced. 
 *
 *
 *     argonne national laboratory. minpack project. march 1980. 
 *     burton s. Garbow, kenneth e. Hillstrom, jorge j. More 
 */

int minpack_fdjac1 (minpack_fcn1 fcn, int *n, double *x, double *fvec, double *fjac,
		    int *ldfjac, int *iflag, int *ml, int *mu, double *epsfcn,
		    double *wa1, double *wa2, void *data)
{
  const double zero = 0.;
  int fjac_dim1, fjac_offset, i1,  j, k, msum;
  double d__1, temp, h__,epsmch, eps;

  fjac_dim1 = *ldfjac;
  fjac_offset = fjac_dim1 + 1;
  fjac -= fjac_offset;

  /*     epsmch is the machine precision. */

  epsmch = minpack_dpmpar (1);

  eps = sqrt ((Max (*epsfcn, epsmch)));
  msum = *ml + *mu + 1;
  if (msum >= *n)
    {
      /*        computation of dense approximate jacobian. */
      for (j = 1; j <= *n ; ++j)
	{
	  temp = x[j-1];
	  h__ = eps * Abs (temp);
	  if (h__ == zero)
	    {
	      h__ = eps;
	    }
	  x[j-1] = temp + h__;
	  (*fcn) (n, x, wa1, iflag,data);
	  if (*iflag < 0) return 0;
	  x[j-1] = temp;
	  for (i1 = 1; i1 <= *n; ++i1)
	    {
	      fjac[i1 + j * fjac_dim1] = (wa1[i1-1] - fvec[i1-1]) / h__;
	    }
	}
    }
  else
    {
      /*        computation of banded approximate jacobian. */

      for (k = 1; k <= msum ; ++k)
	{
	  for (j = k; msum < 0 ? j >= *n : j <= *n; j += msum )
	    {
	      wa2[j-1] = x[j-1];
	      h__ = eps * (d__1 = wa2[j-1], Abs (d__1));
	      if (h__ == zero)
		{
		  h__ = eps;
		}
	      x[j-1] = wa2[j-1] + h__;
	    }
	  (*fcn) (n, x, wa1, iflag,data);
	  if (*iflag < 0) return 0;
	  for (j = k; msum < 0 ? j >= *n : j <= *n; j += msum )
	    {
	      x[j-1] = wa2[j-1];
	      h__ = eps * (d__1 = wa2[j-1], Abs (d__1));
	      if (h__ == zero)
		{
		  h__ = eps;
		}
	      for (i1 = 1; i1 <= *n ; ++i1)
		{
		  fjac[i1 + j * fjac_dim1] = zero;
		  if (i1 >= j - *mu && i1 <= j + *ml)
		    {
		      fjac[i1 + j * fjac_dim1] = (wa1[i1-1] - fvec[i1-1]) / h__;
		    }
		}
	    }
	}
    }
  return 0;
}

