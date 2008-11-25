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

	


/*     subroutine hybrd1 
 *
 *     the purpose of hybrd1 is to find a zero of a system of 
 *     n nonlinear functions in n variables by a modification 
 *     of the powell hybrid method. this is done by using the 
 *     more general nonlinear equation solver hybrd. the user 
 *     must provide a subroutine which calculates the functions. 
 *     the jacobian is then calculated by a forward-difference 
 *     approximation. 
 *
 *     the subroutine statement is 
 *
 *       subroutine hybrd1(fcn,n,x,fvec,tol,tolf,info,wa,lwa) 
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
 *         --------- 
 *         return 
 *         end 
 *
 *         the value of iflag should not be changed by fcn unless 
 *         the user wants to terminate execution of hybrd1. 
 *         in this case set iflag to a negative int. 
 *
 *       n is a positive int input variable set to the number 
 *         of functions and variables. 
 *
 *       x is an array of length n. on input x must contain 
 *         an initial estimate of the solution vector. on output x 
 *         contains the final estimate of the solution vector. 
 *
 *       fvec is an output array of length n which contains 
 *         the functions evaluated at the output x. 
 *
 *       tol is a nonnegative input variable. termination occurs 
 *         when the algorithm estimates that the relative error 
 *         between x and the solution is at most tol. 
 *
 *       tolf is a nonnegative input variable. termination 
 *         occurs also if Max_i |f_i(x)| <= ftol (added by Bruno).
 *
 *       info is an int output variable. if the user has 
 *         terminated execution, info is set to the (negative) 
 *         value of iflag. see description of fcn. otherwise, 
 *         info is set as follows. 
 *
 *         info = 0   improper input parameters. 
 *
 *         info = 1   algorithm estimates that the relative error 
 *                    between x and the solution is at most tol. 
 *
 *         info = 2   number of calls to fcn has reached or exceeded 
 *                    200*(n+1). 
 *
 *         info = 3   tol is too small. no further improvement in 
 *                    the approximate solution x is possible. 
 *
 *         info = 4   iteration is not making good progress. 
 *
 *       wa is a work array of length lwa. 
 *
 *       lwa is a positive int input variable not less than 
 *         (n*(3*n+13))/2. 
 *
 *     subprograms called 
 *
 *       user-supplied ...... fcn 
 *
 *       minpack-supplied ... hybrd 
 *
 *     argonne national laboratory. minpack project. march 1980. 
 *     burton s. Garbow, kenneth e. Hillstrom, jorge j. More 
 */


#include "minpack.h"

int minpack_hybrd1 (minpack_fcn1 fcn, int *n, double *x, double *fvec, double *tol, 
		    double *tolf, int *info, double *wa, int *lwa,void *data)
{
  const double factor = 100.;
  const double one = 1.;
  const double zero = 0.;

  /* System generated locals */
  int i__1;

  /* Local variables */
  int mode, nfev;
  double xtol, ftol;
  int j, index;
  int ml, lr, mu;
  double epsfcn;
  int maxfev, nprint;

  --fvec;
  --x;
  --wa;

  /* Function Body */
  *info = 0;

  /*     check the input parameters for errors. */

  if (*n <= 0 || *tol < zero || *tolf < zero || *lwa < *n * (*n * 3 + 13) / 2)
    {
      goto L20;
    }

  /*     call hybrd. */

  maxfev = (*n + 1) * 200;
  xtol = *tol; ftol = *tolf;
  ml = *n - 1;
  mu = *n - 1;
  epsfcn = zero;
  mode = 2;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      wa[j] = one;
      /* L10: */
    }
  nprint = 0;
  lr = *n * (*n + 1) / 2;
  index = *n * 6 + lr;
  minpack_hybrd ( fcn, n, &x[1], &fvec[1], &xtol, &ftol, &maxfev, &ml, &mu,
		  &epsfcn, &wa[1], &mode, &factor, &nprint, info, &nfev,
		  &wa[index + 1], n, &wa[*n * 6 + 1], &lr, &wa[*n + 1],
		  &wa[(*n << 1) + 1], &wa[*n * 3 + 1], &wa[(*n << 2) + 1],
		  &wa[*n * 5 + 1],data);
  if (*info == 5)
    {
      *info = 4;
    }
 L20:
  return 0;
}
