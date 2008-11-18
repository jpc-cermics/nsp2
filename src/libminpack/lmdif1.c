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

	


/*     subroutine lmdif1 
 *
 *     the purpose of lmdif1 is to minimize the sum of the squares of 
 *     m nonlinear functions in n variables by a modification of the 
 *     levenberg-marquardt algorithm. this is done by using the more 
 *     general least-squares solver lmdif. the user must provide a 
 *     subroutine which calculates the functions. the jacobian is 
 *     then calculated by a forward-difference approximation. 
 *
 *     the subroutine statement is 
 *
 *       subroutine lmdif1(fcn,m,n,x,fvec,tol,info,iwa,wa,lwa) 
 *
 *     where 
 *
 *       fcn is the name of the user-supplied subroutine which 
 *         calculates the functions. fcn must be declared 
 *         in an external statement in the user calling 
 *         program, and should be written as follows. 
 *
 *         subroutine fcn(m,n,x,fvec,iflag) 
 *         int m,n,iflag 
 *         double precision x(n),fvec(m) 
 *         ---------- 
 *         calculate the functions at x and 
 *         return this vector in fvec. 
 *         ---------- 
 *         return 
 *         end 
 *
 *         the value of iflag should not be changed by fcn unless 
 *         the user wants to terminate execution of lmdif1. 
 *         in this case set iflag to a negative int. 
 *
 *       m is a positive int input variable set to the number 
 *         of functions. 
 *
 *       n is a positive int input variable set to the number 
 *         of variables. n must not exceed m. 
 *
 *       x is an array of length n. on input x must contain 
 *         an initial estimate of the solution vector. on output x 
 *         contains the final estimate of the solution vector. 
 *
 *       fvec is an output array of length m which contains 
 *         the functions evaluated at the output x. 
 *
 *       tol is a nonnegative input variable. termination occurs 
 *         when the algorithm estimates either that the relative 
 *         error in the sum of squares is at most tol or that 
 *         the relative error between x and the solution is at 
 *         most tol. 
 *
 *       info is an int output variable. if the user has 
 *         terminated execution, info is set to the (negative) 
 *         value of iflag. see description of fcn. otherwise, 
 *         info is set as follows. 
 *
 *         info = 0  improper input parameters. 
 *
 *         info = 1  algorithm estimates that the relative error 
 *                   in the sum of squares is at most tol. 
 *
 *         info = 2  algorithm estimates that the relative error 
 *                   between x and the solution is at most tol. 
 *
 *         info = 3  conditions for info = 1 and info = 2 both hold. 
 *
 *         info = 4  fvec is orthogonal to the columns of the 
 *                   jacobian to machine precision. 
 *
 *         info = 5  number of calls to fcn has reached or 
 *                   exceeded 200*(n+1). 
 *
 *         info = 6  tol is too small. no further reduction in 
 *                   the sum of squares is possible. 
 *
 *         info = 7  tol is too small. no further improvement in 
 *                   the approximate solution x is possible. 
 *
 *       iwa is an int work array of length n. 
 *
 *       wa is a work array of length lwa. 
 *
 *       lwa is a positive int input variable not less than 
 *         m*n+5*n+m. 
 *
 *     subprograms called 
 *
 *       user-supplied ...... fcn 
 *
 *       minpack-supplied ... lmdif 
 *
 *     argonne national laboratory. minpack project. march 1980. 
 *     burton s. Garbow, kenneth e. Hillstrom, jorge j. More 
 */


#include "minpack.h"

int minpack_lmdif1 (minpack_fcn2 fcn, int *m, int *n, double *x, double *fvec,
		    double *tol, int *info, int *iwa, double *wa, int *lwa,void *data)
{
  /* Initialized data */

  const double factor = 100.;
  const double zero = 0.;

  int mode, nfev;
  double ftol, gtol, xtol;
  double epsfcn;
  int maxfev, nprint, mp5n;

  --fvec;
  --iwa;
  --x;
  --wa;

  *info = 0;

  /*     check the input parameters for errors. */

  if (*n <= 0 || *m < *n || *tol < zero || *lwa < *m * *n + *n * 5 + *m)
    {
      goto L10;
    }

  /*     call lmdif. */

  maxfev = (*n + 1) * 200;
  ftol = *tol;
  xtol = *tol;
  gtol = zero;
  epsfcn = zero;
  mode = 1;
  nprint = 0;
  mp5n = *m + *n * 5;
  minpack_lmdif ( fcn, m, n, &x[1], &fvec[1], &ftol, &xtol, &gtol,
		 &maxfev, &epsfcn, &wa[1], &mode, &factor, &nprint, info,
		 &nfev, &wa[mp5n + 1], m, &iwa[1], &wa[*n + 1],
		 &wa[(*n << 1) + 1], &wa[*n * 3 + 1], &wa[(*n << 2) + 1],
		  &wa[*n * 5 + 1],data);
  if (*info == 8)
    {
      *info = 4;
    }
 L10:
  return 0;
}
