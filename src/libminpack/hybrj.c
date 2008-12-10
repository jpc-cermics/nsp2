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

	


/*     subroutine hybrj 
 *
 *     the purpose of hybrj is to find a zero of a system of 
 *     n nonlinear functions in n variables by a modification 
 *     of the powell hybrid method. the user must provide a 
 *     subroutine which calculates the functions and the jacobian. 
 *
 *     the subroutine statement is 
 *
 *       subroutine hybrj(fcn,n,x,fvec,fjac,ldfjac,xtol,ftol,maxfev,diag, 
 *                        mode,factor,nprint,info,nfev,njev,r,lr,qtf, 
 *                        wa1,wa2,wa3,wa4) 
 *
 *     where 
 *
 *       fcn is the name of the user-supplied subroutine which 
 *         calculates the functions and the jacobian. fcn must 
 *         be declared in an external statement in the user 
 *         calling program, and should be written as follows. 
 *
 *         subroutine fcn(n,x,fvec,fjac,ldfjac,iflag) 
 *         int n,ldfjac,iflag 
 *         double precision x(n),fvec(n),fjac(ldfjac,n) 
 *         ---------- 
 *         if iflag = 1 calculate the functions at x and 
 *         return this vector in fvec. do not alter fjac. 
 *         if iflag = 2 calculate the jacobian at x and 
 *         return this matrix in fjac. do not alter fvec. 
 *         --------- 
 *         return 
 *         end 
 *
 *         the value of iflag should not be changed by fcn unless 
 *         the user wants to terminate execution of hybrj. 
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
 *       fjac is an output n by n array which contains the 
 *         orthogonal matrix q produced by the qr factorization 
 *         of the final approximate jacobian. 
 *
 *       ldfjac is a positive int input variable not less than n 
 *         which specifies the leading dimension of the array fjac. 
 *
 *       xtol is a nonnegative input variable. termination 
 *         occurs when the relative error between two consecutive 
 *         iterates is at most xtol. 
 *
 *       ftol is a nonnegative input variable. termination 
 *         occurs also if Max_i |f_i(x)| <= ftol (added by Bruno).
 *
 *       maxfev is a positive int input variable. termination 
 *         occurs when the number of calls to fcn with iflag = 1 
 *         has reached maxfev. 
 *
 *       diag is an array of length n. if mode = 1 (see 
 *         below), diag is internally set. if mode = 2, diag 
 *         must contain positive entries that serve as 
 *         multiplicative scale factors for the variables. 
 *
 *       mode is an int input variable. if mode = 1, the 
 *         variables will be scaled internally. if mode = 2, 
 *         the scaling is specified by the input diag. other 
 *         values of mode are equivalent to mode = 1. 
 *
 *       factor is a positive input variable used in determining the 
 *         initial step bound. this bound is set to the product of 
 *         factor and the euclidean norm of diag*x if nonzero, or else 
 *         to factor itself. in most cases factor should lie in the 
 *         interval (.1,100.). 100. is a generally recommended value. 
 *
 *       nprint is an int input variable that enables controlled 
 *         printing of iterates if it is positive. in this case, 
 *         fcn is called with iflag = 0 at the beginning of the first 
 *         iteration and every nprint iterations thereafter and 
 *         immediately prior to return, with x and fvec available 
 *         for printing. fvec and fjac should not be altered. 
 *         if nprint is not positive, no special calls of fcn 
 *         with iflag = 0 are made. 
 *
 *       info is an int output variable. if the user has 
 *         terminated execution, info is set to the (negative) 
 *         value of iflag. see description of fcn. otherwise, 
 *         info is set as follows. 
 *
 *         info = 0   improper input parameters. 
 *
 *         info = 1   relative error between two consecutive iterates 
 *                    is at most xtol. 
 *
 *         info = 2   number of calls to fcn with iflag = 1 has 
 *                    reached maxfev. 
 *
 *         info = 3   xtol is too small. no further improvement in 
 *                    the approximate solution x is possible. 
 *
 *         info = 4   iteration is not making good progress, as 
 *                    measured by the improvement from the last 
 *                    five jacobian evaluations. 
 *
 *         info = 5   iteration is not making good progress, as 
 *                    measured by the improvement from the last 
 *                    ten iterations. 
 *
 *       nfev is an int output variable set to the number of 
 *         calls to fcn with iflag = 1. 
 *
 *       njev is an int output variable set to the number of 
 *         calls to fcn with iflag = 2. 
 *
 *       r is an output array of length lr which contains the 
 *         upper triangular matrix produced by the qr factorization 
 *         of the final approximate jacobian, stored rowwise. 
 *
 *       lr is a positive int input variable not less than 
 *         (n*(n+1))/2. 
 *
 *       qtf is an output array of length n which contains 
 *         the vector (q transpose)*fvec. 
 *
 *       wa1, wa2, wa3, and wa4 are work arrays of length n. 
 *
 *     subprograms called 
 *
 *       user-supplied ...... fcn 
 *       minpack-supplied ... dogleg,dpmpar,enorm, 
 *                            qform,qrfac,r1mpyq,r1updt 
 *       fortran-supplied ... dabs,dmax1,dmin1,mod 
 *
 *     argonne national laboratory. minpack project. march 1980. 
 *     burton s. Garbow, kenneth e. Hillstrom, jorge j. More 
 */

#define WITH_JAC 
#include "hybrdorj.c" 
