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

	


/*     subroutine lmder 
 *
 *     the purpose of lmder is to minimize the sum of the squares of 
 *     m nonlinear functions in n variables by a modification of 
 *     the levenberg-marquardt algorithm. the user must provide a 
 *     subroutine which calculates the functions and the jacobian. 
 *
 *     the subroutine statement is 
 *
 *       subroutine lmder(fcn,m,n,x,fvec,fjac,ldfjac,ftol,xtol,gtol, 
 *                        maxfev,diag,mode,factor,nprint,info,nfev, 
 *                        njev,ipvt,qtf,wa1,wa2,wa3,wa4) 
 *
 *     where 
 *
 *       fcn is the name of the user-supplied subroutine which 
 *         calculates the functions and the jacobian. fcn must 
 *         be declared in an external statement in the user 
 *         calling program, and should be written as follows. 
 *
 *         subroutine fcn(m,n,x,fvec,fjac,ldfjac,iflag) 
 *         int m,n,ldfjac,iflag 
 *         double precision x(n),fvec(m),fjac(ldfjac,n) 
 *         ---------- 
 *         if iflag = 1 calculate the functions at x and 
 *         return this vector in fvec. do not alter fjac. 
 *         if iflag = 2 calculate the jacobian at x and 
 *         return this matrix in fjac. do not alter fvec. 
 *         ---------- 
 *         return 
 *         end 
 *
 *         the value of iflag should not be changed by fcn unless 
 *         the user wants to terminate execution of lmder. 
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
 *       fjac is an output m by n array. the upper n by n submatrix 
 *         of fjac contains an upper triangular matrix r with 
 *         diagonal elements of nonincreasing magnitude such that 
 *
 *                t     t           t 
 *               p *(jac *jac)*p = r *r, 
 *
 *         where p is a permutation matrix and jac is the final 
 *         calculated jacobian. column j of p is column ipvt(j) 
 *         (see below) of the identity matrix. the lower trapezoidal 
 *         part of fjac contains information generated during 
 *         the computation of r. 
 *
 *       ldfjac is a positive int input variable not less than m 
 *         which specifies the leading dimension of the array fjac. 
 *
 *       ftol is a nonnegative input variable. termination 
 *         occurs when both the actual and predicted relative 
 *         reductions in the sum of squares are at most ftol. 
 *         therefore, ftol measures the relative error desired 
 *         in the sum of squares. 
 *
 *       xtol is a nonnegative input variable. termination 
 *         occurs when the relative error between two consecutive 
 *         iterates is at most xtol. therefore, xtol measures the 
 *         relative error desired in the approximate solution. 
 *
 *       gtol is a nonnegative input variable. termination 
 *         occurs when the cosine of the angle between fvec and 
 *         any column of the jacobian is at most gtol in absolute 
 *         value. therefore, gtol measures the orthogonality 
 *         desired between the function vector and the columns 
 *         of the jacobian. 
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
 *         interval (.1,100.).100. is a generally recommended value. 
 *
 *       nprint is an int input variable that enables controlled 
 *         printing of iterates if it is positive. in this case, 
 *         fcn is called with iflag = 0 at the beginning of the first 
 *         iteration and every nprint iterations thereafter and 
 *         immediately prior to return, with x, fvec, and fjac 
 *         available for printing. fvec and fjac should not be 
 *         altered. if nprint is not positive, no special calls 
 *         of fcn with iflag = 0 are made. 
 *
 *       info is an int output variable. if the user has 
 *         terminated execution, info is set to the (negative) 
 *         value of iflag. see description of fcn. otherwise, 
 *         info is set as follows. 
 *
 *         info = 0  improper input parameters. 
 *
 *         info = 1  both actual and predicted relative reductions 
 *                   in the sum of squares are at most ftol. 
 *
 *         info = 2  relative error between two consecutive iterates 
 *                   is at most xtol. 
 *
 *         info = 3  conditions for info = 1 and info = 2 both hold. 
 *
 *         info = 4  the cosine of the angle between fvec and any 
 *                   column of the jacobian is at most gtol in 
 *                   absolute value. 
 *
 *         info = 5  number of calls to fcn with iflag = 1 has 
 *                   reached maxfev. 
 *
 *         info = 6  ftol is too small. no further reduction in 
 *                   the sum of squares is possible. 
 *
 *         info = 7  xtol is too small. no further improvement in 
 *                   the approximate solution x is possible. 
 *
 *         info = 8  gtol is too small. fvec is orthogonal to the 
 *                   columns of the jacobian to machine precision. 
 *
 *       nfev is an int output variable set to the number of 
 *         calls to fcn with iflag = 1. 
 *
 *       njev is an int output variable set to the number of 
 *         calls to fcn with iflag = 2. 
 *
 *       ipvt is an int output array of length n. ipvt 
 *         defines a permutation matrix p such that jac*p = q*r, 
 *         where jac is the final calculated jacobian, q is 
 *         orthogonal (not stored), and r is upper triangular 
 *         with diagonal elements of nonincreasing magnitude. 
 *         column j of p is column ipvt(j) of the identity matrix. 
 *
 *       qtf is an output array of length n which contains 
 *         the first n elements of the vector (q transpose)*fvec. 
 *
 *       wa1, wa2, and wa3 are work arrays of length n. 
 *
 *       wa4 is a work array of length m. 
 *
 *     argonne national laboratory. minpack project. march 1980. 
 *     burton s. Garbow, kenneth e. Hillstrom, jorge j. More 
 */


#include "minpack.h"

#define WITH_JAC 
#include "lmderordif.c"

