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

	


#include "minpack.h"

/*     subroutine lmdif 
 *
 *     the purpose of lmdif is to minimize the sum of the squares of 
 *     m nonlinear functions in n variables by a modification of 
 *     the levenberg-marquardt algorithm. the user must provide a 
 *     subroutine which calculates the functions. the jacobian is 
 *     then calculated by a forward-difference approximation. 
 *
 *     the subroutine statement is 
 *
 *       subroutine lmdif(fcn,m,n,x,fvec,ftol,xtol,gtol,maxfev,epsfcn, 
 *                        diag,mode,factor,nprint,info,nfev,fjac, 
 *                        ldfjac,ipvt,qtf,wa1,wa2,wa3,wa4) 
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
 *         the user wants to terminate execution of lmdif. 
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
 *         occurs when the number of calls to fcn is at least 
 *         maxfev by the end of an iteration. 
 *
 *       epsfcn is an input variable used in determining a suitable 
 *         step length for the forward-difference approximation. this 
 *         approximation assumes that the relative errors in the 
 *         functions are of the order of epsfcn. if epsfcn is less 
 *         than the machine precision, it is assumed that the relative 
 *         errors in the functions are of the order of the machine 
 *         precision. 
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
 *         for printing. if nprint is not positive, no special calls 
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
 *         info = 5  number of calls to fcn has reached or 
 *                   exceeded maxfev. 
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
 *         calls to fcn. 
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


int minpack_lmdif (minpack_fcn2 fcn, int *m, int *n, double *x, double *fvec,
		   double *ftol, double *xtol, double *gtol, int *maxfev,
		   double *epsfcn, double *diag, int *mode, const double *factor,
		   int *nprint, int *info, int *nfev, double *fjac, int *ldfjac,
		   int *ipvt, double *qtf, double *wa1, double *wa2, double *wa3,
		   double *wa4,void *data)
{
  /* Initialized data */
  int c__1 = 1;
  int c_true = TRUE;
  const double one = 1.;
  const double p1 = .1;
  const double p5 = .5;
  const double p25 = .25;
  const double p75 = .75;
  const double p0001 = 1e-4;
  const double zero = 0.;

  /* System generated locals */
  int fjac_dim1, fjac_offset, i__1, i__2;
  double d__1, d__2, d__3;

  /* Local variables */
  int iter;
  double temp, temp1, temp2;
  int i__, j, l, iflag;
  double delta;
  double ratio;
  double fnorm, gnorm;
  double pnorm, xnorm, fnorm1, actred, dirder, epsmch, prered;
  double par, sum;


  /*     ********** */
  /* Parameter adjustments */
  --wa4;
  --fvec;
  --wa3;
  --wa2;
  --wa1;
  --qtf;
  --ipvt;
  --diag;
  --x;
  fjac_dim1 = *ldfjac;
  fjac_offset = fjac_dim1 + 1;
  fjac -= fjac_offset;

  /* Function Body */

  /*     epsmch is the machine precision. */

  epsmch = minpack_dpmpar (&c__1);

  *info = 0;
  iflag = 0;
  *nfev = 0;

  /*     check the input parameters for errors. */

  if (*n <= 0 || *m < *n || *ldfjac < *m || *ftol < zero || *xtol < zero
      || *gtol < zero || *maxfev <= 0 || *factor <= zero)
    {
      goto L300;
    }
  if (*mode != 2)
    {
      goto L20;
    }
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      if (diag[j] <= zero)
	{
	  goto L300;
	}
      /* L10: */
    }
 L20:

  /*     evaluate the function at the starting point */
  /*     and calculate its norm. */

  iflag = 1;
  (*fcn) (m, n, &x[1], &fvec[1], &iflag,data);
  *nfev = 1;
  if (iflag < 0)
    {
      goto L300;
    }
  fnorm = minpack_enorm (m, &fvec[1]);

  /*     initialize levenberg-marquardt parameter and iteration counter. */

  par = zero;
  iter = 1;

  /*     beginning of the outer loop. */

 L30:

  /*        calculate the jacobian matrix. */

  iflag = 2;
  minpack_fdjac2 ( fcn, m, n, &x[1], &fvec[1], &fjac[fjac_offset],
		   ldfjac, &iflag, epsfcn, &wa4[1],data);
  *nfev += *n;
  if (iflag < 0)
    {
      goto L300;
    }

  /*        if requested, call fcn to enable printing of iterates. */

  if (*nprint <= 0)
    {
      goto L40;
    }
  iflag = 0;
  if ((iter - 1) % *nprint == 0)
    {
      (*fcn) (m, n, &x[1], &fvec[1], &iflag,data);
    }
  if (iflag < 0)
    {
      goto L300;
    }
 L40:

  /*        compute the qr factorization of the jacobian. */

  minpack_qrfac (m, n, &fjac[fjac_offset], ldfjac, &c_true, &ipvt[1], n,
		 &wa1[1], &wa2[1], &wa3[1]);

  /*        on the first iteration and if mode is 1, scale according */
  /*        to the norms of the columns of the initial jacobian. */

  if (iter != 1)
    {
      goto L80;
    }
  if (*mode == 2)
    {
      goto L60;
    }
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      diag[j] = wa2[j];
      if (wa2[j] == zero)
	{
	  diag[j] = one;
	}
      /* L50: */
    }
 L60:

  /*        on the first iteration, calculate the norm of the scaled x */
  /*        and initialize the step bound delta. */

  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      wa3[j] = diag[j] * x[j];
      /* L70: */
    }
  xnorm = minpack_enorm (n, &wa3[1]);
  delta = *factor * xnorm;
  if (delta == zero)
    {
      delta = *factor;
    }
 L80:

  /*        form (q transpose)*fvec and store the first n components in */
  /*        qtf. */

  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      wa4[i__] = fvec[i__];
      /* L90: */
    }
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      if (fjac[j + j * fjac_dim1] == zero)
	{
	  goto L120;
	}
      sum = zero;
      i__2 = *m;
      for (i__ = j; i__ <= i__2; ++i__)
	{
	  sum += fjac[i__ + j * fjac_dim1] * wa4[i__];
	  /* L100: */
	}
      temp = -sum / fjac[j + j * fjac_dim1];
      i__2 = *m;
      for (i__ = j; i__ <= i__2; ++i__)
	{
	  wa4[i__] += fjac[i__ + j * fjac_dim1] * temp;
	  /* L110: */
	}
    L120:
      fjac[j + j * fjac_dim1] = wa1[j];
      qtf[j] = wa4[j];
      /* L130: */
    }

  /*        compute the norm of the scaled gradient. */

  gnorm = zero;
  if (fnorm == zero)
    {
      goto L170;
    }
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      l = ipvt[j];
      if (wa2[l] == zero)
	{
	  goto L150;
	}
      sum = zero;
      i__2 = j;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  sum += fjac[i__ + j * fjac_dim1] * (qtf[i__] / fnorm);
	  /* L140: */
	}
      /* Computing MAX */
      d__2 = gnorm, d__3 = (d__1 = sum / wa2[l], Abs (d__1));
      gnorm = Max (d__2, d__3);
    L150:
      /* L160: */
      ;
    }
 L170:

  /*        test for convergence of the gradient norm. */

  if (gnorm <= *gtol)
    {
      *info = 4;
    }
  if (*info != 0)
    {
      goto L300;
    }

  /*        rescale if necessary. */

  if (*mode == 2)
    {
      goto L190;
    }
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      /* Computing MAX */
      d__1 = diag[j], d__2 = wa2[j];
      diag[j] = Max (d__1, d__2);
      /* L180: */
    }
 L190:

  /*        beginning of the inner loop. */

 L200:

  /*           determine the levenberg-marquardt parameter. */

  minpack_lmpar (n, &fjac[fjac_offset], ldfjac, &ipvt[1], &diag[1], &qtf[1],
		 &delta, &par, &wa1[1], &wa2[1], &wa3[1], &wa4[1]);

  /*           store the direction p and x + p. calculate the norm of p. */

  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      wa1[j] = -wa1[j];
      wa2[j] = x[j] + wa1[j];
      wa3[j] = diag[j] * wa1[j];
      /* L210: */
    }
  pnorm = minpack_enorm (n, &wa3[1]);

  /*           on the first iteration, adjust the initial step bound. */

  if (iter == 1)
    {
      delta = Min (delta, pnorm);
    }

  /*           evaluate the function at x + p and calculate its norm. */

  iflag = 1;
  (*fcn) (m, n, &wa2[1], &wa4[1], &iflag,data);
  ++(*nfev);
  if (iflag < 0)
    {
      goto L300;
    }
  fnorm1 = minpack_enorm (m, &wa4[1]);

  /*           compute the scaled actual reduction. */

  actred = -one;
  if (p1 * fnorm1 < fnorm)
    {
      /* Computing 2nd power */
      d__1 = fnorm1 / fnorm;
      actred = one - d__1 * d__1;
    }

  /*           compute the scaled predicted reduction and */
  /*           the scaled directional derivative. */

  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      wa3[j] = zero;
      l = ipvt[j];
      temp = wa1[l];
      i__2 = j;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  wa3[i__] += fjac[i__ + j * fjac_dim1] * temp;
	  /* L220: */
	}
      /* L230: */
    }
  temp1 = minpack_enorm (n, &wa3[1]) / fnorm;
  temp2 = sqrt (par) * pnorm / fnorm;
  /* Computing 2nd power */
  d__1 = temp1;
  /* Computing 2nd power */
  d__2 = temp2;
  prered = d__1 * d__1 + d__2 * d__2 / p5;
  /* Computing 2nd power */
  d__1 = temp1;
  /* Computing 2nd power */
  d__2 = temp2;
  dirder = -(d__1 * d__1 + d__2 * d__2);

  /*           compute the ratio of the actual to the predicted */
  /*           reduction. */

  ratio = zero;
  if (prered != zero)
    {
      ratio = actred / prered;
    }

  /*           update the step bound. */

  if (ratio > p25)
    {
      goto L240;
    }
  if (actred >= zero)
    {
      temp = p5;
    }
  if (actred < zero)
    {
      temp = p5 * dirder / (dirder + p5 * actred);
    }
  if (p1 * fnorm1 >= fnorm || temp < p1)
    {
      temp = p1;
    }
  /* Computing MIN */
  d__1 = delta, d__2 = pnorm / p1;
  delta = temp * Min (d__1, d__2);
  par /= temp;
  goto L260;
 L240:
  if (par != zero && ratio < p75)
    {
      goto L250;
    }
  delta = pnorm / p5;
  par = p5 * par;
 L250:
 L260:

  /*           test for successful iteration. */

  if (ratio < p0001)
    {
      goto L290;
    }

  /*           successful iteration. update x, fvec, and their norms. */

  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      x[j] = wa2[j];
      wa2[j] = diag[j] * x[j];
      /* L270: */
    }
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      fvec[i__] = wa4[i__];
      /* L280: */
    }
  xnorm = minpack_enorm (n, &wa2[1]);
  fnorm = fnorm1;
  ++iter;
 L290:

  /*           tests for convergence. */

  if (Abs (actred) <= *ftol && prered <= *ftol && p5 * ratio <= one)
    {
      *info = 1;
    }
  if (delta <= *xtol * xnorm)
    {
      *info = 2;
    }
  if (Abs (actred) <= *ftol && prered <= *ftol && p5 * ratio <= one
      && *info == 2)
    {
      *info = 3;
    }
  if (*info != 0)
    {
      goto L300;
    }

  /*           tests for termination and stringent tolerances. */

  if (*nfev >= *maxfev)
    {
      *info = 5;
    }
  if (Abs (actred) <= epsmch && prered <= epsmch && p5 * ratio <= one)
    {
      *info = 6;
    }
  if (delta <= epsmch * xnorm)
    {
      *info = 7;
    }
  if (gnorm <= epsmch)
    {
      *info = 8;
    }
  if (*info != 0)
    {
      goto L300;
    }

  /*           end of the inner loop. repeat if iteration unsuccessful. */

  if (ratio < p0001)
    {
      goto L200;
    }

  /*        end of the outer loop. */

  goto L30;
 L300:

  /*     termination, either normal or user imposed. */

  if (iflag < 0)
    {
      *info = iflag;
    }
  iflag = 0;
  if (*nprint > 0)
    {
      (*fcn) (m, n, &x[1], &fvec[1], &iflag,data);
    }
  return 0;
}

