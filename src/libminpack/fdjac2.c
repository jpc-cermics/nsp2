#include "minpack.h"

/*     subroutine fdjac2 
 *     this subroutine computes a forward-difference approximation 
 *     to the m by n jacobian matrix associated with a specified 
 *     problem of m functions in n variables. 
 *
 *     the subroutine statement is 
 *       subroutine fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa) 
 *     where 
 *       fcn is the name of the user-supplied subroutine which 
 *         calculates the functions. fcn must be declared 
 *         in an external statement in the user calling 
 *         program, and should be written as follows. 
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
 *         the user wants to terminate execution of fdjac2. 
 *         in this case set iflag to a negative int. 
 *       m is a positive int input variable set to the number 
 *         of functions. 
 *       n is a positive int input variable set to the number 
 *         of variables. n must not exceed m. 
 *       x is an input array of length n. 
 *       fvec is an input array of length m which must contain the 
 *         functions evaluated at x. 
 *       fjac is an output m by n array which contains the 
 *         approximation to the jacobian matrix evaluated at x. 
 *       ldfjac is a positive int input variable not less than m 
 *         which specifies the leading dimension of the array fjac. 
 *       iflag is an int variable which can be used to terminate 
 *         the execution of fdjac2. see description of fcn. 
 *       epsfcn is an input variable used in determining a suitable 
 *         step length for the forward-difference approximation. this 
 *         approximation assumes that the relative errors in the 
 *         functions are of the order of epsfcn. if epsfcn is less 
 *         than the machine precision, it is assumed that the relative 
 *         errors in the functions are of the order of the machine 
 *         precision. 
 *       wa is a work array of length m. 
 *     argonne national laboratory. minpack project. march 1980. 
 *     burton s. Garbow, kenneth e. Hillstrom, jorge j. More 
 */

int minpack_fdjac2 (minpack_fcn2 fcn, int *m, int *n, double *x, double *fvec,
		    double *fjac, int *ldfjac, int *iflag, double *epsfcn,
		    double *wa,void *data)
{
  int c__1 = 1;
  const double zero = 0.;
  int fjac_dim1, fjac_offset, i__1, i__2;
  double temp, h__;
  int i__, j;
  double epsmch;
  double eps;

  --wa;
  --fvec;
  --x;
  fjac_dim1 = *ldfjac;
  fjac_offset = fjac_dim1 + 1;
  fjac -= fjac_offset;

  /*     epsmch is the machine precision. */
  epsmch = minpack_dpmpar (&c__1);

  eps = sqrt ((Max (*epsfcn, epsmch)));
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      temp = x[j];
      h__ = eps * Abs (temp);
      if (h__ == zero)
	{
	  h__ = eps;
	}
      x[j] = temp + h__;
      (*fcn) (m, n, &x[1], &wa[1], iflag,data);
      if (*iflag < 0)
	{
	  goto L30;
	}
      x[j] = temp;
      i__2 = *m;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  fjac[i__ + j * fjac_dim1] = (wa[i__] - fvec[i__]) / h__;
	}
    }
 L30:
  return 0;
}
