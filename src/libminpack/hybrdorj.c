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

	

/* This file is included in hydrd.c and hydrj.c 
 * in order to share common code. 
 */

#include "minpack.h"

#ifdef WITH_JAC
int minpack_hybrj (minpack_fcn3 fcn, int *n, double *x, double *fvec, double *fjac,
		   int *ldfjac, double *xtol, double *ftol, int *maxfev, double *diag,
		   int *mode,const double *factor, int *nprint, int *info, int *nfev,
		   int *njev, double *r__, int *lr, double *qtf, double *wa1,
		   double *wa2, double *wa3, double *wa4,void *data)
#else 
int minpack_hybrd (minpack_fcn1 fcn, int *n, double *x, double *fvec, double *xtol, 
		   double *ftol, int *maxfev, int *ml, int *mu, double *epsfcn, double *diag,
		   int *mode,const double *factor, int *nprint, int *info, int *nfev,
		   double *fjac, int *ldfjac, double *r__, int *lr, double *qtf,
		   double *wa1, double *wa2, double *wa3, double *wa4, void *data)
#endif 
{
  const double one = 1.0,  p1 = .1,  p5 = .5,  p001 = .001, p0001 = 1e-4, zero = 0.;
  double d__1, d__2,  temp,  delta,  ratio;
  double fnorm, fninf= DBL_MAX,  pnorm, xnorm=0.0, fnorm1;
  double actred, epsmch, prered,  sum;
  int c_false = FALSE, c__1 = 1, fjac_dim1, fjac_offset ;
  int sing, iter,  i1, j, l, iflag,  jeval,  ncsuc, nslow1, nslow2;
  int ncfail, jm1, iwa[1];

#ifndef WITH_JAC
  int msum;
#endif 

  /* Parameter adjustments */

  --wa4;
  --wa3;
  --wa2;
  --wa1;
  --qtf;
  --diag;
  --fvec;
  --x;
  fjac_dim1 = *ldfjac;
  fjac_offset = fjac_dim1 + 1;
  fjac -= fjac_offset;
  --r__;

  /*     epsmch is the machine precision. */
  epsmch = minpack_dpmpar (1);

  *info = 0;
  iflag = 0;
  *nfev = 0;
#ifdef WITH_JAC
  *njev = 0;
#endif 

  /*     check the input parameters for errors. */

  if (*n <= 0 || *xtol < zero || *ftol < zero || *maxfev <= 0
      || *factor <= zero || *ldfjac < *n || *lr < *n * (*n + 1) / 2 
#ifndef WITH_JAC
      || *ml < 0 || *mu < 0
#endif 
      )
    {
      goto L300;
    }
  if (*mode != 2)
    {
      goto L20;
    }

  for (j = 1; j <= *n ; ++j)
    {
      if (diag[j] <= zero) goto L300;
    }

 L20:
  /*     evaluate the function at the starting point 
   *     and calculate its norm. 
   */

  iflag = 1;
#ifdef WITH_JAC
  (*fcn) (n, &x[1], &fvec[1], &fjac[fjac_offset], ldfjac, &iflag,data);
#else 
  (*fcn) (n, &x[1], &fvec[1], &iflag,data);
#endif 
  *nfev = 1;
  if (iflag < 0)
    {
      goto L300;
    }
  fnorm = minpack_enorm (*n, &fvec[1]);

#ifndef WITH_JAC
  /*     determine the number of calls to fcn needed to compute 
   *     the jacobian matrix. 
   */

  msum = Min ((*ml + *mu + 1), *n);
#endif 

  /*     initialize iteration counter and monitors. */

  iter = 1;
  ncsuc = 0;
  ncfail = 0;
  nslow1 = 0;
  nslow2 = 0;

  /*     beginning of the outer loop. */

 L30:
  jeval = TRUE;

  /*        calculate the jacobian matrix. */

  iflag = 2;
#ifdef WITH_JAC 
  (*fcn) (n, &x[1], &fvec[1], &fjac[fjac_offset], ldfjac, &iflag,data);
  ++(*njev);
#else 
  minpack_fdjac1 ( fcn, n, &x[1], &fvec[1], &fjac[fjac_offset], ldfjac,
		   &iflag, ml, mu, *epsfcn, &wa1[1], &wa2[1],data);
  *nfev += msum;
#endif 
  if (iflag < 0)
    {
      goto L300;
    }

  /*        compute the qr factorization of the jacobian. */

  minpack_qrfac (n, n, &fjac[fjac_offset], ldfjac, &c_false, iwa, &c__1,
		 &wa1[1], &wa2[1], &wa3[1]);


  if (iter == 1)
    {
      /*        on the first iteration and if mode is 1, scale according 
       *        to the norms of the columns of the initial jacobian. 
       */
      if (*mode != 2)
	{
	  for (j = 1; j <= *n ; ++j)
	    {
	      diag[j] =(wa2[j] == zero) ? one : wa2[j];
	    }
	}
      /*        on the first iteration, calculate the norm of the scaled x 
       *        and initialize the step bound delta. 
       */
      for (j = 1; j <= *n ; ++j)
	{
	  wa3[j] = diag[j] * x[j];
	}
      xnorm = minpack_enorm (*n, &wa3[1]);
      delta = *factor * xnorm;
      if (delta == zero)
	{
	  delta = *factor;
	}
    }

  /*        form (q transpose)*fvec and store in qtf. */

  for (i1 = 1; i1 <= *n ; ++i1)
    {
      qtf[i1] = fvec[i1];
    }
  for (j = 1; j <= *n ; ++j)
    {
      if (fjac[j + j * fjac_dim1] == zero) continue;
      sum = zero;
      for (i1 = j; i1 <= *n ; ++i1)
	{
	  sum += fjac[i1 + j * fjac_dim1] * qtf[i1];
	}
      temp = -sum / fjac[j + j * fjac_dim1];

      for (i1 = j; i1 <= *n ; ++i1)
	{
	  qtf[i1] += fjac[i1 + j * fjac_dim1] * temp;
	}
    }

  /*        copy the triangular factor of the qr factorization into r. */

  sing = FALSE;
  for (j = 1; j <= *n ; ++j)
    {
      l = j;
      jm1 = j - 1;
      if (jm1 >= 1)
	{
	  for (i1 = 1; i1 <= jm1 ; ++i1)
	    {
	      r__[l] = fjac[i1 + j * fjac_dim1];
	      l = l + *n - i1;
	    }
	}
      r__[l] = wa1[j];
      if (wa1[j] == zero)
	{
	  sing = TRUE;
	}
    }
  
  /*        accumulate the orthogonal factor in fjac. */

  minpack_qform (n, n, &fjac[fjac_offset], ldfjac, &wa1[1]);

  /*        rescale if necessary. */

  if (*mode != 2)
    {
      for (j = 1; j <= *n ; ++j)
	{
	  /* Computing MAX */
	  d__1 = diag[j], d__2 = wa2[j];
	  diag[j] = Max (d__1, d__2);
	}
    }

  /*        beginning of the inner loop. */

 L180:


  if (*nprint > 0)
    {
      /*           if requested, call fcn to enable printing of iterates. */
      iflag = 0;
      if ((iter - 1) % *nprint == 0)
	{
#ifdef WITH_JAC
	  (*fcn) (n, &x[1], &fvec[1], &fjac[fjac_offset], ldfjac, &iflag,data);
#else 
	  (*fcn) (n, &x[1], &fvec[1], &iflag,data);
#endif 
	}
      if (iflag < 0)
	{
	  goto L300;
	}
    }


  /*           determine the direction p. */

  minpack_dogleg (*n, &r__[1], *lr, &diag[1], &qtf[1], delta, &wa1[1], &wa2[1],
		  &wa3[1]);

  /*           store the direction p and x + p. calculate the norm of p. */

  for (j = 1; j <= *n ; ++j)
    {
      wa1[j] = -wa1[j];
      wa2[j] = x[j] + wa1[j];
      wa3[j] = diag[j] * wa1[j];
    }
  pnorm = minpack_enorm (*n, &wa3[1]);

  /*           on the first iteration, adjust the initial step bound. */

  if (iter == 1)
    {
      delta = Min (delta, pnorm);
    }

  /*           evaluate the function at x + p and calculate its norm. */

  iflag = 1;

#ifdef WITH_JAC
  (*fcn) (n, &wa2[1], &wa4[1], &fjac[fjac_offset], ldfjac, &iflag,data);
#else 
  (*fcn) (n, &wa2[1], &wa4[1], &iflag,data);
#endif 

  ++(*nfev);
  if (iflag < 0)
    {
      goto L300;
    }
  fnorm1 = minpack_enorm (*n, &wa4[1]);

  /*           compute the scaled actual reduction. */

  actred = -one;
  if (fnorm1 < fnorm)
    {
      /* Computing 2nd power */
      d__1 = fnorm1 / fnorm;
      actred = one - d__1 * d__1;
    }

  /*           compute the scaled predicted reduction. */

  l = 1;

  for (i1 = 1; i1 <= *n ; ++i1)
    {
      sum = zero;
      for (j = i1; j <= *n ; ++j)
	{
	  sum += r__[l] * wa1[j];
	  ++l;
	}
      wa3[i1] = qtf[i1] + sum;
    }
  temp = minpack_enorm (*n, &wa3[1]);
  prered = zero;
  if (temp < fnorm)
    {
      /* Computing 2nd power */
      d__1 = temp / fnorm;
      prered = one - d__1 * d__1;
    }

  /*           compute the ratio of the actual to the predicted 
   *           reduction. 
   */

  ratio = zero;
  if (prered > zero)
    {
      ratio = actred / prered;
    }

  /*           update the step bound. */

  if (ratio >= p1)
    {
      ncfail = 0;
      ++ncsuc;
      if (ratio >= p5 || ncsuc > 1)
	{
	  /* Computing MAX */
	  d__1 = delta, d__2 = pnorm / p5;
	  delta = Max (d__1, d__2);
	}
      if ((d__1 = ratio - one, Abs (d__1)) <= p1)
	{
	  delta = pnorm / p5;
	}
    }
  else
    {
      ncsuc = 0;
      ++ncfail;
      delta = p5 * delta;
    }

  /*           test for successful iteration. */

  if (ratio >= p0001)
    {
      /*           successful iteration. update x, fvec, and their norms. */
      fninf = 0.0;   /* added by Bruno */
      for (j = 1; j <= *n ; ++j)
	{
	  x[j] = wa2[j];
	  wa2[j] = diag[j] * x[j];
	  fvec[j] = wa4[j];
	  if ( fabs(fvec[j]) > fninf ) fninf = fabs(fvec[j]);  /* added by Bruno */
	}
      xnorm = minpack_enorm (*n, &wa2[1]);
      fnorm = fnorm1;
      ++iter;
    }

  /*           determine the progress of the iteration. */

  ++nslow1;
  if (actred >= p001)
    {
      nslow1 = 0;
    }
  if (jeval)
    {
      ++nslow2;
    }
  if (actred >= p1)
    {
      nslow2 = 0;
    }

  /*           test for convergence. */

  /* if (delta <= *xtol * xnorm || fnorm == zero) old test  */
  if (delta <= *xtol * xnorm || fninf <= *ftol)
    {
      *info = 1;
    }
  if (*info != 0)
    {
      goto L300;
    }

  /*           tests for termination and stringent tolerances. */

  if (*nfev >= *maxfev)
    {
      *info = 2;
    }
  /* Computing MAX */
  d__1 = p1 * delta;
  if (p1 * Max (d__1, pnorm) <= epsmch * xnorm)
    {
      *info = 3;
    }
  if (nslow2 == 5)
    {
      *info = 4;
    }
  if (nslow1 == 10)
    {
      *info = 5;
    }
  if (*info != 0)
    {
      goto L300;
    }

  /*           criterion for recalculating jacobian or 
   *           for recalculating jacobian approximation 
   *           by forward differences.
   */

  if (ncfail == 2)
    {
      goto L290;
    }

  /*           calculate the rank one modification to the jacobian */
  /*           and update qtf if necessary. */

  for (j = 1; j <= *n ; ++j)
    {
      sum = zero;
      for (i1 = 1; i1 <= *n ; ++i1)
	{
	  sum += fjac[i1 + j * fjac_dim1] * wa4[i1];
	}
      wa2[j] = (sum - wa3[j]) / pnorm;
      wa1[j] = diag[j] * (diag[j] * wa1[j] / pnorm);
      if (ratio >= p0001)
	{
	  qtf[j] = sum;
	}
    }

  /*           compute the qr factorization of the updated jacobian. */

  minpack_r1updt (n, n, &r__[1], lr, &wa1[1], &wa2[1], &wa3[1], &sing);
  minpack_r1mpyq (n, n, &fjac[fjac_offset], ldfjac, &wa2[1], &wa3[1]);
  minpack_r1mpyq (&c__1, n, &qtf[1], &c__1, &wa2[1], &wa3[1]);

  /*           end of the inner loop. */

  jeval = FALSE;
  goto L180;
 L290:

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
#ifdef WITH_JAC
      (*fcn) (n, &x[1], &fvec[1], &fjac[fjac_offset], ldfjac, &iflag,data);
#else 
      (*fcn) (n, &x[1], &fvec[1], &iflag,data);
#endif 
    }
  return 0;
}
