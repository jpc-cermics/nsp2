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

/* This file is included in lmder.c, or lmdif.c, or lmstr.c
 * in order to share common code. 
 */

#include "minpack.h"

#ifdef WITH_JAC 
int minpack_lmder (minpack_fcn4 fcn, int *m, int *n, double *x, double *fvec,
		   double *fjac, int *ldfjac, double *ftol, double *xtol,
		   double *gtol, int *maxfev, double *diag, int *mode,
		   const double *factor, int *nprint, int *info, int *nfev, int *njev,
		   int *ipvt, double *qtf, double *wa1, double *wa2, double *wa3,
		   double *wa4,void *data)
#else
#ifdef WITH_JAC_MIN
int minpack_lmstr (minpack_fcn5 fcn, int *m, int *n, double *x, double *fvec,
		   double *fjac, int *ldfjac, double *ftol, double *xtol,
		   double *gtol, int *maxfev, double *diag, int *mode,
		   const double *factor, int *nprint, int *info, int *nfev, int *njev,
		   int *ipvt, double *qtf, double *wa1, double *wa2, double *wa3,
		   double *wa4,void *data)
#else 
int minpack_lmdif (minpack_fcn2 fcn, int *m, int *n, double *x, double *fvec,
		   double *ftol, double *xtol, double *gtol, int *maxfev,
		   double *epsfcn, double *diag, int *mode, const double *factor,
		   int *nprint, int *info, int *nfev, double *fjac, int *ldfjac,
		   int *ipvt, double *qtf, double *wa1, double *wa2, double *wa3,
		   double *wa4,void *data)
#endif 
#endif 
{
  int c_true = TRUE;
  const double one = 1., p1 = .1, p5 = .5, p25 = .25, p75 = .75, p0001 = 1e-4, zero = 0.;
  int fjac_dim1, fjac_offset;
  double d1, d2, d3;
  int iter;
  double temp=0.0, temp1, temp2;
  int i, j, l, iflag;
  double delta;
  double ratio;
  double fnorm, gnorm, pnorm, xnorm=0.0, fnorm1, actred, dirder, epsmch, prered;
  double par, sum;

#ifdef WITH_JAC_MIN
  int sing;
#endif 

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

  /*     epsmch is the machine precision. */
  epsmch = minpack_dpmpar (1);

  *info = 0;
  iflag = 0;
  *nfev = 0;
#ifdef WITH_JAC 
  *njev = 0;
#endif 

  /*     check the input parameters for errors. */
#ifdef WITH_JAC_MIN
  if (*n <= 0 || *m < *n || *ldfjac < *n || *ftol < zero || *xtol < zero
      || *gtol < zero || *maxfev <= 0 || *factor <= zero)
#else 
  if (*n <= 0 || *m < *n || *ldfjac < *m || *ftol < zero || *xtol < zero
      || *gtol < zero || *maxfev <= 0 || *factor <= zero)
#endif 
    {
      goto L300;
    }
  if (*mode != 2)
    {
      goto L20;
    }

  for (j = 1; j <= *n ; ++j)
    {
      if (diag[j] <= zero)
	{
	  goto L300;
	}
    }
 L20:

  /*     evaluate the function at the starting point */
  /*     and calculate its norm. */

  iflag = 1;
#ifdef WITH_JAC 
  (*fcn) (m, n, &x[1], &fvec[1], &fjac[fjac_offset], ldfjac, &iflag,data);
#else 
#ifdef WITH_JAC_MIN
  (*fcn) (m, n, &x[1], &fvec[1], &wa3[1], &iflag,data);
#else 
  (*fcn) (m, n, &x[1], &fvec[1], &iflag,data);
#endif
#endif 
  *nfev = 1;
  if (iflag < 0)
    {
      goto L300;
    }
  fnorm = minpack_enorm (*m, &fvec[1]);

  /*     initialize levenberg-marquardt parameter and iteration counter. */

  par = zero;
  iter = 1;

  /*     beginning of the outer loop. */

 L30:

  /*        calculate the jacobian matrix. */

#ifndef WITH_JAC_MIN 
  iflag = 2;
#ifdef WITH_JAC 
  (*fcn) (m, n, &x[1], &fvec[1], &fjac[fjac_offset], ldfjac, &iflag,data);
  ++(*njev);
#else 
  minpack_fdjac2 ( fcn, m, n, &x[1], &fvec[1], &fjac[fjac_offset],
		   ldfjac, &iflag, *epsfcn, &wa4[1],data);
  *nfev += *n;
#endif
  if (iflag < 0)
    {
      goto L300;
    }
#endif 

  /*        if requested, call fcn to enable printing of iterates. */

  if (*nprint <= 0)
    {
      goto L40;
    }
  iflag = 0;
  if ((iter - 1) % *nprint == 0)
    {
#ifdef WITH_JAC 
      (*fcn) (m, n, &x[1], &fvec[1], &fjac[fjac_offset], ldfjac, &iflag,data);
#else 
#ifdef WITH_JAC_MIN
      (*fcn) (m, n, &x[1], &fvec[1], &wa3[1], &iflag, data);
#else 
      (*fcn) (m, n, &x[1], &fvec[1], &iflag,data);
#endif  
#endif
    }
  if (iflag < 0)
    {
      goto L300;
    }
 L40:


#ifndef WITH_JAC_MIN 

  /*        compute the qr factorization of the jacobian. */

  minpack_qrfac (m, n, &fjac[fjac_offset], ldfjac, &c_true, &ipvt[1], n,
		 &wa1[1], &wa2[1], &wa3[1]);

#else 
  /*        compute the qr factorization of the jacobian matrix */
  /*        calculated one row at a time, while simultaneously */
  /*        forming (q transpose)*fvec and storing the first */
  /*        n components in qtf. */


  for (j = 1; j <= *n; ++j)
    {
      qtf[j] = zero;

      for (i = 1; i <= *n ; ++i)
	{
	  fjac[i + j * fjac_dim1] = zero;
	}
    }
  iflag = 2;

  for (i = 1; i <= *m; ++i)
    {
      (*fcn) (m, n, &x[1], &fvec[1], &wa3[1], &iflag, data);
      if (iflag < 0)
	{
	  goto L300;
	}
      temp = fvec[i];
      minpack_rwupdt (n, &fjac[fjac_offset], ldfjac, &wa3[1], &qtf[1], &temp,
		      &wa1[1], &wa2[1]);
      ++iflag;
    }
  ++(*njev);

  /*        if the jacobian is rank deficient, call qrfac to */
  /*        reorder its columns and update the components of qtf. */

  sing = FALSE;

  for (j = 1; j <= *n ; ++j)
    {
      if (fjac[j + j * fjac_dim1] == zero)
	{
	  sing = TRUE;
	}
      ipvt[j] = j;
      wa2[j] = minpack_enorm (j, &fjac[j * fjac_dim1 + 1]);
    }
  if (!sing)
    {
      goto L130;
    }
  minpack_qrfac (n, n, &fjac[fjac_offset], ldfjac, &c_true, &ipvt[1], n,
		 &wa1[1], &wa2[1], &wa3[1]);

  for (j = 1; j <= *n ; ++j)
    {
      if (fjac[j + j * fjac_dim1] == zero)
	{
	  goto L110;
	}
      sum = zero;

      for (i = j; i <= *n ; ++i)
	{
	  sum += fjac[i + j * fjac_dim1] * qtf[i];
	}
      temp = -sum / fjac[j + j * fjac_dim1];

      for (i = j; i <= *n ; ++i)
	{
	  qtf[i] += fjac[i + j * fjac_dim1] * temp;
	}
    L110:
      fjac[j + j * fjac_dim1] = wa1[j];
    }
 L130:
#endif 

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

  for (j = 1; j <= *n ; ++j)
    {
      diag[j] = wa2[j];
      if (wa2[j] == zero)
	{
	  diag[j] = one;
	}
    }
 L60:

  /*        on the first iteration, calculate the norm of the scaled x */
  /*        and initialize the step bound delta. */


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
 L80:

#ifndef WITH_JAC_MIN 
  /*        form (q transpose)*fvec and store the first n components in */
  /*        qtf. */


  for (i = 1; i <= *m ; ++i)
    {
      wa4[i] = fvec[i];
    }

  for (j = 1; j <= *n ; ++j)
    {
      if (fjac[j + j * fjac_dim1] == zero)
	{
	  goto L120;
	}
      sum = zero;

      for (i = j; i <= *m; ++i)
	{
	  sum += fjac[i + j * fjac_dim1] * wa4[i];
	}
      temp = -sum / fjac[j + j * fjac_dim1];

      for (i = j; i <= *m; ++i)
	{
	  wa4[i] += fjac[i + j * fjac_dim1] * temp;
	}
    L120:
      fjac[j + j * fjac_dim1] = wa1[j];
      qtf[j] = wa4[j];
    }
#endif 

  /*        compute the norm of the scaled gradient. */

  gnorm = zero;
  if (fnorm == zero)
    {
      goto L170;
    }

  for (j = 1; j <= *n; ++j)
    {
      l = ipvt[j];
      if (wa2[l] == zero)
	{
	  goto L150;
	}
      sum = zero;

      for (i = 1; i <= j; ++i)
	{
	  sum += fjac[i + j * fjac_dim1] * (qtf[i] / fnorm);
	}
      /* Computing MAX */
      d2 = gnorm, d3 = (d1 = sum / wa2[l], Abs (d1));
      gnorm = Max (d2, d3);
    L150:
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

  for (j = 1; j <= *n ; ++j)
    {
      d1 = diag[j], d2 = wa2[j];
      diag[j] = Max (d1, d2);
    }
 L190:

  /*        beginning of the inner loop. */

 L200:

  /*           determine the levenberg-marquardt parameter. */

  minpack_lmpar (n, &fjac[fjac_offset], ldfjac, &ipvt[1], &diag[1], &qtf[1],
		 &delta, &par, &wa1[1], &wa2[1], &wa3[1], &wa4[1]);

  /*           store the direction p and x + p. calculate the norm of p. */


  for (j = 1; j <= *n; ++j)
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
  (*fcn) (m, n, &wa2[1], &wa4[1], &fjac[fjac_offset], ldfjac, &iflag,data);
#else 
#ifdef WITH_JAC_MIN
  (*fcn) (m, n, &wa2[1], &wa4[1], &wa3[1], &iflag, data);
#else 
  (*fcn) (m, n, &wa2[1], &wa4[1], &iflag,data);
#endif
#endif
  ++(*nfev);
  if (iflag < 0)
    {
      goto L300;
    }
  fnorm1 = minpack_enorm (*m, &wa4[1]);

  /*           compute the scaled actual reduction. */

  actred = -one;
  if (p1 * fnorm1 < fnorm)
    {
      /* Computing 2nd power */
      d1 = fnorm1 / fnorm;
      actred = one - d1 * d1;
    }

  /*           compute the scaled predicted reduction and */
  /*           the scaled directional derivative. */

  for (j = 1; j <= *n; ++j)
    {
      wa3[j] = zero;
      l = ipvt[j];
      temp = wa1[l];

      for (i = 1; i <= j; ++i)
	{
	  wa3[i] += fjac[i + j * fjac_dim1] * temp;
	}
    }
  temp1 = minpack_enorm (*n, &wa3[1]) / fnorm;
  temp2 = sqrt (par) * pnorm / fnorm;
  d1 = temp1;
  d2 = temp2;
  prered = d1 * d1 + d2 * d2 / p5;
  d1 = temp1;
  d2 = temp2;
  dirder = -(d1 * d1 + d2 * d2);

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
  d1 = delta, d2 = pnorm / p1;
  delta = temp * Min (d1, d2);
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

  for (j = 1; j <= *n; ++j)
    {
      x[j] = wa2[j];
      wa2[j] = diag[j] * x[j];
    }

  for (i = 1; i <= *m; ++i)
    {
      fvec[i] = wa4[i];
    }
  xnorm = minpack_enorm (*n, &wa2[1]);
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
#ifdef WITH_JAC 
      (*fcn) (m, n, &x[1], &fvec[1], &fjac[fjac_offset], ldfjac, &iflag,data);
#else 
#ifdef WITH_JAC_MIN 
      (*fcn) (m, n, &x[1], &fvec[1], &wa3[1], &iflag, data);
#else 
      (*fcn) (m, n, &x[1], &fvec[1], &iflag,data);
#endif
#endif
    }
  return 0;
}
