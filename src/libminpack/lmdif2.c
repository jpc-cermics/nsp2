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

static int minpack_lmdif3 (minpack_fcn2 fcn, int *m, int *n, double *x, double *fvec,
			   double *ftol, double *xtol, double *gtol, int *maxfev,
			   double *epsfcn, double *diag, int *mode, const double *factor,
			   int *nprint, int *info, int *nfev, double *fjac, int *ldfjac,
			   int *ipvt, double *qtf, double *wa1, double *wa2, double *wa3,
			   double *wa4,void *data);


/*     subroutine lmdif 
 */

int minpack_lmdif2 (minpack_fcn2 fcn, int *m, int *n, double *x, double *fvec,
		    double *tol, int *info, int *iwa, double *wa, int *lwa,
		    double *wb, int *lwb,
		    void *data)
{
  const double factor = 100.;
  const double zero = 0.;
  int mode=1, nfev, maxfev= (*n + 1) * 200, nprint=0;
  double ftol=*tol, gtol= zero, xtol=*tol, epsfcn= zero;
  --fvec;
  --iwa;
  --x;
  --wa;
  *info = 1;
  /*     check the input parameters for errors. */
  if (*n <= 0 || *m < *n || *tol < zero || *lwa <  *n * 5 + *m ||  *lwb <*m * *n )
    {
      *info = 0;
      return 0;
    }
  minpack_lmdif3 ( fcn, m, n, &x[1], &fvec[1], &ftol, &xtol, &gtol,
		   &maxfev, &epsfcn, &wa[1], &mode, &factor, &nprint, info,
		   &nfev, wb, m, &iwa[1], &wa[*n + 1],
		   &wa[(*n << 1) + 1], &wa[*n * 3 + 1], &wa[(*n << 2) + 1],
		   &wa[*n * 5 + 1],data);
  return 0;
}


static int minpack_lmdif3 (minpack_fcn2 fcn, int *m, int *n, double *x, double *fvec,
			   double *ftol, double *xtol, double *gtol, int *maxfev,
			   double *epsfcn, double *diag, int *mode, const double *factor,
			   int *nprint, int *info, int *nfev, double *fjac, int *ldfjac,
			   int *ipvt, double *qtf, double *wa1, double *wa2, double *wa3,
			   double *wa4,void *data)
{
  const double zero = 0.;
  int c__1 = 1;
  int fjac_dim1, fjac_offset, i__1;
  int j, iflag;
  double fnorm, epsmch;
  
  --wa4;
  --fvec;
  --diag;
  --x;

  fjac_dim1 = *ldfjac;
  fjac_offset = fjac_dim1 + 1;
  fjac -= fjac_offset;

  /* Function Body */

  /*     epsmch is the machine precision. */

  epsmch = minpack_dpmpar (&c__1);

  iflag = 0;
  *nfev = 0;

  /*     check the input parameters for errors. */

  if (*n <= 0 || *m < *n || *ldfjac < *m || *ftol < zero || *xtol < zero
      || *gtol < zero || *maxfev <= 0 || *factor <= zero)
    {
      goto L300;
    }
  if (*mode == 2)
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  if (diag[j] <= zero)
	    {
	      goto L300;
	    }
	}
    }

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
  /*        calculate the jacobian matrix. */
  iflag = 2;
  minpack_fdjac2 ( fcn, m, n, &x[1], &fvec[1], &fjac[fjac_offset],
		   ldfjac, &iflag, epsfcn, &wa4[1],data);
 L300:
  if (iflag < 0)
    {
      *info = iflag;
    }
  return 0;
}

