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

/*     
 * 
 *     A truncated version of hybrd, which is used just to return 
 *     f and its jacobian at the given point 
 *     J.Ph Chancelier from hybrd. 2008
 */

#include "minpack.h"

int minpack_hybrd_eval (minpack_fcn1 fcn, int *n, double *x, double *fvec, 
			int *maxfev, int *ml, int *mu, double *epsfcn, 
			int *info, int *nfev,
			double *fjac, int *ldfjac, double *wa1, double *wa2, void *data)
{
  int  i__1, iter, msum, iflag, jeval, ncsuc, nslow1, nslow2,  ncfail;
  double fnorm;
  
  *info = 0;
  iflag = 0;
  *nfev = 0;

  /*     check the input parameters for errors. */

  if (*n <= 0 || *maxfev <= 0 || *ml < 0 || *mu < 0 || *ldfjac < *n )
    {
      goto L300;
    }

  /*     evaluate the function at the starting point */
  /*     and calculate its norm. */

  iflag = 1;
  (*fcn) (n, x, fvec, &iflag,data);
  *nfev = 1;
  if (iflag < 0)
    {
      goto L300;
    }
  fnorm = minpack_enorm (n, fvec);

  /*     determine the number of calls to fcn needed to compute */
  /*     the jacobian matrix. */

  /* Computing MIN */
  i__1 = *ml + *mu + 1;
  msum = Min (i__1, *n);

  /*     initialize iteration counter and monitors. */

  iter = 1;
  ncsuc = 0;
  ncfail = 0;
  nslow1 = 0;
  nslow2 = 0;

  jeval = TRUE;

  /*        calculate the jacobian matrix. */

  iflag = 2;
  minpack_fdjac1 ( fcn, n, x, fvec, fjac, ldfjac,
		   &iflag, ml, mu, epsfcn, wa1, wa2,data);
  *nfev += msum;
  
  /*     termination, either normal or user imposed. */
 L300:

  if (iflag < 0)
    {
      *info = iflag;
    }
  iflag = 0;

  return 0;
}
