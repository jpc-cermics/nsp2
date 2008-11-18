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

	


#include "nsp/interf.h"
#include "minpack.h"

/*     subroutine vecfcn 
 *     this subroutine defines fourteen test functions. the first 
 *     five test functions are of dimensions 2,4,2,4,3, respectively, 
 *     while the remaining test functions are of variable dimension 
 *     n for any n greater than or equal to 1 (problem 6 is an 
 *     exception to this, since it does not allow n = 1). 
 *     the subroutine statement is 
 *       subroutine vecfcn(n,x,fvec,nprob)  
 *     where 
 *       n is a positive int input variable. 
 *       x is an input array of length n. 
 *       fvec is an output array of length n which contains the nprob 
 *         function vector evaluated at x. 
 *       nprob is a positive int input variable which defines the 
 *         number of the problem. nprob must not exceed 14. 
 *
 *     argonne national laboratory. minpack project. march 1980. 
 *     burton s. Garbow, kenneth e. Hillstrom, jorge j. More 
 */

/*     rosenbrock function. */

/* fvect is filled with f(x) */

int minpack_rosenbrock (const int *n, double *x, double *fvec, int *iflag, void *data)
{
  if ( *iflag == 0) return 0 ;
  if ( *n != 2 )
    {
      Scierror("Error: rosenbrock test should be used with n==2\n");
      *iflag=-1;
    }
  fvec[0] = 1.0 - x[0];
  fvec[1] = 10.0 * (x[1] - x[0]*x[0]);
  return 0;
}

/* fjac : jacobian matrix, *ldfjac gives the number of rows */

int minpack_jac_rosenbrock (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data)
{
  const double zero = 0.,one = 1.,ten = 10., twenty = 20.;
  if ( *iflag == 0) return 0 ;
  fjac[0] = -one;
  fjac[1] = -twenty * x[0];
  fjac[2] = zero;
  fjac[3] = ten;
  return 0;
}


/*     powell singular function. */

int minpack_powell_singular (const int *n, double *x, double *fvec, int *iflag, void *data)
{
  const double two = 2., five = 5., ten = 10.;
  double d;
  if ( *iflag == 0) return 0 ;
  fvec[0] = x[0] + ten * x[1];
  fvec[1] = sqrt (five) * (x[2] - x[3]);
  d = x[1] - two * x[2];
  fvec[2] = d * d;
  /* computing 2nd power */
  d = x[0] - x[3];
  fvec[3] = sqrt (ten) * (d * d);
  return 0;
}


int minpack_jac_powell_singular (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data)
{
  int k, fjac_dim1 = *ldfjac;
  const double zero = 0.,one = 1.,two = 2.,five = 5.,ten = 10.;

  if ( *iflag == 0) return 0 ;
  for (k = 0 ; k < (*n)*(*n) ; k++ )  fjac[k] = zero;
  fjac[0] = one;
  fjac[fjac_dim1] = ten;
  fjac[fjac_dim1*2 + 1] = sqrt (five);
  fjac[fjac_dim1*3 + 1] = - fjac[fjac_dim1*2 + 1];
  fjac[fjac_dim1*2 + 2] = two * (x[1] - two * x[2]);
  fjac[fjac_dim1*3 + 2] = -two * fjac[fjac_dim1*2 + 2];
  fjac[3] = two * sqrt (ten) * (x[0] - x[3]);
  fjac[fjac_dim1*3 + 3] = - fjac[3];
  return 0;
}


/*     powell badly scaled function. */

int minpack_powell_badly_scaled (const int *n, double *x, double *fvec, int *iflag, void *data)
{
  const double one = 1.;
  const double c1 = 1e4;
  const double c2 = 1.0001;
  if ( *iflag == 0) return 0 ;
  --fvec;
  --x;
  fvec[1] = c1 * x[1] * x[2] - one;
  fvec[2] = exp (-x[1]) + exp (-x[2]) - c2;
  return 0;
}


int minpack_jac_powell_badly_scaled (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data)
{
  const double c1 = 1e4;
  int fjac_dim1 =  *ldfjac;
  if ( *iflag == 0) return 0 ;
  --x;
  fjac -= fjac_dim1 + 1;

  fjac[fjac_dim1 + 1] = c1 * x[2];
  fjac[(fjac_dim1 << 1) + 1] = c1 * x[1];
  fjac[fjac_dim1 + 2] = -exp (-x[1]);
  fjac[(fjac_dim1 << 1) + 2] = -exp (-x[2]);
  return 0;

}

/*     wood function. */


int minpack_wood (const int *n, double *x, double *fvec, int *iflag, void *data)
{
  const double one = 1.;
  const double c3 = 200.;
  const double c4 = 20.2;
  const double c5 = 19.8;
  const double c6 = 180.;

  double d__1, temp1,temp2;
  if ( *iflag == 0) return 0 ;

  --fvec;
  --x;

  /* computing 2nd power */
  d__1 = x[1];
  temp1 = x[2] - d__1 * d__1;
  /* computing 2nd power */
  d__1 = x[3];
  temp2 = x[4] - d__1 * d__1;
  fvec[1] = -c3 * x[1] * temp1 - (one - x[1]);
  fvec[2] = c3 * temp1 + c4 * (x[2] - one) + c5 * (x[4] - one);
  fvec[3] = -c6 * x[3] * temp2 - (one - x[3]);
  fvec[4] = c6 * temp2 + c4 * (x[4] - one) + c5 * (x[2] - one);
  return 0;
}

int minpack_jac_wood (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data)
{
  const double zero = 0.,one = 1.,two = 2.,three = 3.;
  const double c3 = 200.,c4 = 20.2,c5 = 19.8,c6 = 180.;

  int fjac_dim1,k,j;
  double d__1,temp1, temp2;
  fjac_dim1 = *ldfjac;

  if ( *iflag == 0) return 0 ;
  --x;
  fjac -= fjac_dim1 + 1;

  for (k = 1; k <= 4; ++k)
    {
      for (j = 1; j <= 4; ++j)
	{
	  fjac[k + j * fjac_dim1] = zero;
	}
    }
  /* computing 2nd power */
  d__1 = x[1];
  temp1 = x[2] - three * (d__1 * d__1);
  /* computing 2nd power */
  d__1 = x[3];
  temp2 = x[4] - three * (d__1 * d__1);
  fjac[fjac_dim1 + 1] = -c3 * temp1 + one;
  fjac[(fjac_dim1 << 1) + 1] = -c3 * x[1];
  fjac[fjac_dim1 + 2] = -two * c3 * x[1];
  fjac[(fjac_dim1 << 1) + 2] = c3 + c4;
  fjac[(fjac_dim1 << 2) + 2] = c5;
  fjac[fjac_dim1 * 3 + 3] = -c6 * temp2 + one;
  fjac[(fjac_dim1 << 2) + 3] = -c6 * x[3];
  fjac[(fjac_dim1 << 1) + 4] = c5;
  fjac[fjac_dim1 * 3 + 4] = -two * c6 * x[3];
  fjac[(fjac_dim1 << 2) + 4] = c6 + c4;
  return 0;

}



/*     helical valley function. */

static double d_sign(double *a, double *b)
{
  double x = (*a >= 0 ? *a : - *a);
  return( *b >= 0 ? x : -x);
}

int minpack_helical_valley (const int *n, double *x, double *fvec, int *iflag, void *data)
{
  double c7 = .25;
  const double zero = 0.;
  const double one = 1.;
  const double ten = 10.;
  const double c8 = .5;
  const double eight = 8.;

  double d__1,d__2, temp1, temp2,tpi;
  if ( *iflag == 0) return 0 ;
  --fvec;
  --x;

  tpi = eight * atan (one);
  temp1 = d_sign (&c7, &x[2]);
  if (x[1] > zero)
    {
      temp1 = atan (x[2] / x[1]) / tpi;
    }
  if (x[1] < zero)
    {
      temp1 = atan (x[2] / x[1]) / tpi + c8;
    }
  /* computing 2nd power */
  d__1 = x[1];
  /* computing 2nd power */
  d__2 = x[2];
  temp2 = sqrt (d__1 * d__1 + d__2 * d__2);
  fvec[1] = ten * (x[3] - ten * temp1);
  fvec[2] = ten * (temp2 - one);
  fvec[3] = x[3];
  return 0;
}




int minpack_jac_helical_valley (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data)
{
  const double zero = 0.,one = 1.,eight = 8.,ten = 10.;
  const double hundrd = 100.;

  int fjac_dim1;
  double d__1, d__2, temp, temp1, temp2, tpi;
  fjac_dim1 = *ldfjac;

  if ( *iflag == 0) return 0 ;
  --x;
  fjac -= fjac_dim1 + 1;

  tpi = eight * atan (one);
  /* computing 2nd power */
  d__1 = x[1];
  /* computing 2nd power */
  d__2 = x[2];
  temp = d__1 * d__1 + d__2 * d__2;
  temp1 = tpi * temp;
  temp2 = sqrt (temp);
  fjac[fjac_dim1 + 1] = hundrd * x[2] / temp1;
  fjac[(fjac_dim1 << 1) + 1] = -hundrd * x[1] / temp1;
  fjac[fjac_dim1 * 3 + 1] = ten;
  fjac[fjac_dim1 + 2] = ten * x[1] / temp2;
  fjac[(fjac_dim1 << 1) + 2] = ten * x[2] / temp2;
  fjac[fjac_dim1 * 3 + 2] = zero;
  fjac[fjac_dim1 + 3] = zero;
  fjac[(fjac_dim1 << 1) + 3] = zero;
  fjac[fjac_dim1 * 3 + 3] = one;
  return 0;
}


/*     watson function. */

int minpack_watson (const int *n, double *x, double *fvec, int *iflag, void *data)
{
  const double zero = 0.;
  const double one = 1.;
  const double two = 2.;
  const double c9 = 29.;
  int i__1, i__2;
  double d__1;
  double  temp, temp1, temp2;
  int i__, j, k;
  double ti;
  double  sum1, sum2;
  if ( *iflag == 0) return 0 ;
  --fvec;
  --x;

  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      fvec[k] = zero;
      /* l70: */
    }
  for (i__ = 1; i__ <= 29; ++i__)
    {
      ti = (double) i__ / c9;
      sum1 = zero;
      temp = one;
      i__1 = *n;
      for (j = 2; j <= i__1; ++j)
	{
	  i__2 = j - 1;
	  sum1 += (double) i__2 *temp * x[j];
	  temp = ti * temp;
	  /* l80: */
	}
      sum2 = zero;
      temp = one;
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  sum2 += temp * x[j];
	  temp = ti * temp;
	  /* l90: */
	}
      /* computing 2nd power */
      d__1 = sum2;
      temp1 = sum1 - d__1 * d__1 - one;
      temp2 = two * ti * sum2;
      temp = one / ti;
      i__1 = *n;
      for (k = 1; k <= i__1; ++k)
	{
	  i__2 = k - 1;
	  fvec[k] += temp * ((double) i__2 - temp2) * temp1;
	  temp = ti * temp;
	  /* l100: */
	}
      /* l110: */
    }
  /* computing 2nd power */
  d__1 = x[1];
  temp = x[2] - d__1 * d__1 - one;
  fvec[1] += x[1] * (one - two * temp);
  fvec[2] += temp;
  return 0;
}


int minpack_jac_watson (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data)
{
  const double zero = 0.,one = 1.,two = 2.,three = 3.,six = 6.;
  const double c9 = 29.;

  int fjac_dim1, i__1, i__2, i__3, i__4, i__, j, k;
  double d__1, temp, temp1, temp2, ti, tj, tk, sum1, sum2;
  fjac_dim1 = *ldfjac;
  if ( *iflag == 0) return 0 ;
  --x;
  fjac -= fjac_dim1 + 1;

  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      i__2 = *n;
      for (j = k; j <= i__2; ++j)
	{
	  fjac[k + j * fjac_dim1] = zero;
	  /* l110: */
	}
      /* l120: */
    }
  for (i__ = 1; i__ <= 29; ++i__)
    {
      ti = (double) i__ / c9;
      sum1 = zero;
      temp = one;
      i__1 = *n;
      for (j = 2; j <= i__1; ++j)
	{
	  i__2 = j - 1;
	  sum1 += (double) i__2 *temp * x[j];
	  temp = ti * temp;
	  /* l130: */
	}
      sum2 = zero;
      temp = one;
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  sum2 += temp * x[j];
	  temp = ti * temp;
	  /* l140: */
	}
      /* computing 2nd power */
      d__1 = sum2;
      temp1 = two * (sum1 - d__1 * d__1 - one);
      temp2 = two * sum2;
      /* computing 2nd power */
      d__1 = ti;
      temp = d__1 * d__1;
      tk = one;
      i__1 = *n;
      for (k = 1; k <= i__1; ++k)
	{
	  tj = tk;
	  i__2 = *n;
	  for (j = k; j <= i__2; ++j)
	    {
	      i__3 = k - 1;
	      i__4 = j - 1;
	      fjac[k + j * fjac_dim1] +=
		tj * (((double) i__3 / ti - temp2) *
		      ((double) i__4 / ti - temp2) - temp1);
	      tj = ti * tj;
	      /* l150: */
	    }
	  tk = temp * tk;
	  /* l160: */
	}
      /* l170: */
    }
  /* computing 2nd power */
  d__1 = x[1];
  fjac[fjac_dim1 + 1] =
    fjac[fjac_dim1 + 1] + six * (d__1 * d__1) - two * x[2] + three;
  fjac[(fjac_dim1 << 1) + 1] -= two * x[1];
  fjac[(fjac_dim1 << 1) + 2] += one;
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      i__2 = *n;
      for (j = k; j <= i__2; ++j)
	{
	  fjac[j + k * fjac_dim1] = fjac[k + j * fjac_dim1];
	  /* l180: */
	}
      /* l190: */
    }
  return 0;


}

/*     chebyquad function. */


int minpack_chebyquad (const int *n, double *x, double *fvec, int *iflag, void *data)
{
  const double zero = 0.;
  const double one = 1.;
  const double two = 2.;
  int i__1, i__2;
  double d__1 ;
  double temp, temp1, temp2;
  int i__, j, k;
  double ti, tk;
  int  iev;
  if ( *iflag == 0) return 0 ;
  --fvec;
  --x;

  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      fvec[k] = zero;
    }
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      temp1 = one;
      temp2 = two * x[j] - one;
      temp = two * temp2;
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  fvec[i__] += temp2;
	  ti = temp * temp2 - temp1;
	  temp1 = temp2;
	  temp2 = ti;
	}
    }
  tk = one / (double) (*n);
  iev = -1;
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      fvec[k] = tk * fvec[k];
      if (iev > 0)
	{
	  /* computing 2nd power */
	  d__1 = (double) k;
	  fvec[k] += one / (d__1 * d__1 - one);
	}
      iev = -iev;
    }
  return 0;
}

int minpack_jac_chebyquad (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data)
{
  const double zero = 0.,one = 1.,two = 2.,four = 4.;

  int fjac_dim1, i__1, i__2, j, k;
  double temp, temp1, temp2, temp3, temp4, ti, tk;
  fjac_dim1 = *ldfjac;
  if ( *iflag == 0) return 0 ;
  --x;
  fjac -= fjac_dim1 + 1;

  tk = one / (double) (*n);
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      temp1 = one;
      temp2 = two * x[j] - one;
      temp = two * temp2;
      temp3 = zero;
      temp4 = two;
      i__2 = *n;
      for (k = 1; k <= i__2; ++k)
	{
	  fjac[k + j * fjac_dim1] = tk * temp4;
	  ti = four * temp2 + temp * temp4 - temp3;
	  temp3 = temp4;
	  temp4 = ti;
	  ti = temp * temp2 - temp1;
	  temp1 = temp2;
	  temp2 = ti;
	  /* l210: */
	}
      /* l220: */
    }
  return 0;


}

/*     brown almost-linear function. */


int minpack_brown (const int *n, double *x, double *fvec, int *iflag, void *data)
{
  const double one = 1.;
  int i__1;
  double prod;
  int j, k;
  double  sum;

  if ( *iflag == 0) return 0 ;
  --fvec;
  --x;

  i__1 = *n + 1;
  sum = -((double) i__1);
  prod = one;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      sum += x[j];
      prod = x[j] * prod;
      /* l180: */
    }
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      fvec[k] = x[k] + sum;
      /* l190: */
    }
  fvec[*n] = prod - one;
  return 0;
}



int minpack_jac_brown (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data)
{
  const double zero = 0.,one = 1.,two = 2.;

  int fjac_dim1, i__1, i__2, j, k;
  double prod, temp;
  fjac_dim1 = *ldfjac;

  if ( *iflag == 0) return 0 ;
  --x;
  fjac -= fjac_dim1 + 1;

  prod = one;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      prod = x[j] * prod;
      i__2 = *n;
      for (k = 1; k <= i__2; ++k)
	{
	  fjac[k + j * fjac_dim1] = one;
	  /* l240: */
	}
      fjac[j + j * fjac_dim1] = two;
      /* l250: */
    }
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      temp = x[j];
      if (temp != zero)
	{
	  goto l270;
	}
      temp = one;
      prod = one;
      i__2 = *n;
      for (k = 1; k <= i__2; ++k)
	{
	  if (k != j)
	    {
	      prod = x[k] * prod;
	    }
	  /* l260: */
	}
    l270:
      fjac[*n + j * fjac_dim1] = prod / temp;
      /* l280: */
    }
  return 0;
}

/*     discrete boundary value function. */


int minpack_discrete_boundary (const int *n, double *x, double *fvec, int *iflag, void *data)
{
  const double zero = 0.;
  const double one = 1.;
  const double two = 2.;

  int i__1;
  double d__1, d__2;
  double temp, temp1, temp2, h__;
  int  k;

  if ( *iflag == 0) return 0 ;
  --fvec;
  --x;

  i__1 = *n + 1;
  h__ = one / (double) i__1;
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      /* computing 3rd power */
      d__1 = x[k] + (double) k *h__ + one, d__2 = d__1;
      temp = d__2 * (d__1 * d__1);
      temp1 = zero;
      if (k != 1)
	{
	  temp1 = x[k - 1];
	}
      temp2 = zero;
      if (k != *n)
	{
	  temp2 = x[k + 1];
	}
      /* computing 2nd power */
      d__1 = h__;
      fvec[k] = two * x[k] - temp1 - temp2 + temp * (d__1 * d__1) / two;
    }
  return 0;
}


int minpack_jac_discrete_boundary (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data)
{
  const double zero = 0.,one = 1.,two = 2.,three = 3.;
  
  int fjac_dim1, i__1, i__2,  j, k;
  double d__1, temp, h__;
  fjac_dim1 = *ldfjac;

  if ( *iflag == 0) return 0 ;

  --x;
  fjac -= fjac_dim1 + 1;

  i__1 = *n + 1;
  h__ = one / (double) i__1;
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      /* computing 2nd power */
      d__1 = x[k] + (double) k *h__ + one;
      temp = three * (d__1 * d__1);
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  fjac[k + j * fjac_dim1] = zero;
	  /* l300: */
	}
      /* computing 2nd power */
      d__1 = h__;
      fjac[k + k * fjac_dim1] = two + temp * (d__1 * d__1) / two;
      if (k != 1)
	{
	  fjac[k + (k - 1) * fjac_dim1] = -one;
	}
      if (k != *n)
	{
	  fjac[k + (k + 1) * fjac_dim1] = -one;
	}
      /* l310: */
    }
  return 0;


}

/*     discrete integral equation function. */

int minpack_discrete_integral (const int *n, double *x, double *fvec, int *iflag, void *data)
{
  const double zero = 0.;
  const double one = 1.;
  const double two = 2.;
  int i__1, i__2;
  double d__1, d__2;
  double  temp, h__;
  int j, k;
  double tj, tk;
  int kp1;
  double sum1, sum2;

  if ( *iflag == 0) return 0 ;
  --fvec;
  --x;

  i__1 = *n + 1;
  h__ = one / (double) i__1;
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      tk = (double) k *h__;
      sum1 = zero;
      i__2 = k;
      for (j = 1; j <= i__2; ++j)
	{
	  tj = (double) j *h__;
	  /* computing 3rd power */
	  d__1 = x[j] + tj + one, d__2 = d__1;
	  temp = d__2 * (d__1 * d__1);
	  sum1 += tj * temp;
	  /* l230: */
	}
      sum2 = zero;
      kp1 = k + 1;
      if (*n < kp1)
	{
	  goto l250;
	}
      i__2 = *n;
      for (j = kp1; j <= i__2; ++j)
	{
	  tj = (double) j *h__;
	  /* computing 3rd power */
	  d__1 = x[j] + tj + one, d__2 = d__1;
	  temp = d__2 * (d__1 * d__1);
	  sum2 += (one - tj) * temp;
	  /* l240: */
	}
    l250:
      fvec[k] = x[k] + h__ * ((one - tk) * sum1 + tk * sum2) / two;
      /* l260: */
    }
  return 0;
}

int minpack_jac_discrete_integral (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data)
{
  const double one = 1.,two = 2.,three = 3.;

  int fjac_dim1, i__1, i__2, j, k;
  double d__1, d__2, temp, h__, tj, tk;
  fjac_dim1 = *ldfjac;
  if ( *iflag == 0) return 0 ;
  --x;
  fjac -= fjac_dim1 + 1;

  i__1 = *n + 1;
  h__ = one / (double) i__1;
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      tk = (double) k *h__;
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  tj = (double) j *h__;
	  /* computing 2nd power */
	  d__1 = x[j] + tj + one;
	  temp = three * (d__1 * d__1);
	  /* computing min */
	  d__1 = tj * (one - tk), d__2 = tk * (one - tj);
	  fjac[k + j * fjac_dim1] = h__ * Min (d__1, d__2) * temp / two;
	  /* l330: */
	}
      fjac[k + k * fjac_dim1] += one;
      /* l340: */
    }
  return 0;

}


/*     trigonometric function. */

int minpack_trigonometric (const int *n, double *x, double *fvec, int *iflag, void *data)
{
  const double zero = 0.;
  int i__1, i__2;
  int  j, k;
  double sum;

  if ( *iflag == 0) return 0 ;
  --fvec;
  --x;

  sum = zero;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      fvec[j] = cos (x[j]);
      sum += fvec[j];
    }
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      i__2 = *n + k;
      fvec[k] = (double) i__2 - sin (x[k]) - sum - (double) k *fvec[k];
    }
  return 0;
}

int minpack_jac_trigonometric (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data)
{
  int fjac_dim1, i__1, i__2, j, k;
  double  temp;
  fjac_dim1 = *ldfjac;

  if ( *iflag == 0) return 0 ;
  --x;
  fjac -= fjac_dim1 + 1;

  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      temp = sin (x[j]);
      i__2 = *n;
      for (k = 1; k <= i__2; ++k)
	{
	  fjac[k + j * fjac_dim1] = temp;
	  /* l360: */
	}
      i__2 = j + 1;
      fjac[j + j * fjac_dim1] = (double) i__2 *temp - cos (x[j]);
      /* l370: */
    }
  return 0;

}

/*     variably dimensioned function. */

int minpack_variably_dimensioned(const int *n, double *x, double *fvec, int *iflag, void *data)
{
  const double zero = 0.;
  const double one = 1.;
  const double two = 2.;
  int i__1;
  double d__1;
  double temp;
  int  j, k;
  double sum;

  if ( *iflag == 0) return 0 ;
  --fvec;
  --x;

  sum = zero;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      sum += (double) j *(x[j] - one);
    }
  /* computing 2nd power */
  d__1 = sum;
  temp = sum * (one + two * (d__1 * d__1));
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      fvec[k] = x[k] - one + (double) k *temp;
    }
  return 0;
}


int minpack_jac_variably_dimensioned (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data)
{
  const double zero = 0.,one = 1.,six = 6.;
  int fjac_dim1, i__1, i__2, i__3, j, k;
  double d__1, temp, sum;
  fjac_dim1 = *ldfjac;

  if ( *iflag == 0) return 0 ;

  --x;
  fjac -= fjac_dim1 + 1;

  sum = zero;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      sum += (double) j *(x[j] - one);
      /* l390: */
    }
  /* computing 2nd power */
  d__1 = sum;
  temp = one + six * (d__1 * d__1);
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      i__2 = *n;
      for (j = k; j <= i__2; ++j)
	{
	  i__3 = k * j;
	  fjac[k + j * fjac_dim1] = (double) i__3 *temp;
	  fjac[j + k * fjac_dim1] = fjac[k + j * fjac_dim1];
	  /* l400: */
	}
      fjac[k + k * fjac_dim1] += one;
      /* l410: */
    }
  return 0;


}

/*     broyden tridiagonal function. */

int minpack_broyden_tridiagonal (const int *n, double *x, double *fvec, int *iflag, void *data)
{
  const double zero = 0.;
  const double one = 1.;
  const double two = 2.;
  const double three = 3.;
  int i__1;
  double temp, temp1, temp2;
  int k;

  if ( *iflag == 0) return 0 ;
  --fvec;
  --x;

  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      temp = (three - two * x[k]) * x[k];
      temp1 = zero;
      if (k != 1)
	{
	  temp1 = x[k - 1];
	}
      temp2 = zero;
      if (k != *n)
	{
	  temp2 = x[k + 1];
	}
      fvec[k] = temp - temp1 - two * temp2 + one;
    }
  return 0;
}

int minpack_jac_broyden_tridiagonal (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data)
{
  const double zero = 0.,one = 1.,two = 2.,three = 3.,four = 4.;
  int fjac_dim1, i__1, i__2, j, k;
  fjac_dim1 = *ldfjac;

  if ( *iflag == 0) return 0 ;
  --x;
  fjac -= fjac_dim1 + 1;

  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  fjac[k + j * fjac_dim1] = zero;
	  /* l430: */
	}
      fjac[k + k * fjac_dim1] = three - four * x[k];
      if (k != 1)
	{
	  fjac[k + (k - 1) * fjac_dim1] = -one;
	}
      if (k != *n)
	{
	  fjac[k + (k + 1) * fjac_dim1] = -two;
	}
      /* l440: */
    }
  return 0; 
}


/*     broyden banded function. */

int minpack_broyden_banded (const int *n, double *x, double *fvec, int *iflag, void *data )
{
  const double zero = 0.;
  const double one = 1.;
  const double two = 2.;
  const double five = 5.;

  int i__1, i__2, i__3;
  double d__1;
  double  temp;
  int  j, k, k1, k2, ml, mu;

  if ( *iflag == 0) return 0 ;
  --fvec;
  --x;

  ml = 5;
  mu = 1;
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      /* computing max */
      i__2 = 1, i__3 = k - ml;
      k1 = Max (i__2, i__3);
      /* computing min */
      i__2 = k + mu;
      k2 = Min (i__2, *n);
      temp = zero;
      i__2 = k2;
      for (j = k1; j <= i__2; ++j)
	{
	  if (j != k)
	    {
	      temp += x[j] * (one + x[j]);
	    }
	}
      /* computing 2nd power */
      d__1 = x[k];
      fvec[k] = x[k] * (two + five * (d__1 * d__1)) + one - temp;
    }
  return 0;
}	


int minpack_jac_broyden_banded (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data)
{
  const double zero = 0.,one = 1.,two = 2., fiftn=15.0;
  int fjac_dim1,  i__1, i__2, i__3, j, k, k1, k2, ml, mu;
  double d__1;
  fjac_dim1 = *ldfjac;

  if ( *iflag == 0) return 0 ;
  --x;
  fjac -= fjac_dim1 + 1;

  ml = 5;
  mu = 1;
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  fjac[k + j * fjac_dim1] = zero;
	  /* l460: */
	}
      /* computing max */
      i__2 = 1, i__3 = k - ml;
      k1 = Max (i__2, i__3);
      /* computing min */
      i__2 = k + mu;
      k2 = Min (i__2, *n);
      i__2 = k2;
      for (j = k1; j <= i__2; ++j)
	{
	  if (j != k)
	    {
	      fjac[k + j * fjac_dim1] = -(one + two * x[j]);
	    }
	  /* l470: */
	}
      /* computing 2nd power */
      d__1 = x[k];
      fjac[k + k * fjac_dim1] = two + fiftn * (d__1 * d__1);
      /* l480: */
    }
  return 0;
}


/*     subroutine initpt 
 *
 *     this subroutine specifies the standard starting points for 
 *     the functions defined by subroutine vecfcn. the subroutine 
 *     returns in x a multiple (factor) of the standard starting 
 *     point. for the sixth function the standard starting point is 
 *     zero, so in this case, if factor is not unity, then the 
 *     subroutine returns the vector  x(j) = factor, j=1,...,n. 
 *
 *     the subroutine statement is 
 *
 *       subroutine initpt(n,x,nprob,factor) 
 *
 *     where 
 *
 *       n is a positive int input variable. 
 *
 *       x is an output array of length n which contains the standard 
 *         starting point for problem nprob multiplied by factor. 
 *
 *       nprob is a positive int input variable which defines the 
 *         number of the problem. nprob must not exceed 14. 
 *
 *       factor is an input variable which specifies the multiple of 
 *         the standard starting point. if factor is unity, no 
 *         multiplication is performed. 
 *
 *     argonne national laboratory. minpack project. march 1980. 
 *     burton s. Garbow, kenneth e. Hillstrom, jorge j. More 
 */

int minpack_initpt (const int *n, double *x, int *nprob, double *factor)
{
  const double zero = 0.;
  const double half = .5;
  const double one = 1.;
  const double three = 3.;
  const double c1 = 1.2;
  int i__1;
  double h__;
  int j;
  double tj;

  --x;
  /*     selection of initial point. */

  switch (*nprob)
    {
    case 1:
      goto l10;
    case 2:
      goto l20;
    case 3:
      goto l30;
    case 4:
      goto l40;
    case 5:
      goto l50;
    case 6:
      goto l60;
    case 7:
      goto l80;
    case 8:
      goto l100;
    case 9:
      goto l120;
    case 10:
      goto l120;
    case 11:
      goto l140;
    case 12:
      goto l160;
    case 13:
      goto l180;
    case 14:
      goto l180;
    }

  /*     rosenbrock function. */

 l10:
  x[1] = -c1;
  x[2] = one;
  goto l200;

  /*     powell singular function. */

 l20:
  x[1] = three;
  x[2] = -one;
  x[3] = zero;
  x[4] = one;
  goto l200;

  /*     powell badly scaled function. */

 l30:
  x[1] = zero;
  x[2] = one;
  goto l200;

  /*     wood function. */

 l40:
  x[1] = -three;
  x[2] = -one;
  x[3] = -three;
  x[4] = -one;
  goto l200;

  /*     helical valley function. */

 l50:
  x[1] = -one;
  x[2] = zero;
  x[3] = zero;
  goto l200;

  /*     watson function. */

 l60:
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      x[j] = zero;
      /* l70: */
    }
  goto l200;

  /*     chebyquad function. */

 l80:
  i__1 = *n + 1;
  h__ = one / (double) i__1;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      x[j] = (double) j *h__;
      /* l90: */
    }
  goto l200;

  /*     brown almost-linear function. */

 l100:
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      x[j] = half;
      /* l110: */
    }
  goto l200;

  /*     discrete boundary value and integral equation functions. */

 l120:
  i__1 = *n + 1;
  h__ = one / (double) i__1;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      tj = (double) j *h__;
      x[j] = tj * (tj - one);
      /* l130: */
    }
  goto l200;

  /*     trigonometric function. */

 l140:
  h__ = one / (double) (*n);
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      x[j] = h__;
      /* l150: */
    }
  goto l200;

  /*     variably dimensioned function. */

 l160:
  h__ = one / (double) (*n);
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      x[j] = one - (double) j *h__;
      /* l170: */
    }
  goto l200;

  /*     broyden tridiagonal and banded functions. */

 l180:
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      x[j] = -one;
      /* l190: */
    }
 l200:

  /*     compute multiple of initial point. */

  if (*factor == one)
    {
      goto l250;
    }
  if (*nprob == 6)
    {
      goto l220;
    }
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      x[j] = *factor * x[j];
      /* l210: */
    }
  goto l240;
 l220:
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      x[j] = *factor;
      /* l230: */
    }
 l240:
 l250:
  return 0;
}	

/* Scilab default test 
 *
 *
 */



/*     broyden banded function. */

int minpack_fsol1(const int *n, double *x, double *fvec, int *iflag, void *data )
{
  static const double a[]={1.0,2.0,7.0, 8.0};
  static const double b[]={10.0 ,11.0};
  int i,j;
  if ( *iflag == 0) return 0 ;
  for ( i= 0 ; i< 2 ; i++) 
    {
      fvec[i]= b[i];
      for ( j= 0 ; j< 2 ; j++) 
	fvec[i] += a[i+2*(j)]*x[j];
    }
  return 0;
}	


int minpack_jac_fsol1(const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data)
{
  static const double a[]={1.0,2.0,7.0, 8.0};
  int i,j;
  if ( *iflag == 0) return 0 ;
  for ( i= 0 ; i< 2 ; i++) 
    for ( j= 0 ; j< 2 ; j++) 
      fjac[i+2*j] = a[i+2*(j)];
  return 0;
}
