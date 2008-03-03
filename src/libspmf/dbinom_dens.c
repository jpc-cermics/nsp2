/*
 * The author of this software is Catherine Loader, c@herine.net
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose without fee is hereby granted, with the exceptions noted below,
 * and provided that this entire notice is included in all copies of any
 * software which is or includes a copy or modification of this software
 * and in all copies of the supporting documentation for such software.
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, NEITHER THE AUTHOR NOR LUCENT TECHNOLOGIES
 * MAKE ANY REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE
 * MERCHANTABILITY OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/*
 * modifs (Bruno Pincon) for inclusion in nsp: 
 * - this file contains the original dens.c file
 *   of Catherine Loader 's dbinom package.
 * - add a nsp_pdf_ prefix for the names
 * - the dbinom.h header file is in the nsp/spmf.h
 * - move to ANSI C declarations
 */

#include <nsp/math.h> 
#include <nsp/spmf.h>


/*
 * Beta density,
 *               (a+b-1)!     a-1       b-1
 *  p(x;a,b) = ------------ x     (1-x)
 *             (a-1)!(b-1)!
 *
 *           = (a+b-1) dbinom(a-1; a+b-2,x)
 *
 *  We must modify this when a<1 or b<1, to avoid passing negative
 *  arguments to dbinom_raw. Note that the modifications require
 *  division by x and/or 1-x, so cannot be used except where necessary.
 *
 *  a and b should be > 0, this is verified by the nsp interface
 */

double nsp_pdf_beta(double x, NTYPE a, NTYPE b, int give_log)
{ 
  double f, p;

  /* NaNs propagated correctly */
  if (ISNAN(x)) return x;

  if ((x<=0) | (x>=1)) return 0.0;

  if (a<1)
  { if (b<1)                                    /* a<1, b<1 */
    { f = a*b/((a+b)*x*(1-x));
      p = nsp_dbinom_raw(a,a+b,x,1-x,give_log);
    }
    else                                        /* a<1, b>=1 */
    { f = a/x;
      p = nsp_dbinom_raw(a,a+b-1,x,1-x,give_log);
    }
  }
  else
  { if (b<1)                                    /* a>=1, b<1 */
    { f = b/(1-x);
      p = nsp_dbinom_raw(a-1,a+b-1,x,1-x,give_log);
    }
    else                                        /* a>=1, b>=1 */
    { f = a+b-1;
      p = nsp_dbinom_raw(a-1,(a-1)+(b-1),x,1-x,give_log);
    }
  }

  return( (give_log) ? p + log(f) : p*f );
  
}

/*
 * Binomial probability. Call dbinom_raw() directly after argument checks.
 *
 *   p should be in [0,1] and n should be a not negative integer
 *   this is verified by the nsp interface
 */

double nsp_pdf_binom(NTYPE x, NTYPE n, double p, int give_log)
{ 
  if (ISNAN(x)) return x;

  if ( x < 0.0 || x > n || x != floor(x) ) return 0.0;
  
  return( nsp_dbinom_raw(x,n,p,1-p,give_log) );
}

/*
 *   F density with m (numerator) and n (denominator) degrees of freedom.
 *   The density can be written as a Binomial probability
 *   with p = x*m/(n+x*m). For m>=2, we use the simplest conversion.
 *   For m<2, (m-2)/2<0 so the conversion will not work, and we must use
 *   a second conversion. Note the division by p; this seems unavoidable
 *   for m < 2, since the F density has a singularity as x (or p) -> 0.
 *
 *   m and n should be > 0
 *   this is verified by the nsp interface
 *
 */

double nsp_pdf_f(double x, NTYPE m, NTYPE n, int give_log)
{ 
  double p, q, f, dens;

  if (ISNAN(x)) return x;

  if (x <= 0.0) return 0.0;

  f = 1.0/(n+x*m);
  q = n*f;
  p = x*m*f;

  if (m>=2)
    { 
      f = m*q/2;
      dens = nsp_dbinom_raw((m-2)/2, (m+n-2)/2, p, q, give_log);
    }
  else
    { 
      f = m*m*q / (2*p*(m+n));
      dens = nsp_dbinom_raw(m/2, (m+n)/2, p, q, give_log);
    }

  return((give_log) ? log(f)+dens : f*dens);
}

/*
 * Gamma density,
 *                  lb^r x^{r-1} exp(-lb*x)
 *      p(x;r,lb) = -----------------------
 *                          (r-1)!
 *
 * If USE_SCALE is defined below, the lb argument will be interpreted
 * as a scale parameter (i.e. replace lb by 1/lb above). Otherwise,
 * it is interpreted as a rate parameter, as above.
 *
 * the density is easily converted to a poisson probability.
 *
 * r must be > 0 and lambda >= 0
 * this is verified by the nsp interface
 */

/* #define USE_SCALE comment this because nsp uses rate for gamma and not scale */

double nsp_pdf_gamma(double x, NTYPE r, double lambda, int give_log)
{ 
  double pr;

  if (ISNAN(x)) return x;

  if ( x <= 0.0 ) return 0.0;

#ifdef USE_SCALE
  lambda = 1.0/lambda;
#endif

  if (r<1)
    { 
      pr = nsp_dpois_raw(r,lambda*x,give_log);
      return( (give_log) ?  pr + log(r/x) : pr*r/x );
    }

  pr = nsp_dpois_raw(r-1,lambda*x,give_log);
  return( (give_log) ? pr + log(lambda) : lambda*pr);
}


/*
 * Hypergeometric probability.
 * Given a sequence of r successes and b failures, we sample n (\le b+r)
 * items without replacement. The hypergeometric probability is the
 * probability of x successes:
 *
 *                dbinom(x,r,p) * dbinom(n-x,b,p)
 *   p(x;r,b,n) = ---------------------------------
 *                          dbinom(n,r+b,p)
 *
 * for any p. For numerical stability, take p=n/(r+b); with this choice,
 * the denominator is not exponentially small.
 */

double nsp_pdf_hyper(NTYPE x, NTYPE r, NTYPE b, NTYPE n, int give_log)
{ 
  double p, q, p1, p2, p3;

#ifdef IEEE_754
  if (ISNAN(x) || ISNAN(r) || ISNAN(b) || ISNAN(n))
    return x + r + b + n;
#endif

  if (NOT_NNEG_INT(r) | NOT_NNEG_INT(b) | NOT_NNEG_INT(n) | (n>r+b))
    return( INVALID_PARAMS );

  if (NOT_NNEG_INT(x)) return(D_0);
  x = FORCE_INT(x);
  r = FORCE_INT(r);
  b = FORCE_INT(b);
  n = FORCE_INT(n);

  if (n==0) return((x==0) ? D_1 : D_0);

  p = ((double)n)/((double)(r+b));
  q = ((double)(r+b-n))/((double)(r+b));

  p1 = nsp_dbinom_raw(x,r,p,q,give_log);
  p2 = nsp_dbinom_raw(n-x,b,p,q,give_log);
  p3 = nsp_dbinom_raw(n,r+b,p,q,give_log);

  return( (give_log) ? p1 + p2 - p3 : p1*p2/p3 );
}

/*
 * Negative binomial probability: the probability of obtaining x failures
 * before the nth success in a sequence of Bernoulli trials.
 *
 * The negative binomial distribution is well defined for non-integer n,
 * and this can be useful for e.g. overdispersed discrete survival times.
 *
 * n should be > 0 and p in [0,1]
 * this is verified by the nsp interface
 *
 */

double nsp_pdf_nbinom(NTYPE x, NTYPE n, double p, int give_log)
{ 
  double prob, f;

  if (ISNAN(x)) return x;

  if ( x != floor(x) ) return 0.0;
  
  prob = nsp_dbinom_raw(n,x+n,p,1-p,give_log);
  f = ((double)n)/((double)(n+x));

  return((give_log) ? log(f) + prob : f*prob);
}

/*
 * Poisson probability  lb^x exp(-lb) / x!.
 * directly call dpois_raw after argument checks.
 *
 * lambda should be >= 0
 * this is verified by the nsp interface
 */
double nsp_pdf_pois(NTYPE x, double lambda, int give_log)
{
  /* check NaN's */
  if (ISNAN(x)) return x;

  if ( x != floor(x) ) return 0.0;
  
  return( nsp_dpois_raw(x,lambda,give_log) );
}

/*
 * t density.
 */
double nsp_pdf_t(double x, NTYPE df, int give_log)
{ 
  double t, u, f;
#ifdef IEEE_754
  if (ISNAN(x) || ISNAN(df))
    return x + df;
#endif

  if (df<=0.0) return(INVALID_PARAMS);

  t = -nsp_bd0(df/2.0,(df+1)/2.0) + STIRLERR((df+1)/2.0) - STIRLERR(df/2.0);

  if (x*x>df)
    u = log( 1+ x*x/df ) * df/2;
  else
    u = -nsp_bd0(df/2.0,(df+x*x)/2.0) + x*x/2.0;

  f = PIx2*(1+x*x/df);

  return( FEXP(f,t-u) );
}
