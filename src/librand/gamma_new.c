/* Nsp
 * Copyright (C) 2006-2007 Bruno Pincon Esial/Iecn
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include "grand.h"
#include <math.h>

/**
 * SECTION:random
 * @Title: LibRand
 * @Short_Description: random numbers and vectors generation
 * @Stability_Level: 
 * @See_Also: 
 *
 * 
 */

/**
 * nsp_rand_gamma_init:
 * @a:  parameter of the gamma distribution
 * @G: a pointer to an allocated #GammaStruct
 * 
 * initialize the struct @G for random generation
 * with #nsp_rand_gamma 
 *
 * Returns %OK or %FAIL
 *
 **/
int nsp_rand_gamma_init(double a, GammaStruct *G)
{
  if ( ! ( a > 0.0 ) )
    return FAIL;

  G->a = a;
  if ( a < 1.0 ) a++;
  G->d = a - 0.333333333333333333;
  G->c = 1.0 / sqrt(9.0*G->d);

  return OK;
}

/**
 * nsp_rand_gamma:
 * @G: a pointer to an initialized #GammaStruct
 * 
 * generates a random number from G(a).@G must be
 * initilized with #nsp_rand_gamma_init. This routine 
 * must be used when several gamma deviates of same
 * fixed parameter are needed. Otherwise uses 
 * #nsp_rand_gamma_direct.
 *
 * method:see "Marsaglia G and WW Tsang, A Simple 
 * Method for Generating Gamma Variables", ACM 
 * Transactions on math. Software, Vol 26, No 3, 
 * Sept 2000, pages 363-372"
 *
 * Returns a double
 **/
double nsp_rand_gamma(GammaStruct *G)
{
  double x, x2, u, v, res;

  while ( 1 )
    {
      do
	{
	  x = nsp_rand_nor_core();
	  v = 1.0 + G->c*x;
	}
      while ( v <= 0.0 );
      v = v*v*v;

      u = rand_ranf();
      x2 = x*x;
      if ( u < 1.0 - 0.0331*x2*x2 )
	break;
      
      if ( log(u) < 0.5*x2 + G->d*(1.0 - v + log(v)) )
	break;
    }
  res = G->d * v;

  if ( G->a >= 1.0 )
    return res;
  else
    return res * exp(-nsp_rand_exp_core()/G->a);
}


/**
 * nsp_rand_gamma_direct:
 * @a: parameter of the gamma distribution
 * 
 * generates a random number from G(a). When several
 * random deviates from G(a) are needed it is faster 
 * to use #nsp_rand_gamma.
 *
 * method:see "Marsaglia G and WW Tsang, A Simple 
 * Method for Generating Gamma Variables", ACM 
 * Transactions on math. Software, Vol 26, No 3, 
 * Sept 2000, pages 363-372"
 *
 * Returns a double
 **/
double nsp_rand_gamma_direct(double a)
{
  double aa, d, c, x, x2, u, v;

  aa = a >= 1.0 ? a : a+1.0;

  d = aa - 0.333333333333333333;
  c = 1.0 / sqrt(9.0*d);

  while ( 1 )
    {
      do
	{
	  x = nsp_rand_nor_core();
	  v = 1.0 + c*x;
	}
      while ( v <= 0.0 );
      v = v*v*v;
      
      u = rand_ranf();
      x2 = x*x;
      if ( u < 1.0 - 0.0331*x2*x2 )
	break;

      if ( log(u) < 0.5*x2 + d*(1.0 - v + log(v)) )
	break;
    }

  if ( a >= 1.0 )
    return d*v;
  else
    return d*v*exp(-nsp_rand_exp_core()/a);
}


/**
 * nsp_rand_beta_init:
 * @a:  first parameter of the beta distribution
 * @b:  second parameter of the beta distribution
 * @B: a pointer to an allocated #BetaStruct
 * 
 * initialize the struct @B for random generation
 * with #nsp_rand_beta 
 *
 * Returns %OK or %FAIL
 *
 **/
int nsp_rand_beta_init(double a, double b, BetaStruct *B)
{
  if ( nsp_rand_gamma_init(a, &(B->a)) == FAIL )
    return FAIL;
  if ( nsp_rand_gamma_init(b, &(B->b)) == FAIL )
    return FAIL;
  return OK;
}


/**
 * nsp_rand_beta:
 * @B: a pointer to an initialized #BetaStruct
 * 
 * generates a random number from B(a,b).@B must be
 * initilized with nsp_rand_beta_init. This routine 
 * must be used when several gamma deviates with same
 * fixed parameters are needed. Otherwise uses 
 * #nsp_rand_beta_direct.
 *
 * method: see Knuth TAOCP vol 2, 2d ed, p. 129
 *
 * Returns a double
 **/
double nsp_rand_beta(BetaStruct *B)
{
  double x1, x2;
  x1 = nsp_rand_gamma(&(B->a));
  x2 = nsp_rand_gamma(&(B->b));
  return x1 / (x1 + x2);
}

/**
 * nsp_rand_beta_direct:
 * @a:  first parameter of the beta distribution
 * @b:  second parameter of the beta distribution
 * 
 * generates a random number from B(a,b). When several
 * random deviates from B(a,b) are needed, it is faster 
 * to use #nsp_rand_beta.
 *
 * method: see Knuth TAOCP vol 2, 2d ed, p. 129
 *
 * Returns a double
 **/
double nsp_rand_beta_direct(double a, double b)
{
  double x1, x2;
  x1 = nsp_rand_gamma_direct(a);
  x2 = nsp_rand_gamma_direct(b);
  return x1 / (x1 + x2);
}

/**
 * nsp_rand_chi2_init:
 * @nu: parameter of the chi2 distribution, number of df
 * @C: a pointer to an allocated #Chi2Struct
 * 
 * initialize the struct @C for random generation
 * with #nsp_rand_chi2
 *
 * Returns %OK or %FAIL
 *
 **/
int nsp_rand_chi2_init(double nu, Chi2Struct *C)
{
  C->nu = nu;
  return nsp_rand_gamma_init(0.5*nu, &(C->G));
}

/**
 * nsp_rand_chi2:
 * @C: a pointer to an initialized #Chi2Struct
 * 
 * generates a random number from chi square distribution.
 * @C must be initilized with #nsp_rand_chi2_init. This routine 
 * must be used when several chi2 deviates with the same
 * fixed parameter are needed. Otherwise uses 
 * #nsp_rand_chi2_direct.
 *
 * method: see Knuth TAOCP vol 2, 2d ed, p. 130
 *
 * Returns a double
 **/
double nsp_rand_chi2(Chi2Struct *C)
{
  return 2.0 * nsp_rand_gamma(&(C->G));
}

/**
 * nsp_rand_chi2_direct:
 * @nu: parameter of the chi2 distribution, number of df 
 * 
 * generates a random number from chi square distribution.
 * When several random deviates from chi2(nu) are needed, 
 * it is faster to use #nsp_rand_chi2.
 *
 * method: see Knuth TAOCP vol 2, 2d ed, p. 130
 *
 * Returns a double
 **/
double nsp_rand_chi2_direct(double nu)
{
  return 2.0 * nsp_rand_gamma_direct(0.5*nu);
}


/**
 * nsp_rand_ncchi2_init:
 * @nu: first parameter of the non central chi2 distribution
 * @xnonc: second parameter of the non central chi2 distribution
 * @C: a pointer to an allocated #NcChi2Struct
 * 
 * initialize the struct @C for random generation
 * with #nsp_rand_ncchi2
 *
 * Returns %OK or %FAIL
 *
 **/
int nsp_rand_ncchi2_init(double nu, double xnonc, NcChi2Struct *C)
{
  if ( ! ( nu >= 1.0 && xnonc >= 0 ) )
    return FAIL;
  C->nu = nu;
  C->xnonc = xnonc;
  C->sqrt_xnonc = sqrt(xnonc);
  if ( nu > 1.0 ) 
    return nsp_rand_gamma_init(0.5*(nu-1.0), &(C->G));
  else
    return OK;
}


/**
 * nsp_rand_ncchi2:
 * @C: a pointer to an initialized #NcChi2Struct
 * 
 * generates a random number from the non central chi square distribution.
 * @C must be initilized with #nsp_rand_ncchi2_init. This routine 
 * must be used when several non central chi2 deviates with the same
 * fixed parameter are needed. Otherwise uses 
 * #nsp_rand_ncchi2_direct.
 *
 * Returns a double
 **/
double nsp_rand_ncchi2(NcChi2Struct *C)
{
  double Y;
  Y = C->sqrt_xnonc + nsp_rand_nor_core();
  Y *= Y;
  if ( C->nu > 1.0 )
    Y += 2.0 * nsp_rand_gamma(&(C->G));
  return Y;
}

/**
 * nsp_rand_ncchi2_direct:
 * @nu: first parameter of the non central chi2 distribution
 * @xnonc: second parameter of the non central chi2 distribution
 * 
 * generates a random number from the non central chi square distribution.
 * When several non central chi2 deviates with the same
 * fixed parameter are needed, it is faster to use #nsp_rand_ncchi2.
 *
 * Returns a double
 **/
double nsp_rand_ncchi2_direct(double nu, double xnonc)
{
  double Y;
  Y = sqrt(xnonc) + nsp_rand_nor_core();
  Y *= Y;
  if ( nu > 1.0 )
    Y += 2.0 * nsp_rand_gamma_direct(0.5*(nu-1.0));
  return Y;
}


/**
 * nsp_rand_F_init:
 * @nu1: first parameter of the F distribution
 * @nu2: second parameter of the F distribution
 * @F: a pointer to an allocated #FStruct
 * 
 * initialize the struct @F for random generation
 * with #nsp_rand_F
 *
 * Returns %OK or %FAIL
 *
 **/
int nsp_rand_F_init(double nu1, double nu2, FStruct *F)
{
  F->nu1 = nu1;
  F->nu2 = nu2;
  if ( nsp_rand_gamma_init(0.5*nu1, &(F->G1)) == FAIL )
    return FAIL;
  if ( nsp_rand_gamma_init(0.5*nu2, &(F->G2)) == FAIL )
    return FAIL;
  return OK;
}

/**
 * nsp_rand_F:
 * @F: a pointer to an initialized #FStruct
 * 
 * generates a random number from the F (variance-ratio) distribution.
 * @F must be initilized with #nsp_rand_F_init. This routine 
 * must be used when several F deviates with the same
 * fixed parameters are needed. Otherwise uses 
 * #nsp_rand_F_direct.
 *
 * method: see Knuth TAOCP vol 2, 2d ed, p. 130
 *
 * Returns a double
 **/
double nsp_rand_F(FStruct *F)
{
  return F->nu2*nsp_rand_gamma(&(F->G1)) / (F->nu1*nsp_rand_gamma(&(F->G2)));
}

/**
 * nsp_rand_F_direct:
 * @nu1: first parameter of the F distribution
 * @nu2: second parameter of the F distribution
 * 
 * generates a random number from the F (variance-ratio) 
 * distribution of parameters nu1 and nu2..
 * When several random deviates from the F distribution
 * with the same fixed parameters are needed, 
 * it is faster to use #nsp_rand_F.
 *
 * method: see Knuth TAOCP vol 2, 2d ed, p. 130
 *
 * Returns a double
 **/
double nsp_rand_F_direct(double nu1, double nu2)
{
  return  nu2*nsp_rand_gamma_direct(0.5*nu1) / (nu1*nsp_rand_gamma_direct(0.5*nu2));
}


/**
 * nsp_rand_ncF_init:
 * @nu1: first parameter of the non central F distribution
 * @nu2: second parameter of the non central F distribution
 * @xnonc: third parameter of the non central F distribution
 * @E: a pointer to an allocated #NcFStruct
 * 
 * initialize the struct @E for random generation
 * with #nsp_rand_ncF
 *
 * Returns %OK or %FAIL
 *
 **/
int nsp_rand_ncF_init(double nu1, double nu2, double xnonc, NcFStruct *E)
{
  if ( nsp_rand_ncchi2_init(nu1, xnonc, &(E->X)) == FAIL )
    return FAIL;
  if ( nsp_rand_chi2_init(nu2, &(E->Y)) == FAIL )
    return FAIL;
  return OK;
}

/**
 * nsp_rand_ncF:
 * @E: a pointer to an initialized #NcFStruct
 * 
 * generates a random number from the non central F (variance-ratio) distribution.
 * @E must be initilized with #nsp_rand_ncF_init. This routine 
 * must be used when several ncF deviates with the same
 * fixed parameters are needed. Otherwise uses 
 * #nsp_rand_ncF_direct.
 *
 * method: uses  (X/nu1)/(Y/nu2) where X is a non central 
 * chi2 variate (nu1 dof and xnonc) and Y a chi2 variate (nu2 dof)
 *
 * Returns a double
 **/
double nsp_rand_ncF(NcFStruct *E)
{
  return (E->Y.nu)*nsp_rand_ncchi2(&(E->X)) / ((E->X.nu)*nsp_rand_chi2(&(E->Y)));
}

/**
 * nsp_rand_ncF_direct:
 * @nu1: first parameter of the non central F distribution
 * @nu2: second parameter of the non central F distribution
 * 
 * generates a random number from the non central F (variance-ratio) 
 * distribution of parameters nu1, nu2 and xnonc
 * When several random deviates from the ncF distribution
 * with the same fixed parameters are needed, 
 * it is faster to use #nsp_rand_ncF.
 *
 * method: uses  (X/nu1)/(Y/nu2) where X is a non central 
 * chi2 variate (nu1 dof and xnonc) and Y a chi2 variate (nu2 dof)
 *
 * Returns a double
 **/
double nsp_rand_ncF_direct(double nu1, double nu2, double xnonc)
{
  return  nu2*nsp_rand_ncchi2_direct(nu1,xnonc) / (nu1*nsp_rand_chi2_direct(nu2));
}


/**
 * nsp_rand_nbn_init:
 * @n: first parameter of the distribution
 * @p: second parameter of the distribution
 * @N: a pointer to an allocated #NbnStruct
 * 
 * initialize the struct @N for random generation
 * with #nsp_rand_nbn
 *
 * Returns %OK or %FAIL
 *
 **/
int nsp_rand_nbn_init(int n, double p, NbnStruct *N)
{
  if ( ! ( 0 < n  &&  0.0 < p  &&  p <= 1.0 ) )
    return FAIL;
  N->n = n;
  N->p = p;
  N->coef = (1.0 - p)/p;
  return nsp_rand_gamma_init((double) n, &(N->G));
}

/**
 * nsp_rand_nbn:
 * @N: a pointer to an initialized #NbnStruct
 * 
 * generates a random number from the negative binomial distribution.
 * @N must be initilized with #nsp_rand_nbn_init. This routine 
 * must be used when several Nbn(n,p) deviates with the same
 * fixed parameters n and p are needed. Otherwise uses 
 * #nsp_rand_nbn_direct.
 *
 * method: Algorithm from page 489 of Luc Devroye,
 * Non-Uniform Random Variate Generation.  Springer-Verlag,
 * New York, 1986.
 * (available at the Luc Devroye 's home page :
 * http://cg.scs.carleton.ca/~luc/rnbookindex.html)
 *
 * Returns an int
 **/
int nsp_rand_nbn(NbnStruct *N)
{
  return nsp_rand_poisson_direct( N->coef * nsp_rand_gamma(&(N->G)) );
}

/**
 * nsp_rand_nbn_direct:
 * @n: first parameter of the negative binomial distribution
 * @p: second parameter of the distribution
 * 
 * generates a random number from the negative binomial distribution.
 * When several Nbn(n,p) random deviates with the same fixed parameters 
 * n and p are needed, it is faster to use #nsp_rand_nbn.
 *
 * method: Algorithm from page 489 of Luc Devroye,
 * Non-Uniform Random Variate Generation.  Springer-Verlag,
 * New York, 1986.
 * (available at the Luc Devroye 's home page :
 * http://cg.scs.carleton.ca/~luc/rnbookindex.html)
 *
 * Returns an int
 **/
int nsp_rand_nbn_direct(int n, double p)
{
  double Y;
  Y = nsp_rand_gamma_direct((double) n)*(1.0-p)/p;
  return nsp_rand_poisson_direct(Y);
}



/**
 * nsp_rand_ndgauss:
 * @Mean: (input) a vector with @n components
 * @C: (input) @nx@n matrix (lower triangular matrix of the Cholesky
 *             factorization of the covariance matrix)
 * @res: (output) random vector generated
 * @n: dimension of the nd gaussian distribution
 *
 * n-dim gaussian normal distribution generator N(Mean,Cov).
 * @C is obtained from the covariance matrix Cov with a call
 * to the lapack routine dpotrf.
 *
 * Algorithm :  res = C * x + Mean  where x is a vector
 *              with n independant samples from N(0,1)
 * 
 **/
void nsp_rand_ndgauss(double *Mean, double *C, double *res, int n)
{
  int i, j;
  double *col, u;

  /*  init res with Mean */
  for ( i = 0 ; i < n ; i++ )
     res[i] = Mean[i];

  /*  res += C * x  (at each j iteration, col point to 
   *  the beginning of the jth column of C)
   */
  for ( j = 0, col = C ; j < n ; j++, col+=n )
    {
      u = nsp_rand_nor_core();
      for ( i = j ; i < n ; i++ )
	res[i] += col[i]*u;
    }
}

/**
 * nsp_rand_sphere:
 * @res: (output) random vector generated
 * @n: space dimension of the unit hyper-sphere
 *
 * provide random vectors uniformly distributed on the
 * unit sphere
 *
 * method: see Knuth TAOCP vol 2, 2d ed, p. 130
 * 
 **/
void nsp_rand_sphere(double *res, int n)
{
  int i;
  double x, r = 0.0;

  for ( i = 0 ; i < n ; i++ )
    {
      x = nsp_rand_nor_core();
      res[i] =  x;
      r += x*x;
    }
  r = sqrt(r);

  for ( i = 0 ; i < n ; i++ )
    res[i] /= r;
}


/**
 * nsp_rand_geom_init:
 * @p: parameter of the distribution
 * @G: a pointer to an allocated #GeomStruct
 * 
 * initialize the struct @G for random generation
 * with #nsp_rand_geom
 *
 * Returns %OK or %FAIL
 *
 **/
int nsp_rand_geom_init(double p, GeomStruct *G)
{
  if ( ! (1.3e-307 <= p && p <= 1 ) )
    return FAIL;
  G->p = p;
  G->inv_ln_1_m_p = 1.0/nsp_log1p(-p);
  return OK;
}
 
/**
 * nsp_rand_geom:
 * @G: a pointer to an initialized #GeomStruct
 * 
 * generates a random number from the geometric distribution.
 * @G must be initilized with #nsp_rand_geom_init. This routine 
 * must be used when several G(p) deviates with the same
 * fixed parameter p are needed. Otherwise uses 
 * #nsp_rand_geom_direct.
 *
 * method: 
 *     inversion of the cdf leads to :
 *
 *     X = 1 + floor(log(u)/log(1-p))
 *
 *     u being a random deviate from U[0,1). Taking into account 
 *     that e = -log(u) as exponential distribution, we use finally :
 *                     
 *     X = 1 + floor(-e/log(1-p))
 *
 *     with e an exponential random variate
 *
 * Returns an unsigned int
 **/
unsigned int nsp_rand_geom(GeomStruct *G)
{
  if ( G->p == 1.0 )
    return 1;
  else
    return (unsigned int) (1.0 - G->inv_ln_1_m_p * nsp_rand_exp_core());
}

/**
 * nsp_rand_geom_direct:
 * @p: parameter of the distribution
 * 
 * generates a random number from the geometric distribution.
 * When several G(p) random deviates with the same fixed parameter 
 * p are needed, it is faster to use #nsp_rand_geom.
 *
 * method: see #nsp_rand_geom
 *
 * Returns an unsigned int
 **/
 
unsigned int nsp_rand_geom_direct(double p)
{
  if ( p == 1.0 )
    return 1;
  else
    return (unsigned int) (1.0 -  nsp_rand_exp_core()/nsp_log1p(-p));
}

