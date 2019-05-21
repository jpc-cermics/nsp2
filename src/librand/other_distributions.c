/* Nsp
 * Copyright (C) 2006-2019 Bruno Pincon Esial/Iecn
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
#include <nsp/object.h> 
#include <nsp/smatrix.h> 
#include <nsp/matrix.h> 
#include <nsp/matint.h> 
#include "nsp/gsort-p.h"
#include "nsp/spmf.h"
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
 * Returns: %OK or %FAIL
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
 * Returns: a double
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
 * Returns: a double
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
 * Returns: %OK or %FAIL
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
 * Returns: a double
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
 * Returns: a double
 **/
double nsp_rand_beta_direct(double a, double b)
{
  double x1, x2;
  x1 = nsp_rand_gamma_direct(a);
  x2 = nsp_rand_gamma_direct(b);
  return x1 / (x1 + x2);
}

/**
 * nsp_rand_t:
 * @nu:  parameter of the t distribution
 * @delta: non centrality parameter (0 for usual student distribution)
 * 
 * generates a random number from t(nu,delta).
 *
 * method: page 15 of Luc Devroye,
 * Non-Uniform Random Variate Generation.  Springer-Verlag,
 * New York, 1986.
 * (available at the Luc Devroye 's home page :
 * http://cg.scs.carleton.ca/~luc/rnbookindex.html)
 *
 * Returns: a double
 **/
double nsp_rand_t(double nu, double delta)
{
  double x, y;
  x = nsp_rand_nor_core() + delta;
  y = 2.0*nsp_rand_gamma_direct(0.5*nu);
  return x / sqrt( y / nu );
}

/**
 * nsp_rand_chi2_init:
 * @nu: parameter of the chi2 distribution, number of df
 * @C: a pointer to an allocated #Chi2Struct
 * 
 * initialize the struct @C for random generation
 * with #nsp_rand_chi2
 *
 * Returns: %OK or %FAIL
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
 * Returns: a double
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
 * Returns: a double
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
 * Returns: %OK or %FAIL
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
 * Returns: a double
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
 * Returns: a double
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
 * Returns: %OK or %FAIL
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
 * Returns: a double
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
 * Returns: a double
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
 * Returns: %OK or %FAIL
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
 * Returns: a double
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
 * Returns: a double
 **/
double nsp_rand_ncF_direct(double nu1, double nu2, double xnonc)
{
  return  nu2*nsp_rand_ncchi2_direct(nu1,xnonc) / (nu1*nsp_rand_chi2_direct(nu2));
}


/**
 * nsp_rand_nbn_init:
 * @r: first parameter of the distribution
 * @p: second parameter of the distribution
 * @N: a pointer to an allocated #NbnStruct
 * 
 * initialize the struct @N for random generation
 * with #nsp_rand_nbn
 *
 * Returns: %OK or %FAIL
 *
 **/
int nsp_rand_nbn_init(double r, double p, NbnStruct *N)
{
  if ( ! ( 0 < r  &&  0.0 < p  &&  p <= 1.0 ) )
    return FAIL;
  N->r = r;
  N->p = p;
  N->coef = (1.0 - p)/p;
  return nsp_rand_gamma_init(r, &(N->G));
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
 * Returns: an int
 **/
int nsp_rand_nbn(NbnStruct *N)
{
  return nsp_rand_poisson_direct( N->coef * nsp_rand_gamma(&(N->G)) );
}

/**
 * nsp_rand_nbn_direct:
 * @r: first parameter of the negative binomial distribution
 * @p: second parameter of the distribution
 * 
 * generates a random number from the negative binomial distribution.
 * When several Nbn(r,p) random deviates with the same fixed parameters 
 * r and p are needed, it is faster to use #nsp_rand_nbn.
 *
 * method: Algorithm from page 489 of Luc Devroye,
 * Non-Uniform Random Variate Generation.  Springer-Verlag,
 * New York, 1986.
 * (available at the Luc Devroye 's home page :
 * http://cg.scs.carleton.ca/~luc/rnbookindex.html)
 *
 * Returns: an int
 **/
int nsp_rand_nbn_direct(double r, double p)
{
  double Y;
  Y = nsp_rand_gamma_direct(r)*(1.0-p)/p;
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
 * @n: space dimension
 *
 * provide random vectors uniformly distributed on the
 * unit sphere of R^n
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
 * nsp_rand_in_sphere:
 * @res: (output) random vector generated
 * @n: space dimension
 *
 * provide random vectors uniformly distributed inside the
 * unit sphere of R^n
 *
 * method: rejection for n = 2 or 3 else choose a point on the
 * sphere (nsp_rand_sphere) and multiply it by U^(1/n)
 * (see Knuth TAOCP vol 2, 2d ed, p. 131).
 * 
 **/
void nsp_rand_in_sphere(double *res, int n)
{
  if ( n == 1 )
    res[0] = 2*(rand_ranf()-0.5);
  else if ( n == 2 )
    {
      double x, y;
      do
	{
	  x =  2*(rand_ranf()-0.5);
	  y =  2*(rand_ranf()-0.5);
	  /* the squeeze "z=|x|+|y|; accept if z <= 1, reject if z > sqrt(2)" don't accelerate too much */
	}
      while ( x*x + y*y > 1.0 );
      res[0] = x; res[1] = y;
    }
  else if ( n == 3 )
    {
      double x, y, z;
      do
	{
	  x =  2*(rand_ranf()-0.5);
	  y =  2*(rand_ranf()-0.5);
	  z =  2*(rand_ranf()-0.5);
	}
      while ( x*x + y*y + z*z > 1.0 );
      res[0] = x; res[1] = y; res[2] = z;
    }
  else
    {
      int i;
      double coef;
      nsp_rand_sphere(res, n);
      coef = pow(rand_ranf(),1.0/n);
      for ( i = 0 ; i < n ; i++ )
	res[i] *= coef;
    }
}

/**
 * nsp_rand_simplex:
 * @res: (output) random vectors generated (array of size m x n, must be pre-allocated)
 * @m: space dimension of underlying space
 * @n: number of random vectors to generate
 *
 * provide n random vectors uniformly distributed on the
 * simplex  { (x_1,...,x_m): x_i >=0, sum_i x_i = 1 }
 *
 * method: see chap 5 (p 207) of Luc Devroye 's book, "Non-Uniform 
 * Random Variate Generation".  Springer-Verlag, New York, 1986.
 * (available at the Luc Devroye 's home page :
 * http://cg.scs.carleton.ca/~luc/rnbookindex.html)
 * 
 **/
void nsp_rand_simplex(double *res, int m, int n)
{
  int i,j,k;
  double temp;
  
  for ( j = 0, k = 0 ; j < n ; j++, k+=m )
    {
      for ( i = 0 ; i < m ; i++ )
	res[k+i] = rand_ranf();
      /* sort the array */
      
      if ( m == 2 ) 
	{
	  if ( res[k] > res[k+1] ) { temp = res[k]; res[k] = res[k+1]; res[k+1] = temp; }
	}	
      else
	nsp_qsort_bp_double( res+k, m, NULL, 0, 'i');

      for ( i = m-1; i > 0 ; i-- )
	res[k+i] -= res[k+i-1]; 
    }
}

/**
 * nsp_rand_geom_init:
 * @p: parameter of the distribution
 * @G: a pointer to an allocated #GeomStruct
 * 
 * initialize the struct @G for random generation
 * with #nsp_rand_geom
 *
 * Returns: %OK or %FAIL
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
 * Returns: an unsigned int
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
 * Returns: an unsigned int
 **/
 
unsigned int nsp_rand_geom_direct(double p)
{
  if ( p == 1.0 )
    return 1;
  else
    return (unsigned int) (1.0 -  nsp_rand_exp_core()/nsp_log1p(-p));
}


/**
 * nsp_rand_multinomial:
 * @p: (input) probability vector (p[i] = probability to fall to category i)
 * @ix: (output) output random vector (ix[i] = number of events which have fall
 *      in category i)
 * @ncat: (input) number of categories
 * @n: (input) number of event to class in ncat categories.
 *  
 * generates a random vector from the multinomial distribution.
 *
 * method: algorithm chap 11 (p 559) of Luc Devroye 's book, "Non-Uniform 
 * Random Variate Generation".  Springer-Verlag, New York, 1986.
 * (available at the Luc Devroye 's home page :
 * http://cg.scs.carleton.ca/~luc/rnbookindex.html)
 *
 * When n is not enough bigger than ncat (says roughly n < 5 ncat) 
 * #nsp_rand_multinomial_bis could be faster
 *
 **/
void nsp_rand_multinomial(double *p, int *ix, int ncat, int n)
{
  int i, j;
  double ptot = 1;
  for ( i = 0 ; i < ncat-1 ; i++ )
    {
      ix[i] = nsp_rand_binomial_direct(n,p[i]/ptot);
      n -= ix[i];
      if ( n == 0 )
	{
	  for ( j = i+1 ; j <= ncat-1 ; j++ ) ix[j] = 0;
	  return;
	}
      ptot -= p[i];
    }
  ix[ncat-1] = n;
}

/**
 * nsp_rand_multinomial_bis:
 * @q: (input) double array
 * @key: (input) int array
 * @ix: (output) output random vector (ix[i] = number of events which have fall
 *      in category i)
 * @ncat: (input) number of categories
 * @n: (input) number of event to class in ncat categories.
 *  
 * generates a random vector from the multinomial distribution.
 * @q and @key must be initialized with #nsp_guide_table_method 
 * or #nsp_guide_table_method_bis from the probability vector.
 *
 * To use when n is small comparing to ncat (roughly n < 5 ncat) 
 * else #nsp_rand_multinomial is faster.
 *
 **/
void nsp_rand_multinomial_bis(double *q, int *key, int *ix, int ncat, int n)
{
  int i;
  for ( i = 0 ; i < ncat ; i++ )
    ix[i] = 0;

  for ( i = 0 ; i < n ; i++ )
    ix[nsp_rand_discrete_guide(q, key, ncat)]++;
}



/**
 * nsp_markov_setup
 * @p: (input) array of double of size n*n
 * @q: (output) array of doubles of size (n+1)*(n+1) (must be pre-allocated)
 * @key: (output) array of int of size n*n (must be pre-allocated)
 * @n: (input) number of states of the Markov chain.
 * 
 * initialization routine for random generation of Markov chains with
 * #nsp_rand_markov. p(i,j) = p(i * n*j) is the transition probability 
 * from state i to state j.
 *
 * Returns: %OK or %FAIL
 *
 **/
int nsp_markov_setup(double *p, double *q, int *key, int n)
{
  int i;
  for ( i = 0 ; i < n ; i++ )
    if ( nsp_guide_table_method(&(p[i]), n, &(q[(n+1)*i]), &(key[n*i]), n) == FAIL )
      return FAIL;
  return OK;
}


/**
 * nsp_rand_markov
 * @q: (input) array of doubles of size (n+1)*(n+1)
 * @key: (intput) array of int of size n*n
 * @X0: (input) array of size X0mn, must be integer values in [1,n]
 * @X: (output) array of double of size X0mn * m (must be pre-allocated)
 * @n: (input) number of states of the Markov chain.
 * @X0mn: (input) size of @X0
 * @m: (input) number of steps in the markov process
 * 
 * random generation of a Markov chain. X0 is the vector of initial states
 * Arrays @q and @key must initialized with #nsp_markov_setup from the
 * probability matrix of the Markov chain.
 *
 **/
void nsp_rand_markov(double *q, int *key, double *X0, double *X, int n, int X0mn, int m)
{
  int i, j, k, icur;

  for ( i = 0 ; i < X0mn ; i++ )
    {
      icur = (int) X0[i] - 1;
      for ( j = 0, k = i ; j < m ; j++, k+=X0mn )
	{
	  icur = nsp_rand_discrete_guide(&q[(n+1)*icur], &key[n*icur], n);
	  X[k] = (double) (icur + 1);
	}
    }
}
	 
/**
 * nsp_rand_prm_array:
 * @Objarray:  array of any kind (implement matint interface)
 *
 * permute each column of @Objarray with a random permutation .
 *
 **/
#define PERMUTE_ARRAY()			   \
  for ( j = 0 ; j < n ; j++, A+=m )	   \
    for ( i = 0 ; i < m-1 ; i++ )          \
	  {                                \
	    iwhich = rand_ignuin (i, m-1); \
	    elt = A[iwhich];               \
	    A[iwhich] = A[i];              \
	    A[i] = elt;                    \
	  }                                

int nsp_rand_prm_array(NspObject *Objarray)
{
  NspSMatrix *array = (NspSMatrix *)Objarray;
  NspTypeBase *type;
  int m = array->m, n = array->n, i, j, iwhich;
  unsigned int elt_size; /* size in number of bytes */

  type = check_implements(Objarray, nsp_type_matint_id);
  MAT_INT(type)->canonic(Objarray);
  elt_size = MAT_INT(type)->elt_size(Objarray);

  if ( elt_size == sizeof(double) )
    {
      double elt;
      double *A = (double *) array->S;
      PERMUTE_ARRAY()
    }
  else if ( elt_size == sizeof(doubleC) )
    {
      doubleC elt;
      doubleC *A = (doubleC *) array->S;
      PERMUTE_ARRAY()
    }
  else if ( elt_size == sizeof(int) )
    {
      int elt;
      int *A = (int *) array->S;
      PERMUTE_ARRAY()
    }
  else if ( elt_size == sizeof(short) )
    {
      short elt;
      short *A = (short *) array->S;
      PERMUTE_ARRAY()
    }
  else if ( elt_size == sizeof(char) )
    {
      char elt;
      char *A = (char *) array->S;
      PERMUTE_ARRAY()
    }
  else
    {
      char *elt, *A=(char *) array->S;
      if ( (elt = malloc(elt_size*sizeof(char))) == NULL )
	return FAIL;
      
      for ( j = 0 ; j < n ; j++ , A+=m*elt_size )
	for ( i = 0 ; i < m-1 ; ++i )
	  {                          
	    iwhich = rand_ignuin (i, m-1);
	    memcpy(elt,A+elt_size*iwhich,elt_size);          /* elt = A[iwhich]; */
	    memcpy(A+elt_size*iwhich,A+elt_size*i,elt_size); /* A[iwhich] = A[i]; */
	    memcpy(A+elt_size*i,elt,elt_size);               /* A[i] = elt; */
	  }
    }
  return OK;
}
	 
	 
/**
 * nsp_rand_prm:
 * @array:  array of int
 * @n: length of array
 * @base: an int
 *
 * fills @array with a random permutation of [base,base+n-1] 
 *
 **/
void nsp_rand_prm (int *array, int n, int base)
{
  int i, iwhich, elt;
  for ( i = 0 ; i < n ; i++ ) 
    array[i] = base + i;
  for ( i = 0 ; i < n-1 ; i++ )
    {
      iwhich = rand_ignuin (i, n-1);
      elt = array[iwhich];
      array[iwhich] = array[i];
      array[i] = elt;
    }
}


/**
 * nsp_rand_smpl:
 * @p:  array of int of length @n
 * @n: sample size
 * @N: set size
 * @base: an int (see after)
 * @head, @next: int work arrays of size n
 *
 * choose a uniform sample of n elements in [base, base+N-1].
 * n must be <= N, typically we have n << N but when
 * n is large enough (says n > N/35 on my machine)
 * #nsp_rand_smpl_bis should be used instead).
 *
 * method: algo p 616 (chapter 12) of Luc Devroye 's book, 
 * "Non-Uniform Random Variate Generation". Springer-Verlag, 
 * New York, 1986. (available at the Luc Devroye 's home page: 
 * http://cg.scs.carleton.ca/~luc/rnbookindex.html)
 * 
 **/
void nsp_rand_smpl(int *p, int n, int N, int base, int *head, int *next)
{
  int i, k, bucket, Top, TopStar;

  /* init bucket search struct */
  for ( i = 0 ; i < n ; i++ ) { head[i]=-1; next[i]=-1;}

  /* fill the array p */
  for ( i = 0 ; i < n ; i++ )
    {
      do 
	{
	  k = rand_ignuin (0, N-1);
	  /* bucket search */
	  bucket = (int) ((double) n*k / (double) N);
	  Top = head[bucket];
	  if ( Top == -1 )   /* bucket is empty */
	    {
	      head[bucket] = i; p[i] = k;
	    }
	  else               /* bucket not empty, look if it already contains k or not */
	    {
	      while ( p[Top] != k  &&  Top != -1 )
		{
		  TopStar = Top; Top = next[Top]; 
		}
	      if ( Top == -1 ) /* the bucklet don't contains k */
		{
		  next[TopStar] = i; p[i] = k;
		}
	    }
	}
      while ( Top != -1 );
    }

  /* add base... */
  if ( base != 0 )
    for ( i = 0 ; i < n ; i++ ) p[i] += base;
}

/**
 * nsp_rand_smpl_bis:
 * @p:  array of int of length N
 * @n: sample size
 * @N: set size
 * @base: an int (see after)
 *
 * choose a sample of n elements in [base, base+N-1].
 * n must be <= N
 *
 * The sample is located in p[0..n-1]
 *
 * This version must be used when n is big enough 
 * (#nsp_rand_smpl should be faster for n << N)
 *
 **/
void nsp_rand_smpl_bis (int *p, int n, int N, int base)
{
  int i, iwhich, elt;
  for ( i = 0 ; i < N ; i++ ) 
    p[i] = base + i;
  for ( i = 0 ; i < n ; i++ )
    {
      iwhich = rand_ignuin (i, N-1);
      elt = p[iwhich];
      p[iwhich] = p[i];
      p[i] = elt;
    }
}


/**
 * nsp_rand_cauchy:
 * @sigma: parameter of the distribution
 * 
 * generates a random number from the Cauchy distribution.
 *
 * method: direct inversion. See p 29 of Luc Devroye 's book, "Non-Uniform 
 * Random Variate Generation".  Springer-Verlag, New York, 1986.
 * (available at the Luc Devroye 's home page :
 * http://cg.scs.carleton.ca/~luc/rnbookindex.html)
 *
 * Rmk: as Pi is not exact we don't need special treatment if ever
 * rand_ranf() return 0.5
 *
 * Returns: a double
 **/
double nsp_rand_cauchy(double sigma)
{
  return sigma * tan( M_PI * rand_ranf() );
}


/**
 * nsp_rand_pareto:
 * @a: parameter of the distribution
 * @b: parameter of the distribution
 * 
 * generates a random number from the Pareto distribution.
 *
 * method: direct inversion. See p 29 of Luc Devroye 's book, "Non-Uniform 
 * Random Variate Generation".  Springer-Verlag, New York, 1986.
 * (available at the Luc Devroye 's home page :
 * http://cg.scs.carleton.ca/~luc/rnbookindex.html)
 *
 * Rmk: we can't use the simplified form b/U^(1/a) because
 *      rand_ranf can output 0.
 *
 * Returns: a double
 **/
double nsp_rand_pareto(double a, double b)
{
  return b / pow( 1.0-rand_ranf(), 1.0/a );
}


/**
 * nsp_rand_logistic:
 * @a: parameter of the distribution
 * @b: parameter of the distribution
 * 
 * generates a random number from the logistic distribution.
 *
 * method: direct inversion. See p 39 of Luc Devroye 's book, "Non-Uniform 
 * Random Variate Generation".  Springer-Verlag, New York, 1986.
 * (available at the Luc Devroye 's home page :
 * http://cg.scs.carleton.ca/~luc/rnbookindex.html)
 *
 * Returns: a double
 **/
double nsp_rand_logistic(double a, double b)
{
  double u;

  do   /* just avoid 0 which could be output by rand_ranf() */
    {
      u = rand_ranf(); 
    }
  while ( u == 0.0 );

  return a + b * log( u / (1.0 - u) );
}


/**
 * nsp_rand_rayleigh:
 * @sigma: parameter of the distribution
 * 
 * generates a random number from the Rayleigh distribution.
 *
 *  density(x) = 1(x>=0) * x/sigma^2 exp(-0.5(x/sigma)^2)
 *      cdf(x) = 1(x>=0) * (1 - exp(-0.5(x/sigma)^2))
 *
 * method: direct inversion cdf(x) = y, leads to
 *    invcdf(y) = x = sigma * sqrt(-2 log(1-y))
 *
 * with y following the U(0,1) distribution
 * -log(1-y) follows the exponential distribution E(1)
 *
 * Returns: a double
 **/
double nsp_rand_rayleigh(double sigma)
{
  return sigma * sqrt( 2.0 * nsp_rand_exp_core() );
}


/**
 * nsp_rand_tailrayleigh:
 * @sigma,@a: parameters of the distribution
 * 
 * generates a random number from the tail of Rayleigh distribution.
 *
 * density(x) = 1(x>=a) * x/sigma^2 exp(-0.5( (x^2-a^2)/sigma^2 ))
 *     cdf(x) = 1(x>=a) * (1 - exp(-0.5( (x^2-a^2)/sigma^2 )))
 *
 * method: direct inversion cdf(x) = y, leads to
 *     invcdf(y) = x = sqrt( a^2 - 2 sigma^2 log(1-y) )
 *
 * with y following the U(0,1) distribution
 * -log(1-y) follows the exponential distribution E(1)
 *
 * Returns: a double
 **/
double nsp_rand_tailrayleigh(double sigma, double a)
{
  return sqrt( a*a + 2.0*sigma*sigma*nsp_rand_exp_core() );
}


/**
 * nsp_rand_weibull:
 * @a: parameter of the distribution
 * @b: parameter of the distribution
 * 
 * generates a random number from the Weibull distribution:
 * 
 *   dweibull(x) = (b/a) (x/a)^(b-1) exp(-(x/a)^b)  density
 *   Fweibull(x) = 1 - exp(-(x/a)^b)   x > 0        cdf
 *   inversion leads to x = a( (-log(1-y))^(1/b) )
 *   and we can use:
 *
 *        X = a ( E(1)^(1/b) )  where E(1) is an exp(1) rnd variable
 *
 * Returns: a double
 **/
double nsp_rand_weibull(double a, double b)
{
  return a * pow( nsp_rand_exp_core(), 1.0/b );
}

/**
 * nsp_rand_laplace:
 * @a: parameter of the distribution
 * 
 * generates a random number from the Laplace distribution:
 * 
 *   dlaplace(x) = 1/(2a) exp(-|x/a|) density
 *   Flaplace(x) = (1/2) exp(x/a) x <= 0
 *               = 1 - (1/2) exp(-x/a) x >= 0
 *
 * inversion leads to output -a E(1) with probability 1/2
 *                      and   a E(1) with probability 1/2   
 *
 * Returns: a double
 **/
double nsp_rand_laplace(double a)
{
  if ( rand_ranf() < 0.5 )
    return - a * nsp_rand_exp_core();
  else
    return   a * nsp_rand_exp_core();
}


/**
 * nsp_rand_lognormal:
 * @mu,@sigma: parameters of the distribution
 * 
 * generates a random number from the lognormal distribution.
 * 
 * Returns: a double
 **/
double nsp_rand_lognormal(double mu, double sigma)
{
  return  exp( nsp_rand_nor(mu, sigma) );
}

