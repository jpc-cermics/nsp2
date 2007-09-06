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


/*    gamma generator as explained in "Marsaglia G and WW Tsang,
 *    A Simple Method for Generating Gamma Variables"
 *    ACM Transactions on math. Software, Vol 26, No 3, Sept 2000,
 *    pages 363-372"
 */

#include "grand.h"
#include <math.h>


int init_rand_gamma(double a, GammaStruct *G)
{
  if ( ! ( a > 0.0 ) )
    return FAIL;

  G->a = a;
  if ( a < 1.0 ) a++;
  G->d = a - 0.333333333333333333;
  G->c = 1.0 / sqrt(9.0*G->d);

  return OK;
}

double rand_gamma(GammaStruct *G)
{
  double x, x2, u, v, res;

  while ( 1 )
    {
      do
	{
	  x = rand_nor_core();
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
    return res * exp(-rand_exp_core()/G->a);
}

double rand_gamma_direct(double a)
{
  double aa, d, c, x, x2, u, v;

  aa = a >= 1.0 ? a : a+1.0;

  d = aa - 0.333333333333333333;
  c = 1.0 / sqrt(9.0*d);

  while ( 1 )
    {
      do
	{
	  x = rand_nor_core();
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
    return d*v*exp(-rand_exp_core()/a);
}

/*    
 * beta generator using the previous gamma generator
 * see Knuth TAOCP vol 2, 2d ed, p. 129
 */

int init_rand_beta(double a, double b, BetaStruct *B)
{
  if ( init_rand_gamma(a, &(B->a)) == FAIL )
    return FAIL;
  if ( init_rand_gamma(b, &(B->b)) == FAIL )
    return FAIL;
  return OK;
}

double rand_beta(BetaStruct *B)
{
  double x1, x2;
  x1 = rand_gamma(&(B->a));
  x2 = rand_gamma(&(B->b));
  return x1 / (x1 + x2);
}

double rand_beta_direct(double a, double b)
{
  double x1, x2;
  x1 = rand_gamma_direct(a);
  x2 = rand_gamma_direct(b);
  return x1 / (x1 + x2);
}

/*    
 * chi-square generator using the previous gamma generator
 * see Knuth TAOCP vol 2, 2d ed, p. 130
 */

int init_rand_chi2(double nu, Chi2Struct *C)
{
  C->nu = nu;
  return init_rand_gamma(0.5*nu, &(C->G));
}

double rand_chi2(Chi2Struct *C)
{
  return 2.0 * rand_gamma(&(C->G));
}

double rand_chi2_direct(double nu)
{
  return 2.0 * rand_gamma_direct(0.5*nu);
}

/*    
 * non central chi-square generator using the previous gamma generator
 */

int init_rand_nc_chi2(double nu, double xnonc, NcChi2Struct *C)
{
  if ( ! ( nu >= 1.0 && xnonc >= 0 ) )
    return FAIL;
  C->nu = nu;
  C->xnonc = xnonc;
  C->sqrt_xnonc = sqrt(xnonc);
  if ( nu > 1.0 ) 
    return init_rand_gamma(0.5*(nu-1.0), &(C->G));
  else
    return OK;
}

double rand_nc_chi2(NcChi2Struct *C)
{
  double Y;
  Y = C->sqrt_xnonc + rand_nor_core();
  Y *= Y;
  if ( C->nu > 1.0 )
    Y += 2.0 * rand_gamma(&(C->G));
  return Y;
}

double rand_nc_chi2_direct(double nu, double xnonc)
{
  double Y;
  Y = sqrt(xnonc) + rand_nor_core();
  Y *= Y;
  if ( nu > 1.0 )
    Y += 2.0 * rand_gamma_direct(0.5*(nu-1.0));
  return Y;
}


/*    
 * F (variance-ratio) generator using the previous gamma generator
 * see Knuth TAOCP vol 2, 2d ed, p. 130
 */

int init_rand_F(double nu1, double nu2, FStruct *F)
{
  F->nu1 = nu1;
  F->nu2 = nu2;
  if ( init_rand_gamma(0.5*nu1, &(F->G1)) == FAIL )
    return FAIL;
  if ( init_rand_gamma(0.5*nu2, &(F->G2)) == FAIL )
    return FAIL;
  return OK;
}

double rand_F(FStruct *F)
{
  return F->nu2*rand_gamma(&(F->G1)) / (F->nu1*rand_gamma(&(F->G2)));
}

double rand_F_direct(double nu1, double nu2)
{
  return  nu2*rand_gamma_direct(0.5*nu1) / (nu1*rand_gamma_direct(0.5*nu2));
}


/*    
 * non central F (variance-ratio) generator
 * uses  (X/nu1)/(Y/nu2) where X is a non central chi2 variate (nu1 dof and xnonc)
 * and Y a chi2 variate (nu2 dof)
 */

int init_rand_nc_F(double nu1, double nu2, double xnonc, NcFStruct *E)
{
  if ( init_rand_nc_chi2(nu1, xnonc, &(E->X)) == FAIL )
    return FAIL;
  if ( init_rand_chi2(nu2, &(E->Y)) == FAIL )
    return FAIL;
  return OK;
}

double rand_nc_F(NcFStruct *E)
{
  return (E->Y.nu)*rand_nc_chi2(&(E->X)) / ((E->X.nu)*rand_chi2(&(E->Y)));
}

double rand_nc_F_direct(double nu1, double nu2, double xnonc)
{
  return  nu2*rand_nc_chi2_direct(nu1,xnonc) / (nu1*rand_chi2_direct(nu2));
}


/*    
 * negative binomial generator using the previous gamma generator
 *     Algorithm from page 489 of Luc Devroye,
 *     Non-Uniform Random Variate Generation.  Springer-Verlag,
 *     New York, 1986.
 *     (available at the Luc Devroye 's home page :
 *      http://cg.scs.carleton.ca/~luc/rnbookindex.html)
 */

int init_rand_nbn(int n, double p, NbnStruct *N)
{
  if ( ! ( 0 < n  &&  0.0 < p  &&  p <= 1.0 ) )
    return FAIL;
  N->n = n;
  N->p = p;
  N->coef = (1.0 - p)/p;
  return init_rand_gamma((double) n, &(N->G));
}

int rand_nbn(NbnStruct *N)
{
  return poi_trd_direct( N->coef * rand_gamma(&(N->G)) );
}

int rand_nbn_direct(int n, double p)
{
  double Y;
  Y = rand_gamma_direct((double) n)*(1.0-p)/p;
  return poi_trd_direct(Y);
}

