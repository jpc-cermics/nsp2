/*
 *  PURPOSE
 *     generate a random deviate from G(p) : the geometric
 *     law. If a r.v. X ~ G(p), X is the number of Bernouilli trials
 *     (B(p)) until succes is met. So X take its values in
 *
 *         {1, 2, 3, ...., } 
 *
 *     and  P(X=i) = p * (1-p)^(i-1) 
 *
 *  METHOD 
 *     inversion of the cdf leads to :
 *
 *     (1)   X = 1 + floor( log(1-u) / log(1-p) )
 *
 *     u being a random deviate from U[0,1).
 *
 *     by taking into account that 1-u follows also U(0,1)) this may be 
 *     replaced with X = 1 + floor(log(u)/log(1-p))
 *
 *     or taking into acount that e = -log(u) as exponential distribution
 *     we could use :
 *                      X = 1 + floor(-e/log(1-p))
 *
 *     with e an exponential random variate
 *     
 *  AUTHOR
 *     Bruno Pincon (<Bruno.Pincon@iecn.u-nancy.fr>)
 *     logp1 lnp1m1 translated from fortran and added 
 *     Jean-Philippe Chancelier Oct 2004.
 *
 */

#include <nsp/machine.h> 
#include <nsp/math.h> 

#include "grand.h" 

/* double logp1(double x); */
static double lnp1m1(double);

int init_rand_geom(double p, GeomStruct *G)
{

  if ( ! (1.3e-307 <= p && p <= 1 ) )
    return FAIL;
  G->p = p;
  G->inv_ln_1_m_p = 1.0/logp1(-p);
  return OK;
}
 
unsigned int rand_geom(GeomStruct *G)
{
  if ( G->p == 1.0 )
    return 1;
  else
    return (unsigned int) (1.0 - G->inv_ln_1_m_p * rand_exp_core());
}

 
unsigned int rand_geom_direct(double p)
{
  if ( p == 1.0 )
    return 1;
  else
    return (unsigned int) (1.0 -  rand_exp_core()/logp1(-p));
}


   
/* log(1+x) */

double logp1(double x)
{
  const double a = -1.0E0/3.0E0;
  const double b = 0.5E0; 
  if ( a <= x && x <= b) 
    {
      /* use the function log((1+g)/(1-g)) with g = x/(x + 2) */
      return lnp1m1( x / (x + 2.0));
    } 
  else 
    {
      /* use the standard formula */
      return log(x + 1.);
    }
} 

/*     PURPOSE :  Compute   v = log ( (1 + s)/(1 - s) ) 
 *        for small s, this is for |s| < SLIM = 0.20 
 *     ALGORITHM : 
 *     1/ if |s| is "very small" we use a truncated 
 *        taylor dvp (by keeping 3 terms) from : 
 *                               2        4          6 
 *        t = 2 * s * ( 1 + 1/3 s  + 1/5 s  + [ 1/7 s  + ....] ) 
 *                               2        4 
 *        t = 2 * s * ( 1 + 1/3 s  + 1/5 s  + er) 
 *        The limit E until we use this formula may be simply 
 *        gotten so that the negliged part er is such that : 
 *                                    2        4 
 *        (#) er <= epsm * ( 1 + 1/3 s  + 1/5 s )   for all |s|<= E 
 *        As  er  = 1/7 s^6 + 1/9 s^8 + ... 
 *            er <= 1/7 * s^6 ( 1 + s^2 + s^4 + ...) = 1/7  s^6/(1-s^2) 
 *        the inequality (#) is forced if : 
 *        1/7  s^6 / (1-s^2)  <= epsm * ( 1 + 1/3 s^2  + 1/5 s^4 ) 
 *        s^6 <= 7 epsm * (1 - 2/3 s^2 - 3/15 s^4 - 1/5 s^6) 
 *        So that E is very near (7 epsm)^(1/6) (approximately 3.032d-3): 
 *     2/ For larger |s| we used a minimax polynome : 
 *        yi = s * (2  + d3 s^3 + d5 s^5 .... + d13 s^13 + d15 s^15) 
 *        This polynome was computed (by some remes algorithm) following 
 *        (*) the sin(x) example (p 39) of the book : 
 *         "ELEMENTARY FUNCTIONS" 
 *         "Algorithms and implementation" 
 *         J.M. Muller (Birkhauser) 
 *        (*) without the additionnal raffinement to get the first coefs 
 *         very near floating point numbers) 
 */

/* static double lnp1m1(double s) */
/* { */
/*   double  s2; */
/*   const double E = 3.032E-3, C3  = 2E0 / 3E0, C5  = 2E0 / 5E0; */
/*   const double  */
/*     D3 = 0.66666666666672679472E0, D5 = 0.39999999996176889299E0, */
/*     D7 = 0.28571429392829380980E0, D9 = 0.22222138684562683797E0, */
/*     D11= 0.18186349187499222459E0, D13= 0.15250315884469364710E0, */
/*     D15= 0.15367270224757008114E0; */
/*   s2 = s * s; */
/*   if (Abs(s) <=  E)  */
/*     return  s * (2E0 + s2*(C3 + C5*s2)); */
/*   else */
/*     return s * (2.E0 + s2*(D3 + s2*(D5 + s2*( D7 + s2*(D9 + s2*(D11 + s2*(D13 + s2*D15))))))); */
/* } */


static double lnp1m1(double s)
{
  double  s2;
  const double E = 1.e-2, C3  = 2.0/3.0, C5  = 2.0/5.0, C7 = 2.0/7.0;
  const double 
    D3 = 0.66666666666672679472E0, D5 = 0.39999999996176889299E0,
    D7 = 0.28571429392829380980E0, D9 = 0.22222138684562683797E0,
    D11= 0.18186349187499222459E0, D13= 0.15250315884469364710E0,
    D15= 0.15367270224757008114E0;
  s2 = s * s;
  if (Abs(s) <=  E) 
    return  s * (2E0 + s2*(C3 + s2*(C5 + s2*C7)));
  else
    return s * (2.E0 + s2*(D3 + s2*(D5 + s2*( D7 + s2*(D9 + s2*(D11 + s2*(D13 + s2*D15)))))));
}

