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
 *     replaced with X = ceil( log(u) / log(1-p) ) or 1 + floor(log(u)/log(1-p))
 *     which needs less work. But as ranf() provides number in [0,1[ , 0 may be 
 *     gotten and these formulae may give then +oo.
 * 
 *     With ranf() the max number is 1 - 2^(-32). This let us choose a safe min
 *     value for p (to avoid a +oo due to log(1-p)) in the following manner :
 *
 *        the max is gotten for  M = log(2^(-32)) / (-p)
 *
 *     (for very small |x|, the accurate func logp1(x):=log(1+x) return simply x)
 *
 *     and we want  M <= Rmax (near 1.798+308 in ieee 754 double)
 *
 *     so p >= 32 log(2)/Rmax which is near 1.234e-307 ; Says pmin = 1.3e-307.
 *     (anyway the results gotten for such small values of p are certainly 
 *      not meaningful...)
 *
 *  NOTE
 *     this function returns a double instead of an integer type : this is 
 *     to avoid an extra conversion because in scilab it will be a double.
 *
 *  ASSUMPTION
 *     p must be in [pmin,1]  (to do at the calling level).
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

static double logp1(double x);
static double lnp1m1(double);

double rand_igngeom(double p)
{
  static double p_save = 1.0, ln_1_m_p = 0.0;
  double u;

  if ( p == 1 ) 
    return ( 1.0 );
  else if ( p != p_save )   /* => recompute log(1-p) */
    {
      p_save = p; u = -p; ln_1_m_p = logp1(u);
    };
  u = - rand_ranf();
  return ( floor( 1.0 + logp1(u)/ln_1_m_p) );
}      
   
/* log(1+x) */

static double logp1(double x)
{
  const double a = -1.0E0/3.0E0;
  const double b = 0.5E0; 
  if (x < -1.) 
    {
      return  (x - x) / (x - x); /* Nan */
    }
  else if ( a <= x && x <= b) 
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

/*     PURPOSE :  Compute   v = log ( (1 + s)/(1 - s) ) */
/*        for small s, this is for |s| < SLIM = 0.20 */
/*     ALGORITHM : */
/*     1/ if |s| is "very small" we use a truncated */
/*        taylor dvp (by keeping 3 terms) from : */
/*                               2        4          6 */
/*        t = 2 * s * ( 1 + 1/3 s  + 1/5 s  + [ 1/7 s  + ....] ) */
/*                               2        4 */
/*        t = 2 * s * ( 1 + 1/3 s  + 1/5 s  + er) */
/*        The limit E until we use this formula may be simply */
/*        gotten so that the negliged part er is such that : */
/*                                    2        4 */
/*        (#) er <= epsm * ( 1 + 1/3 s  + 1/5 s )   for all |s|<= E */
/*        As  er  = 1/7 s^6 + 1/9 s^8 + ... */
/*            er <= 1/7 * s^6 ( 1 + s^2 + s^4 + ...) = 1/7  s^6/(1-s^2) */

/*        the inequality (#) is forced if : */
/*        1/7  s^6 / (1-s^2)  <= epsm * ( 1 + 1/3 s^2  + 1/5 s^4 ) */
/*        s^6 <= 7 epsm * (1 - 2/3 s^2 - 3/15 s^4 - 1/5 s^6) */
/*        So that E is very near (7 epsm)^(1/6) (approximately 3.032d-3): */
/*     2/ For larger |s| we used a minimax polynome : */
/*        yi = s * (2  + d3 s^3 + d5 s^5 .... + d13 s^13 + d15 s^15) */
/*        This polynome was computed (by some remes algorithm) following */
/*        (*) the sin(x) example (p 39) of the book : */
/*         "ELEMENTARY FUNCTIONS" */
/*         "Algorithms and implementation" */
/*         J.M. Muller (Birkhauser) */
/*        (*) without the additionnal raffinement to get the first coefs */
/*         very near floating point numbers) */

double lnp1m1(double s)
{
  double  s2;
  const double E = 3.032E-3, C3  = 2E0 / 3E0, C5  = 2E0 / 5E0;
  const double 
    D3 = 0.66666666666672679472E0, D5 = 0.39999999996176889299E0,
    D7 = 0.28571429392829380980E0, D9 = 0.22222138684562683797E0,
    D11= 0.18186349187499222459E0, D13= 0.15250315884469364710E0,
    D15= 0.15367270224757008114E0;
  s2 = s * s;
  if (Abs(s) <=  E) 
    return  s * (2E0 + s2*(C3 + C5*s2));
  else
    return s * (2.E0 + s2*(D3 + s2*(D5 + s2*( D7 + s2*(D9 + s2*(D11 + s2*(D13 + s2*D15)))))));
}

