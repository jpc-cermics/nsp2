/* log(1+x) */

#include <math.h>

static double lnp1m1(double s);

double nsp_log1p(double x)
{
  if ( -0.5 <= x && x <= 1.0) 
    {
      /* use the function log((1+g)/(1-g)) with g = x/(x + 2) */
      return lnp1m1( x / (x + 2.0));
    } 
  else 
    {
      /* use the standard formula */
      return log(x + 1.0);
    }
} 

/*     PURPOSE :  Compute   v = log ( (1 + s)/(1 - s) ) 
 *        for small s, this is for |s| < 0.334
 *     ALGORITHM : 
 *     1/ if |s| is "very small" we use a truncated 
 *        taylor dvp (by keeping 4 terms) from : 
 *
 *        t = 2 * s * ( 1 + 1/3 s^2  + 1/5 s^4  +  1/7 s^6   [+  1/9 s^8 .... ]) 
 *
 *        the relative error er is about 1/9 s^8, er <= epsm leads to
 *         |s| <= (9 epsm)^(1/8) which is near 0.0133. We take 0.01
 *        as limit for this formula.
 *
 *     2/ for 0.01 < |s| <= 0.2  we used a minimax polynome : 
 *        yi = s * (2  + d3 s^3 + d5 s^5 .... + d13 s^13 + d15 s^15) 
 *        computed (by some remes algorithm) following 
 *        (*) the sin(x) example (p 39) of the book : 
 *         "ELEMENTARY FUNCTIONS" 
 *         "Algorithms and implementation" 
 *         J.M. Muller (Birkhauser) 
 *        (*) without the additionnal raffinement to get the first coefs 
 *         very near floating point numbers) 
 *
 *     3/ for 0.2 < |s| <= 0.334 we used another minimax polynome
 *        yi = s * (2  + e3 s^2 + e5 s^4 + .... + e15 s^15 + e17 s^17) 
 *        got less or more empiricaly with the pari-gp software.
 *        
 */
static double lnp1m1(double s)
{
  double  s2;
  const double E = 1.e-2, C3  = 2.0/3.0, C5  = 2.0/5.0, C7 = 2.0/7.0;
  const double 
    D3 = 0.66666666666672679472E0, D5 = 0.39999999996176889299E0,
    D7 = 0.28571429392829380980E0, D9 = 0.22222138684562683797E0,
    D11= 0.18186349187499222459E0, D13= 0.15250315884469364710E0,
    D15= 0.15367270224757008114E0, E1 = 2.000000000009506159978903118, 
    E3 = 0.6666666653952079503982298174, E5 = 0.4000000741877232817059141633,
    E7 = 0.2857118089055873685381586584, E9 = 0.2222743062829225981951620178,
    E11 =0.1811042136269037116837648205, E13 =0.1601985962777479638905952230,
    E15 =0.0983334292284553038580498852, E17 =0.2223610247590649232825929055;

  s2 = s * s;
  if (fabs(s) <=  E) 
    return  s * (2E0 + s2*(C3 + s2*(C5 + s2*C7)));
  else if ( fabs(s) <= 0.2 )
    return s * (2.E0 + s2*(D3 + s2*(D5 + s2*( D7 + s2*(D9 + s2*(D11 + s2*(D13 + s2*D15)))))));
  else
    return s * (E1 + s2*(E3 + s2*(E5 + s2*( E7 + s2*(E9 + s2*(E11 + s2*(E13 + s2*(E15 + s2*E17))))))));
}

