#include "grand.h"

/*
 *
 *     (STANDARD-)  E X P O N E N T I A L   DISTRIBUTION                
 *     FOR DETAILS SEE:                                                 
 *                                                                      
 *               AHRENS, J.H. AND DIETER, U.                            
 *               COMPUTER METHODS FOR SAMPLING FROM THE                 
 *               EXPONENTIAL AND NORMAL DISTRIBUTIONS.                  
 *               COMM. ACM, 15,10 (OCT. 1972), 873 - 882.               
 *                                                                      
 *     ALL STATEMENT NUMBERS CORRESPOND TO THE STEPS OF ALGORITHM       
 *     'SA' IN THE ABOVE PAPER (SLIGHTLY MODIFIED IMPLEMENTATION)       
 *                                                                      
 *     Modified by Barry W. Brown, Feb 3, 1988 to use RANF instead of   
 *     SUNIF.  The argument IR thus goes away.                          
 *                                                                      
 *     Q(N) = SUM(ALOG(2.0)**K/K!)    K=1,..,N ,      THE HIGHEST N
 *     (HERE 8) IS DETERMINED BY Q(N)=1.0 WITHIN STANDARD PRECISION
 *
 *  translated fo C with the help of f2c 
 *  Jean-Philippe Chancelier Oct 2004.
 */

double rand_sexpo (void)
{
  const double q[]= {.6931472, .9333737, .9888778, .9984959,
		     .9998293, .9999833, .9999986, .9999999};
  double q1 = q[0], umin, a=0.0 ,u, ustar;
  int i;
  u = rand_ranf ();
  while (1) 
    {
      u += u;
      /*     JJV changed the following to reflect the true algorithm and */
      /*     JJV prevent unpredictable behavior if U is initially 0.5. */
      /*     IF (u.LE.1.0) GO TO 20 */
      if (u < 1.) 
	a += q1;
      else 
	break;
    }
  u += -1.;
  if ( u <= q1 ) return a+u;
  i= 0;
  ustar = rand_ranf ();
  umin = ustar;
  while (1)
    {
      ustar = rand_ranf ();
      if (ustar < umin)  umin = ustar;
      ++i;
      if (u <= q[i]) break;
    }
  return a + umin * q1;
}


