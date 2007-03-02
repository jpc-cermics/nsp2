/*  
 *  PURPOSE
 *     uniform random number generator developed by Pierre 
 *     Lecuyer based on a clever and tested combination of 
 *     two linear congruential sequences
 *
 *        s1 <- a1*s1 mod m1 ,  a1 = 40014, m1 = 2147483563
 *        s2 <- a2*s2 mod m2 ,  a2 = 40692, m2 = 2147483399
 *
 *        output <-  s1-s2 mod (m1 - 1)  
 *
 *        so output is in [0, 2147483561], period about 2.3 10^18
 *
 *        The state is given by (s1, s2). In case of a user
 *        modification of the state we must have :
 * 
 *              s1 in [1, m1-1]
 *              s2 in [1, m2-1]
 *
 *  ORIGIN
 *     The basic code is provided at the Luc Devroye 's home page.
 *     Modifications by Bruno Pincon (in particular added routines 
 *     to set and get the state, and modify the generator to get 
 *     exactly  s1-s2 mod (m1 - 1) for "coherence" with the others 
 *     generators : provides numbers in [0, MaxRngInt(generator)] 
 *     (see NOTE some lines after)
 *
 */
#include "grand.h"
#include "basic_generators.h"

NspRandomGen Clcg2 = { CLCG2 , clcg2, "clcg2", 2, 
		       2147483561ul,
		       4.6566130595601735e-10,
		       get_state_clcg2, 
		       set_state_clcg2, 
		       set_state_clcg2_simple };

/* initial default state (seeds) : */
static long s1 = 1234567890 ;
static long s2 = 123456789  ;

unsigned long clcg2()
{
  register long k,z;

  /*  s1 = a1*s1 mod m1  (Schrage 's method)  */
  k= s1 /53668;
  s1 =40014*(s1%53668)-k*12211;
  if (s1 < 0) s1 += 2147483563;

  /*  s2 = a2*s2 mod m2  (Schrage 's method)  */
  k=s2/52774;
  s2=40692*(s2%52774)-k*3791;
  if (s2 < 0) s2 += 2147483399;

  /* final step : z = s1-s2 mod m1-1  */
  z = s1 - s2;  /* here z is in [2-m2,m1-2] */
  if (z < 0) z += 2147483562;

  /* NOTE : in the  original implementation the final test is :
   *     if (z < 1) z += 2147483562;
   * 
   *   which is not exactly  z = s1-s2 mod (m1 - 1)
   *
   *   This is also why it is different from the version used by
   *   randlib.
   */
  
  return( (unsigned long) z );
}

int set_state_clcg2(double g[])
{
  
  if ( g[0] == floor(g[0]) && g[1] == floor(g[1])  && 
       1 <= g[0] && g[0] <= 2147483562    &&
       1 <= g[1] && g[1] <= 2147483398 )
    {
      s1 = (long) g[0];
      s2 = (long) g[1];
      return OK;
    }
  else
    {
      Scierror("bad seeds for clcg2, must be integers with  s1 in [1, 2147483562]\n");
      Scierror("                                       and  s2 in [1, 2147483398]\n");
      return FAIL;
    }
}
int set_state_clcg2_simple(double seed)
{
  unsigned long s_test;
  
  if ( seed != floor(seed) || seed < 0  || seed > 4294967295.0 )
    {
      Scierror("bad simple seed for clcg2, must be an integer in  [0,2^32-1]\n");
      return FAIL;
    }

  s_test = (unsigned long) seed;
  do
    s_test = randbcpl( s_test );
  while ( s_test < 1 || s_test > 2147483562 );
  s1 = (long) s_test;

  do
    s_test = randbcpl( s_test );
  while ( s_test < 1 || s_test > 2147483398 );
  s2 = (long) s_test;

  return OK;
}

void get_state_clcg2(double g[])
{
  g[0] = (double) s1;
  g[1] = (double) s2;
}

