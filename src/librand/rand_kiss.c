/* 
 *   PURPOSE
 *      the kiss generator of G. Marsaglia
 *      generate random integers (uint) in [0, 2^32 - 1]
 *      the state is given by 4 integers (z, w, jsr, jcong)
 *
 *   NOTES
 *      The code was given by G. Marsaglia at the end of  a
 *      thread  concerning  RNG  in C in several newsgroups
 *      (whom sci.math.num-analysis) "My offer of RNG's for
 *      C  was an invitation to dance..."
 *
 *      Slight modifications by Bruno Pincon for inclusion in
 *      Scilab (added set/get state routines)
 *
 *      kiss is made of combinaison of severals  others but  
 *      they  are not interfaced at the scilab level.
 *
 *      Need that it is assumed that the 
 *         unsigned long arithmetic is the classic 32 bits 
 *         unsigned arithmetic modulo 2^32 (ie all is exact
 *         modulo 2^32) 
 *
 */
#include "grand.h"
#include "basic_generators.h"

/* header for kiss */
static unsigned long kiss();
static int set_state_kiss(double g[]);
static int set_state_kiss_simple(double g);
static void get_state_kiss(double g[]);

NspRandomGen Kiss = { KISS , kiss, "kiss", 4, 
		      4294967295ul,
		      2.3283064365386963e-10,
		      get_state_kiss, 
		      set_state_kiss, 
		      set_state_kiss_simple };

/* The Marsaglia 's macros : */
#define znew  (z=36969*(z&65535)+(z>>16))
#define wnew  (w=18000*(w&65535)+(w>>16))
#define MWC   ((znew<<16)+wnew )
#define CONG  (jcong=69069*jcong+1234567)
#define SHR3  (jsr^=(jsr<<17), jsr^=(jsr>>13), jsr^=(jsr<<5))
#define KISSGEN  ((MWC^CONG)+SHR3)

/*  the kiss 's state  (any int in [0,2^32-1] are OK ?) */
static unsigned long z=362436069, w=521288629, jsr=123456789, jcong=380116160;

static unsigned long kiss()
{
  return KISSGEN;
}

static int set_state_kiss(double *g)
{
  int k;
  for ( k = 0 ; k < 4 ; k++ )
    if ( g[k] != floor(g[k]) || g[k] < 0.0 || g[k] > 4294967295.0 )
      {
	Scierror("bad seeds for kiss, must be integers in [0,2^32-1]\n");
	return FAIL;
      }

  z = (unsigned long) g[0];
  w = (unsigned long) g[1];
  jsr = (unsigned long) g[2];
  jcong = (unsigned long) g[3];
  return OK;
}

static int set_state_kiss_simple(double g)
{
  if ( g != floor(g) || g < 0.0 || g > 4294967295.0 )
    {
      Scierror("bad simple seed for kiss, must be an integer in [0,2^32-1]\n");
      return FAIL;
    }

  z = randbcpl( (unsigned long) g );
  w = randbcpl(z);
  jsr = randbcpl(w);
  jcong = randbcpl(jsr);
  return OK;
}

static void get_state_kiss(double *g)
{
  g[0] = (double) z;
  g[1] = (double) w;
  g[2] = (double) jsr;
  g[3] = (double) jcong;
}

