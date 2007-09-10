/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */
/* Important note : P. L'Ecuyer authorizes the use of this code under GNU-GPL    */
/* (personnal communication between P. L'Ecuyer and me (B.P.)                    */
/*********************************************************************************/

/*   
 *  NOTES
 *    slightly modified par Bruno Pincon for inclusion in nsp 
 *      - names have changed (for uniformity with the others genators)
 *      - add get state routine
 *      - add a simple init routine to fill the state like in rand_mt.c
 */
#include "grand.h"
#include "basic_generators.h"

/* header for the well1024a */
static unsigned long well1024a();
static int set_state_well1024a_simple(double s);
static int set_state_well1024a(double seed_array[]);
static void get_state_well1024a(double state[]);

NspRandomGen Well1024a = { WELL1024A , well1024a, "well1024a", 33, 
			   4294967295ul,
			   2.3283064365386963e-10,
			   get_state_well1024a, 
			   set_state_well1024a, 
			   set_state_well1024a_simple };

#define W 32
#define R 32
#define M1 3
#define M2 24
#define M3 10

#define MAT0POS(t,v) (v^(v>>t))
#define MAT0NEG(t,v) (v^(v<<(-(t))))
#define Identity(v) (v)

#define V0            STATE[state_i                   ]
#define VM1           STATE[(state_i+M1) & 0x0000001fU]
#define VM2           STATE[(state_i+M2) & 0x0000001fU]
#define VM3           STATE[(state_i+M3) & 0x0000001fU]
#define VRm1          STATE[(state_i+31) & 0x0000001fU]
#define newV0         STATE[(state_i+31) & 0x0000001fU]
#define newV1         STATE[state_i                   ]

static unsigned int state_i = 0;
static unsigned int STATE[R];
static unsigned int z0, z1, z2;
static int is_init=0;  
static double DEFAULT_SEED=2006.0;


static int set_state_well1024a(double seed_array[])
{
  int i, state_i_try;

  state_i_try = (int) seed_array[0];
  if ( state_i_try < 0  ||  state_i_try >= R )
    {
      Scierror("the first component of the well1024a state, must be an integer in [0, %d] \n",R-1);
      return FAIL;
    }
  state_i = state_i_try;
  for ( i = 0 ; i < R ; i++ ) 
    STATE[i] = ((unsigned long) seed_array[i+1]) & 0xffffffff;
  is_init = 1;
  return OK;
}

static int set_state_well1024a_simple(double s)
{
  /*   set the initial state with the same procedure than for the *
   *   mersenne-twister (see rand_mt.c)                           */
  unsigned long seed;
  int i;

  if ( s == floor(s) && 0.0 <= s && s <= 4294967295.0 )
    {
      seed = (unsigned long) s;
      STATE[0]= seed & 0xffffffff;
      for ( i=1 ; i < R ; i++)
	{
	  STATE[i] =  (1812433253UL * (STATE[i-1] ^ (STATE[i-1] >> 30)) + i); 
	  STATE[i] &= 0xffffffffUL;   /* for >32 bit machines */
	}
      state_i = 0;
      is_init = 1;
      return OK;
    }
  else
    {
      Scierror("bad seed for well1024a, must be an integer in [0, 2^32-1] \n");
      return FAIL;
    }
}

static void get_state_well1024a(double state[])
{
  int i;

  if ( ! is_init )
    set_state_well1024a_simple(DEFAULT_SEED);
    
  state[0] = (double) state_i;
  for ( i = 0 ; i < R ; i++ ) 
    state[i+1] = (double) STATE[i];
}

static unsigned long well1024a (void)
{

  if ( ! is_init )
    set_state_well1024a_simple(DEFAULT_SEED);

  z0    = VRm1;
  z1    = Identity(V0)       ^ MAT0POS (8, VM1);
  z2    = MAT0NEG (-19, VM2) ^ MAT0NEG(-14,VM3);
  newV1 = z1                 ^ z2; 
  newV0 = MAT0NEG (-11,z0)   ^ MAT0NEG(-7,z1)    ^ MAT0NEG(-13,z2) ;
  state_i = (state_i + 31) & 0x0000001fU;
  return STATE[state_i];
}
