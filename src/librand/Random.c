/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
 * Copyright (C) 2001-2007 Bruno Pinçon Esial/Iecn
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
 *
 *--------------------------------------------------------------------------*/

/* #include <math.h> */
/* #include "nsp/machine.h" */

/* external functions to be called through this interface */
#include "grand.h"
#include "basic_generators.h"

NspRandomGen *NspRNG[NbGenInNsp] = { &MersenneTwister,
				     &Kiss,
				     &Fsultra,
				     &Well1024a,
				     &Clcg4,
				     &Clcg2};

/* the current generator id : */
static int current_gen_id = MT;  

/* the current generator func (avoid an indirection) */
unsigned long int (*current_gen)() = randmt;

/* the current factor (avoid an indirection) */
static double current_factor = 2.3283064365386963e-10;

/* the current max int (avoid an indirection) */
static long unsigned current_max_int = 4294967295ul;

/* an auxiliary generator only used ... */
unsigned long randbcpl( unsigned long s )
{
  return 2147001325ul * s + 715136305ul;
}

void nsp_set_current_gen(int new_id)
{
  current_gen_id = new_id;
  current_factor = NspRNG[current_gen_id]->factor;
  current_max_int = NspRNG[current_gen_id]->max_int;
  current_gen = NspRNG[current_gen_id]->gen;
}

int nsp_get_current_gen(void)
{
  return current_gen_id;
}


/* random deviate from U[0,1) */
double rand_ranf(void)   
{
  return  current_factor * (double) current_gen();
}

unsigned long int rand_lgi(void)
{
  return current_gen();
}

void rand_unf_01_and_uin_0_127(double *u, int *k_7bits)
{
  if ( current_gen_id < CLCG4 )   /* 32 bits generators */
    {
      unsigned long int n = current_gen();
      *k_7bits = n & 127;
      n >>= 7;
      *u = n * 2.98023223876953125e-8;
    }
  else                         /* other generators */
    {
      *k_7bits = 127 & current_gen();
      *u = rand_ranf();
    }
}

void rand_unf_01_and_uin_0_127_and_sign(double *u, int *k_7bits, int *k_1bit)
{
  int k7, k1;
  if ( current_gen_id < CLCG4 )   /* 32 bits generators */
    {
      unsigned long int n = current_gen();
      k7 = n & 255;
      k1 = k7 & 1;
      k7 >>= 1;
      n >>= 8;
      *u = n * 5.9604644775390625e-8;
    }
  else                         /* other generators */
    {
      k7 = 255 & current_gen();
      k1 = k7 & 1;
      k7 >>= 1;
      *u = rand_ranf();
    }
  *k_7bits = k7; *k_1bit = k1;
}


/*  random deviate from Ui[a,b] 
 *
 *  We use the classic method : to choose
 *  uniformly an integer in [a,b] (ie d=b-a+1 numbers) with a generator
 *  which provides uniformly integers in [0,RngMaxInt] (ie m=RngMaxInt+1
 *  numbers) we do the Euclidian division :
 *                                           m = q d + r,   r in [0,d-1]
 * 
 *  and accept only numbers l in [0, qd-1], then the output is k = a + (l mod d)
 *  (ie numbers falling in [qd , RngMaxInt] are rejected that is r numbers
 *   out of a total of m).
 *  The problem is that RngMaxInt is 2^32-1 for mt and kiss so that m=RngMaxInt+1 = 0
 *  with the 32 bits unsigned integer arithmetic. So we use instead :
 *
 *                        (m-1) = q' d + r',  r' in [0,d-1]
 *
 *  if  r < d-1  then  q' = q  and  r' = r+1      we reject r+1 numbers
 *  else (that is r = d-1)  q' = q+1  and r' = 0  we reject d=r+1 numbers
 *
 *  So using  RngMaxInt in place of m, we reject r+1 numbers (in place of r).
 *  The constraint is that (b-a+1) <= RngMaxInt and if we doesn't want to deal 
 *  we each generator we take (b-a+1) <= Min RngMaxInt =  2147483561 (clcg2)
 */                 

int rand_ignuin(int a, int b)
{
  unsigned long k, d = (b-a+1);

  static unsigned long d_save=-1, qd;
  if ( d == 1 )
    return a;
  else
    {
      if ( d != d_save )
	{
	  d_save = d;
	  qd = current_max_int - current_max_int % d;
	}
      do 
	k =  current_gen(); 
      while ( k >= qd );
      return a + (int) k % d;
    }
}

