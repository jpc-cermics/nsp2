/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
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
 * Interface for grand
 * jpc@cermics.enpc.fr 
 *
 * stuff to deal with several generators added 
 * by Bruno Pincon (12/11/2001) 
 *
 *--------------------------------------------------------------------------*/

#include <math.h>
#include <strings.h>
#include <nsp/machine.h>
#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"

/* external functions to be called through this interface */
#include "grand.h"
#include "rand_clcg4.h"
#include "rand_others.h"
#include "rand_ultra.h"

enum {MT, KISS, CLCG4, CLCG2, URAND, FSULTRA};

/* the current generator : */
static int current_gen = MT;  

/* for clcg4 : the current virtual gen (current_clcg4 in [0, Maxgen]) */
static int current_clcg4 = 0; 

/* clcg4 must be called with the virtual generator number */

static unsigned long int clcg4_with_gen(void)
{
  return ( clcg4(current_clcg4) );
}

#define NbGenInScilab 6

/*  pointers onto the generators func */
unsigned long int (*gen[NbGenInScilab])() = { randmt, kiss,  clcg4_with_gen, clcg2 , urandc , fsultra};

/*  names at the scilab level */
static char *names_gen[NbGenInScilab] = { "mt",  "kiss","clcg4", "clcg2", "urand", "fsultra" };

/* all the generators provided integers in [0, RngMaxInt] :        */
static
unsigned long RngMaxInt[NbGenInScilab] = { 4294967295ul,  /* mt    */
					   4294967295ul,  /* kiss  */
					   2147483646ul,  /* clcg4 */
					   2147483561ul,  /* clcg2 */
					   2147483647ul,  /* urand */
					   4294967295ul}; /* fsultra*/
/* the factors (1/(RngMaxInt+1)) to get reals in [0,1) :           */
static
double factor[NbGenInScilab] = { 2.3283064365386963e-10,  /* mt    */
				 2.3283064365386963e-10,  /* kiss  */
                                 4.6566128752457969e-10,  /* clcg4 */
		                 4.6566130595601735e-10,  /* clcg2 */
		                 4.6566128730773926e-10,  /* urand */
				 2.3283064365386963e-10}; /* fsultra*/

double rand_ranf(void)   
{
  /* random deviate from U[0,1) */
  return ( (double) gen[current_gen]() * factor[current_gen] );
}

double ignlgi(void)
{
  /* random deviate from Ui[0,RngMaxInt] (direct output of the current gen) */
  return ( (double) gen[current_gen]() );
}

/*  random deviate from Ui[a,b] 
 *  it is assumed that : (i)  a and b are integers (stored in double) 
 *                       (ii) b-a+1 <= RngMaxInt[current_gen]
 *  (these verif are done at the calling level)
 *
 *  We use the classic method with a minor difference : to choose
 *  uniformly an integer in [a,b] (ie d=b-a+1 numbers) with a generator
 *  which provides uniformly integers in [0,RngMaxInt] (ie m=RngMaxInt+1
 *  numbers) we do the Euclidian division :
 *                                           m = q d + r,   r in [0,d-1]
 * 
 *  and accept only numbers l in [0, qd-1], then the output is k = a + (l mod d)
 *  (ie numbers falling in [qd , RngMaxInt] are rejected).
 *  The problem is that RngMaxInt is 2^32-1 for mt and kiss so that RngMaxInt+1 = 0
 *  with the 32 bits unsigned integer arithmetic. So in place of rejected r
 *  numbers we reject r+1 by using RngMaxInt in place of m. The constraint is
 *  then that (b-a+1) <= RngMaxInt and if we doesn't want to deal we each generator
 *  we take (b-a+1) <= Min RngMaxInt =  2147483561 (clcg2)
 */                 

double rand_ignuin(double *a, double *b)
{
  unsigned long k, d = (unsigned long)((*b-*a)+1), qd;
  
  if ( d == 1 || (*b < *a) ) return (*a);

  qd = RngMaxInt[current_gen] - RngMaxInt[current_gen] % d;
  do 
    { 
      k = (unsigned long)ignlgi();
    } 
  while ( k >= qd );
  return ( *a + (double)(k % d) );
}

/*
 *  hand written interface for the randlib 
 */

int int_nsp_grand( Stack stack, int rhs, int opt, int lhs)
{ 
  char *law;
  NspMatrix *M=NULL;
  int minrhs, maxrhs = 10;
  int ResL,ResC,i,suite;
  if (rhs >= 1 &&  IsSMatObj(stack,1)) 
    {
      const int dim_state_mt=625, dim_state_4=4, dim_state_fsultra = 40;
      char *str;
      if ((str = GetString(stack,1)) == (char*)0) return RET_BUG;
      if ( strcmp(str,"getsd")==0) 
       {
	 if ( rhs != 1) 
	   {
	     Scierror("%s: only one argument expected for grand('getsd')\n",NspFname(stack));
	     return RET_BUG;
	   }
	 switch(current_gen)
	   {
	   case(MT) :
	     if ((M = nsp_matrix_create(NVOID,'r',dim_state_mt,1))== NULLMAT) return RET_BUG;
	     get_state_mt(M->R);
	     break;
	   case(KISS) :
	     if ((M = nsp_matrix_create(NVOID,'r',dim_state_4,1))== NULLMAT) return RET_BUG;
	     get_state_kiss(M->R);
	     break;
	   case(CLCG4) :
	     if ((M = nsp_matrix_create(NVOID,'r',dim_state_4,1))== NULLMAT) return RET_BUG;
	     get_state_clcg4(current_clcg4, M->R);
	     break;
	   case(CLCG2) :
	     if ((M = nsp_matrix_create(NVOID,'r',2,1))== NULLMAT) return RET_BUG;
	     get_state_clcg2(M->R);
	     break;
	   case(URAND) : 
	     if ((M = nsp_matrix_create(NVOID,'r',1,1))== NULLMAT) return RET_BUG;
	     get_state_urand(M->R);
	     break;
	   case(FSULTRA) : 
	     if ((M = nsp_matrix_create(NVOID,'r',dim_state_fsultra,1))== NULLMAT) return RET_BUG;
	     get_state_fsultra(M->R);
	     break;
	   };
	 MoveObj(stack,1,(NspObject *) M);
	 return 1;
       }
      else if ( strcmp(str,"setall")==0 ) 
	{
	  double x1,x2,x3,x4;
	  if ( current_gen != CLCG4 )
	    Scierror("Error: the setall option affect only the clcg4 generator !\n");
	  if ( rhs != 5 ) 
	    {
	      Scierror("Error: rhs should be 5 for 'setall'  option\n");
	      return RET_BUG;
	    }
	  if (GetScalarDouble(stack,2,&x1) == FAIL) return RET_BUG;
	  if (GetScalarDouble(stack,3,&x2) == FAIL) return RET_BUG;
	  if (GetScalarDouble(stack,4,&x3) == FAIL) return RET_BUG;
	  if (GetScalarDouble(stack,5,&x4) == FAIL) return RET_BUG;
	  if ( set_initial_seed_clcg4(x1,x2,x3,x4) == FAIL) return RET_BUG;
	  return 0;
	}
      else if ( strcmp(str,"setsd")==0 ) 
	{
	  double x1,x2,x3,x4;
	  switch(current_gen)
	    {
	    case(MT) :
	      if ( rhs != 2 ) 
		{
		  Scierror("Error: rhs should be 2 for 'setsd' option with the mt generator\n");
		  return RET_BUG;
		}
	      if ((M = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
	      switch ( M->mn ) 
		{
		case 1: 
		  if ( set_state_mt_simple(M->R[0]) == FAIL ) return RET_BUG;
		  break;
		case 625: 
		  if ( set_state_mt(M->R) == FAIL)  return RET_BUG;
		  break;
		default: 
		  Scierror("Error: for mt you must init the state with a vector of 1 or 625 values !\n");
		  return RET_BUG;
		} 
	      break;
	    case(KISS) :
	    case(CLCG4) :
	      switch (rhs) 
		{
		case 2 : 
		  if ((M=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
		  CheckLength(NspFname(stack),2,M,4);
		  x1 = M->R[0];		  
		  x2 = M->R[1];
		  x3 = M->R[2];
		  x4 = M->R[3];
		  break ;
		case 5 :
		  if (GetScalarDouble(stack,2,&x1) == FAIL) return RET_BUG;
		  if (GetScalarDouble(stack,3,&x2) == FAIL) return RET_BUG;
		  if (GetScalarDouble(stack,4,&x3) == FAIL) return RET_BUG;
		  if (GetScalarDouble(stack,5,&x4) == FAIL) return RET_BUG;
		  break;
		default :
		  Scierror("Error: rhs should be 2 or 5 for 'setsd' option with the kiss or clcg4 generator\n");
		  return RET_BUG;
		}
	      if (current_gen == KISS) 
		{if ( set_state_kiss(x1,x2,x3,x4) == FAIL)  return RET_BUG;}
	      else
		{if ( set_seed_clcg4(current_clcg4,x1,x2,x3,x4) == FAIL)  return RET_BUG;}
	      break;
	    case(CLCG2) :
	      switch (rhs ) 
		{
		case 2: 
		  if ((M=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
		  CheckLength(NspFname(stack),2,M,2);
		  x1 = M->R[0];		  
		  x2 = M->R[1];
		  break;
		case 3: 
		  if (GetScalarDouble(stack,2,&x1) == FAIL) return RET_BUG;
		  if (GetScalarDouble(stack,3,&x2) == FAIL) return RET_BUG;
		  break;
		default : 
		  Scierror("Error: rhs should be 2 or 3 for 'setsd' option with clcg2 generator\n");
		  return RET_BUG;
		}
	      if ( set_state_clcg2(x1,x2) == FAIL) return RET_BUG; 
	      break;
	    case(FSULTRA) :
	      switch (rhs ) 
		{
		case 2: 
		  /* init via a "complete" state */
		  if ((M=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
		  CheckLength(NspFname(stack),2,M,dim_state_fsultra );
		  if ( set_state_fsultra(M->R) == FAIL ) return RET_BUG;
		  break;
		case 3: 
		  if (GetScalarDouble(stack,2,&x1) == FAIL) return RET_BUG;
		  if (GetScalarDouble(stack,3,&x2) == FAIL) return RET_BUG;
		  if ( set_state_fsultra_simple(x1,x2) == FAIL) return RET_BUG;
		  break;
		default : 
		  Scierror("Error: rhs should be 2 or 3 for 'setsd' option with fsultra generator\n");
		  return RET_BUG;
		}
	      break;
	    case(URAND) :
	      if ( rhs != 2 ) 
		{
		  Scierror("Error: rhs should be 2 for 'setsd' option with the urand generator\n");
		  return RET_BUG;
		}
	      if (GetScalarDouble(stack,2,&x1) == FAIL) return RET_BUG;
	      if ( set_state_urand(x1) == FAIL) return RET_BUG; 
	    };
	  return 0;
	}
      else if (strcmp("phr2sd",str) == 0) 
	{
	  char *str1;
	  int i1,i2;
	  if ( rhs != 2) 
	    {
	      Scierror("Error: rhs should be 2 for 'phr2sd' option\n");
	      return RET_BUG;
	    }
	  if ((str1 = GetString(stack,2)) == (char*)0) return RET_BUG;
	  if ((M = nsp_matrix_create(NVOID,'r',1,2))== NULLMAT) return RET_BUG;
	  rand_phrtsd(str1,&i1,&i2);
	  M->R[0]=i1; 	  M->R[1]=i2; 
	  MoveObj(stack,1,(NspObject *) M);
	  return 1;
	}

      else if (strcmp("initgn",str)==0) 
	{
	  int i1;
	  SeedType Where;
	  if ( current_gen != CLCG4 )
	    {
	      Scierror("Error: initgn option affects only the clcg4 generator\n");
	      return RET_BUG;
	    }
	  if ( rhs != 2) 
	    {
	      Scierror("Error: rhs should be 2 for 'initgn' option\n");
	      return RET_BUG;
	    }
	  if (GetScalarInt(stack,2,&i1) == FAIL) return RET_BUG;
	  if ( i1 != 0 && i1 != -1 && i1 != 1)
	    {
	      Scierror("Error: for initgn option argument must be -1,0 or 1\n");
	      return RET_BUG;
	    }
	  Where = (SeedType) (i1 + 1);
	  init_generator_clcg4(current_clcg4, Where);	  
	  return 0;
	}
      else if (strcmp("setcgn",str)==0) 
	{
	  int i1;
	  if ( current_gen != CLCG4 )
	    {
	      Scierror("Error: the 'setcgn' option affect only the clcg4 generator\n");
	      return RET_BUG;
	    }
	  if ( rhs != 2) 
	    {
	      Scierror("Error: rhs should be 2 for 'setcgn' option\n");
	      return RET_BUG;
	    }
	  if (GetScalarInt(stack,2,&i1) == FAIL) return RET_BUG;
	  if ( i1 < 0 || i1 > Maxgen )
	    {
	      Scierror("Error: bad virtual number generator %d (must be in [0,%d])\n",i1,Maxgen);
	      return RET_BUG;
	    }
	  current_clcg4 = i1;
	  return 0;
	}
      else if (strcmp("advnst",str)==0) 
	{
	  int i1;
	  if ( current_gen != CLCG4 )
	    {
	      Scierror("Error: the 'advnst' option affect only the clcg4 generator !\n");
	      return RET_BUG;
	    }
	  if ( rhs != 2) 
	    {
	      Scierror("Error: rhs should be 2 for 'advnst' option\n");
	      return RET_BUG;
	    }
	  if (GetScalarInt(stack,2,&i1) == FAIL) return RET_BUG;
	  if ( i1 < 1 )
	    {
	      Scierror("Error: parameter K must be > 0 for 'advnst' option\n");
	      return RET_BUG;
	    }
	  advance_state_clcg4(current_clcg4,i1);
	  return 0;
	}
      else if (strcmp("getcgn",str)==0) 
	{
	  if ( rhs != 1) 
	    {
	      Scierror("Error: rhs should be 1 for 'getcgn' option\n");
	      return RET_BUG;
	    }
	  if ( current_gen != CLCG4 )
	    {
	      Scierror("Error: this information concerns only the clcg4 generator\n");
	      return RET_BUG;
	    }
	  if ( nsp_move_double(stack,1,(double)current_clcg4 )== FAIL) return RET_BUG;
	  return 1;
	}
      else if (strcmp("setgen",str)==0) 
	{
	  char *str1;
	  if ( rhs != 2) 
	    {
	      Scierror("Error: rhs should be 2 for 'setgen' option\n");
	      return RET_BUG;
	    }
	  if ((str1 = GetString(stack,2)) == (char*)0) return RET_BUG;
	  if (strcmp("mt",str1)==0) 	  
	    current_gen = MT;
	  else if (strcmp("kiss",str1)==0)
	    current_gen = KISS;
	  else if (strcmp("clcg4",str1)==0)
	    current_gen = CLCG4;
	  else if (strcmp("clcg2",str1)==0)
	    current_gen = CLCG2;
	  else if (strcmp("urand",str1)==0)
	    current_gen = URAND;
	  else if (strcmp("fsultra",str1)==0)
	    current_gen = FSULTRA;
	  else
	    {
	      Scierror("Error: unknown generator (choose among : mt kiss clcg4 clcg2 urand fsultra) \n");
	      return RET_BUG;
	    }
	  return 0;
	}
      else if (strcmp("getgen",str)==0) 
	{
	  if ( rhs != 1) 
	    {
	      Scierror("Error: rhs should be 1 for 'getgen' option\n");
	      return RET_BUG;
	    }
	  if ( nsp_move_string(stack,1,names_gen[current_gen],-1)== FAIL) return RET_BUG;
	  return 1;
	}
      else 
	{
	  Scierror("Error: %s Wrong first argument %s\n",NspFname(stack),str);
	  return RET_BUG;
	}      
    }

  minrhs = 2;
  CheckRhs(minrhs,maxrhs);

  if (IsMatObj(stack,2)) 
    {
      if (GetScalarInt(stack,1,&ResL) == FAIL) return RET_BUG;
      CheckNonNegative(NspFname(stack),ResL,1);
      if (GetScalarInt(stack,2,&ResC) == FAIL) return RET_BUG;
      CheckNonNegative(NspFname(stack),ResC,2);
      if (rhs <= 2 ) 
	{
	  Scierror("Error: expecting more than two numbers as arguments \n",NspFname(stack));
	  return RET_BUG;
	}
      if ((law = GetString(stack,3)) == (char*)0) return RET_BUG;
      suite=4;
    }
  else 
    {
      if ((M = GetMat(stack,1)) == NULLMAT) return RET_BUG;
      ResL=M->m; ResC=M->n;
      if (rhs <= 1 ) 
	{
	  Scierror("Error: expecting more than one number as arguments \n",NspFname(stack));
	  return RET_BUG;
	}
      if ((law = GetString(stack,2)) == (char*)0) return RET_BUG;
      suite=3;
    }
  
  if ( ResL*ResC == 0) 
    {
      if ((M = nsp_matrix_create(NVOID,'r',0,0))== NULLMAT) return RET_BUG;
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

  if ( strcmp(law,"bet")==0) 
    {
      double A,B,minlog=1.e-37;
      if ( rhs != suite + 1) 
	{ Scierror("Error: Missing A and B for beta law\n");return RET_BUG;}
      if (GetScalarDouble(stack,suite,&A) == FAIL) return RET_BUG;      
      if (GetScalarDouble(stack,suite+1,&B) == FAIL) return RET_BUG;      
      if ( A < minlog || B < minlog)
	{
	  Scierror("Error:  grand(...,'bet',..): A or B < %f \n",minlog);
	  return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++)  M->R[i]= rand_genbet(&A,&B);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }
  else if ( strcmp(law,"f")==0) 
    {
      double A,B;
      if ( rhs != suite + 1) 
	{ Scierror("Error: Missing Dfn and Dfd for F law\n");return RET_BUG;}
      if (GetScalarDouble(stack,suite,&A) == FAIL) return RET_BUG;      
      if (GetScalarDouble(stack,suite+1,&B) == FAIL) return RET_BUG;      

      if ( A <= 0.0 || B <= 0.0)
	{
	  Scierror("Error: non positive freedom degrees !\n");
	  return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++)  M->R[i] = rand_genf(&A,&B);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }
  else if ( strcmp(law,"mul")==0) 
    {
      int N, *I=NULL;
      NspMatrix *P,*B=NULL;
      int i,nn,ncat;
      double ptot;
      if ( suite != 3 || M->mn != 1)
	{ Scierror("Error: First argument for 'mul' option must be the number of random deviate \n"); return RET_BUG;	}
      nn= M->R[0] ;
      if ( rhs != suite + 1) { Scierror("Error: Missing N and P for MULtinomial law\n");return RET_BUG;}

      if (GetScalarInt(stack,suite,&N) == FAIL) return RET_BUG;      
      if ((P = GetMat(stack,suite+1)) == NULLMAT) return RET_BUG;
      ncat = P->mn+1;
      if ((B = nsp_matrix_create(NVOID,'r',ncat,nn))== NULLMAT) return RET_BUG;
      if ( N < 0 ) 
	{
	  Scierror("Error: N < 0 \n");
	  return RET_BUG;
	}
      if ( ncat <= 1) 
	{
	  Scierror("Error: Ncat <= 1 \n");
	  return RET_BUG;
	}
      ptot = 0.0;
      for ( i= 0 ; i < ncat -1 ; i++ )
	{
	  if ( P->R[i] < 0 ||  P->R[i]  > 1 ) 
	    {
	      Scierror("Error: P(%d)=%f is not a in [0,1] \n",P->R[i],i+1);
	      return RET_BUG;
	    }
	  ptot +=  P->R[i];
	}
      if ( ptot > 0.9999) 
	{
	  Scierror("Error: Sum of P(i) > 1 \n");
	  return RET_BUG;
	}
      I = (int *)  B->R;
      for ( i=0 ; i < nn ; i++)  rand_genmul(&N,P->R,&ncat,I + ncat*i);
      B->convert = 'i';
      Mat2double(B);
      MoveObj(stack,1,(NspObject *) B);
      return 1;
    }
  else if ( strcmp(law,"gam")==0) 
    {
      double A,B;
      if ( rhs != suite + 1) 
	/*  ETRE PLUS CONSISTANT ICI : choisir entre shape , scale ou
	 * bien A et R (idem pour le man)
	 */
	{ Scierror("Error: Missing shape and scale for Gamma law\n");return RET_BUG;}
      if (GetScalarDouble(stack,suite,&A) == FAIL) return RET_BUG;      
      if (GetScalarDouble(stack,suite+1,&B) == FAIL) return RET_BUG;      
      if ( (A) <= 0.0 ||  (B) <= 0.0 )
	{
	  Scierror("Error: grand(..'gam',A,R) : A <= 0.0 or R <= 0.0 \n"); return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++) 
	{
	  /** WARNING : order is changed in parameters for 
	   *    compatibility between Rand(...'gam',..) and cdfgam 
	   **/
	  M->R[i]= rand_gengam(&B,&A);
	}
      MoveObj(stack,1,(NspObject *) M);
      return 1;

    }

  else if ( strcmp(law,"nor")==0) 
    {
      double A,B;
      if ( rhs != suite + 1) 
	{ Scierror("Error: Missing Av and Sd for Normal law\n");return RET_BUG;}
      if (GetScalarDouble(stack,suite,&A) == FAIL) return RET_BUG;      
      if (GetScalarDouble(stack,suite+1,&B) == FAIL) return RET_BUG;      
      if ( B < 0 ) 
	{  Scierror("Error: SD < 0.0 \n");return RET_BUG;}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++) { M->R[i]= rand_gennor(&A,&B); }
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }
  else if ( strcmp(law,"unf")==0) 
    {
      double low, high;
      if ( rhs != suite + 1) 
	{ Scierror("Error: Missing Low and High for Uniform Real law\n");return RET_BUG;}
      if (GetScalarDouble(stack,suite,&low) == FAIL) return RET_BUG;      
      if (GetScalarDouble(stack,suite+1,&high) == FAIL) return RET_BUG;      
      if ( low > high ) 
	{
	  Scierror("Error: Low > High \n"); return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++) M->R[i]= low + (high - low)* rand_ranf();
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }
  else if ( strcmp(law,"uin")==0) 
    {
      double a, b;
      if ( rhs != suite + 1) 
	{ Scierror("Error: Missing Low and High for Uniform integer law\n");return RET_BUG;}
      if (GetScalarDouble(stack,suite,&a) == FAIL) return RET_BUG;      
      if (GetScalarDouble(stack,suite+1,&b) == FAIL) return RET_BUG;      
      if ( a != floor(a) || b != floor(b) || (b-a+1) > 2147483561 || b < a )
	{
	  Scierror("Error: a and b must integers with (b-a+1) <= 2147483561 and b >= a\n");
	  return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++) M->R[i]= rand_ignuin(&a,&b);
      MoveObj(stack,1,(NspObject *) M);
      return 1;

    }
  else if ( strcmp(law,"lgi")==0) 
    {
      if ( rhs != suite -1 ) 
	{ 
	  Scierror("Error: only %d arguments required for 'lgi' option\n",suite-1);
	  return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++) M->R[i]= ignlgi();
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }
  else if ( strcmp(law,"prm")==0)
    {
      NspMatrix *prm;
      int nn,j;
      if ( suite != 3 || M->mn != 1)
	{ 
	  Scierror("Error: First argument for 'prm' option must be the number of random simulation \n");
	  return RET_BUG;
	}
      nn= M->R[0];
      if ( rhs != suite) {  Scierror("Error: Missing vect for random permutation\n");  return RET_BUG;}
      if ((prm = GetMat(stack,suite)) == NULLMAT) return RET_BUG;
      if ( prm->n != 1)	{ Scierror("Error: vect must be column vector\n");  return RET_BUG;}

      if ((M = nsp_matrix_create(NVOID,'r',prm->m,nn))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->n ; i++) 
	{
	  for (j=0; j < M->m ; j++ ) M->R[(M->m)*i+j]= prm->R[j];
	  rand_genprm( M->R + M->m*i,&M->m);
	}
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }
  else if ( strcmp(law,"nbn")==0) 
    {
      int iA;
      double B;
      if ( rhs != suite + 1) 
	{ Scierror("Error: Missing N and P for Negative Binomial law\n");return RET_BUG;}
      if (GetScalarInt(stack,suite,&iA) == FAIL) return RET_BUG;      
      if (GetScalarDouble(stack,suite+1,&B) == FAIL) return RET_BUG;      
      if ( B < 0.0 || B > 1.0 ) 
	{
	  Scierror("Error: P is not in [0,1] \n");
	  return RET_BUG;
	}
      if ( iA < 0 ) 
	{
	  Scierror("Error: N < 0 \n");
	  return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++)   M->R[i]= (double) rand_ignnbn(&iA,&B);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }
  else if ( strcmp(law,"bin")==0) 
    {
      int iA;
      double B;
      if ( rhs != suite + 1) 
	{ Scierror("Error: Missing N and P for Binomial law\n");return RET_BUG;}
      if (GetScalarInt(stack,suite,&iA) == FAIL) return RET_BUG;      
      if (GetScalarDouble(stack,suite+1,&B) == FAIL) return RET_BUG;      
      if ( B < 0.0 || B > 1.0 ) 
	{
	  Scierror("Error: P is not in [0,1] \n");
	  return RET_BUG;
	}
      if ( iA < 0 ) 
	{
	  Scierror("Error: N < 0 \n");  return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++)  M->R[i]= (double) rand_ignbin(&iA,&B);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }
  else if ( strcmp(law,"mn")==0) 
    {
      NspMatrix *Mean,*Cov,*Work,*Parm,*Res;
      int nn,ierr,mp;
      if ( suite != 3 || M->mn != 1)
	{ Scierror("Error: First argument for 'mn' option must be the number of random simulation \n");return RET_BUG;
	}
      nn= M->R[0];
      if ( rhs != suite + 1) 
	{ Scierror("Error: Missing Mean and Cov for Multivariate Normal law\n");return RET_BUG;}
      if ((Mean = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
      if ( Mean->n != 1) { Scierror("Error: Mean must be column vector\n");return RET_BUG;}
      if ((Cov = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
      if ( Cov->m != Cov->n ) { Scierror("Error: Cov must be a square matrix\n");return RET_BUG;}
      if ( Cov->m != Mean->m ) { Scierror("Error: Mean and Cov have incompatible dimensions\n");return RET_BUG;}

      if ( Cov->m <= 0 ) 
	{
	  Scierror("Error: Mean and Cov are of null size\n");
	  return RET_BUG;
	}

      if ((Res = nsp_matrix_create(NVOID,'r',Cov->m,nn))== NULLMAT) return RET_BUG;
      if ((Work = nsp_matrix_create(NVOID,'r',Cov->m,1))== NULLMAT) return RET_BUG;
      mp=Cov->m*(Cov->m+3)/2 + 1;
      if ((Parm = nsp_matrix_create(NVOID,'r',mp,1))== NULLMAT) return RET_BUG;

      rand_setgmn(Mean->R,Cov->R,&Cov->m,&Cov->n,Parm->R,&ierr);
      if ( ierr == 1) return RET_BUG;
      for ( i=0 ; i < nn ; i++) 
	{
	  rand_genmn(Parm->R,Res->R + Res->m*i,Work->R);
	}
      /* Destroy */
      nsp_matrix_destroy(Work);
      nsp_matrix_destroy(Parm);
      MoveObj(stack,1,(NspObject *) Res);
      return 1;
    }
  else if ( strcmp(law,"markov")==0) 
    {
      NspMatrix *P,*X0,*Res,*CumP;
      int nn,j,icur,jj;
      if ( suite != 3 || M->mn != 1)
	{ 
	  Scierror("Error: First argument for 'markov' option must be the number of random simulation \n");return RET_BUG;
	}
      nn= M->R[0];
      if ( rhs != suite +1 ) { Scierror("Error: Missing P matrix and X0 for Markov chain\n");return RET_BUG;}
      if ((P = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
      if ( P->m != P->n  && P->m != 1 ) 
	{ 
	  Scierror("Error: P must be a square matrix or a row vector\n");return RET_BUG;
	}
      /* Check that P is a Markov Matrix */
      for ( i= 0 ; i < P->m ; i++ )
	{
	  double ptot = 0.0;
	  for ( j = 0 ; j < P->n ; j++ )
	    {
	      if ( P->R[i+P->m*j] < 0 || P->R[i+P->m*j] > 1 )
		{
		  Scierror("Error: P(%d,%d)=%f is not in the range [0,1]\n",P->R[i+P->m*j],i+1,j+1);
		  return RET_BUG;
		}
	      ptot += P->R[i+P->m*j];
	    }
	  if ( ptot -1.0 > 1.e-6 ) 
	    {
	      Scierror("Error: Sum of P(%d,1:%d)=%f > 1 \n",i+1,P->n,ptot);
	      return RET_BUG;
	    }
	}

      if ((X0 = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
      for ( i = 0 ; i < X0->mn ; i++)
	if ( X0->R[i] -1 < 0 ||X0->R[i] -1 >= P->n ) 
	  {
	    Scierror("Error: X0(%d) must be in the range [1,%d]\n",i,P->n);
	    return RET_BUG;
	  }

      if ((Res = nsp_matrix_create(NVOID,'r',X0->mn,nn))== NULLMAT) return RET_BUG;
      
      if ((CumP = nsp_matrix_create(NVOID,'r',P->m,P->n+1))== NULLMAT) return RET_BUG;

      /** Computing the cumulative sum of the P matrix **/
      for ( i = 0 ; i < CumP->m ; i++) 
	{
	  double cumsum=0.0;
	  CumP->R[i] = cumsum;
	  for ( j= 1; j < CumP->n ; j++ ) 
	    {
	      cumsum += P->R[i + P->m*(j-1)];
	      CumP->R[i +CumP->m*j] = cumsum;
	    }
	}
      /* Now the simulation */
      for ( jj = 0 ; jj < X0->mn ; jj++) 
	{
	  icur = X0->R[jj]-1;
	  for ( i=0 ; i < Res->n ; i++) 
	    {
	      int niv=0;
	      double rr = rand_ranf();
	      if ( P->m == 1 ) icur =0;
	      while ( rr >= CumP->R[icur +CumP->m*niv] && niv < CumP->n ) niv++;
	      /** projection to avoid boundaries **/
	      niv = Max(Min(niv,P->n),1); 
	      Res->R[jj+Res->m*i]= niv ; 
	      icur=niv-1;
	    }
	}
      nsp_matrix_destroy(CumP);
      MoveObj(stack,1,(NspObject *) Res);
      return 1;
    }
  else if ( strcmp(law,"def")==0) 
    {
      if ( rhs != suite -1 ) 
	{ Scierror("Error: no argument required for 'def' option\n");return RET_BUG;}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++)   M->R[i]= rand_ranf();
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

  else if ( strcmp(law,"nch")==0) 
    {
      double A,B;
      if ( rhs != suite + 1) 
	{  Scierror("Error: Missing Df and Xnonc for non-central chi-square law\n");return RET_BUG;}
      if (GetScalarDouble(stack,suite,&A) == FAIL) return RET_BUG;      
      if (GetScalarDouble(stack,suite+1,&B) == FAIL) return RET_BUG;      
      if ( A < 1.0 || B < 0.0 )
	{
	  Scierror("Error: DF < 1 or XNONC < 0 \n");
	  return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++)   M->R[i]= rand_gennch(&A,&B);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }
  else if ( strcmp(law,"nf")==0) 
    {
      double A,B,C;
      if ( rhs != suite + 2) 
	{ 
	  Scierror("Error: Missing Dfn, Dfd and Xnonc for non-central F law\n");
	  return RET_BUG;
	}
      if (GetScalarDouble(stack,suite,&A) == FAIL) return RET_BUG;      
      if (GetScalarDouble(stack,suite+1,&B) == FAIL) return RET_BUG;      
      if (GetScalarDouble(stack,suite+2,&C) == FAIL) return RET_BUG;      
      if ( A < 1.0 || B < 0.0 || C < 0.0 ) 
	{
	  Scierror("Error: DF < 1.0 or DF <= 0.0 or Xnonc < 0.0 \n");
	  return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++)   M->R[i]= rand_gennf(&A,&B,&C);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

  else if ( strcmp(law,"chi")==0)
    {
      double A;
      if ( rhs != suite ) 
	{ Scierror("Error: Missing Df for chi-square law\n");
	return RET_BUG;
	}
      if (GetScalarDouble(stack,suite,&A) == FAIL) return RET_BUG;      
      if  ( A <= 0.0)
	{
	  Scierror("Error: Rand: DF <= 0 \n");return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++)   M->R[i]= rand_genchi(&A);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }
  else if ( strcmp(law,"poi")==0)
    {
      double A;
      if ( rhs != suite ) 
	{ 
	  Scierror("Error: Missing Av for Poisson law\n"); return RET_BUG; 
	}
      if (GetScalarDouble(stack,suite,&A) == FAIL) return RET_BUG;      
      if ( A < 0.0 )
	{
	  Scierror("Error: Av < 0 \n"); return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++)  M->R[i]= (double) rand_ignpoi(&A);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }
  else if ( strcmp(law,"geom")==0)
    {
      double p;
      if ( rhs != suite ) 
	{ 
	  Scierror("Error: Missing p for Geometric law\n");  return RET_BUG;
	}
      if (GetScalarDouble(stack,suite,&p) == FAIL) return RET_BUG;      
      if ( p < 1.3e-307 || p > 1 ) { Scierror("p must be in [pmin,1]\n");return RET_BUG;}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++)       M->R[i]= rand_igngeom(p);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

  else if ( strcmp(law,"exp")==0)
    {
      double A;
      if ( rhs != suite ) 
	{ 
	  Scierror("Error: Missing Av for exponential law\n");
	  return RET_BUG;
	}
      if (GetScalarDouble(stack,suite,&A) == FAIL) return RET_BUG;      
      if ( A < 0.0 ) 
	{
	  Scierror("Error: option 'exp' Av  < 0.0 !\n");
	  return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++)  M->R[i] = rand_genexp(&A);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }
  else 
    {
      Scierror("Error: %s wrong argument %s\n",NspFname(stack),law);
      return RET_BUG;
    }      
}

