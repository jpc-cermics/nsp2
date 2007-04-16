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
 */

#include <math.h>
#include <strings.h>
#include <nsp/machine.h>
#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"

/* external functions to be called through this interface */
#include "grand.h"
#include "basic_generators.h"

static int get_gen_from_name(char *str)
{
  int k;
  for ( k = 0 ; k < NbGenInNsp ; k++ )
    if ( strcmp(str, NspRNG[k]->name_gen) == 0 )
      return k;
  return -1;
}

static void display_gen_names()
{
  int k;
  for ( k = 0 ; k < NbGenInNsp-1 ; k++ )
    Sciprintf("%s, ",NspRNG[k]->name_gen);
  Sciprintf("%s\n",NspRNG[NbGenInNsp-1]->name_gen);
}

/*
 *  hand written interface for the librand 
 */

static int int_nsp_grand( Stack stack, int rhs, int opt, int lhs)
{ 
  char *law;
  NspMatrix *M=NULL;
  int minrhs, maxrhs = 10;
  int ResL,ResC,i,suite;

  if (rhs >= 1 &&  IsSMatObj(stack,1)) 
    {
      char *str;

      if ((str = GetString(stack,1)) == (char*)0) return RET_BUG;

      if (strcmp("getgen",str)==0) 
	{
	  int current_gen = nsp_get_current_gen();
	  if ( rhs != 1) 
	    {
	      Scierror("Error: rhs should be 1 for 'getgen' option\n");
	      return RET_BUG;
	    }
	  if ( nsp_move_string(stack,1,NspRNG[current_gen]->name_gen,-1)== FAIL) return RET_BUG;
	  return 1;
	}

      else if (strcmp("setgen",str)==0) 
	{
	  char *str1; int new_current_gen;
	  if ( rhs != 2) 
	    {
	      Scierror("Error: rhs should be 2 for 'setgen' option\n");
	      return RET_BUG;
	    }
	  if ((str1 = GetString(stack,2)) == (char*)0) return RET_BUG;
	  
	  if ( (new_current_gen =get_gen_from_name(str1)) == -1 )
	    {
	      Scierror("Error: unknown generator !\n");
	      Sciprintf("       choose among :");
	      display_gen_names();
	      return RET_BUG;
	    }
	  nsp_set_current_gen(new_current_gen);
	  return 0;
	}

      else if ( strcmp(str,"getsd")==0) 
	{
	  int current_gen = nsp_get_current_gen();
	  if ( rhs != 1) 
	    {
	      Scierror("%s: only one argument expected for grand('getsd')\n",NspFname(stack));
	      return RET_BUG;
	    }
	  
	  if ( (M =nsp_matrix_create(NVOID,'r',NspRNG[current_gen]->dim_state,1)) == NULLMAT) 
	    return RET_BUG;
	  NspRNG[current_gen]->get_state(M->R);
	  MoveObj(stack,1,(NspObject *) M);
	  return 1;
	}
      
      else if ( strcmp(str,"setsd")==0 ) 
	{
	  int current_gen = nsp_get_current_gen();
	  if ( rhs != 2 ) 
	    {
	      Scierror("Error: rhs should be 2 for 'setsd' option\n");
	      return RET_BUG;
	    }
	  
	  if ((M = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
	  if ( M->mn == 1 ) 
	    {
	      if ( NspRNG[current_gen]->set_state_simple(M->R[0]) == FAIL ) return RET_BUG;
	    }
	  else
	    {
	      CheckLength(NspFname(stack),2,M, NspRNG[current_gen]->dim_state);
	      if ( NspRNG[current_gen]->set_state(M->R) == FAIL ) return RET_BUG; 
	    }
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

      /* from now all the next options are only for the clcg4 generator */

      else if ( nsp_get_current_gen() == CLCG4 )
	{
	  if ( strcmp(str,"setall")==0 ) 
	    {
	      if ( rhs != 2 ) 
		{
		  Scierror("Error: rhs should be 2 for 'setall'  option\n");
		  return RET_BUG;
		}
	      if ( (M =GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
	      CheckLength(NspFname(stack),2,M, 4);
	      if ( set_initial_seed_clcg4(M->R) == FAIL ) return RET_BUG;
	      return 0;
	    }

	  else if (strcmp("initgn",str)==0) 
	    {
	      int i1;
	      SeedType Where;
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
	      init_generator_clcg4(Where);	  
	      return 0;
	    }

	  else if (strcmp("setcgn",str)==0) 
	    {
	      int i1;
	      if ( rhs != 2) 
		{
		  Scierror("Error: rhs should be 2 for 'setcgn' option\n");
		  return RET_BUG;
		}
	      if (GetScalarInt(stack,2,&i1) == FAIL) return RET_BUG;
	      if ( set_current_clcg4(i1) == FAIL ) return RET_BUG;
	      return 0;
	    }
	  else if (strcmp("advnst",str)==0) 
	    {
	      int i1;
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
	      advance_state_clcg4(i1);
	      return 0;
	    }

	  else if (strcmp("getcgn",str)==0) 
	    {
	      if ( rhs != 1) 
		{
		  Scierror("Error: rhs should be 1 for 'getcgn' option\n");
		  return RET_BUG;
		}
	      if ( nsp_move_double(stack,1,(double) get_current_clcg4())== FAIL) return RET_BUG;
	      return 1;
	    }
	  else 
	    {
	      Scierror("Error: %s Wrong first argument %s\n",NspFname(stack),str);
	      return RET_BUG;
	    }      
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

  if ( strcmp(law,"beta")==0) 
    {
      double A,B,minlog=1.e-37;
      if ( rhs != suite + 1) 
	{ Scierror("Error: Missing A and B for beta law\n");return RET_BUG;}
      if (GetScalarDouble(stack,suite,&A) == FAIL) return RET_BUG;      
      if (GetScalarDouble(stack,suite+1,&B) == FAIL) return RET_BUG;      
      if ( A < minlog || B < minlog)
	{
	  Scierror("Error:  grand(...,'beta',..): A or B < %f \n",minlog);
	  return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++)  M->R[i]= rand_beta(A,B);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

  if ( strcmp(law,"betav")==0) 
    {
      double A,B,minlog=1.e-37;
      if ( rhs != suite + 1) 
	{ Scierror("Error: Missing A and B for beta law\n");return RET_BUG;}
      if (GetScalarDouble(stack,suite,&A) == FAIL) return RET_BUG;      
      if (GetScalarDouble(stack,suite+1,&B) == FAIL) return RET_BUG;      
      if ( A < minlog || B < minlog)
	{
	  Scierror("Error:  grand(...,'beta',..): A or B < %f \n",minlog);
	  return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++)  M->R[i]= rand_sgamma(A);
      for ( i=0 ; i < M->mn ; i++)  M->R[i] /= (M->R[i] +  rand_sgamma(B));
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
      for ( i=0 ; i < M->mn ; i++)  M->R[i] = rand_genf(A,B);
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
	  Scierror("Error: grand(..'gam',A,R) : A (=%g) <= 0.0 or R (=%g) <= 0.0 \n",A,B); return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++) 
	{
	  /** WARNING : order is changed in parameters for 
	   *    compatibility between Rand(...'gam',..) and cdfgam 
	   **/
	  M->R[i]= rand_gengam(B,A);
	}
      MoveObj(stack,1,(NspObject *) M);
      return 1;

    }

  else if ( strcmp(law,"gamma")==0) 
    {
      double a;
      if ( rhs != suite) 
	{ Scierror("Error: Missing a for Gamma law\n");return RET_BUG;}
      if (GetScalarDouble(stack,suite,&a) == FAIL) return RET_BUG;      
      if ( a <= 0.0 )
	{
	  Scierror("Error: grand(..'gamma',a) : a (=%g) <= 0.0 \n",a); return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i]= rand_gamma(a);
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
      for ( i=0 ; i < M->mn ; i++) { M->R[i]= rand_gennor(A,B); }
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

  else if ( strcmp(law,"gauss")==0) 
    {
      double A,B;
      if ( rhs != suite + 1) 
	{ Scierror("Error: Missing Av and Sd for Normal law\n");return RET_BUG;}
      if (GetScalarDouble(stack,suite,&A) == FAIL) return RET_BUG;      
      if (GetScalarDouble(stack,suite+1,&B) == FAIL) return RET_BUG;      
      if ( B < 0 ) 
	{  Scierror("Error: SD < 0.0 \n");return RET_BUG;}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++) { M->R[i]= rand_nor(A,B); }
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
      int a, b;
      if ( rhs != suite + 1) 
	{ Scierror("Error: Missing Low and High for Uniform integer law\n");return RET_BUG;}
      if (GetScalarInt(stack,suite,&a) == FAIL) return RET_BUG;      
      if (GetScalarInt(stack,suite+1,&b) == FAIL) return RET_BUG;      
      if ( b < a  )  /* normally we must have also (b-a+1) <= Min RngMaxInt = 2147483561) */
	{
	  Scierror("Error: a and b must integers with a <= b\n");
	  return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++) M->R[i]= (double) rand_ignuin(a,b);
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
      for ( i=0 ; i < M->mn ; i++) M->R[i]= (double) rand_lgi();
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

  else if ( strcmp(law,"8bits")==0) 
    {
      if ( rhs != suite -1 ) 
	{ 
	  Scierror("Error: only %d arguments required for '8bits' option\n",suite-1);
	  return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++) M->R[i]= (double) (rand_lgi() & 0xff);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

  else if ( strcmp(law,"prm")==0)
    {
      NspMatrix *prm;
      double *col_j;
      int nn,j;
      if ( suite != 3 || M->mn != 1)
	{ 
	  Scierror("Error: First argument for 'prm' option must be the number of random simulation \n");
	  return RET_BUG;
	}
      nn = M->R[0];
      if ( rhs != suite) {  Scierror("Error: Missing vect for random permutation\n");  return RET_BUG;}
      if ((prm = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
      if ( prm->n != 1)	{ Scierror("Error: vect must be column vector\n");  return RET_BUG;}

      if ((M = nsp_matrix_create(NVOID,'r',prm->m,nn))== NULLMAT) return RET_BUG;
      for ( j = 0 ; j < M->n ; j++) 
	{
	  col_j = M->R + M->m*j;
	  memcpy(col_j, prm->R, sizeof(double)*M->m);
	  rand_genprm(col_j, M->m);
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
      for ( i=0 ; i < M->mn ; i++)   M->R[i]= (double) rand_ignnbn(iA,B);
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

  else if ( strcmp(law,"binv")==0) 
    {
      NspMatrix *NN, *pp;
      int incN, incp, N;
      double p;
      if ( rhs != suite + 1) 
	{ Scierror("Error: Missing N and p for Binomial distribution\n");return RET_BUG;}
      if ( (NN=GetRealMat(stack,suite)) == NULLMAT ) return RET_BUG;      
      if ( NN->mn == 1 ) 
	incN = 0;
      else
        { incN = 1 ; CheckDims(NspFname(stack),suite,NN,ResL,ResC); }
      if ( (pp=GetRealMat(stack,suite+1)) == NULLMAT ) return RET_BUG;
      if ( pp->mn == 1 ) 
	incp = 0;
      else
        { incp = 1 ; CheckDims(NspFname(stack),suite+1,pp,ResL,ResC); }

      if ( (M=nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT ) return RET_BUG;

      for ( i=0 ; i < M->mn ; i++)  
	{
	  if ( i == 0 || incN == 1)
	    {
	      N = (int) NN->R[i];
	      if ( N < 0 ) 
		{
		  Scierror("Error: N < 0 \n"); nsp_matrix_destroy(M); return RET_BUG;
		}
	    }
	  if ( i == 0 || incp == 1)
	    {
	      p =  pp->R[i];
	      if ( p < 0.0 || p > 1.0 ) 
		{
		  Scierror("Error: p is not in [0,1] \n"); nsp_matrix_destroy(M); return RET_BUG;
		}
	    }
	  M->R[i]= (double) rand_ignbin(&N,&p);
	}
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

  else if ( strcmp(law,"bin_trd")==0) 
    {
      int N;
      double p;
      if ( rhs != suite + 1) 
	{ Scierror("Error: Missing N and p for Binomial law\n");return RET_BUG;}
      if (GetScalarInt(stack,suite,&N) == FAIL) return RET_BUG;      
      if (GetScalarDouble(stack,suite+1,&p) == FAIL) return RET_BUG;      
      if ( p < 0.0 || p > 1.0 )
	{
	  Scierror("Error: p is not in [0,1] \n");
	  return RET_BUG;
	}
      if ( N < 0 )
	{
	  Scierror("Error: N < 0 \n");  return RET_BUG;
	}
      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
      for ( i=0 ; i < M->mn ; i++)  M->R[i]= (double) bin_trd(N, p);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

  else if ( strcmp(law,"bin_trdv")==0) 
    {
      NspMatrix *NN, *pp;
      int incN, incp, N;
      double p;
      if ( rhs != suite + 1) 
	{ Scierror("Error: Missing N and p for Binomial distribution\n");return RET_BUG;}
      if ( (NN=GetRealMat(stack,suite)) == NULLMAT ) return RET_BUG;      
      if ( NN->mn == 1 ) 
	incN = 0;
      else
        { incN = 1 ; CheckDims(NspFname(stack),suite,NN,ResL,ResC); }
      if ( (pp=GetRealMat(stack,suite+1)) == NULLMAT ) return RET_BUG;
      if ( pp->mn == 1 ) 
	incp = 0;
      else
        { incp = 1 ; CheckDims(NspFname(stack),suite+1,pp,ResL,ResC); }

      if ( (M=nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT ) return RET_BUG;

      for ( i=0 ; i < M->mn ; i++)  
	{
	  if ( i == 0 || incN == 1)
	    {
	      N = (int) NN->R[i];
	      if ( N < 0 ) 
		{
		  Scierror("Error: N < 0 \n"); nsp_matrix_destroy(M); return RET_BUG;
		}
	    }
	  if ( i == 0 || incp == 1)
	    {
	      p =  pp->R[i];
	      if ( p < 0.0 || p > 1.0 ) 
		{
		  Scierror("Error: p is not in [0,1] \n"); nsp_matrix_destroy(M); return RET_BUG;
		}
	    }
	  M->R[i]= (double) bin_trd(N, p);
	}
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
      for ( i=0 ; i < M->mn ; i++)   M->R[i]= rand_genchi(A);
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
      for ( i=0 ; i < M->mn ; i++)  M->R[i]= (double) rand_ignpoi(A);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

  else if ( strcmp(law,"pois")==0)
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
      for ( i=0 ; i < M->mn ; i++)  M->R[i]= (double) poi_trd(A);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

  else if ( strcmp(law,"poisv")==0)
    {
      NspMatrix *Av;
      double A;
      int inc;
      if ( rhs != suite ) 
	{ 
	  Scierror("Error: Missing Av for Poisson law\n"); return RET_BUG; 
	}
      if ( (Av=GetRealMat(stack,suite)) == NULLMAT ) return RET_BUG;      
      if ( Av->mn == 1 ) 
	inc = 0;
      else
        { inc = 1 ; CheckDims(NspFname(stack),suite,Av,ResL,ResC); }

      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;

      for ( i=0 ; i < M->mn ; i++)  
	{
	  if ( i == 0 || inc == 1)
	    {
	      A = Av->R[i];
	      if ( A < 0.0 )
		{
		  nsp_matrix_destroy(M);
		  Scierror("Error: Av < 0 \n"); 
		  return RET_BUG;
		}
	    }
	  M->R[i]= (double) poi_trd(A);
	}

      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

  else if ( strcmp(law,"poiv")==0)
    {
      NspMatrix *Av;
      double A;
      int inc;
      if ( rhs != suite ) 
	{ 
	  Scierror("Error: Missing Av for Poisson law\n"); return RET_BUG; 
	}
      if ( (Av=GetRealMat(stack,suite)) == NULLMAT ) return RET_BUG;      
      if ( Av->mn == 1 ) 
	inc = 0;
      else
        { inc = 1 ; CheckDims(NspFname(stack),suite,Av,ResL,ResC); }

      if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;

      for ( i=0 ; i < M->mn ; i++)  
	{
	  if ( i == 0 || inc == 1)
	    {
	      A = Av->R[i];
	      if ( A < 0.0 )
		{
		  nsp_matrix_destroy(M);
		  Scierror("Error: Av < 0 \n"); 
		  return RET_BUG;
		}
	    }
	  M->R[i]= (double) rand_ignpoi(A);
	}

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
      for ( i=0 ; i < M->mn ; i++)       M->R[i]= (double) rand_igngeom(p);
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
      for ( i=0 ; i < M->mn ; i++)  M->R[i] = rand_genexp(A);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

  else if ( strcmp(law,"expo")==0)
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
      for ( i=0 ; i < M->mn ; i++)  M->R[i] = rand_exp(A);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }
  else 
    {
      Scierror("Error: %s wrong argument %s\n",NspFname(stack),law);
      return RET_BUG;
    }      
}

static int int_nsp_randn(Stack stack, int rhs, int opt, int lhs)
{
  int m, n, k;
  NspMatrix *A;
  CheckRhs (0, 2);
  CheckLhs (1, 1);

  switch (rhs)
    {
    case 0:
      m = 1; n = 1;
      break;
    case 1:
      if (! IsMatObj (stack, 1) )
	{
	  Scierror("Error: %s when used with one arg this one must be a matrix\n",NspFname(stack));
	  return RET_BUG;
	}      
      m = nsp_object_get_size (NthObj (1), 1);
      n = nsp_object_get_size (NthObj (1), 2);
      break;
    case 2:
      if (GetScalarInt(stack,1,&m) == FAIL) return RET_BUG;
      CheckNonNegative(NspFname(stack),m,1);
      if (GetScalarInt(stack,2,&n) == FAIL) return RET_BUG;
      CheckNonNegative(NspFname(stack),n,2);
    }

  if ( (A  =nsp_matrix_create(NVOID,'r',m, n)) == NULLMAT )
    return RET_BUG;

  for ( k = 0 ; k < A->mn ; k++ )
    A->R[k] = rand_nor_core();

  MoveObj(stack,1,(NspObject *) A);
  return 1;
}
  

static int int_nsp_rand_discrete(Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *M=NULLMAT, *p=NULLMAT, *q=NULLMAT;
  NspBMatrix *k=NULLBMAT;
  NspSMatrix *S=NULLSMAT;
  NspList *L;
  Cell *C;
  char *meth=NULL, *default_meth="guide";
  nsp_option opts[] ={{"meth",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckRhs (2, 3);
  CheckLhs (1, 1);

  if ( IsSMatObj(stack,1) )  /* rand_discrete("setup", p, meth=) */
    {
      char *str;
      if ( (str = GetString(stack,1)) == NULL ) 
	return RET_BUG;
      if ( strcmp(str,"setup")==0) 
	{
	  if ( rhs-opt != 2) 
	    {
	      Scierror("%s: two non optional arguments expected\n",NspFname(stack));
	      return RET_BUG;
	    }
	 
	  if ( (p = GetRealMat(stack,2)) == NULLMAT )  
	    return RET_BUG;
	  CheckVector(NspFname(stack), 2, p);
	  
	  if ( get_optional_args(stack, rhs, opt, opts, &meth) == FAIL )
	    return RET_BUG;

	  if ( meth == NULL )
	    meth = default_meth;
	  else
	    if ( strcmp(meth,default_meth)!=0 && strcmp(meth,"alias")!=0 )
	      {
		Scierror("%s: bad optional argument meth (must be alias or guide)\n",NspFname(stack));
		return RET_BUG;
	      }

	  if ( (q  = nsp_matrix_create("lel",'r',p->mn+1,1)) == NULLMAT )
	    return RET_BUG;

	  if ( (k  = nsp_bmatrix_create("lel",p->mn,1)) == NULLBMAT )
	    {
	      nsp_matrix_destroy(q);
	      return RET_BUG;
	    }

	  if ( strcmp(meth,default_meth) == 0 )
	    {
	      if ( nsp_guide_table_method(p->R, q->R, (int *) k->B, p->mn) == FAIL )
		goto err1;
	      if ( (S=nsp_smatrix_create("lel", 1, 1,"rd_guide", 1)) == NULLSMAT )
		goto err2;
	    }
	  else
	    {
	      if ( nsp_alias_method(p->R, q->R, (int *) k->B, p->mn) == FAIL )
		goto err1;
	      if ( (S=nsp_smatrix_create("lel", 1, 1,"rd_alias", 1)) == NULLSMAT )
		goto err2;
	    }

	  /* Put the objects into a list. The first element (a string) lets *
           * to decide between the guide code or the alias code.            */
	  if ((L =nsp_list_create(NVOID))==NULLLIST) 
	    goto err2;
	  
	  if ( nsp_list_end_insert(L, (NspObject *) S) == FAIL )
	    goto err3;
	  if ( nsp_list_end_insert(L, (NspObject *) q) == FAIL )
	    goto err3;
	  if ( nsp_list_end_insert(L, (NspObject *) k) == FAIL )
	    goto err3;

	  MoveObj(stack,1,(NspObject *) L);
	  return 1;
	}
      else
	{
	  Scierror("%s: uncorrect first argument\n",NspFname(stack));
	  return RET_BUG;
	}
    }
  else
    {
      int suite, i, m, n;
      if ( rhs == 2 ) /* rand_discrete(Mat, L) */
	{      
	  if ((M = GetMat(stack,1)) == NULLMAT) return RET_BUG;
	  m=M->m; n=M->n;
	  suite = 2;
	}
      else            /* rand_discrete(m, n, L) */
	{
	  if ( GetScalarInt(stack, 1, &m) == FAIL ) return RET_BUG;      
	  CheckNonNegative(NspFname(stack), m, 1);
	  if ( GetScalarInt(stack, 2, &n) == FAIL ) return RET_BUG;
	  CheckNonNegative(NspFname(stack), n, 2);
	  suite = 3;
	}
      
      if ( (L = GetList(stack, suite)) == NULLLIST ) return RET_BUG;

      if ( nsp_list_length(L) != 3 )
	{
	  Scierror("%s: the list must have 3 elements\n",NspFname(stack));
	  return RET_BUG;
	}

      C = L->first;
      S = (NspSMatrix *) C->O;
      C = C->next;
      q = (NspMatrix *) C->O;
      C = C->next;
      k = (NspBMatrix *) C->O;
	  
      if ( (M  = nsp_matrix_create(NVOID,'r', m, n)) == NULLMAT )
	return RET_BUG;

      if ( strcmp(S->S[0],"rd_guide") == 0 )
	for ( i = 0 ; i < M->mn ; i++ )
	  M->R[i] = 1.0 + (double) nsp_rand_discrete_guide(q->R, (int *)k->B, k->mn);
      else if ( strcmp(S->S[0],"rd_alias") == 0 )
	for ( i = 0 ; i < M->mn ; i++ )
	  M->R[i] = 1.0 + (double) nsp_rand_discrete_alias(q->R, (int *)k->B, k->mn);
      else
	{
	  Scierror("%s: the list has not the good type\n",NspFname(stack));
	  return RET_BUG;
	}

      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

 err1:
  Scierror("%s: uncorrect probability vector \n",NspFname(stack));
 err2:
  nsp_matrix_destroy(q); nsp_bmatrix_destroy(k); nsp_smatrix_destroy(S);
  return RET_BUG;
 err3:
  nsp_matrix_destroy(q); nsp_bmatrix_destroy(k); nsp_smatrix_destroy(S);
  nsp_list_destroy_bis(L);
  return RET_BUG;
}

static OpTab Random_func[]={
  {"grand", int_nsp_grand},
  /*     {"rand", int_nsp_rand}, */
  {"randn", int_nsp_randn},
  {"rand_discrete", int_nsp_rand_discrete},
  {(char *) 0, NULL}
};

int Random_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Random_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 * (for adding or removing functions) 
 */

void Random_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Random_func[i].name;
  *f = Random_func[i].fonc;
}
