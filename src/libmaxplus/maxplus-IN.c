/* 
 * Maxplus interface 
 * Copyright (C) 2005 Gaubert, Quadrat, Chancelier Inria/Enpc 
 *
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
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "nsp/interf.h"
#include "maxplus.h"

static int int_maxp_howard(Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *Ij,*a,*Chi,*V,*Pi,*NIterations,*NComponents;
  int i,nnodes,ncolumns=1,verbosemode=0,errorflag;
  int *ij=NULL;

  CheckRhs(3,3);
  CheckLhs(5,5);

  if ((Ij = GetRealMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  if ((Ij = Mat2int(Ij))  == NULLMAT)  return RET_BUG;
  ij = (int *) Ij->R;
  /* in Howard... matrices are numbered starting from zero */
  for( i=0 ; i < Ij->mn ; i++) ij[i]--;

  if ((a = GetRealMatCopy(stack,2)) == NULLMAT)  return RET_BUG;
  if ( GetScalarInt(stack,3,&nnodes) == FAIL) return RET_BUG;
  if ((Chi = nsp_matrix_create(NVOID,'r',nnodes,ncolumns)) == NULLMAT) return RET_BUG;
  if ((V = nsp_matrix_create(NVOID,'r',nnodes,ncolumns)) == NULLMAT) return RET_BUG;
  if ((Pi = nsp_matrix_create(NVOID,'r',nnodes,ncolumns)) == NULLMAT) return RET_BUG;
  if ((NIterations = nsp_matrix_create(NVOID,'r',ncolumns,ncolumns)) == NULLMAT) return RET_BUG;
  if ((NComponents = nsp_matrix_create(NVOID,'r',ncolumns,ncolumns)) == NULLMAT) return RET_BUG;
  /* these three matrices are used as int * */
  Pi->convert = 'i'; NIterations->convert = 'i' ; NComponents->convert = 'i';

  errorflag =Howard(ij,a->R,nnodes,a->m,Chi->R,V->R,(int *) Pi->R,
		    (int *) NIterations->R,(int *) NComponents->R,verbosemode);
  switch (errorflag)
    {
    case 1:
      sciprint("Error in Howard: input matrix has one empty row");
      return RET_BUG;
    case 3:
      sciprint("Error in Howard: number of nodes must be a positive integer");
      return RET_BUG;
    case 4:
      sciprint("Error in Howard: number of arcs must be a positive integer");
      return RET_BUG;
    case 5:
      sciprint("AARRGH... error in Howard: maximal number of iterations is reached\n");
      return RET_BUG;
    }

  NthObj(4) = NSP_OBJECT(Chi) ;  
  NthObj(5) = NSP_OBJECT(V) ;  
  NthObj(6) = NSP_OBJECT(Pi) ; 
  NthObj(7) = NSP_OBJECT(NComponents) ; 
  NthObj(8) = NSP_OBJECT(NIterations) ; 

  for ( i = 0 ; i < lhs ; i++) NthObj(4+i)->ret_pos = i+1;
  return lhs;
}

/* HOWARD ALGORITHM FOR DETERMINISTIC SEMI MARKOV PROCESSES */

static int int_maxp_semi_howard(Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *Ij,*a,*Chi,*V,*Pi,*NIterations,*NComponents,*T;
  int *ij=NULL,i,nnodes,ncolumns=1,verbosemode=0,errorflag;
  CheckRhs(4,4);
  CheckLhs(5,5);

  if ((Ij = GetRealMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  if ((Ij = Mat2int(Ij))  == NULLMAT)  return RET_BUG;
  ij = (int *) Ij->R;
  /* in Howard... matrices are numbered starting from zero */
  for( i=0 ; i < Ij->mn ; i++) ij[i]--;

  if ((a = GetRealMatCopy(stack,2)) == NULLMAT)  return RET_BUG;
  if ( GetScalarInt(stack,3,&nnodes) == FAIL) return RET_BUG;
  if ((T = GetRealMatCopy(stack,4)) == NULLMAT)  return RET_BUG;

  if ((Chi = nsp_matrix_create(NVOID,'r',nnodes,ncolumns)) == NULLMAT) return RET_BUG;
  if ((V = nsp_matrix_create(NVOID,'r',nnodes,ncolumns)) == NULLMAT) return RET_BUG;
  if ((Pi = nsp_matrix_create(NVOID,'r',nnodes,ncolumns)) == NULLMAT) return RET_BUG;
  if ((NIterations = nsp_matrix_create(NVOID,'r',ncolumns,ncolumns)) == NULLMAT) return RET_BUG;
  if ((NComponents = nsp_matrix_create(NVOID,'r',ncolumns,ncolumns)) == NULLMAT) return RET_BUG;
  /* these three matrices are used as int * */
  Pi->convert = 'i'; NIterations->convert = 'i' ; NComponents->convert = 'i';

  errorflag =Semi_Howard(ij,a->R,T->R,nnodes,a->m,Chi->R,V->R,(int *) Pi->R,
			 (int *) NIterations->R,(int *) NComponents->R,verbosemode);
  switch (errorflag)
    {
    case 1:
      sciprint("Error in Semi Howard: input matrix has one empty row");
      return RET_BUG;
    case 2: 
      sciprint("Error in Semi Howard: weight matrix has a circuit with non positive weight");
      return RET_BUG;
    case 3:
      sciprint("Error in Semi Howard: number of nodes must be a positive integer");
      return RET_BUG;
    case 4:
      sciprint("Error in Semi Howard: number of arcs must be a positive integer");
      return RET_BUG;
    case 5:
      sciprint("AARRGH... Semi error in Howard: maximal number of iterations is reached\n");
      return RET_BUG;
    }

  NthObj(5) = NSP_OBJECT(Chi) ;  
  NthObj(6) = NSP_OBJECT(V) ;  
  NthObj(7) = NSP_OBJECT(Pi) ; 
  NthObj(8) = NSP_OBJECT(NComponents) ; 
  NthObj(9) = NSP_OBJECT(NIterations) ; 

  for ( i = 0 ; i < lhs ; i++) NthObj(5+i)->ret_pos = i+1;
  return lhs;

}


/*
 * version for a standard Matrix 
 */

static int int_maxp_karp(Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *A;
  double res;
  int entry=1;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((A = GetRealMat(stack,1)) == NULLMAT)  return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ( GetScalarInt(stack,2,&entry) == FAIL) return RET_BUG;
    }
  if ( maxplus_matrix_karp(A,entry-1,&res) == FAIL)
    {
      return RET_BUG;
    }
  if ( nsp_move_double(stack,1,res )== FAIL) return RET_BUG;
  return 1;
}

/*
 * version for a sparse Matrix 
 */

static int int_maxp_sp_karp(Stack stack, int rhs, int opt, int lhs)
{ 
  NspSpMatrix *A;
  double res;
  int entry=1;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((A = GetRealSp(stack,1)) == NULLSP)  return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ( GetScalarInt(stack,2,&entry) == FAIL) return RET_BUG;
    }
  if ( maxplus_spmatrix_karp(A,entry-1,&res) == FAIL) return RET_BUG;
  if ( nsp_move_double(stack,1,res )== FAIL) return RET_BUG;
  return 1;
}


static int int_maxp_sparse2full(Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *Ij,*a,*Res;
  int *ij=NULL,i,I, nnodes,narcs;
  CheckRhs(3,3);
  CheckLhs(1,1);

  if ((Ij = GetRealMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  if ((Ij = Mat2int(Ij))  == NULLMAT)  return RET_BUG;
  ij = (int *) Ij->R;
  /* in Howard... matrices are numbered starting from zero */
  if ((a = GetRealMatCopy(stack,2)) == NULLMAT)  return RET_BUG;
  if ( GetScalarInt(stack,3,&nnodes) == FAIL) return RET_BUG;
  narcs=a->m;
  if ((Res = nsp_matrix_create(NVOID,'r',nnodes,nnodes)) == NULLMAT) return RET_BUG;
  for (I=0; I< Res->mn; I++ ) Res->R[i] = -HUGE_VAL;
  for (I=0; I< narcs; I++)
    {
       Res->R[(ij[2*I]-1)+ (ij[2*I+1]-1)*nnodes]=a->R[I];
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


static int int_maxp_in_span(Stack stack, int rhs, int opt, int lhs)
{ 
  int is ; 
  double precision ;
  NspMatrix *A,*B;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetRealMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  if ((B = GetRealMatCopy(stack,2)) == NULLMAT)  return RET_BUG;
  if ( GetScalarDouble(stack,3,&precision)  == FAIL)  return RET_BUG;
  is =in_span(A->R,A->m,A->n,B->R,precision);
  if ( nsp_move_double(stack,1,(double)is) == FAIL) return RET_BUG;
  return 1;
}


static int int_maxp_weakbasis(Stack stack, int rhs, int opt, int lhs)
{ 
  int q,i;
  double *S;
  double precision; 

  NspMatrix *A,*Res;
  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((A = GetRealMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  if ((GetScalarDouble(stack,2,&precision)) == FAIL)  return RET_BUG;
  weakbasis(A->R,A->m,A->n,&S,&q,precision);
  if ((Res = nsp_matrix_create(NVOID,'r',A->m,q)) == NULLMAT) return RET_BUG;
  for (i=0; i< Res->mn; i++) Res->R[i]= S[i];
  MoveObj(stack,1,(NspObject *) Res);
  free(S);
  return 1;
}


static int int_maxp_weakbasis2(Stack stack, int rhs, int opt, int lhs)
{ 
  int q,i;
  double *S;
  double precision; 

  NspMatrix *A,*Res;
  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((A = GetRealMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  if ((GetScalarDouble(stack,2,&precision)) == FAIL)  return RET_BUG;
  weakbasis2(A->R,A->m,A->n,&S,&q,precision);

  if ((Res = nsp_matrix_create(NVOID,'r',A->m,q)) == NULLMAT) return RET_BUG;
  for (i=0; i< Res->mn; i++) Res->R[i]= S[i];
  MoveObj(stack,1,(NspObject *) Res);
  free(S);
  return 1;
}



static int int_maxp_include_span(Stack stack, int rhs, int opt, int lhs)
{ 
  int is ; 
  double precision ;
  NspMatrix *A,*B;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetRealMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  if ((B = GetRealMatCopy(stack,2)) == NULLMAT)  return RET_BUG;
  if ((GetScalarDouble(stack,3,&precision)) == FAIL)  return RET_BUG;
  CheckDimProp(stack.fname,1,2, A->m != B->m );
  is =include_span(A->R,A->m,A->n,B->R,B->n,precision);
  if ( nsp_move_double(stack,1,(double)is) == FAIL) return RET_BUG;
  return 1;
}


static int int_maxp_product(Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *A,*B,*Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRealMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  if ((B = GetRealMatCopy(stack,2)) == NULLMAT)  return RET_BUG;
  CheckDimProp(stack.fname,1,2, A->n != B->m );
  if ((Res = nsp_matrix_create(NVOID,'r',A->m,B->n)) == NULLMAT) return RET_BUG;
  product(A->R,A->m,A->n,B->R,B->n,Res->R);
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}



static int int_maxp_rowbasis(Stack stack, int rhs, int opt, int lhs)
{ 
  int q,i;
  double *U;
  double precision; 
  NspMatrix *A,*B,*Res;

  CheckRhs(3,3);
  CheckLhs(1,1);

  if ((A = GetRealMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  if ((B = GetRealMatCopy(stack,2)) == NULLMAT)  return RET_BUG;
  if ((GetScalarDouble(stack,3,&precision)) == FAIL)  return RET_BUG;

  rowbasis(A->R,A->n,B->R,&U,&q,precision);
  if ((Res = nsp_matrix_create(NVOID,'r',A->n,q)) == NULLMAT) return RET_BUG;
  for (i=0; i < Res->mn; i++) Res->R[i]= U[i];
  MoveObj(stack,1,(NspObject *) Res);
  free(U);
  return 1;
}



static int int_maxp_solve2(Stack stack, int rhs, int opt, int lhs)
{ 
  int q,i;
  double *S;
  double precision; 

  NspMatrix *A,*B,*Res;
  CheckRhs(3,3);
  CheckLhs(1,1);

  if ((A = GetRealMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  if ((B = GetRealMatCopy(stack,2)) == NULLMAT)  return RET_BUG;
  if ((GetScalarDouble(stack,3,&precision)) == FAIL)  return RET_BUG;

  q=maxp_solve2(A->R,A->m,A->n,B->R,&S,precision);
  if ((Res = nsp_matrix_create(NVOID,'r',A->n,q)) == NULLMAT) return RET_BUG;
  for (i=0; i< Res->mn; i++) Res->R[i]= S[i];
  MoveObj(stack,1,(NspObject *) Res);
  free(S);
  return 1;
}



static int int_maxp_solve3(Stack stack, int rhs, int opt, int lhs)
{ 
  int q,i;
  double *S;
  double precision; 

  NspMatrix *A,*B, *Res;
  CheckRhs(3,3);
  CheckLhs(1,1);

  if ((A = GetRealMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  if ((B = GetRealMatCopy(stack,2)) == NULLMAT)  return RET_BUG;
  if ((GetScalarDouble(stack,3,&precision)) == FAIL)  return RET_BUG;

  q=maxp_solve3(A->R,A->m,A->n,B->R,&S,precision);
  if ((Res = nsp_matrix_create(NVOID,'r',A->n,q)) == NULLMAT) return RET_BUG;
  for (i=0; i< Res->mn; i++) Res->R[i]= S[i];
  MoveObj(stack,1,(NspObject *) Res);
  free(S);
  return 1;
}



static int int_maxp_star(Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *A,*Res;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetRealMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  CheckSquare(stack.fname,1,A);
  if ((Res = nsp_matrix_create(NVOID,'r',A->m,A->n)) == NULLMAT) return RET_BUG;
  matrix_star(A->R,A->m,Res->R);
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


static int int_maxp_plus(Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *A,*Res;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetRealMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  CheckSquare(stack.fname,1,A);
  if ((Res = nsp_matrix_create(NVOID,'r',A->m,A->n)) == NULLMAT) return RET_BUG;
  matrix_plus(A->R,A->m,Res->R);
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}



static int int_maxp_ford_bellman(Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *Ij,*a,*A,*Pi,*B;
  int i,nnodes,ncolumns=1,errorflag, *ij=NULL, *pi=NULL,entry=0;

  CheckRhs(4,4);
  CheckLhs(1,3);

  if ((Ij = GetRealMatCopy(stack,1)) == NULLMAT)  return RET_BUG;
  if ((Ij = Mat2int(Ij))  == NULLMAT)  return RET_BUG;
  ij = (int *) Ij->R;
  /* in Howard... matrices are numbered starting from zero */
  for( i=0 ; i < Ij->mn ; i++) ij[i]--;

  if ((a = GetRealMatCopy(stack,2)) == NULLMAT)  return RET_BUG;
  if ( GetScalarInt(stack,3,&nnodes) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,4,&entry) == FAIL) return RET_BUG;

  if ((A = nsp_matrix_create(NVOID,'r',nnodes,ncolumns)) == NULLMAT) return RET_BUG;
  if ((Pi = nsp_matrix_create(NVOID,'r',nnodes,ncolumns)) == NULLMAT) return RET_BUG;
  if ((B = nsp_matrix_create(NVOID,'r',ncolumns,ncolumns)) == NULLMAT) return RET_BUG;

  /* these three matrices are used as int * */
  Pi->convert = 'i'; B->convert = 'i' ;  pi = (int *) Pi->R;

  errorflag =FordBellman(ij,a->R,nnodes,a->n,entry-1,A->R,pi,(int *) B->R);

  switch (errorflag)
    {
    case 2:
      sciprint("Error in FordBellman: initial point is out of range");
      return RET_BUG;
    case 1:
      sciprint("Error in FordBellman: the digraph has a circuit with positive weight");
      return RET_BUG;
    }
  for (i=0 ; i < Pi->mn ; i++) pi[i]++;

  NthObj(5) = NSP_OBJECT(A) ;  
  NthObj(6) = NSP_OBJECT(Pi) ; 
  NthObj(7) = NSP_OBJECT(B) ; 

  for ( i = 0 ; i < lhs ; i++) NthObj(5+i)->ret_pos = i+1;
  return lhs;
}


/*
 * interface 
 */

static OpTab Maxplus_func[]={
  {"hhoward",      int_maxp_howard},	
  {"hsemihoward",  int_maxp_semi_howard},	
  {"karp_m",	   int_maxp_karp},		
  {"karp_sp",      int_maxp_sp_karp},
  {"Max_Full",	   int_maxp_sparse2full},	
  {"hin_span",	   int_maxp_in_span},	
  {"hweakbasis",   int_maxp_weakbasis},	
  {"hinclude_span",int_maxp_include_span},  	
  {"hproduct",	   int_maxp_product},	    	
  {"hrowbasis",	   int_maxp_rowbasis},	    
  {"hsolve",	   int_maxp_solve2},	    
  {"hsolve3",	   int_maxp_solve3},	    
  {"hweakbasis2",  int_maxp_weakbasis2},    
  {"hstar",	   int_maxp_star},	    	
  {"hplus",	   int_maxp_plus},	    	
  {"hFordBellman", int_maxp_ford_bellman},  	
  {(char *) 0, NULL}
};

int Maxplus_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Maxplus_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void Maxplus_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Maxplus_func[i].name;
  *f = Maxplus_func[i].fonc;
}




