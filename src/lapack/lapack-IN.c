/* 
 * interface of lapack for Nsp 
 * Copyright (C) 2005 Jean-Philippe Chancelier
 * Copyright (C) 2005 Bruno Pinçon
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

#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <strings.h>
#include <nsp/machine.h>
#include <nsp/math.h>
#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"
#include "nsp/approx.h"
#include "nsp/nsp_lapack.h"

/*
 * interface for nsp_qr 
 */

static int int_qr( Stack stack, int rhs, int opt, int lhs)
{ 
  char *mode = NULL,cmode ;
  double tol = 0, *Tol=NULL;
  NspMatrix *A;
  NspMatrix *Q=NULL, *R=NULL, *E=NULL, *rank=NULL, *sval;
  NspMatrix **hE=NULL,**hrank=NULL,**hsval=NULL;
  int_types T[] = {mat,new_opts,t_end} ;
  nsp_option opts[] ={{ "tol",s_double,NULLOBJ,-1},
		      { "mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&A,&opts,&tol,&mode) == FAIL) return RET_BUG;
  
  Tol = ( opts[0].obj == NULLOBJ) ? NULL : &tol; 
  cmode = ( opts[1].obj == NULLOBJ) ? 'x' : mode[0]; 
  if ( cmode != 'x' && cmode != 'e' ) 
    {
      Scierror("%s: mode should be 'x' or 'e' \n",NspFname(stack));
      return RET_BUG;
    }
  CheckLhs(1,5);
  if ( lhs >= 3) { hE= &E;}
  if ( lhs >= 4) { hrank= &rank;}
  if ( lhs >= 5) { hsval= &sval;}
  if ( nsp_qr(A,&Q,&R,hE,hrank,hsval,Tol,cmode)== FAIL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(Q));
  if ( lhs >= 2 ) MoveObj(stack,2,NSP_OBJECT(R));
  if ( lhs >= 3 ) MoveObj(stack,3,NSP_OBJECT(E));
  if ( lhs >= 4 ) MoveObj(stack,4,NSP_OBJECT(rank));
  if ( lhs >= 5 ) MoveObj(stack,5,NSP_OBJECT(sval));

  return Max(lhs,1);
}


/*
 * interface for lsq : prévoir le choix de la méthode (qr ou svd) ?
 */
static int int_lsq( Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *A, *B, *Rank;
  int rank;
  double tol, *Tol=NULL;
  int_types T[] = {matcopy,matcopy,new_opts,t_end} ;
  nsp_option opts[] ={{ "tol",s_double,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&A,&B,&opts,&tol) == FAIL ) return RET_BUG;
  if ( opts[0].obj != NULLOBJ )
    {
      if ( tol < 0.0 || tol > 1.0 ) 
	{
	  Scierror("%s: tol should be in [0,1] \n",NspFname(stack));
	  return RET_BUG;
	}
      Tol = &tol;
    }

  CheckLhs(1,2);
  if ( nsp_lsq(A,B,Tol,&rank) == FAIL ) return RET_BUG;
  NSP_OBJECT(B)->ret_pos = 1;  
  if ( lhs >= 2 )
    {
      if ( (Rank =nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return RET_BUG;
      Rank->R[0] = (double) rank; 
      MoveObj(stack,1, NSP_OBJECT(Rank));
      NSP_OBJECT(Rank)->ret_pos = 2;  
    }

  return Max(lhs,1);

}

/*
 * interface for nsp_svd: 
 *    S is always computed 
 *    if U != NULL then 
 *       U and V are computed. 
 *       if flag is set to "S" then Min(m,n) columns 
 *       are computed for U and V (rows of Vt) else 
 *      U and V are fully computed 
 *    rank is computed if non null (using tol if tol != NULL)
 */

static int int_svd( Stack stack, int rhs, int opt, int lhs)
{
  char *mode = NULL,cmode ;
  double tol = 0, *Tol=NULL;
  NspMatrix *A;
  NspMatrix *S=NULL, *U=NULL,*V=NULL, *rank=NULL;
  NspMatrix **hrank=NULL,**hU=NULL;
  int_types T[] = {matcopy,new_opts,t_end} ;
  nsp_option opts[] ={{ "tol",s_double,NULLOBJ,-1},
		      { "mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&A,&opts,&tol,&mode) == FAIL) return RET_BUG;
  
  Tol = ( opts[0].obj == NULLOBJ) ? NULL : &tol; 
  cmode = ( opts[1].obj == NULLOBJ) ? '\0' : mode[0]; 
  if ( cmode != '\0' && cmode != 'e' ) 
    {
      Scierror("%s: mode should be '' or  'e' \n",NspFname(stack));
      return RET_BUG;
    }
  CheckLhs(1,4);
  if ( lhs >= 2) { hU= &U;}
  if ( lhs >= 4) { hrank= &rank;}
  if ( nsp_svd(A,&S,hU,&V,cmode,hrank,Tol)== FAIL) return RET_BUG;
  lhs = Max(lhs,1);
  switch (lhs)
    {
    case 1:
      MoveObj(stack,1,NSP_OBJECT(S)); 
      break;
    case 2: 
      MoveObj(stack,1,NSP_OBJECT(U));
      MoveObj(stack,2,NSP_OBJECT(S)); 
      nsp_matrix_destroy(V);
      break;
    case 3: 
      MoveObj(stack,1,NSP_OBJECT(U));
      MoveObj(stack,2,NSP_OBJECT(S)); 
      MoveObj(stack,3,NSP_OBJECT(V));
      break;
    case 4: 
      MoveObj(stack,1,NSP_OBJECT(U));
      MoveObj(stack,2,NSP_OBJECT(S)); 
      MoveObj(stack,3,NSP_OBJECT(V));
      MoveObj(stack,4,NSP_OBJECT(rank));
      break;
    }
  return lhs;
}

/*
 * interface for nsp_det: 
 */

static int int_det( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *res,*A;
  char *mode = NULL,cmode ;
  int_types T[] = {matcopy,new_opts,t_end} ;
  nsp_option opts[] ={{ "mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&A,&opts,&mode) == FAIL) return RET_BUG;
  cmode = ( opts[0].obj == NULLOBJ) ? 'd' : mode[0]; 
  if ( cmode != '\0' && cmode != 'd' ) 
    {
      Scierror("%s: mode should be '' or  'd' \n",NspFname(stack));
      return RET_BUG;
    }
  if (( res = nsp_det(A,cmode))== NULLMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(res));
  return Max(lhs,1);
}

/*
 * interface for nsp_spec: 
 */

static int int_spec( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A,*d,*v;
  NspMatrix **hv=NULL;
  int_types T[] = {matcopy,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A) == FAIL) return RET_BUG;
  CheckLhs(0,3);
  if ( nsp_mat_is_symmetric(A) == TRUE  ) 
    {
      char flag = (lhs == 2) ? 'V' : 'X';
      if ( nsp_spec_sym(A,&d,flag)== FAIL) return RET_BUG;
      if ( lhs <= 1) 
	{
	  MoveObj(stack,1,NSP_OBJECT(d));
	}
      else 
	{
	  NSP_OBJECT(A)->ret_pos = 2;
	  MoveObj(stack,2,NSP_OBJECT(d));
	  NSP_OBJECT(d)->ret_pos = 1;
	}
    }
  else
    {
      if ( lhs == 2) hv = &v;
      if ( nsp_spec(A,&d,hv)== FAIL) return RET_BUG;
      if ( lhs <= 1) 
	{
	  MoveObj(stack,1,NSP_OBJECT(d));
	}
      else 
	{
	  MoveObj(stack,1,NSP_OBJECT(d));
	  MoveObj(stack,2,NSP_OBJECT(v));
	}
    }
  return Max(lhs,1);
}

/*
 * interface for nsp_gspec: to merge with spec 
 */

static int int_gspec( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *B, *alpha, *beta, *Vl, *Vr;
  NspMatrix **hl=NULL, **hr=NULL;
  int_types T[] = {mat,mat,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A,&B) == FAIL) return RET_BUG;
  CheckLhs(0,4);

  if ( lhs >= 3) 
    {
      hl = &Vl;
      if ( lhs >= 4 ) hr = &Vr;
    }

  if ( nsp_gspec(A, B, hl, hr, &alpha, &beta) == FAIL ) return RET_BUG;

  if ( lhs <= 1 )
    {
      nsp_mat_div_el(alpha, beta);
      MoveObj(stack,1,NSP_OBJECT(alpha));
      nsp_matrix_destroy(beta);
    }
  else
    {
      MoveObj(stack,1,NSP_OBJECT(alpha));
      MoveObj(stack,2,NSP_OBJECT(beta));
      if ( lhs >= 3 )
	{
	  MoveObj(stack,3,NSP_OBJECT(Vl));
	  if ( lhs == 4 )
	    MoveObj(stack,4,NSP_OBJECT(Vr));
	}
    }

  return Max(lhs,1);
}

/*
 * interface for nsp_inv: 
 */

static int int_inv( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  int_types T[] = {matcopy,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A) == FAIL) return RET_BUG;
  CheckLhs(0,1);
  if ( nsp_inv(A)== FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos=1;
  return Max(lhs,1);
}

/*
 * interface for nsp_rcond: 
 */

static int int_rcond( Stack stack, int rhs, int opt, int lhs)
{
  double rcond;
  NspMatrix *A;
  int_types T[] = {mat,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A) == FAIL) return RET_BUG;
  CheckLhs(0,1);
  if ( A->mn == 0 ) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
    }
  else 
    {
      if ( nsp_rcond(A,&rcond)== FAIL) return RET_BUG;
      if ( nsp_move_double(stack,1,(double)rcond )== FAIL) return RET_BUG;
    }
  return Max(lhs,1);
}

/*
 * interface for nsp_cholesky: 
 */

static int int_cholesky( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  int_types T[] = {matcopy,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A) == FAIL) return RET_BUG;
  CheckLhs(0,1);
  if ( nsp_cholesky(A)== FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos=1;
  return Max(lhs,1);
}

/*
 * interface for nsp_lu. Modified by Bruno June 21 2005
 * the matrix A is now used to store U on return.
 */
static int int_lu( Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *A;
  NspMatrix *L=NULL, *E=NULL;
  NspMatrix **hE=NULL;
  int_types T[] = {matcopy,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A) == FAIL) return RET_BUG;
  CheckLhs(1,3);
  if ( lhs >= 3) { hE= &E;}
  if ( nsp_lu(A,&L,hE)== FAIL) return RET_BUG;

  NthObj(rhs+1) = NSP_OBJECT(L);
  NSP_OBJECT(L)->ret_pos = 1;  
  if ( lhs >= 2 ) NSP_OBJECT(A)->ret_pos = 2;
  if ( lhs >= 3 ) 
    {
      NthObj(rhs+2) = NSP_OBJECT(E);
      NSP_OBJECT(E)->ret_pos = 3;        
    }
  return Max(lhs,1);
}

/*
 * interface for nsp_lufact
 * dans cette routine on ressort la factorisation de manière
 * compacte dans une table de hachage
 */

/* static int int_lufact( Stack stack, int rhs, int opt, int lhs) */
/* {  */
/*   NspMatrix *A, *E; */
/*   int_types T[] = {matcopy,t_end} ; */
/*   CheckLhs(1,1); */
/*   CheckRhs(1,1); */
/*   if ( GetArgs(stack,rhs,opt,T,&A) == FAIL) return RET_BUG; */
  
/*   if ( nsp_lu(A,&L,&U,hE)== FAIL) return RET_BUG; */
/*   MoveObj(stack,1,NSP_OBJECT(L)); */
/*   return Max(lhs,1); */
/* } */

/*
 * interface for norm 
 */

static int int_norm( Stack stack, int rhs, int opt, int lhs)
{
  double norm, p=2.0;
  int id=1, is_vector;
  char *norm_table[] =       {"1","2","inf","fro","Inf","Fro","M",NULL};
  char norm_lapack_table[] = {'1','2','I'  ,'F'  ,'I'  ,'F'  ,'M'};
  NspMatrix *A;

  CheckRhs(1,2);
  CheckLhs(0,1);

  if ( (A=GetMat(stack, 1)) == NULLMAT ) return RET_BUG;
  is_vector = A->m==1 || A->n==1;
 
  if (rhs == 2)
    {
      if (IsMatObj(stack,2))
	{
	  if ( GetScalarDouble(stack, 2, &p) == FAIL ) return RET_BUG; 
	  if ( is_vector )
	    {
	      if ( !(p >= 1.0) )   /* to detect also nan */ 
		{ 
		  Scierror("%s: second argument must be >= 1 and also not %%nan\n",NspFname(stack));
		  return RET_BUG;
		}
	    }
	  else  /* A is a matrix */
	    {
	      if ( !(p==1.0 || p==2.0 || isinf(p)) )
		{ 
		  Scierror("%s: second argument must be 1, 2 or %%inf \n",NspFname(stack));
		  return RET_BUG;
		}
	      if ( isinf(p) ) id = 2; else id = floor(p)-1;
	    }
	}
      else if ( IsSMatObj(stack,2))
	{
	  if ( (id=GetStringInArray(stack,2,norm_table,1)) == -1) return RET_BUG; 
	  if ( is_vector ) /* define p (p is initialised with 2 so corresponding to id == 1 || id == 3 || id == 5)  */
	    {
	      if ( id == 0 ) p = 1.0;
	      else if ( id == 2 || id == 4 || id == 6 ) p = 1.0/(2.0 - p);  /* got an Inf */
	    }
	}
      else
	{
	  Scierror("%s: second argument must be 1,2,%%inf or '1','2','inf','fro','Inf','Fro','M' \n      (or any real >= 1 for a vector)\n",
		   NspFname(stack));
	  return RET_BUG;
	}
    }

  if ( is_vector )
    norm = nsp_vector_norm(A, p);
  else
    norm = nsp_matrix_norm(A,norm_lapack_table[id]);

  if ( norm < 0 ) return RET_BUG;  /* in some cases a work array must be allocated  */ 
                                   /* and if this fails, nsp_xxxx_norm return -1.0  */

  if ( nsp_move_double(stack,1,norm) == FAIL ) return RET_BUG;
  return Max(lhs,1);
}

/* interface for balanc 
 * [Ab,X]=balanc(A) 
 */

static int int_balanc( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A,*B,*X,*Y;
  CheckRhs(1,2);
  if ((A = GetMatCopy (stack, 1)) == NULLMAT) return RET_BUG;
  if (rhs == 1)
    {
      CheckLhs(0,2);
      if ( nsp_balanc(A,&X) == FAIL) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
      if ( lhs == 2 ) 
	{
	  MoveObj(stack,2,NSP_OBJECT(X));
	}
      else 
	{
	  nsp_matrix_destroy(X);
	}
    }
  else 
    {
      CheckLhs(0,4);
      if ((B = GetMatCopy (stack, 2)) == NULLMAT) return RET_BUG;
      if ( nsp_gbalanc(A,B,&X,&Y) == FAIL) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
      if ( lhs >= 2 ) NSP_OBJECT(B)->ret_pos = 2;
      if ( lhs >= 3 )
	{
	  NthObj(3)= NSP_OBJECT(X);
	  NthObj(3)->ret_pos = 3;
	}
      else 
	{
	  nsp_matrix_destroy(X);
	}
      if ( lhs >= 4 ) 
	{
	  NthObj(4)= NSP_OBJECT(Y);
	  NthObj(4)->ret_pos = 4;
	}
      else 
	{
	  nsp_matrix_destroy(Y);
	}
    }
  return Max(lhs,1);
}



/* 
 * [U,H]=hess(A) 
 * interface for hessenberg  form
 *   H is returned in A 
 *   U is computed if requested 
 */

static int int_hess( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A,*U,**hU=NULL;
  CheckRhs(1,1);
  CheckLhs(0,2);
  if ((A = GetMatCopy (stack, 1)) == NULLMAT) return RET_BUG;
  if ( lhs == 2) hU = &U;
  if ( nsp_hess(A,hU) == FAIL) return RET_BUG;
  if ( lhs < 2) 
    NSP_OBJECT(A)->ret_pos = 1;
  else 
    {
      NthObj(2) = NSP_OBJECT(A);
      NSP_OBJECT(A)->ret_pos = 2;
      NthObj(1) = NSP_OBJECT(U);
      NthObj(1)->ret_pos = 1;
    }
  return Max(lhs,1);
}

/*
 * interface for nsp_expm: 
 */

static int int_expm( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  int_types T[] = {matcopy,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A) == FAIL) return RET_BUG;
  CheckLhs(0,1);
  if ( nsp_expm(A) == FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos=1;
  return Max(lhs,1);
}

/*
 * interface for nsp_solve_banded: 
 */
static int int_solve_banded( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *B, *X;
  int_types T[] = {mat, mat,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A,&B) == FAIL ) return RET_BUG;
  CheckLhs(0,1);
  if ( nsp_solve_banded(A, B, &X)== FAIL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(X));
  return Max(lhs,1);
}

/*
 * The Interface for basic matrices operation 
 */

static OpTab Lapack_func[] = {
  {"qr",int_qr},
  {"lsq",int_lsq},
  {"svd",int_svd},
  {"det",int_det},
  {"spec",int_spec},
  {"gspec",int_gspec},
  {"inv",int_inv},
  {"chol",int_cholesky},
  {"rcond",int_rcond},
  {"norm",int_norm},
  {"lu",int_lu},
  {"balanc",int_balanc},
  {"hess",int_hess},
  {"expm",int_expm},
  {"solve_banded",int_solve_banded},
  {(char *) 0, NULL}
};

int
Lapack_Interf (int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Lapack_func[i].fonc)) (stack, rhs, opt, lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) **/

void
Lapack_Interf_Info (int i, char **fname, function (**f))
{
  *fname = Lapack_func[i].name;
  *f = Lapack_func[i].fonc;
}

