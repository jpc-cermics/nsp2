/* 
 * interface of lapack for Nsp 
 * Copyright (C) 2005-2006 Jean-Philippe Chancelier
 * Copyright (C) 2005-2006 Bruno Pin�on
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
  NspMatrix *Q=NULL, *R=NULL, *E=NULL, *rank=NULL, *sval=NULL;
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
  if ( lhs >= 2 ) 
    {
      MoveObj(stack,2,NSP_OBJECT(R));
    }
  else 
    {
      if ( R != NULL) nsp_matrix_destroy(R);
    }
  if ( lhs >= 3 ) 
    {
      MoveObj(stack,3,NSP_OBJECT(E));
    }
  else 
    {
      if ( E != NULL) nsp_matrix_destroy(E);
    }

  if ( lhs >= 4 ) 
    {
      MoveObj(stack,4,NSP_OBJECT(rank));
    }

  else 
    {
      if ( rank != NULL) nsp_matrix_destroy(rank);
    }
  if ( lhs >= 5 ) 
    {
      MoveObj(stack,5,NSP_OBJECT(sval));
    }
  else 
    {
      if ( sval != NULL) nsp_matrix_destroy(sval);
    }
  return Max(lhs,1);
}


/*
 * interface for lsq : pr�voir le choix de la m�thode (qr ou svd) ?
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
 * interface for rank (i.e like the rank scilab macro) 
 * using the svd. 
 */

static int int_rank( Stack stack, int rhs, int opt, int lhs)
{
  char cmode ='\0' ;
  double tol = 0, *Tol=NULL;
  NspMatrix *A;
  NspMatrix *S=NULL, *rank=NULL;
  int_types T[] = {matcopy,new_opts,t_end} ;
  nsp_option opts[] ={{ "tol",s_double,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&A,&opts,&tol) == FAIL) return RET_BUG;
  Tol = ( opts[0].obj == NULLOBJ) ? NULL : &tol; 
  CheckLhs(0,1);
  if ( nsp_svd(A,&S,NULL,NULL,cmode,&rank,Tol)== FAIL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(rank));
  nsp_matrix_destroy(S);
  return 1;
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
  int minor=0;
  NspMatrix *A;
  int_types T[] = {matcopy,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A) == FAIL) return RET_BUG;
  CheckLhs(0,2);
  if ( lhs == 2 ) minor =-1;
  if( nsp_cholesky(A,&minor)== OK) 
    {
      if ( minor == -1 || minor == 0 ) 
	{
	  /* matrix is OK */
	  NSP_OBJECT(A)->ret_pos=1;
	  if ( lhs == 2 ) 
	    {
	      if ( nsp_move_double(stack,2,0)== FAIL) return RET_BUG;
	    }
	  return Max(lhs,1);
	}
      else 
	{
	  /* a minor is wrong */
	  int q= Max(minor-1,0);
	  NspMatrix *Res;
	  int j, size= (A->rc_type== 'r') ? 1:2  ;
	  if (( Res = nsp_matrix_create(NVOID,A->rc_type,q,q))== NULLMAT)
	    return RET_BUG;
	  for ( j= 0 ; j < Res->n ; j++) 
	    {
	      memcpy(Res->R + j*Res->m*size,A->R + j*A->m*size, 
		     Res->m*size*sizeof(double));
	    }
	  MoveObj(stack,1,NSP_OBJECT(Res));
	  if ( lhs == 2 ) 
	    {
	      
	      if ( nsp_move_double(stack,2,minor)== FAIL) return RET_BUG;
	    }
	  return Max(lhs,1);
	}
    }
  else 
    {
      return RET_BUG;
    }
}

/*
 * interface for nsp_lu. Modified by Bruno June 21 2005
 * the matrix A is now used to store U on return.
 */
static int int_lu( Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *A;
  NspMatrix *L=NULL, *E=NULL, *Rcond;
  NspMatrix **hE=NULL, **hRcond=NULL;
  int_types T[] = {matcopy,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&A) == FAIL) return RET_BUG;
  CheckLhs(1,4);
  if ( lhs >= 3) 
    { 
      hE= &E;  
      if ( lhs == 4) hRcond= &Rcond;
    }

  if ( nsp_lu(A,&L,hE,hRcond)== FAIL) return RET_BUG;

  NthObj(rhs+1) = NSP_OBJECT(L);
  NSP_OBJECT(L)->ret_pos = 1;  
  if ( lhs >= 2 ) NSP_OBJECT(A)->ret_pos = 2;
  if ( lhs >= 3 ) 
    {
      NthObj(rhs+2) = NSP_OBJECT(E);
      NSP_OBJECT(E)->ret_pos = 3;
      if ( lhs == 4 )
	{
	  NthObj(rhs+3) = NSP_OBJECT(Rcond);
	  NSP_OBJECT(Rcond)->ret_pos = 4;
	}
    }
  return Max(lhs,1);
}


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
 * interface for schur 
 */

extern int intdgees0(NspMatrix *A,NspMatrix **U,int (*F)(const double *re,const double *im),
		     NspMatrix **Sdim);
extern int intzgees0(NspMatrix *A,NspMatrix **U,int (*F)(const doubleC *w), NspMatrix **Sdim) ;

extern int nsp_dschur_cont_stable(const double *reig,const double *ieig);
extern int nsp_dschur_discr_stable(const double *reig,const double *ieig);
extern int nsp_zschur_cont_stable(const doubleC *eig);
extern int nsp_zschur_discr_stable(const doubleC *eig);

/* external function for schur where code is redirected 
 * to nsp evaluation. 
 */

static int nsp_zschur_fun(const doubleC *eig);
static int nsp_dschur_fun(const double *reig,const double *ieig);

/* 
 * data used when nsp is used to evaluate a schur function 
 */

typedef struct _schur_data schur_data;

struct _schur_data
{
  NspObject *args; /* a list to pass extra arguments to the objective function */
  NspMatrix *x;  /* value of x */
  NspObject *objective; /* schur function */
};

static schur_data schur_d= {NULL,NULL,NULL};
static int schur_prepare(NspObject *f,NspObject *args,schur_data *obj);
static void schur_clean(schur_data *obj);

/**
 * schur_prepare:
 * @f: a #NspPList giving the nsp code of the function to be called
 * @args: extra arguments which are transmited to the nsp objective function 
 * @obj: a #schur_data structure used to store objective function nsp data.
 * 
 * fills @obj with data. @obj is used in nsp_dschur_fun() or nsp_zschur_fun() to evaluate the 
 * nsp function @f.
 * 
 * Return value: %OK or %FAIL 
 **/

static int schur_prepare(NspObject *f,NspObject *args,schur_data *obj)
{
  if (( obj->objective =nsp_object_copy(f)) == NULL) return FAIL;
  if (( nsp_object_set_name(obj->objective,"schur_f")== FAIL)) return FAIL;
  if ( args != NULL ) 
    {
      if (( obj->args = nsp_object_copy(args)) == NULL ) return FAIL;
      if (( nsp_object_set_name((NspObject *) obj->args,"arg")== FAIL)) return FAIL;
    }
  else 
    {
      obj->args = NULL;
    }
  if ((obj->x = nsp_matrix_create("y",'c',1,1))== NULL) return FAIL;
  return OK;
}

/**
 * schur_clean:
 * @obj: a #schur_data structure used to store objective function nsp data.
 * 
 * deallocate data stored in @obj
 * 
 **/

static void schur_clean(schur_data *obj)
{
  if ( obj->args != NULL) nsp_object_destroy(&obj->args);
  nsp_object_destroy(&obj->objective);
  nsp_matrix_destroy(obj->x);
}

/*
 * nsp_dschur_fun:
 * @x: state 
 * @f: a #NspPList giving the nsp code of the function to be called
 * @schur_d: a #schur_data structure used to store objective function nsp data.
 * 
 * evaluates the objective function and return the value 
 * in @f. The nsp coded objective function is stored in @schur_d together with 
 * nsp data.
 * 
 * Return value: %OK or %FAIL
 **/


/**
 * nsp_dschur_fun:
 * @reig: a double pointer 
 * @ieig: a double pointer 
 * 
 * evaluates the nsp function for given eigenvalue and returns 1 or 0 
 * In case of problems in the function an error is raised but the program 
 * is not stopped since we do not have control on it.
 * 
 * Returns: 0 or 1 
 **/

static int nsp_dschur_fun(const double *reig,const double *ieig)
{
  int rep;
  schur_data *schur = &schur_d;
  NspObject *targs[2];/* arguments to be transmited to schur->objective */
  NspObject *nsp_ret;
  int nret = 1,nargs = 1;
  /*
   * targs[0]= NSP_OBJECT(schur->n); 
   * schur->n->R[0] = *n;
   */
  targs[0]= NSP_OBJECT(schur->x); 
  schur->x->C[0].r= *reig;
  schur->x->C[0].i= *ieig;
  if (schur->args != NULL ) 
    {
      targs[1]= NSP_OBJECT(schur->args);
      nargs= 2;
    }
  /* FIXME : a changer pour metre une fonction eval standard */
  if ( nsp_gtk_eval_function((NspPList *)schur->objective ,targs,nargs,&nsp_ret,&nret)== FAIL) 
    {
      return FAIL;
    }
  if ( nret ==1 && IsBMat(nsp_ret) &&((NspBMatrix *) nsp_ret)->mn == 1 ) 
    {
      rep = ((NspBMatrix *) nsp_ret)->B[0];
      nsp_object_destroy((NspObject **) &nsp_ret);
    }
  else 
    {
      Scierror("Error: schur objective function returned argument is wrong\n");
      return 0;
    }
  return rep;
 
}

/**
 * nsp_zschur_fun:
 * @reig: a double pointer 
 * @ieig: a double pointer 
 * 
 * evaluates the nsp function for given eigenvalue and returns 1 or 0 
 * In case of problems in the function an error is raised but the program 
 * is not stopped since we do not have control on it.
 * 
 * Returns: 0 or 1 
 **/

static int nsp_zschur_fun(const doubleC *eig)
{
  return  nsp_dschur_fun( &eig->r, &eig->i);
}

/* interface for schur function 
 *
 */


static int int_schur( Stack stack, int rhs, int opt, int lhs)
{
  int complex=FALSE,rep ;
  int (*F)(const double *reig,const double *ieig)= NULL;
  int (*Fc)(const doubleC *reig)= NULL;
  nsp_option opts[] ={{ "sort",obj,NULLOBJ,-1},
		      { "args",list,NULLOBJ,-1},
		      { "complex", s_bool, NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspObject *select=NULL;
  NspObject *extra_args=NULL;
  NspMatrix *A,*U=NULL,**hU=NULL,*Dim=NULL,**hDim=NULL;
  CheckStdRhs(1,1);
  CheckLhs(0,3);
  if ((A = GetMatCopy (stack, 1)) == NULLMAT) return RET_BUG;

  if ( get_optional_args(stack,rhs,opt,opts,&select,&extra_args,&complex) == FAIL) return RET_BUG;
  
  if ( select != NULL) 
    {
      /* optional argument for eigenvalues selection */
      if (IsString(select) )
	{
	  char *str = ((NspSMatrix *) select)->S[0];
	  if ( strcmp(str,"c") ==0) 
	    {
	      F= nsp_dschur_cont_stable;
	      Fc= nsp_zschur_cont_stable;
	      hDim = &Dim;
	    }
	  else if ( strcmp(str,"d") ==0) 
	    {
	      F= nsp_dschur_discr_stable;
	      Fc= nsp_zschur_discr_stable;
	      hDim = &Dim;
	    }
	  else
	    {
	      Scierror("Error: optional argument sort, when given as a string should be 'c' or 'd'\n");
	      return RET_BUG;
	    }
	}
      else if ( IsNspPList(select) )
	{
	  F = nsp_dschur_fun;
	  Fc =  nsp_zschur_fun;
	}
      else 
	{
	  Scierror("%s: sort optional argument should be a string or a function\n",NspFname(stack));
	  return RET_BUG;
	}
    }


  /* check if U is requested */
  if ( lhs == 3 || lhs == 2 ) hU = &U;
  /* check for dim */
  if ( lhs >= 2) hDim = &Dim;

  /* deals with external fun */

  if ( F == nsp_dschur_fun ) 
    {
      if ( schur_prepare(select,extra_args,&schur_d) == FAIL ) 
	return RET_BUG;
    }
  
  /* force complex result */
  if ( complex == TRUE && A->rc_type == 'r'  ) 
    {
      if (nsp_mat_complexify(A,0.0) == FAIL) return RET_BUG;
    }

  if ( A->rc_type == 'r' ) 
    {
      rep =intdgees0(A,hU,F,hDim); 
    }
  else 
    {
      rep = intzgees0(A,hU,Fc,hDim);
    }

  if ( F == nsp_dschur_fun ) 
    {
      schur_clean(&schur_d);
    }
  if ( rep == FAIL) return RET_BUG;

  switch (lhs ) 
    {
    case -1:
    case 0:
    case 1: 
      NSP_OBJECT(A)->ret_pos = 1;
      if (  Dim != NULL ) nsp_matrix_destroy(Dim);
      break;
    case 2: 
      if ( select == NULL) 
	{
	  NthObj(2) = NSP_OBJECT(A);
	  NSP_OBJECT(A)->ret_pos = 2;
	  NthObj(1) = NSP_OBJECT(U);
	  NthObj(1)->ret_pos = 1;
	  if (  Dim != NULL ) nsp_matrix_destroy(Dim);
	}
      else 
	{
	  NthObj(2) = NSP_OBJECT(Dim);
	  NSP_OBJECT(Dim)->ret_pos = 2;
	  NthObj(1) = NSP_OBJECT(U);
	  NthObj(1)->ret_pos = 1;
	  nsp_matrix_destroy(A);
	}
      break;
    case 3: 
      NthObj(3) = NSP_OBJECT(A);
      NSP_OBJECT(A)->ret_pos = 3;
      NthObj(2) = NSP_OBJECT(Dim);
      NSP_OBJECT(Dim)->ret_pos = 2;
      NthObj(1) = NSP_OBJECT(U);
      NthObj(1)->ret_pos = 1;
      break;
    }
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
  {"rank_m",int_rank},
  {"schur_m",int_schur},
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

