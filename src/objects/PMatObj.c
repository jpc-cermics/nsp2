/* Nsp
 * Copyright (C) 1998-2016 Jean-Philippe Chancelier Enpc/Cermics
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

#include <nsp/nsp.h>
#include <nsp/object.h> 
#include <nsp/matrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/smatrix.h> 
#define PMatrix_Private 
#include <nsp/pmatrix.h> 
#include <nsp/cells.h> 
#include <nsp/matint.h> 
#include <nsp/hobj.h> 
#include <nsp/file.h> 
#include <nsp/type.h> 

#include "nsp/cnumeric.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/matint.h"
#include "nsp/nsp_lapack.h" /* vector norm declaration */

static int int_pmatrix_create(Stack stack, int rhs, int opt, int lhs);

/*
 * NspPMatrix inherits from NspObject 
 * Polynomial matrices 
 */

int nsp_type_pmatrix_id=0;
NspTypePMatrix *nsp_type_pmatrix=NULL;
int nsp_type_pmatrix_init();

NspTypePMatrix *new_type_pmatrix(type_mode mode)
{
  NspTypeMatint *mati;/* interface */
  NspTypePMatrix *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_pmatrix != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pmatrix;
    }
  if (( type =  malloc(sizeof(NspTypePMatrix))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pmatrix_attrs; 
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = pmatrix_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_pmatrix;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for pmatrix */ 

  top->pr = (print_func *)nsp_pmatrix_print;                    /* printing*/   
  top->dealloc = (dealloc_func *)nsp_pmatrix_destroy;              /* dealloc */  
  top->copy  =  (copy_func *)nsp_pmatrix_copy;                   /* copy object */  
  top->size  = (size_func *)nsp_pmatrix_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_pmatrix_type_as_string;                /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_pmatrix_type_short_string;              /* type as a short string */  
  top->info = (info_func *)nsp_pmatrix_info;                    /* info */  
  /* top->is_true = (is_true_func  *) PMatIsTrue; */
  /* top->loop =(loop_func *)nsp_pmatrix_loop_extract;   */             /* for loops */  
  top->path_extract =  NULL;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_pmatrix_object;    /* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_pmatrix_eq;                       /* equality check */  
  top->neq  = (eq_func *)nsp_pmatrix_neq;                      /* non-equality check */

  top->save  = (save_func *)nsp_pmatrix_xdr_save;
  top->load  = (load_func *)nsp_pmatrix_xdr_load;
  top->create = (create_func*) int_pmatrix_create;
  top->latex = (print_func*) nsp_pmatrix_latex_print;
  top->full_copy  =  (copy_func *)nsp_pmatrix_copy;                   /* copy object */  

  /* specific methods for pmatrix */
  type->init = (init_func *) init_pmatrix;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */

  mati = new_type_matint(T_DERIVED);
  mati->methods = matint_get_methods; 
  /* mati->redim = (matint_redim *) nsp_pmatrix_redim;  use default value */
  mati->resize = (matint_resize  *) nsp_pmatrix_resize; 
  mati->free_elt = (matint_free_elt *) nsp_polynom_destroy;
  mati->elt_size = (matint_elt_size *) nsp_pmatrix_elt_size ;
  mati->clone = (matint_clone *) nsp_pmatrix_clone;
  mati->copy_elt = (matint_copy_elt *) nsp_polynom_copy_with_name; 
  mati->enlarge = (matint_enlarge *) nsp_pmatrix_enlarge;
  mati->canonic =  nsp_matint_canonic;
  mati->copy_ind = nsp_matint_basic_copy_pointer;
  type->interface = (NspTypeBase *) mati;
  
  if ( nsp_type_pmatrix_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_pmatrix
       */
      type->id =  nsp_type_pmatrix_id = nsp_new_type_id();
      nsp_type_pmatrix = type;
      if ( nsp_register_type(nsp_type_pmatrix) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_pmatrix(mode);
    }
  else 
    {
      type->id = nsp_type_pmatrix_id;
      return type;
    }

}
/*
 * initialize Pmatrix instances 
 * locally and by calling initializer on parent class 
 */

static int init_pmatrix(NspPMatrix *o,NspTypePMatrix *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Pmatrix 
 */

NspPMatrix *new_pmatrix() 
{
  NspPMatrix *loc; 
  /* type must exists */
  nsp_type_pmatrix = new_type_pmatrix(T_BASE);
  if ( (loc = malloc(sizeof(NspPMatrix)))== NULLPMAT) return loc;
  /* initialize object */
  if ( init_pmatrix(loc,nsp_type_pmatrix) == FAIL) return NULLPMAT;
  return loc;
}


/*
 * MatSize : returns m,n,or m*n 
 */

int nsp_pmatrix_size(NspPMatrix *Mat, int flag)
{
  switch (flag) 
    {
    case 0: return Mat->mn;
    case 1: return Mat->m;
    case 2: return Mat->n;
    }
  return 0;
}

/*
 * MatType 
 */

static char pmat_type_name[]="PMat";
static char pmat_short_type_name[]="p";

char *nsp_pmatrix_type_as_string(void)
{
  return(pmat_type_name);
}

char *nsp_pmatrix_type_short_string(NspObject *v)
{
  return(pmat_short_type_name);
}

static int nsp_pmatrix_full_comp(NspPMatrix * A,NspPMatrix * B,char *op,int *err)
{
  int i, rep;
  if ( A->m != B->m || A->n != B->n) return FALSE;
  if ( ! nsp_pmatrix_same_varname(A,B))  return FALSE;
  for ( i = 0 ; i < A->mn ; i++ ) 
    {
      if ( ! ( A->S[i]->m == B->S[i]->m && A->S[i]->n == B->S[i]->n ))
	return FALSE;
      rep = nsp_mat_fullcomp (A->S[i],B->S[i],op, err);
      if ( *err == TRUE || rep == FALSE ) return FALSE;
    }
  return TRUE;
}

int nsp_pmatrix_eq(NspObject *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_pmatrix_id) == FALSE) return FALSE ;
  rep = nsp_pmatrix_full_comp((NspPMatrix *) A,(NspPMatrix *) B,"==",&err);
  if ( err == TRUE) return FALSE ; 
  return rep;
}

int nsp_pmatrix_neq(NspObject *A, NspObject *B)
{
  return ( nsp_pmatrix_eq(A,B) == TRUE ) ? FALSE : TRUE ;
}

/*
 * Mat == TRUE ? 
 *  if Mat != [] and all the elements of Mat are != 0
 * PMatIsTrue(NspPMatrix *M)
 */


/*
 * Save a Matrix in a file stream 
 */

static int nsp_pmatrix_xdr_save(XDR *xdrs, NspPMatrix *M)
{
  int i;
#if 1 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_pmatrix)) == FAIL) return FAIL;
#else
  if (nsp_xdr_save_i(xdrs, M->type->id) == FAIL)    return FAIL;
#endif 
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->m) == FAIL)    return FAIL;
  if (nsp_xdr_save_i(xdrs, M->n) == FAIL)    return FAIL;
  if (nsp_xdr_save_c(xdrs, M->rc_type) == FAIL)  return FAIL;
  for ( i = 0 ; i < M->mn ; i++ ) 
    {
      if (nsp_object_xdr_save(xdrs,(NspObject *) M->S[i]) == FAIL) return FAIL;
    }
  return OK;
}

/*
 * Load a Matrix from a file stream 
 **/

static NspPMatrix *nsp_pmatrix_xdr_load(XDR *xdrs)
{
  char c;
  int m, n,i;
  NspPMatrix *M;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLPMAT;
  if (nsp_xdr_load_i(xdrs, &m) == FAIL) return NULLPMAT;
  if (nsp_xdr_load_i(xdrs, &n) == FAIL) return NULLPMAT;
  if (nsp_xdr_load_c(xdrs, &c) == FAIL) return NULLPMAT;
  if ((M =nsp_pmatrix_create(name,m,n,NULL,-1, NULL))== NULLPMAT)
    return NULLPMAT;
  for ( i = 0 ; i < M->mn ; i++ ) 
    {
      NspObject *Obj =nsp_object_xdr_load(xdrs); 
      if ( Obj == NULLOBJ ) return NULLPMAT;
      M->S[i]= (nsp_polynom) Obj;
    }
  return M;
}


/*
 * A =nsp_pmatrix_object(O);
 * checks that O is an object of NspPMatrix type. 
 * or a Hobj which points to an object of type PMatrix
 * if so, returns a pointer to that NspPMatrix and else returns NULL
 */

NspPMatrix   *nsp_pmatrix_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_pmatrix_id) == TRUE) return ((NspPMatrix *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_pmatrix));
  return(NULL);
}


/*
 * IsPMatObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  PMatrix 
 * or a Hobj which points to an object of type PMatrix
 */

int IsPMatObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pmatrix_id);
}

/*
 * IsPMat(O)
 * only checks that object is an object of type  PMatrix 
 * or a Hobj which points to an object of type PMatrix
 */

int IsPMat(NspObject *O)
{
  return nsp_object_type(O , nsp_type_pmatrix_id);
}

/*
 * Checks that first+i object on the stack 
 * is a NspPMatrix and returns that NspPMatrix  
 * or a copy of that NspPMatrix if its name 
 * is != NVOID 
 */

NspPMatrix*GetPMatCopy(Stack stack, int i)
{
  if (  GetPMat(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/*
 * Checks that first+i object on the stack 
 * is a NspPMatrix and returns that NspPMatrix  
 */

NspPMatrix*GetPMat(Stack stack, int i)
{
  NspPMatrix *M;
  if (( M =nsp_pmatrix_object(NthObj(i))) == NULLPMAT  )
    ArgMessage(stack,i);
  return M;
}

/*
 * Checks that first+i objects on the stack 
 * is a spolynom and returns a pointer to that string
 */

nsp_polynom GetPolynom(Stack stack, int i)
{
  NspPMatrix *M;
  if (( M =nsp_pmatrix_object(NthObj(i))) == NULLPMAT 
      || ( M->mn != 1 ))
    {
      Scierror("Error:\t%s", ArgPosition(i));
      ArgName(stack,i);
      Scierror(" of function %s should be a polynom (i.e 1x1 pmat)\n",NspFname(stack));
      return NULLPOLY ;
    }
  return M->S[0];
}

/*-------------------------------------------------------------------
 * wrappers for the PMatrix 
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/*
 * Matrix ( used ar row vector 1xn ) -> 1x1 polymatrix filled 
 * with one polynom of degree n-1
 * 
 */

static int int_pmatrix_m2p(Stack stack, int rhs, int opt, int lhs)
{
  int dim=0;
  const char *var = NULL;
  nsp_option opts[] ={{"var",string,NULLOBJ,-1},
		      {"dim",dim_arg,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspPMatrix *P; NspMatrix *A;
  CheckStdRhs(1,1);
  CheckLhs(1,1);
  if (( A=GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if ( get_optional_args(stack, rhs, opt, opts, &var,&dim) == FAIL )
    return RET_BUG;
  if ( dim == -2 )
    {
      Scierror ("Error:\t dim flag equal to -2 or 'm' not supported for function %s\n", NspFname(stack));
      return RET_BUG;
    }
  /* if A is 0xn or nx0 we want p to have same size, we force dim= -1 */
  if ( A->mn == 0 ) dim = -1;
  switch ( dim)
    {
    case 0:
      if (( P=nsp_matrix_to_polynom(A))== NULLPMAT) return RET_BUG;
      break;
    case -1 :
      if (( P=nsp_matrix_to_pmatrix(A))== NULLPMAT) return RET_BUG;
      break;
    default:
      Scierror ("Error:\t dim flag equal to %d not supported for function %s\n",dim, NspFname(stack));
      return RET_BUG;
    }
  if ( nsp_pmatrix_set_varname(P,var) ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) P);
  return 1;
}

/*
 * build of polynomial matrix from its coefficients given 
 * in a cell
 */

static int int_pmatrix_ce2p(Stack stack, int rhs, int opt, int lhs)
{
  const char *var = NULL;
  nsp_option opts[] ={{"var",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspPMatrix *P; NspCells *C;
  CheckStdRhs(1,1);
  CheckLhs(1,1);
  if (( C=GetCells(stack,1)) == NULL) return RET_BUG;
  if ( get_optional_args(stack, rhs, opt, opts, &var) == FAIL )
    return RET_BUG;
  if (( P=nsp_cells_to_pmatrix(NVOID,C))== NULLPMAT) return RET_BUG;
  if ( nsp_pmatrix_set_varname(P,var) ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) P);
  return 1;
}

/*
 * Creation of a PMatrix from a Matrix 
 */

static int int_pmatrix_create(Stack stack, int rhs, int opt, int lhs)
{
  const char *var = NULL;
  nsp_option opts[] ={{"var",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  int m1,n1;
  NspMatrix *A=NULLMAT; 
  NspPMatrix *P; 
  CheckStdRhs(2,3);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if (rhs - opt == 3) 
    {
      if ((A = GetMat(stack,3))== NULLMAT) return RET_BUG;
    }
  if ( get_optional_args(stack, rhs, opt, opts, &var) == FAIL )
    return RET_BUG;
  if ( (P =nsp_pmatrix_create_m(NVOID,m1,n1,A, var)) == NULLPMAT)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) P);
  return 1;
}


/*------------------------------------------------------
 * attributes  
 *------------------------------------------------------*/

/* return all the keys H.keys entered in the hash table as a string matrice  */

static NspObject * int_pmatrix_get_coeffs(void *Hv,const char *attr)
{
  return (NspObject *) nsp_pmatrix_to_cells(NVOID,Hv);
}

static int int_pmatrix_set_coeffs(void *Hv,const char *attr, NspObject *O)
{
  Scierror("attribute __keys of hash instances cannot be set !\n");
  return FAIL;
}

static AttrTab pmatrix_attrs[] = {
  { "coeffs", 	int_pmatrix_get_coeffs , int_pmatrix_set_coeffs , 	NULL, NULL  },
  { (char *) 0, NULL, NULL , NULL , NULL }
};


/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

int nsp_pmatrix_set_varname(NspPMatrix *p, const char *varname)
{
  if ( p->var != NULL) nsp_string_destroy(&(p->var));
  if (varname == NULL)  return OK; /* p->var was set to NULL */
  if (( p->var = nsp_new_string(varname,-1))==NULL )
    {
      return FAIL;
    }
  return OK;
}

static int int_meth_set_varname(void *self, Stack stack, int rhs, int opt, int lhs)
{
  const char *varname;
  CheckStdRhs(1,1);
  if ((varname = GetString(stack,1)) == NULL ) return RET_BUG;
  if (nsp_pmatrix_set_varname(self,varname)== FAIL) return RET_BUG;
  return 0;
}

static int int_meth_get_varname(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *p=self;
  CheckStdRhs(0,0);
  if ( nsp_move_string(stack,1,(p->var != NULL) ? p->var : "x",-1 )== FAIL) return RET_BUG;
  return 1;
}

static int int_meth_degree(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *D;
  NspPMatrix *P=self;
  int i;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ((D= nsp_matrix_create(NVOID,'r', P->m , P->n))==NULLMAT)
    return RET_BUG;
  for ( i = 0 ; i < P->mn ; i++ ) 
    {
      D->R[i]= Max(P->S[i]->mn -1,0); 
    }
  MoveObj(stack,1,NSP_OBJECT(D));
  return 1;
}

/* P.shift[k] or P.shift[k,'r'] -> P*x^k (k>=0) 
 * P.shift[k,'l'] -> P/x^k 
 */

static int int_meth_shift(void *self,Stack stack, int rhs, int opt, int lhs)
{
  char dir='r'; 
  NspPMatrix *P=self;
  int n,i;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ( GetScalarInt (stack, 1, &n) == FAIL ) return RET_BUG;
  if ( n <= 0 ) return 0;
  if (rhs >= 2)
    {
      int rep;
      const char *shift_options1[] = { "r", "l", NULL };
      if ((rep = GetStringInArray (stack,2, shift_options1, 1)) == -1)
	return RET_BUG;
      dir = shift_options1[rep][0];
    }
  if ( dir == 'r') 
    for ( i = 0 ; i < P->mn ; i++) 
      {
	NspMatrix *A= P->S[i];
	int k= A->mn,j ;
	if ( nsp_matrix_resize(A, 1, A->n+n)== FAIL) return RET_BUG;
	if ( A->rc_type == 'r') 
	  {
	    for ( j = 0 ; j < k ; j++) A->R[A->mn-1-j]= A->R[k-1-j];
	    for ( j = 0 ; j < n ; j++) A->R[j]= 0.0;
	  }
	else
	  {
	    for ( j = 0 ; j < k ; j++) A->C[A->mn-1-j]= A->C[k-1-j];
	    for ( j = 0 ; j < n ; j++) A->C[j].r= A->C[j].i= 0.0;
	  }
      }
  else 
    {
      for ( i = 0 ; i < P->mn ; i++) 
	{
	  NspMatrix *A= P->S[i];
	  int j;
	  if ( A->rc_type == 'r') 
	    {
	      for ( j = 0 ; j < A->mn - n  ; j++) A->R[j]= A->R[j+n];
	    }
	  else
	    {
	      for ( j = 0 ; j < A->mn - n  ; j++) A->C[j]= A->C[j+n];
	    }
	  if ( nsp_matrix_resize(A, 1,Max(1,A->mn-n))== FAIL) return RET_BUG;
	}
    }
  return 0;
}

static int int_meth_normalize(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int i,j;
  char type = 'r';
  NspMatrix *N;
  NspPMatrix *P=self;
  CheckRhs(0,0);
  CheckLhs(0,1);
  for ( i = 0 ; i < P->mn ; i++) 
    if (P->S[i]->rc_type == 'c') type = 'c';
  if ((N = nsp_matrix_create(NVOID,type,P->m,P->n)) == NULL) 
    return RET_BUG;
  for ( i = 0 ; i < P->mn ; i++) 
    {
      NspMatrix *A= P->S[i];
      if ( A->rc_type == 'r') 
	{
	  for ( j = 0 ; j < A->mn -1 ; j++) A->R[j] /= A->R[A->mn-1];
	  if ( type == 'r' ) 
	    {
	      N->R[i]= A->R[A->mn-1];
	    }
	  else 
	    {
	      N->C[i].r = A->R[A->mn-1];
	      N->C[i].i = 0;
	    }
	  A->R[A->mn-1]=1.0;
	}
      else
	{
	  for ( j = 0 ; j < A->mn ; j++) 
	    {
	      doubleC res;
	      nsp_div_cc(&A->C[j],&A->C[A->mn-1],&res);
	      A->C[j]=res;
	    }
	  N->C[i]= A->C[A->mn-1];
	  A->C[A->mn-1].r = 1.0;
	  A->C[A->mn-1].i = 0.0;
	}
    }
  MoveObj(stack,1,NSP_OBJECT(N));
  return 1;
}


static int int_meth_derivative(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int i,j;
  NspPMatrix *P=self, *Res= NULL;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ((Res =nsp_pmatrix_create(NVOID,P->m,P->n,NULL,-1, P->var))== NULLPMAT) return RET_BUG;
  for ( i = 0 ; i < P->mn ; i++) 
    {
      NspMatrix *A= P->S[i], *B;
      if ((B = nsp_matrix_create("pe",A->rc_type,1,Max(A->mn-1,1))) == NULLMAT) goto err;
      if ( A->mn <= 1) 
	{
	  if ( B->rc_type == 'r' ) 
	    {
	      B->R[0]= 0;
	    }
	  else
	    {
	      B->C[0].r= B->C[0].i= 0;
	    }
	}
      else 
	{
	  for ( j = 0 ; j < B->mn ; j++) 
	    {
	      if ( B->rc_type == 'r' ) 
		{
		  B->R[j]= A->R[j+1]*(j+1);
		}
	      else 
		{
		  B->C[j].r= A->C[j+1].r*(j+1);
		  B->C[j].i= A->C[j+1].i*(j+1);
		}
	    }
	}
      Res->S[i]= B;
    }
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
 err: 
  nsp_pmatrix_destroy(Res);
  return RET_BUG;
}


static NspMethods pmatrix_methods[] = {
  { "degree", int_meth_degree},
  { "shift", int_meth_shift},
  { "normalize", int_meth_normalize},
  { "derivative", int_meth_derivative},
  { "set_var", int_meth_set_varname},
  { "get_var", int_meth_get_varname},
  { (char *) 0, NULL}
};

static NspMethods *pmatrix_get_methods(void) { return pmatrix_methods;};

/*----------------------------------
 * Interfaces 
 *----------------------------------*/

/*
 * Res = [A,P]  when A is a scalar matrix 
 */

static int int_pmatrix_concatr_m_p(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *B,*Res;
  NspMatrix * A;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( A->mn == 0)
    {
      if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
      /* A and B must have compatible rows */
      if (A->m != B->m && A->m != A->n ) 
	{
	  Scierror("Error: matrices should have same number of rows found %d and %d\n",
		   A->m, B->m);
	  return RET_BUG;
	}
      if ( B->mn == 0) 
	{
	  if ((B = GetPMatCopy(stack,2)) == NULLPMAT) return RET_BUG;
	  B->n += A->n;
	}
      NSP_OBJECT(B)->ret_pos = 1;
      return 1;
    }
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  if (( Res=nsp_matrix_to_pmatrix_with_varname(A,B->var)) == NULLPMAT) return RET_BUG;
  if ( B->mn != 0)
    {
      if (nsp_pmatrix_concat_right(Res,B)!= OK) return RET_BUG;
    }
  else
    {
      if ( Res->m != B->m && B->m != B->n ) 
	{
	  Scierror("Error: matrices should have same number of rows found %d and %d\n",
		   A->m, B->m);
	  return RET_BUG;
	}
      if ( Res->m == 0 )  Res->n += B->n;
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/* Res = [P,A] */

static int int_pmatrix_concatr_p_m(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*P2;
  NspMatrix *B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMatCopy(stack,1))  == NULLPMAT) return RET_BUG;
  if ( A->mn == 0)
    {
      if ((B = GetMat(stack,2)) == NULLMAT) return RET_BUG;
      if ((P2=nsp_matrix_to_pmatrix_with_varname(B,A->var)) == NULLPMAT) return RET_BUG;
      if (A->m != P2->m && A->m != A->n ) 
	{
	  Scierror("Error: matrices should have same number of rows found %d and %d\n",
		   A->m, P2->m);
	  return RET_BUG;
	}
      if ( P2->mn == 0 ) 
	{
	  P2->n += A->n;
	}
      MoveObj(stack,1,NSP_OBJECT(P2));
      return 1;
    }
  if ((B = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( B->mn != 0 )
    {
      if ((P2=nsp_matrix_to_pmatrix_with_varname(B,A->var)) == NULLPMAT) return RET_BUG;
      if (nsp_pmatrix_concat_right(A,P2) != OK) return RET_BUG;
      nsp_pmatrix_destroy(P2);
    }
  else
    {
      if (A->m != B->m && B->m != B->n ) 
	{
	  Scierror("Error: matrices should have same number of rows found %d and %d\n",
		   A->m, B->m);
	  return RET_BUG;
	}
    }
  NSP_OBJECT(A)->ret_pos =1;
  return 1;
}

/* Res = [A;P]; */

static int int_pmatrix_concatd_m_p(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *B,*Res;
  NspMatrix * A;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( A->mn == 0)
    {
      if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
      if (A->n != B->n && A->m != A->n ) 
	{
	  Scierror("Error: matrices should have same number of columns found %d and %d\n",
		   A->n, B->n);
	  return RET_BUG;
	}
      if ( B->mn == 0 ) 
	{
	  if ((B = GetPMatCopy(stack,2)) == NULLPMAT) return RET_BUG;
	  B->m += A->m;
	}
      NSP_OBJECT(B)->ret_pos= 1;
      return 1;
    }
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  if (( Res=nsp_matrix_to_pmatrix_with_varname(A,B->var)) == NULLPMAT) return RET_BUG;
  if ( B->mn != 0)
    {
      if ((Res=nsp_pmatrix_concat_down(Res,B))== NULLPMAT ) return RET_BUG;
    }
  else
    {
      if (A->n != B->n && B->m != B->n ) 
	{
	  Scierror("Error: matrices should have same number of columns found %d and %d\n",
		   A->n, B->n);
	  return RET_BUG;
	}
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/* Res = [P;A] */

static int int_pmatrix_concatd_p_m(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*P2;
  NspMatrix *B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1))  == NULLPMAT) return RET_BUG;
  if ( A->mn == 0)
    {
      if ((B = GetMat(stack,2)) == NULLMAT) return RET_BUG;
      if ((P2=nsp_matrix_to_pmatrix_with_varname(B,A->var)) == NULLPMAT) return RET_BUG;
      if (A->n != P2->n && A->m != A->n ) 
	{
	  Scierror("Error: matrices should have same number of columns found %d and %d\n",
		   A->n, P2->n);
	  return RET_BUG;
	}
      if ( P2->mn == 0 ) 
	{
	  P2->m += A->m;
	}
      MoveObj(stack,1,NSP_OBJECT(P2));
      return 1;
    }
  if ((B = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( B->mn != 0 )
    {
      NspPMatrix *Res;
      if ((P2=nsp_matrix_to_pmatrix_with_varname(B,A->var)) == NULLPMAT) return RET_BUG;
      if ((Res=nsp_pmatrix_concat_down(A,P2))== NULLPMAT ) return RET_BUG;
      nsp_pmatrix_destroy(P2);
      MoveObj(stack,1,NSP_OBJECT(Res));
      return 1;
    }
  else
    {
      if (A->n != B->n && B->m != B->n ) 
	{
	  Scierror("Error: matrices should have same number of columns found %d and %d\n",
		   A->n, B->n);
	  return RET_BUG;
	}
    }
  NSP_OBJECT(A)->ret_pos =1;
  return 1;
}



/*
 *nsp_pmatrix_add_columns: add n cols of zero to NspPMatrix A 
 * A= [A,ones(m,n)] 
 * return 0 on failure ( No more space )
 * A is changed 
 */

static int int_pmatrix_addcols(Stack stack, int rhs, int opt, int lhs)
{
  int n1;
  NspPMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetPMatCopy(stack,1))== NULLPMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ( nsp_pmatrix_add_columns(HMat,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 * AddRows : Add m rows of zero to a NspPMatrix A 
 * A = [A;ones(m,n)]
 * return NULLSMat on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_pmatrix_addrows(Stack stack, int rhs, int opt, int lhs)
{
  int m1;
  NspPMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetPMatCopy(stack,1))== NULLPMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( nsp_pmatrix_add_rows(HMat,m1) != OK) return RET_BUG; ;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 *  diag function 
 */

static int int_pmatrix_diag (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspPMatrix *A, *Res;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((A = GetPMat (stack, 1)) == NULLPMAT)
    return RET_BUG;
  if (A->m == 1 || A->n == 1)
    Res = nsp_pmatrix_create_diag (A, k1);
  else
    Res = nsp_pmatrix_extract_diag (A, k1);
      
  if (Res == NULLPMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}


/*
 * Returns the kthe diag of a NspPMatrix 
 */

static int int_pmatrix_diage(Stack stack, int rhs, int opt, int lhs)
{
  int k1;
  NspPMatrix *A,*Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  Res =nsp_pmatrix_extract_diag( A,k1);
  if ( Res == NULLPMAT)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 *  Creates a NspPMatrix with kth diag set to Diag 
 */

static int int_pmatrix_diagcre(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspPMatrix *Diag,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((Diag = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (Res =nsp_pmatrix_create_diag(Diag,k1)) == NULLPMAT ) return RET_BUG ;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 *nsp_pmatrix_resize: Changes NspPMatrix dimensions
 * Warning : this routine only enlarges the array 
 * of the NspPMatrix storage so as to contain mxn 
 * elements : the previous datas are not moved and 
 * occupy the first array cells 
 * The NspPMatrix is changed 
 * return 0 on failure 
 */

static int int_pmatrix_resize(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  NspPMatrix  *HMat;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (HMat=GetPMatCopy(stack,1))== NULLPMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if ( nsp_pmatrix_resize(HMat,m1,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}


/*
 *nsp_pmatrix_enlarge(A,m,n) 
 *  changes A to B= [ A , 0; 0,0 ]  where 0 stands for "." strings
 *  in such a way that B (max(A->m,m)xmax(A->n,n));
 *  The result is stored in A 
 * WARNING : no copy 
 */

static int int_pmatrix_enlarge(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A;
  int m1,n1;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if (GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if ( nsp_pmatrix_enlarge(A,m1,n1)== FAIL)  return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Operation leading to Boolean result 
 */

/* A < B */ 

static int int_pmatrix_lt(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = nsp_pmatrix_comp(A,B,"<");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_pmatrix_le(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = nsp_pmatrix_comp(A,B,"<=");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_pmatrix_neq(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = nsp_pmatrix_comp(A,B,"<>");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_pmatrix_eq(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B;
  NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = nsp_pmatrix_comp(A,B,"==");
  
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_pmatrix_gt(Stack stack, int rhs, int opt, int lhs)
{

  NspPMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = nsp_pmatrix_comp(A,B,">");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}


static int int_pmatrix_ge(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = nsp_pmatrix_comp(A,B,">=");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Same but returns a unique boolean 
 */

static int int_pmatrix_f_gen(Stack stack, int rhs, int opt, int lhs,char *op)
{
  int rep,err=0;
  NspPMatrix *A,*B; NspObject *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  rep = nsp_pmatrix_full_comp(A,B,op,&err);
  if ( err == 1) 
    {
      Scierror("Error: operator %s , arguments with incompatible dimensions\n",op);
      return RET_BUG;
    }
  if ( rep == TRUE ) 
    {
      if (( Res =nsp_create_true_object(NVOID)) == NULLOBJ) return RET_BUG;
    }
  else 
    {
      if (( Res =nsp_create_false_object(NVOID)) == NULLOBJ) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_pmatrix_flt(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,"<");
}

static int int_pmatrix_fle(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,"<=");
}


static int int_pmatrix_fneq(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,"<>");
}

static int int_pmatrix_feq(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,"==");
}

static int int_pmatrix_fgt(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,">");
}

static int int_pmatrix_fge(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,">=");
}


static int int_pmatrix_dprim_p(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *HMat1,*HMat2;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if (( HMat1 = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if (( HMat2 =nsp_pmatrix_transpose(HMat1))  == NULLPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat2);
  return 1;
}

static int int_pmatrix_quote_p(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  NspPMatrix *HMat1,*HMat2;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if (( HMat1 = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if (( HMat2 =nsp_pmatrix_transpose(HMat1))  == NULLPMAT) return RET_BUG;
  for ( i = 0 ; i < HMat2->mn ; i++)
    {
      nsp_mat_conj(HMat2->S[i]);
    }
  MoveObj(stack,1,(NspObject *) HMat2);
  return 1;
}


static int int_pmatrix_conj(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  NspPMatrix *A;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((A = GetPMatCopy(stack,1)) == NULLPMAT) return RET_BUG;
  for ( i = 0 ; i < A->mn ; i++)
    {
      nsp_mat_conj(A->S[i]);
    }
  NSP_OBJECT(A)->ret_pos =1;
  return 1;
}

/* companion 
 *
 */

static int int_pmatrix_companion_p(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *Pm;
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((Pm= GetPMat(stack,1))== NULL) return RET_BUG;
  if ( Pm->mn == 1 )
    {
      nsp_polynom P;
      if ((P=GetPolynom(stack,1))== NULL) return RET_BUG;
      if ((A= nsp_matrix_companion((NspMatrix *) P))== NULLMAT)
	return RET_BUG;
      MoveObj(stack,1,(NspObject *) A);
      return 1;
    }
  else
    {
      int i;
      if ( Pm->mn == 0)
	{
	  if ((A= nsp_matrix_create(NVOID,'r', Pm->m , Pm->n))==NULLMAT)
	    return RET_BUG;
	  MoveObj(stack,1,(NspObject *) A);
	  return 1;
	}
      if ((A= nsp_matrix_companion(Pm->S[0]))== NULLMAT)
	return RET_BUG;
      for ( i= 1; i < Pm->mn ; i++)
	{
	  NspMatrix *loc,*loc1;
	  if ((loc= nsp_matrix_companion(Pm->S[i]))== NULLMAT)
	    return RET_BUG;
	  if ((loc1 = nsp_matrix_concat_diag(A,loc)) == NULLMAT)
	    return RET_BUG;
	  nsp_matrix_destroy(loc);
	  nsp_matrix_destroy(A);
	  A=loc1;
	}
      MoveObj(stack,1,(NspObject *) A);
      return 1;
    }
}

static int int_pmatrix_companion_m(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A,*P;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((P=GetMat(stack,1))== NULL) return RET_BUG;
  if ((A= nsp_matrix_companion( P))== NULLMAT)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}

static int int_pmatrix_roots(Stack stack, int rhs, int opt, int lhs)
{
  nsp_polynom P;
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((P=GetPolynom(stack,1))== NULL) return RET_BUG;
  if ((A= nsp_polynom_roots(P))== NULLMAT)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}

static int int_pmatrix_add(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P,*Q,*R;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P= GetPMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetPMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_pmatrix_add(P,Q))== NULLPMAT)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_pmatrix_minus_p_p(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P,*Q,*R;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P= GetPMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetPMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_pmatrix_minus(P,Q))== NULLPMAT)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_pmatrix_mult_m_p(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *P;
  NspPMatrix *Q,*R;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P= GetMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetPMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_pmatrix_mult_m_p(P,Q))== NULLPMAT)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_pmatrix_mult_p_m(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P,*R;
  NspMatrix *Q;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P= GetPMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_pmatrix_mult_p_m(P,Q))== NULLPMAT)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

/* test : mult by fft */

static int int_pmatrix_mult_tt(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P,*Q,*R;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetPMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetPMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_pmatrix_mult_tt(P,Q))== NULL)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_pmatrix_mult_tt_p_m(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P,*R;
  NspMatrix *Q;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetPMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_pmatrix_mult_tt_p_m(P,Q))== NULL)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_pmatrix_mult_tt_m_p(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *P;
  NspPMatrix *Q,*R;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetPMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_pmatrix_mult_tt_m_p(P,Q))== NULL)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_pmatrix_div_tt_p_m(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P,*R;
  NspMatrix *Q;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetPMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_pmatrix_div_tt_p_m(P,Q,FALSE))== NULL)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_pmatrix_dsl_tt_p_m(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P,*R;
  NspMatrix *Q;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetPMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_pmatrix_div_tt_p_m(P,Q,TRUE))== NULL)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

#if 0
static int int_pmatrix_div_tt_m_p(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *P;
  NspPMatrix *Q,*R;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetPMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_pmatrix_div_tt_m_p(P,Q))== NULL)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}
#endif

static int int_pmatrix_mult_p_p(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P,*Q,*R;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetPMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetPMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_pmatrix_mult_p_p(P,Q))== NULL)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

/* a horner_p_m */

static int int_pmatrix_horner_p_m(Stack stack, int rhs, int opt, int lhs)
{
  int i,flag = FALSE, ttmode = FALSE;
  NspCells *Res;
  NspPMatrix *P;
  NspMatrix *V;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  nsp_option opts[] ={{"vdim",s_bool,NULLOBJ,-1},
		      {"ttmode",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ((P=GetPMat(stack,1))== NULL) return RET_BUG;
  if ((V=GetMat(stack,2))== NULL) return RET_BUG;
  if ( get_optional_args(stack, rhs, opt, opts, &flag,&ttmode) == FAIL )
    return RET_BUG;
  if ( ttmode == TRUE ) 
    {
      /* term to term mode */
      NspMatrix *R=nsp_pmatrix_horner_tt(P,V);
      if ( R == NULL) return RET_BUG;
      MoveObj(stack,1,(NspObject *) R);
      return 1;
    }
  if ( flag == FALSE ) 
    {
      /* result is a cell dimensioned by P */
      if ((Res = nsp_cells_create(NVOID,P->m,P->n)) == NULL) 
	return RET_BUG;
      for ( i = 0 ; i < Res->mn ; i++)
	{
	  Res->objs[i] =(NspObject *) nsp_polynom_horner(P->S[i],V);
	  if ( Res->objs[i] == NULL ) return RET_BUG;
	}
    }
  else
    {
      /* result is a cell dimensioned by V */
      if ((Res = nsp_cells_create(NVOID,V->m,V->n)) == NULL) 
	return RET_BUG;
      for ( i = 0 ; i < Res->mn ; i++)
	{
	  Res->objs[i] =(NspObject *) nsp_pmatrix_horner(P,V,i);
	  if ( Res->objs[i] == NULL ) return RET_BUG;
	}
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

static int int_pmatrix_hornerm(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  NspCells *Res;
  NspPMatrix *P;
  NspMatrix *V;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetPMat(stack,1))== NULL) return RET_BUG;
  if ((V=GetMat(stack,2))== NULL) return RET_BUG;
  if ( V->m != V->n  )
    {
      Scierror("Error: second argument of function %s should be square\n",
	       NspFname(stack));
      return RET_BUG;
    }
  /* result is a cell dimensioned by P */
  if ((Res = nsp_cells_create(NVOID,P->m,P->n)) == NULL) 
    return RET_BUG;
  for ( i = 0 ; i < Res->mn ; i++)
    {
      Res->objs[i] =(NspObject *) nsp_polynom_hornerm("ce",P->S[i],V);
      if ( Res->objs[i] == NULL ) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


static int int_pmatrix_isreal(Stack stack, int rhs, int opt, int lhs)
{
  int i,strict = FALSE,ans=TRUE;
  NspPMatrix *P;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((P=GetPMat(stack,1))== NULL) return RET_BUG;
  if (rhs==2) 
    {
      if ( GetScalarBool (stack,2,&strict) == FAIL) return RET_BUG;
    }
  for ( i = 0 ; i < P->mn ; i++)
    {
      ans = nsp_mat_isreal(P->S[i],strict);  
      if ( ans == FALSE ) break;
    }
  if ( nsp_move_boolean(stack,1,ans) == FAIL)
    return RET_BUG;
  return 1;
}


static int int_pmatrix_clean (Stack stack, int rhs, int opt, int lhs)
{
  int i;
  double epsr= 1.e-10;/* DBL_EPSILON; */
  double epsa= 1.e-10;/* DBL_EPSILON; */
  NspPMatrix *P;
  CheckStdRhs (1, 3);
  CheckLhs (1, 1);
  if (rhs >= 2)
    {
      if (GetScalarDouble (stack, 2, &epsa) == FAIL)
	return RET_BUG;
    }
  if (rhs >= 3)
    {
      if (GetScalarDouble (stack, 3, &epsr) == FAIL)
	return RET_BUG;
    }

  if ((P = GetPMatCopy (stack, 1)) == NULLPMAT)
    return RET_BUG;
  
  for ( i = 0 ; i < P->mn ; i++)
    {
      nsp_mat_clean(P->S[i],3, epsa, epsr);
      if ( nsp_polynom_resize(P->S[i]) == FAIL) 
	return RET_BUG;
    }
  NSP_OBJECT (P)->ret_pos = 1;
  return 1;
}

typedef void (Fmat)(NspMatrix *A);

static int int_pmatrix_map (Stack stack, int rhs, int opt, int lhs, Fmat f)
{
  NspPMatrix *P;
  int i;
  CheckStdRhs(1,1);
  CheckLhs (1, 1);
  if ((P = GetPMatCopy (stack, 1)) == NULLPMAT)
    return RET_BUG;
  for ( i = 0 ; i < P->mn ; i++)
    {
      (*f)(P->S[i]);
      if ( nsp_polynom_resize(P->S[i]) == FAIL) 
	return RET_BUG;
    }
  NSP_OBJECT (P)->ret_pos = 1;
  return 1;
}

typedef int (Fmatc)(NspMatrix *A);

static int int_pmatrix_map_c (Stack stack, int rhs, int opt, int lhs, Fmatc f)
{
  NspPMatrix *P;
  int i;
  CheckStdRhs(1,1);
  CheckLhs (1, 1);
  if ((P = GetPMatCopy (stack, 1)) == NULLPMAT)
    return RET_BUG;
  for ( i = 0 ; i < P->mn ; i++)
    {
      if ( (*f)(P->S[i]) != OK ) 
	{
	  return RET_BUG;
	}
      if ( nsp_polynom_resize(P->S[i]) == FAIL) 
	return RET_BUG;
    }
  NSP_OBJECT (P)->ret_pos = 1;
  return 1;
}

static int int_pmatrix_abs (Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_map_c(stack, rhs, opt, lhs, nsp_mat_abs);
}

static int int_pmatrix_real (Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_map_c(stack, rhs, opt, lhs, nsp_mat_get_real);
}

static int int_pmatrix_imag (Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_map_c(stack, rhs, opt, lhs, nsp_mat_get_imag);
}

static int int_pmatrix_ceil (Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_map(stack, rhs, opt, lhs, nsp_mat_ceil);
}

static int int_pmatrix_int (Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_map(stack, rhs, opt, lhs, nsp_mat_int);
}

static int int_pmatrix_floor (Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_map(stack, rhs, opt, lhs, nsp_mat_floor);
}

static int int_pmatrix_round (Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_map(stack, rhs, opt, lhs, nsp_mat_round);
}

static int int_pmatrix_norm( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  NspPMatrix *P;
  double p=2.0;
  int id=1, i;
  const char *norm_table[] =       {"1","2","inf",NULL};
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ( (P=GetPMat(stack, 1)) == NULLPMAT ) return RET_BUG;
  if (rhs == 2)
    {
      if (IsMatObj(stack,2))
	{
	  if ( GetScalarDouble(stack, 2, &p) == FAIL ) return RET_BUG; 
	  if ( p < 1 || isnan(p) ) 
	    { 
	      Scierror("%s: second argument must be >= 1\n",NspFname(stack));
	      return RET_BUG;
	    }
	}
      else if ( IsSMatObj(stack,2))
	{
	  if ( (id=GetStringInArray(stack,2,norm_table,1)) == -1) return RET_BUG; 
	  p = id+1;
	  if ( id == 2 ) p = 1.0/(3.0 - p);
	}
      else
	{
	  Scierror("%s: second argument must be any real >= 1 or '1','2','inf','Inf'\n\n",
		   NspFname(stack));
	  return RET_BUG;
	}
    }
  if ((A= nsp_matrix_create(NVOID,'r', P->m , P->n))==NULLMAT)
    return RET_BUG;
  for ( i = 0 ; i < P->mn ; i++)
    {
      A->R[i] = nsp_vector_norm(P->S[i], p);
      if (A->R[i] < 0 ) return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(A));
  return 1;
}

static int int_pmatrix_minus (Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P;
  int i;
  CheckStdRhs(1,1);
  CheckLhs (1, 1);
  if ((P = GetPMatCopy (stack, 1)) == NULLPMAT)
    return RET_BUG;
  for ( i = 0 ; i < P->mn ; i++)
    {
      nsp_mat_minus(P->S[i]);
    }
  NSP_OBJECT (P)->ret_pos = 1;
  return 1;
}

static int int_pmatrix_dh_p_m (Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P,*R;
  NspMatrix *M;
  CheckStdRhs(2,2);
  CheckLhs (1, 1);
  if ((P = GetPMat(stack, 1)) == NULLPMAT)    return RET_BUG;
  if ((M = GetRealMat(stack, 2)) == NULLMAT)    return RET_BUG;
  if ((R= nsp_pmatrix_dh_p_m(P,M)) == NULLPMAT)return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(R));
  return Max(lhs,1);
}


static int int_pmatrix_hat_p_m (Stack stack, int rhs, int opt, int lhs)
{
  double d;
  NspPMatrix *P,*R;
  CheckStdRhs(2,2);
  CheckLhs (1, 1);
  if ((P = GetPMat(stack, 1)) == NULLPMAT)    return RET_BUG;
  if ( GetScalarDouble (stack, 2, &d) == FAIL) return RET_BUG;
  if ( !(floor(d) == d )) 
    {
      Scierror("Error: exponent should be an integer (%f found)\n",d);
      return RET_BUG;
    }
  if ( d < 0 ) 
    {
      Scierror("Error: exponent should be a positive integer (%f found)\n",d);
      return RET_BUG;
    }
  if ((R= nsp_pmatrix_hat_p_m(P,floor(d))) == NULLPMAT) 
    return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(R));
  return Max(lhs,1);
}

static int int_pmatrix_add_p_m(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P,*R;
  NspMatrix *M;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetPMat(stack,1))== NULL) return RET_BUG;
  if ((M=GetMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_pmatrix_add_m(P,M)) == NULL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_pmatrix_add_m_p(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P,*R;
  NspMatrix *M;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetPMat(stack,2))== NULL) return RET_BUG;
  if ((M=GetMat(stack,1))== NULL) return RET_BUG;
  if ((R= nsp_pmatrix_add_m(P,M)) == NULL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}



static int int_pmatrix_minus_p_m(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P,*R;
  NspMatrix *M;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetPMat(stack,1))== NULL) return RET_BUG;
  if ((M=GetMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_pmatrix_minus_m(P,M,TRUE)) == NULL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_pmatrix_minus_m_p(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P,*R;
  NspMatrix *M;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetPMat(stack,2))== NULL) return RET_BUG;
  if ((M=GetMat(stack,1))== NULL) return RET_BUG;
  if ((R= nsp_pmatrix_minus_m(P,M,FALSE)) == NULL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_pmatrix_pdiv_p_p(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B;
  NspPMatrix *Q,*R;
  CheckRhs(2,2);
  CheckLhs(0,2);
  if ((A=GetPMat(stack,1))== NULL) return RET_BUG;
  if ((B=GetPMat(stack,2))== NULL) return RET_BUG;
  if (nsp_pmatrix_pdiv_tt(A,B,&Q,&R)== FAIL) return RET_BUG;
  if ( lhs >= 2)
    {
      /* [R,Q] returned */
      MoveObj(stack,1,(NspObject *) R);
      MoveObj(stack,2,(NspObject *) Q);
    }
  else
    {
      /* Q returned */
      MoveObj(stack,1,(NspObject *) Q);
      nsp_pmatrix_destroy(R);
    }
  return Max(lhs,1);
}


typedef NspPMatrix *(*SuProPM) (NspPMatrix * A, int dim);

static int int_pmatrix_sum_prod_gen (Stack stack, int rhs, int opt, int lhs, SuProPM F)
{
  int dim=0;
  NspPMatrix *Res, *HMat;
  CheckRhs(1, 2);
  CheckOptRhs(0, 1)
  CheckLhs(1, 1);

  if ((HMat = GetPMat (stack, 1)) == NULLPMAT)
    return RET_BUG;

  if (rhs == 2)
    {
      if ( opt == 0 )
	{
	  if ( GetDimArg(stack, 2, &dim) == FAIL )
	    return RET_BUG;
	}
      else /* opt == 1 */
	{
	  nsp_option opts[] ={{"dim",dim_arg,NULLOBJ,-1},
			      { NULL,t_end,NULLOBJ,-1}};
	  if ( get_optional_args(stack, rhs, opt, opts, &dim) == FAIL )
	    return RET_BUG;
 	}
 
     if ( dim == -1 )
	{
	  Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( dim == -2 )  /* matlab compatibility flag */
	dim = GiveMatlabDimFlag(HMat);
    }

  if ((Res = (*F) (HMat, dim)) == NULLPMAT)
    return RET_BUG;

  MoveObj (stack, 1, NSP_OBJECT(Res));
  return 1;
}

int int_pmatrix_sum (Stack stack, int rhs, int opt, int lhs)
{
  return (int_pmatrix_sum_prod_gen (stack, rhs, opt, lhs, nsp_pmatrix_sum));
}

/*
 * matprod : product of all elements of a
 * a is unchanged 
 */

int int_pmatrix_prod (Stack stack, int rhs, int opt, int lhs)
{
  return (int_pmatrix_sum_prod_gen (stack, rhs, opt, lhs, nsp_pmatrix_prod));
}

/*
 * matcusum : cumulative sum of all elements of a
 * a is unchanged 
 */

/*
  int int_pmatrix_cusum (Stack stack, int rhs, int opt, int lhs)
  {
  return (int_pmatrix_sum_prod_gen (stack, rhs, opt, lhs, nsp_pmatrix_cum_sum));
  }
  
*/
 
/*
 * matcuprod : cumulative prod of all elements of a
 * a is unchanged 
 */

/*
  int int_pmatrix_cuprod (Stack stack, int rhs, int opt, int lhs)
  {
  return (int_pmatrix_sum_prod_gen(stack, rhs, opt, lhs, nsp_pmatrix_cum_prod));
}

*/

/*
 * nsp_pmatrix_triu: A=triu(a)
 */
  
int int_pmatrix_triu (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspPMatrix *HMat;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((HMat = GetPMatCopy (stack, 1)) == NULLPMAT)
    return RET_BUG;
  if ( nsp_pmatrix_triu (HMat, k1) == FAIL) return RET_BUG;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

/*
 * nsp_pmatrix_tril: A=Tril(A)
 * A is changed  
 */

int int_pmatrix_tril (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspPMatrix *HMat;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((HMat = GetPMatCopy (stack, 1)) == NULLPMAT)
    return RET_BUG;
  if ( nsp_pmatrix_tril (HMat, k1) == FAIL ) return RET_BUG;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

extern int int_linearsys_create(Stack stack, int rhs, int opt, int lhs);
extern int _wrap_extractelts_linearsys(Stack stack, int rhs, int opt, int lhs);
extern int _wrap_size_linearsys(Stack stack, int rhs, int opt, int lhs);
extern int _wrap_abcd_linearsys(Stack stack, int rhs, int opt, int lhs);
    
/*
 * The Interface for basic matrices operation 
 */

static OpTab PMatrix_func[]={
  /* specific */
  {"companion_m",int_pmatrix_companion_m},
  {"companion_p",int_pmatrix_companion_p},
  {"horner_p_m", int_pmatrix_horner_p_m},
  {"hornerm", int_pmatrix_hornerm},
  {"m2p", int_pmatrix_m2p},
  {"ce2p", int_pmatrix_ce2p},
  {"norm_p",      int_pmatrix_norm},
  {"pdiv", int_pmatrix_pdiv_p_p},
  {"pmat_create",int_pmatrix_create},
  {"roots_p",int_pmatrix_roots},
  /* standard */
  {"abs_p", int_pmatrix_abs},
  {"addcols_p_m",int_pmatrix_addcols},
  {"addrows_p",int_pmatrix_addrows},
  {"ceil_p", int_pmatrix_ceil},
  {"clean_p",  int_pmatrix_clean},
  {"concatd_m_p",int_pmatrix_concatd_m_p},
  {"concatd_p_m",int_pmatrix_concatd_p_m},
  {"concatd_p_p",int_matint_concatd}, /*  int_pmatrix_concatd}, */
  {"concatdiag_p_p",int_matint_concat_diag},
  {"concatr_m_p",int_pmatrix_concatr_m_p},
  {"concatr_p_m",int_pmatrix_concatr_p_m},
  {"concatr_p_p", int_matint_concatr}, /* int_pmatrix_concatr}, */
  {"conj_p", int_pmatrix_conj},
  {"deletecols_p", int_matint_deletecols},
  {"deleteelts_p", int_matint_deleteelts},
  {"deleterows_p", int_matint_deleterows},
  {"dh_p_m",  int_pmatrix_dh_p_m},
  // {"div_m_p",int_pmatrix_div_tt_m_p}, macros in rmatrix 
  {"div_p_m",int_pmatrix_div_tt_p_m},
  {"dprim_p", int_pmatrix_dprim_p},
  // {"dsl_m_p",int_pmatrix_div_tt_m_p}, macros in rmatrix 
  {"dsl_p_m",int_pmatrix_dsl_tt_p_m},
  {"dst_m_p",int_pmatrix_mult_tt_m_p},
  {"dst_p_m",int_pmatrix_mult_tt_p_m},
  {"dst_p_p",int_pmatrix_mult_tt},
  {"enlarge_p", int_pmatrix_enlarge },
  {"eq_p_p" ,  int_pmatrix_eq },
  {"extract_p", int_matint_extract}, 
  {"extractcols_p", int_matint_extractcols}, 
  {"extractelts_p", int_matint_extractelts}, 
  {"extractrows_p", int_matint_extractrows_pointer}, 
  {"feq_p_p" ,  int_pmatrix_feq },
  {"fge_p_p" ,  int_pmatrix_fge },
  {"fgt_p_p" ,  int_pmatrix_fgt },
  {"fle_p_p" ,  int_pmatrix_fle },
  {"floor_p",     int_pmatrix_floor},
  {"flt_p_p" ,  int_pmatrix_flt },
  {"fneq_p_p" ,  int_pmatrix_fneq },
  {"ge_p_p" ,  int_pmatrix_ge },
  {"gt_p_p" ,  int_pmatrix_gt },
  {"hat_p_m",  int_pmatrix_hat_p_m},
  {"imag_p", int_pmatrix_imag},
  {"int_p",    int_pmatrix_int},
  {"isreal_p", int_pmatrix_isreal},
  {"isvector_p", int_matint_isvector},
  {"le_p_p" ,  int_pmatrix_le },
  {"lt_p_p" ,  int_pmatrix_lt },
  {"matrix_p",int_matint_redim},
  {"minus_m_p",  int_pmatrix_minus_m_p},
  {"minus_p", int_pmatrix_minus},
  {"minus_p_m",  int_pmatrix_minus_p_m},
  {"minus_p_p",int_pmatrix_minus_p_p},
  {"mult_m_p",int_pmatrix_mult_m_p},
  {"mult_p_m",int_pmatrix_mult_p_m},
  {"mult_p_p",int_pmatrix_mult_p_p},
  {"ne_p_p" ,  int_pmatrix_neq },
  {"plus_m_p",int_pmatrix_add_m_p},
  {"plus_p_m",int_pmatrix_add_p_m},
  {"plus_p_p",int_pmatrix_add},
  {"quote_p", int_pmatrix_quote_p},
  {"real_p", int_pmatrix_real},
  {"redim_p",int_matint_redim},
  {"reshape_p",int_matint_redim},
  {"resize2vect_p", int_matint_resize2vect},
  {"resize_p",int_pmatrix_resize},
  {"roots_p",int_pmatrix_roots},
  {"round_p",      int_pmatrix_round},
  {"setrowscols_p", int_matint_setrowscols},
  {"tozero_p", int_matint_tozero},
  {"diag_p", int_pmatrix_diag},
  {"diag_p_m", int_pmatrix_diag},
  {"diagcre_p",int_pmatrix_diagcre},
  {"diage_p",int_pmatrix_diage},

  {"sum_p_s", int_pmatrix_sum},
  {"sum_p", int_pmatrix_sum},
  /* 
  {"cumsum_p_s", int_pmatrix_cusum},
  {"cumsum_p", int_pmatrix_cusum},
  */
  {"prod_p_s", int_pmatrix_prod},
  {"prod_p", int_pmatrix_prod},
  /*
  {"cumprod_p_s", int_pmatrix_cuprod},
  {"cumprod_p", int_pmatrix_cuprod},
  */
  {"tril_p", int_pmatrix_tril},
  {"triu_p", int_pmatrix_triu},
  {"linear_system",int_linearsys_create},
  {"extractelts_linearsys",_wrap_extractelts_linearsys},
  {"size_linearsys", _wrap_size_linearsys},
  {"abcd_linearsys", _wrap_abcd_linearsys},
  {(char *) 0, NULL}
};

int PMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(PMatrix_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) */

void PMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = PMatrix_func[i].name;
  *f = PMatrix_func[i].fonc;
}










