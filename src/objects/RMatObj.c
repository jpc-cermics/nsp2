/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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
#include <nsp/pmatrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/smatrix.h> 
#define RMatrix_Private 
#include <nsp/rmatrix.h> 
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

static int int_rmatrix_create(Stack stack, int rhs, int opt, int lhs);

/*
 * NspRMatrix inherits from NspObject 
 * Rational matrices 
 */

int nsp_type_rmatrix_id=0;
NspTypeRMatrix *nsp_type_rmatrix=NULL;
int nsp_type_rmatrix_init();

NspTypeRMatrix *new_type_rmatrix(type_mode mode)
{
  NspTypeMatint *mati;/* interface */
  NspTypeRMatrix *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_rmatrix != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_rmatrix;
    }
  if (( type =  malloc(sizeof(NspTypeRMatrix))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = rmatrix_attrs; 
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = rmatrix_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_rmatrix;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for rmatrix */ 

  top->pr = (print_func *)nsp_rmatrix_print;                    /* printing*/   
  top->dealloc = (dealloc_func *)nsp_rmatrix_destroy;              /* dealloc */  
  top->copy  =  (copy_func *)nsp_rmatrix_copy;                   /* copy object */  
  top->size  = (size_func *)nsp_rmatrix_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_rmatrix_type_as_string;                /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_rmatrix_type_short_string;              /* type as a short string */  
  top->info = (info_func *)nsp_rmatrix_info;                    /* info */  
  /* top->is_true = (is_true_func  *) RMatIsTrue; */
  /* top->loop =(loop_func *)nsp_rmatrix_loop_extract;   */             /* for loops */  
  top->path_extract =  NULL;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_rmatrix_object;    /* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_rmatrix_eq;                       /* equality check */  
  top->neq  = (eq_func *)nsp_rmatrix_neq;                      /* non-equality check */

  top->save  = (save_func *)nsp_rmatrix_xdr_save;
  top->load  = (load_func *)nsp_rmatrix_xdr_load;
  top->create = (create_func*) int_rmatrix_create; 
  top->latex = (print_func*) nsp_rmatrix_latex_print;
  top->full_copy  =  (copy_func *)nsp_rmatrix_copy;                   /* copy object */  

  /* specific methods for rmatrix */
  type->init = (init_func *) init_rmatrix;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */

  mati = new_type_matint(T_DERIVED);
  mati->methods = matint_get_methods; 
  /* mati->redim = (matint_redim *) nsp_rmatrix_redim;  use default value */
  mati->resize = (matint_resize  *) nsp_rmatrix_resize; 
  mati->free_elt = (matint_free_elt *) nsp_rational_destroy;
  mati->elt_size = (matint_elt_size *) nsp_rmatrix_elt_size ;
  mati->clone = (matint_clone *) nsp_rmatrix_clone;
  mati->copy_elt = (matint_copy_elt *) nsp_rational_copy_with_name; 
  mati->enlarge = (matint_enlarge *) nsp_rmatrix_enlarge;
  mati->canonic =  nsp_matint_canonic;
  mati->copy_ind = nsp_matint_basic_copy_pointer;
  type->interface = (NspTypeBase *) mati;
  
  if ( nsp_type_rmatrix_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_rmatrix
       */
      type->id =  nsp_type_rmatrix_id = nsp_new_type_id();
      nsp_type_rmatrix = type;
      if ( nsp_register_type(nsp_type_rmatrix) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_rmatrix(mode);
    }
  else 
    {
      type->id = nsp_type_rmatrix_id;
      return type;
    }

}
/*
 * initialize Pmatrix instances 
 * locally and by calling initializer on parent class 
 */

static int init_rmatrix(NspRMatrix *o,NspTypeRMatrix *type)
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

NspRMatrix *new_rmatrix() 
{
  NspRMatrix *loc; 
  /* type must exists */
  nsp_type_rmatrix = new_type_rmatrix(T_BASE);
  if ( (loc = malloc(sizeof(NspRMatrix)))== NULLRMAT) return loc;
  /* initialize object */
  if ( init_rmatrix(loc,nsp_type_rmatrix) == FAIL) return NULLRMAT;
  return loc;
}


/*
 * MatSize : returns m,n,or m*n 
 */

int nsp_rmatrix_size(NspRMatrix *Mat, int flag)
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

static char rmat_type_name[]="RMat";
static char rmat_short_type_name[]="r";

char *nsp_rmatrix_type_as_string(void)
{
  return(rmat_type_name);
}

char *nsp_rmatrix_type_short_string(NspObject *v)
{
  return(rmat_short_type_name);
}


static int nsp_rmatrix_full_comp(NspRMatrix * A,NspRMatrix * B,char *op,int *err)
{
  int i, rep;
  if ( A->m != B->m || A->n != B->n) return FALSE;
  if ( ! nsp_rmatrix_same_varname(A,B))  return FALSE;
  for ( i = 0 ; i < A->mn ; i++ ) 
    {
      rep = nsp_mat_fullcomp (A->S[i]->num,B->S[i]->num,op, err);
      if ( *err == TRUE || rep == FALSE ) return FALSE;
    }
  for ( i = 0 ; i < A->mn ; i++ ) 
    {
      rep = nsp_mat_fullcomp (A->S[i]->den,B->S[i]->den,op, err);
      if ( *err == TRUE || rep == FALSE ) return FALSE;
    }
  return TRUE;
}

int nsp_rmatrix_eq(NspObject *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_rmatrix_id) == FALSE) return FALSE ;
  rep = nsp_rmatrix_full_comp((NspRMatrix *) A,(NspRMatrix *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

int nsp_rmatrix_neq(NspObject *A, NspObject *B)
{
  return ( nsp_rmatrix_eq(A,B) == TRUE ) ? FALSE : TRUE ;
}

/*
 * Mat == TRUE ? 
 *  if Mat != [] and all the elements of Mat are != 0
 * RMatIsTrue(NspRMatrix *M)
 */


/*
 * Save a Matrix in a file stream 
 */

static int nsp_rmatrix_xdr_save(XDR *xdrs, NspRMatrix *M)
{
  int i;
#if 1 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_rmatrix)) == FAIL) return FAIL;
#else
  if (nsp_xdr_save_i(xdrs, M->type->id) == FAIL)    return FAIL;
#endif 
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->m) == FAIL)    return FAIL;
  if (nsp_xdr_save_i(xdrs, M->n) == FAIL)    return FAIL;
  if (nsp_xdr_save_c(xdrs, M->rc_type) == FAIL)  return FAIL;
  if (nsp_xdr_save_c(xdrs, M->dom) == FAIL)  return FAIL;
  if (nsp_xdr_save_d(xdrs, M->dt) == FAIL)  return FAIL;
  for ( i = 0 ; i < M->mn ; i++ ) 
    {
      if (nsp_object_xdr_save(xdrs,(NspObject *) M->S[i]) == FAIL) return FAIL;
    }
  return OK;
}

/*
 * Load a Matrix from a file stream 
 **/

static NspRMatrix *nsp_rmatrix_xdr_load(XDR *xdrs)
{
  char c,dom;
  double dt;
  int m, n,i;
  NspRMatrix *M;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLRMAT;
  if (nsp_xdr_load_i(xdrs, &m) == FAIL) return NULLRMAT;
  if (nsp_xdr_load_i(xdrs, &n) == FAIL) return NULLRMAT;
  if (nsp_xdr_load_c(xdrs, &c) == FAIL) return NULLRMAT;
  if (nsp_xdr_load_c(xdrs, &dom) == FAIL) return NULLRMAT;
  if (nsp_xdr_load_d(xdrs, &dt) == FAIL) return NULLRMAT;
  if ((M =nsp_rmatrix_create(name,m,n,NULL,-1, NULL,dom,dt))== NULLRMAT)
    return NULLRMAT;
  for ( i = 0 ; i < M->mn ; i++ ) 
    {
      NspObject *Obj =nsp_object_xdr_load(xdrs); 
      if ( Obj == NULLOBJ ) return NULLRMAT;
      M->S[i]= (nsp_rational) Obj;
    }
  return M;
}


/*
 * A =nsp_rmatrix_object(O);
 * checks that O is an object of NspRMatrix type. 
 * or a Hobj which points to an object of type RMatrix
 * if so, returns a pointer to that NspRMatrix and else returns NULL
 */

NspRMatrix   *nsp_rmatrix_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_rmatrix_id) == TRUE) return ((NspRMatrix *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_rmatrix));
  return(NULL);
}


/*
 * IsRMatObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  RMatrix 
 * or a Hobj which points to an object of type RMatrix
 */

int IsRMatObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_rmatrix_id);
}

/*
 * IsRMat(O)
 * only checks that object is an object of type  RMatrix 
 * or a Hobj which points to an object of type RMatrix
 */

int IsRMat(NspObject *O)
{
  return nsp_object_type(O , nsp_type_rmatrix_id);
}

/*
 * Checks that first+i object on the stack 
 * is a NspRMatrix and returns that NspRMatrix  
 * or a copy of that NspRMatrix if its name 
 * is != NVOID 
 */

NspRMatrix*GetRMatCopy(Stack stack, int i)
{
  if (  GetRMat(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/*
 * Checks that first+i object on the stack 
 * is a NspRMatrix and returns that NspRMatrix  
 */

NspRMatrix*GetRMat(Stack stack, int i)
{
  NspRMatrix *M;
  if (( M =nsp_rmatrix_object(NthObj(i))) == NULLRMAT  )
    ArgMessage(stack,i);
  return M;
}

/*-------------------------------------------------------------------
 * wrappers for the RMatrix 
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/*
 * Matrix ( used ar row vector 1xn ) -> 1x1 polymatrix filled 
 * with one polynom of degree n-1
 * 
 */

static int int_rmatrix_m2r(Stack stack, int rhs, int opt, int lhs)
{
  int dim=0;
  const char *var = NULL;
  nsp_option opts[] ={{"var",string,NULLOBJ,-1},
		      {"dim",dim_arg,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspRMatrix *P; NspMatrix *A, *B=NULL;
  CheckStdRhs(1,2);
  CheckLhs(1,1);
  if (( A=GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if ( rhs -opt == 2 )
    {
      if (( B=GetMat(stack,2)) == NULLMAT) return RET_BUG;
    }
  if ( get_optional_args(stack, rhs, opt, opts, &var,&dim) == FAIL )
    return RET_BUG;
  if ( dim == -2 )
    {
      Scierror ("Error:\t dim flag equal to -2 or 'm' not supported for function %s\n", NspFname(stack));
      return RET_BUG;
    }
  switch ( dim)
    {
    case 0:
      if ( rhs -opt == 1)
	{
	  if (( P=nsp_matrix_to_rational(A))== NULLRMAT) return RET_BUG;
	}
      else
	{
	  if (( P=nsp_matrices_to_rational(A,B))== NULLRMAT) return RET_BUG;
	}
      break;
    case -1 :
      if ( rhs -opt == 1)
	{
	  if (( P=nsp_matrix_to_rmatrix(A))== NULLRMAT) return RET_BUG;
	}
      else
	{
	  if (( P=nsp_matrices_to_rmatrix(A,B))== NULLRMAT) return RET_BUG;
	}
      break;
    default:
      Scierror ("Error:\t dim flag equal to %d not supported for function %s\n",dim, NspFname(stack));
      return RET_BUG;
    }
  if ( nsp_rmatrix_set_varname(P,var)) return RET_BUG;
  MoveObj(stack,1,(NspObject *) P);
  return 1;
}

/*
 * PMatrix ( used ar row vector 1xn ) -> 1x1 polymatrix filled 
 * with one polynom of degree n-1
 * 
 */

static int int_rmatrix_p2r(Stack stack, int rhs, int opt, int lhs)
{
  int simp = FALSE;
  const char *var = NULL;
  nsp_option opts[] ={{"var",string,NULLOBJ,-1},
		      {"simp",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspRMatrix *R; NspPMatrix *A, *B=NULL;
  CheckStdRhs(1,2);
  CheckLhs(1,1);
  if (( A=GetPMat(stack,1)) == NULL) return RET_BUG;
  if ( rhs -opt == 2 )
    {
      if (( B=GetPMat(stack,2)) == NULL) return RET_BUG;
    }
  if ( get_optional_args(stack, rhs, opt, opts, &var,&simp) == FAIL )
    return RET_BUG;
  if ( rhs -opt == 1)
    {
      if (( R=nsp_pmatrix_to_rmatrix(A))== NULLRMAT) return RET_BUG;
    }
  else
    {
      if (( R=nsp_pmatrices_to_rmatrix(A,B,simp))== NULLRMAT) return RET_BUG;
    }
  if ( var != NULL)
    {
      if ( nsp_rmatrix_set_varname(R,var) ) return RET_BUG;
    }
  else
    {
      if ( nsp_rmatrix_set_varname(R,A->var) ) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

/*
 * build of rational matrix from its coefficients given 
 * in two cells 
 */

static int int_rmatrix_ce2r(Stack stack, int rhs, int opt, int lhs)
{
  const char *var = NULL;
  nsp_option opts[] ={{"var",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspRMatrix *R; NspCells *C1, *C2;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  if (( C1=GetCells(stack,1)) == NULL) return RET_BUG;
  if (( C2=GetCells(stack,2)) == NULL) return RET_BUG;
  CheckSameDims (NspFname(stack), 1, 2, C1, C2);
  if ( get_optional_args(stack, rhs, opt, opts, &var) == FAIL )
    return RET_BUG;
  if (( R=nsp_cells_to_rmatrix(NVOID,C1,C2))== NULLRMAT) return RET_BUG;
  if ( nsp_rmatrix_set_varname(R,var) ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

/*
 * Creation of a RMatrix from a Matrix 
 */

static int int_rmatrix_create(Stack stack, int rhs, int opt, int lhs)
{
  const char *var = NULL;
  nsp_option opts[] ={{"var",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  int m1,n1;
  NspMatrix *A=NULLMAT; 
  NspRMatrix *P; 
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
  if ( (P =nsp_rmatrix_create_m(NVOID,m1,n1,A, var)) == NULLRMAT)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) P);
  return 1;
}


/*------------------------------------------------------
 * attributes  
 *------------------------------------------------------*/

/* return all the keys H.keys entered in the hash table as a string matrice  */

static NspObject *int_rmatrix_get_num(void *Hv,const char *attr)
{
  int i;
  NspRMatrix *R = Hv;
  NspPMatrix *loc;
  if ((loc =nsp_pmatrix_create(NVOID,R->m,R->n,NULL,-1,R->var))== NULLPMAT)
    return NULL;
  for ( i = 0 ; i < R->mn ; i++)
    {
      if ((loc->S[i] = nsp_polynom_copy_and_name("pe",R->S[i]->num))== NULL)
	return NULL;
    }
  return NSP_OBJECT(loc);
}

static int int_rmatrix_set_num(void *Hv,const char *attr, NspObject *Obj)
{
  int i;
  NspRMatrix *R = Hv;
  NspPMatrix *P = (NspPMatrix *) Obj;
  if (IsPMat(Obj) && (R->m == P->m && R->n == P->n ))
    {
      for ( i = 0 ; i < R->mn ; i++)
	{
	  NspMatrix *M =  nsp_polynom_copy_and_name("pe",P->S[i]);
	  if ( M == NULL) goto err;
	  nsp_polynom_destroy(&R->S[i]->num);
	  R->S[i]->num = M;
	}
      return OK;
    }
  Scierror("Error: argument should be a polynomial %dx%d matrix\n",R->m,R->n);
  return FAIL;
 err:
  return FAIL;
}

static NspObject *int_rmatrix_get_den(void *Hv,const char *attr)
{
  int i;
  NspRMatrix *R = Hv;
  NspPMatrix *loc;
  if ((loc =nsp_pmatrix_create(NVOID,R->m,R->n,NULL,-1,R->var))== NULLPMAT)
    return NULL;
  for ( i = 0 ; i < R->mn; i++)
    {
      if ((loc->S[i] = nsp_polynom_copy_and_name("pe",R->S[i]->den))== NULL)
	return NULL;
    }
  return NSP_OBJECT(loc);
}

static int int_rmatrix_set_den(void *Hv,const char *attr, NspObject *Obj)
{
  int i;
  NspRMatrix *R = Hv;
  NspPMatrix *P = (NspPMatrix *) Obj;
  if (IsPMat(Obj) && (R->m == P->m && R->n == P->n ))
    {
      for ( i = 0 ; i < R->mn ; i++)
	{
	  NspMatrix *M =  nsp_polynom_copy_and_name("pe",P->S[i]);
	  if ( M == NULL) goto err;
	  nsp_polynom_destroy(&R->S[i]->den);
	  R->S[i]->den = M;
	}
      return OK;
    }
  Scierror("Error: argument should be a polynomial %dx%d matrix\n",R->m,R->n);
  return FAIL;
 err:
  return FAIL;
}
static NspObject *int_rmatrix_get_dt(void *Hv,const char *attr)
{
  NspRMatrix *R = Hv;
  NspMatrix *M;
  if (( M = nsp_matrix_create(NVOID,'r',1,1) ) == NULLMAT ) return NULL;
  M->R[0] = R->dt;
  return (NspObject *) M;
}

static int int_rmatrix_set_dt(void *Hv,const char *attr, NspObject *Obj)
{
  double d;
  NspRMatrix *R = Hv;
  if ( DoubleScalar(Obj,&d) == FAIL ) 
    {
      Scierror("Error: argument should be a double\n");
      return FAIL;
    }
  R->dt = d;
  return OK;
}

static NspObject *int_rmatrix_get_dom(void *Hv,const char *attr)
{
  char str[2]={((NspRMatrix *) Hv)->dom,0};
  return nsp_new_string_obj(NVOID,str, -1);
}

static int int_rmatrix_set_dom(void *Hv,const char *attr, NspObject *Obj)
{
  if ( IsString(Obj))
    {
      char *str = ((NspSMatrix *) Obj)->S[0];
      if ( strlen(str) > 0 ) ((NspRMatrix *) Hv)->dom = str[0];
      else
	{
	  Scierror("Error: argument should be a sring of length >0 \n");
	  return FAIL;
	}
    }
  else
    {
      Scierror("Error: argument should be a sring\n");
      return FAIL;
    }
  return OK;
}

static AttrTab rmatrix_attrs[] = {
  { "num", 	int_rmatrix_get_num , int_rmatrix_set_num , 	NULL, NULL  },
  { "den", 	int_rmatrix_get_den , int_rmatrix_set_den , 	NULL, NULL  },
  { "dom", 	int_rmatrix_get_dom , int_rmatrix_set_dom , 	NULL, NULL  },
  { "dt", 	int_rmatrix_get_dt  , int_rmatrix_set_dt  ,     NULL, NULL  },
  { (char *) 0, NULL, NULL , NULL , NULL }
};


/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

int nsp_rmatrix_set_varname(NspRMatrix *p, const char *varname)
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
  if (nsp_rmatrix_set_varname(self,varname)== FAIL) return RET_BUG;
  return 0;
}

static int int_meth_get_varname(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *p=self;
  CheckStdRhs(0,0);
  if ( nsp_move_string(stack,1,(p->var != NULL) ? p->var : "x",-1 )== FAIL) return RET_BUG;
  return 1;
}

static int int_meth_normalize(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int i,j;
  char type = 'r';
  NspMatrix *N;
  NspRMatrix *P=self;
  CheckRhs(0,0);
  CheckLhs(0,1);
  for ( i = 0 ; i < P->mn ; i++) 
    if (P->S[i]->num->rc_type == 'c') type = 'c';
  if ((N = nsp_matrix_create(NVOID,type,P->m,P->n)) == NULL) 
    return RET_BUG;
  for ( i = 0 ; i < P->mn ; i++) 
    {
      NspMatrix *A= P->S[i]->num;
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

static int int_meth_get_num(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *N;
  NspRMatrix *p=self;
  CheckStdRhs(0,0);
  if ((N = int_rmatrix_get_num(p,"num"))== NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(N));
  return 1;
}

static int int_meth_get_den(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *N;
  NspRMatrix *p=self;
  CheckStdRhs(0,0);
  if ((N = int_rmatrix_get_den(p,"den"))== NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(N));
  return 1;
}

static int int_meth_get_dom(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *N;
  NspRMatrix *p=self;
  CheckStdRhs(0,0);
  if ((N = int_rmatrix_get_dom(p,"dom"))== NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(N));
  return 1;
}
static int int_meth_get_dt(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *N;
  NspRMatrix *p=self;
  CheckStdRhs(0,0);
  if ((N = int_rmatrix_get_dt(p,"dt"))== NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(N));
  return 1;
}

static int int_meth_set_num(void *self, Stack stack, int rhs, int opt, int lhs)
{
  int i;
  NspPMatrix *P;
  NspRMatrix *R=self;
  CheckStdRhs(0,1);
  if ((P = GetPMat(stack,1))== NULL) return RET_BUG;
  if ( P->m != R->m || P->n != R->n )
    {
      Scierror("Error: first argument should be of size %dx%d\n",R->m,R->n);
      return RET_BUG;
    }
  if ((P =(NspPMatrix *) nsp_object_copy_and_name("re",(NspObject*)P))==NULL) return RET_BUG;
  for ( i = 0 ; i < R->mn ; i++)
    {
      R->S[i]->num = P->S[i];
      P->S[i] = NULL;
    }
  nsp_pmatrix_destroy(P);
  return 0;
}

static int int_meth_set_den(void *self, Stack stack, int rhs, int opt, int lhs)
{
  int i;
  NspPMatrix *P;
  NspRMatrix *R=self;
  CheckStdRhs(0,1);
  if ((P = GetPMat(stack,1))== NULL) return RET_BUG;
  if ( P->m != R->m || P->n != R->n )
    {
      Scierror("Error: first argument should be of size %dx%d\n",R->m,R->n);
      return RET_BUG;
    }
  if ((P =(NspPMatrix *) nsp_object_copy_and_name("re",(NspObject*)P))==NULL) return RET_BUG;
  for ( i = 0 ; i < R->mn ; i++)
    {
      R->S[i]->den = P->S[i];
      P->S[i] = NULL;
    }
  nsp_pmatrix_destroy(P);
  return 0;
}

static int int_meth_set_dom(void *self, Stack stack, int rhs, int opt, int lhs)
{
  int id;
  NspRMatrix *R=self;
  const char *dom_table[] =       {"c","d","u","s", NULL};
  CheckStdRhs(1,1);
  CheckLhs(0,0);
  if ( IsSMatObj(stack,1))
    {
      if ( (id=GetStringInArray(stack,1,dom_table,1)) == -1) return RET_BUG; 
      R->dom = dom_table[id][0];
    }
  else
    {
      Scierror("%s: first argument should be a string\n", NspFname(stack));
      return RET_BUG;
    }
  return 0;
}

static int int_meth_set_dt(void *self, Stack stack, int rhs, int opt, int lhs)
{
  double dt;
  NspRMatrix *R=self; 
  CheckStdRhs(1,1);
  CheckLhs(0,0);
  if (GetScalarDouble (stack, 1, &dt) == FAIL) return RET_BUG;
  R->dt = dt;
  return 0;
}

static NspMethods rmatrix_methods[] = {
  { "normalize", int_meth_normalize},
  { "set_var", int_meth_set_varname},
  { "get_var", int_meth_get_varname},
  { "set_num", int_meth_set_num},
  { "get_num", int_meth_get_num},
  { "set_den", int_meth_set_den},
  { "get_den", int_meth_get_den},
  { "set_dom", int_meth_set_dom},
  { "get_dom", int_meth_get_dom},
  { "set_dt",  int_meth_set_dt},
  { "get_dt",  int_meth_get_dt},
  { (char *) 0, NULL}
};

static NspMethods *rmatrix_get_methods(void) { return rmatrix_methods;};

/*----------------------------------
 * Interfaces 
 *----------------------------------*/

/*
 * Res = [A,P]  when A is a scalar matrix 
 */

static int int_rmatrix_concatr_m_r(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *B,*Res;
  NspMatrix * A;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( A->mn == 0)
    {
      if ((B = GetRMat(stack,2)) == NULLRMAT) return RET_BUG;
      /* A and B must have compatible rows */
      if (A->m != B->m && A->m != A->n ) 
	{
	  Scierror("Error: matrices should have same number of rows found %d and %d\n",
		   A->m, B->m);
	  return RET_BUG;
	}
      if ( B->mn == 0) 
	{
	  if ((B = GetRMatCopy(stack,2)) == NULLRMAT) return RET_BUG;
	  B->n += A->n;
	}
      NSP_OBJECT(B)->ret_pos = 1;
      return 1;
    }
  if ((B = GetRMat(stack,2)) == NULLRMAT) return RET_BUG;
  if (( Res=nsp_matrix_to_rmatrix_with_varname(A,B->var)) == NULLRMAT) return RET_BUG;
  if ( B->mn != 0)
    {
      if (nsp_rmatrix_concat_right(Res,B)!= OK) return RET_BUG;
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

static int int_rmatrix_concatr_r_m(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *A,*P2;
  NspMatrix *B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRMatCopy(stack,1))  == NULLRMAT) return RET_BUG;
  if ( A->mn == 0)
    {
      if ((B = GetMat(stack,2)) == NULLMAT) return RET_BUG;
      if ((P2=nsp_matrix_to_rmatrix_with_varname(B,A->var)) == NULLRMAT) return RET_BUG;
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
      if ((P2=nsp_matrix_to_rmatrix_with_varname(B,A->var)) == NULLRMAT) return RET_BUG;
      if (nsp_rmatrix_concat_right(A,P2) != OK) return RET_BUG;
      nsp_rmatrix_destroy(P2);
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

static int int_rmatrix_concatd_m_r(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *B,*Res;
  NspMatrix * A;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( A->mn == 0)
    {
      if ((B = GetRMat(stack,2)) == NULLRMAT) return RET_BUG;
      if (A->n != B->n && A->m != A->n ) 
	{
	  Scierror("Error: matrices should have same number of columns found %d and %d\n",
		   A->n, B->n);
	  return RET_BUG;
	}
      if ( B->mn == 0 ) 
	{
	  if ((B = GetRMatCopy(stack,2)) == NULLRMAT) return RET_BUG;
	  B->m += A->m;
	}
      NSP_OBJECT(B)->ret_pos= 1;
      return 1;
    }
  if ((B = GetRMat(stack,2)) == NULLRMAT) return RET_BUG;
  if (( Res=nsp_matrix_to_rmatrix_with_varname(A,B->var)) == NULLRMAT) return RET_BUG;
  if ( B->mn != 0)
    {
      if ((Res=nsp_rmatrix_concat_down(Res,B))== NULLRMAT ) return RET_BUG;
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

static int int_rmatrix_concatd_r_m(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *A,*P2;
  NspMatrix *B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRMat(stack,1))  == NULLRMAT) return RET_BUG;
  if ( A->mn == 0)
    {
      if ((B = GetMat(stack,2)) == NULLMAT) return RET_BUG;
      if ((P2=nsp_matrix_to_rmatrix_with_varname(B,A->var)) == NULLRMAT) return RET_BUG;
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
      NspRMatrix *Res;
      if ((P2=nsp_matrix_to_rmatrix_with_varname(B,A->var)) == NULLRMAT) return RET_BUG;
      if ((Res=nsp_rmatrix_concat_down(A,P2))== NULLRMAT ) return RET_BUG;
      nsp_rmatrix_destroy(P2);
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
 *nsp_rmatrix_add_columns: add n cols of zero to NspRMatrix A 
 * A= [A,ones(m,n)] 
 * return 0 on failure ( No more space )
 * A is changed 
 */

static int int_rmatrix_addcols(Stack stack, int rhs, int opt, int lhs)
{
  int n1;
  NspRMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetRMatCopy(stack,1))== NULLRMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ( nsp_rmatrix_add_columns(HMat,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 * AddRows : Add m rows of zero to a NspRMatrix A 
 * A = [A;ones(m,n)]
 * return NULLSMat on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_rmatrix_addrows(Stack stack, int rhs, int opt, int lhs)
{
  int m1;
  NspRMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetRMatCopy(stack,1))== NULLRMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( nsp_rmatrix_add_rows(HMat,m1) != OK) return RET_BUG; ;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 *  diag function 
 */

static int int_rmatrix_diag (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspRMatrix *A, *Res;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((A = GetRMat (stack, 1)) == NULLRMAT)
    return RET_BUG;
  if (A->m == 1 || A->n == 1)
    Res = nsp_rmatrix_create_diag (A, k1);
  else
    Res = nsp_rmatrix_extract_diag (A, k1);
      
  if (Res == NULLRMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}


/*
 * Returns the kthe diag of a NspRMatrix 
 */

static int int_rmatrix_diage(Stack stack, int rhs, int opt, int lhs)
{
  int k1;
  NspRMatrix *A,*Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
  if ((A = GetRMat(stack,1)) == NULLRMAT) return RET_BUG;
  Res =nsp_rmatrix_extract_diag( A,k1);
  if ( Res == NULLRMAT)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 *  Creates a NspRMatrix with kth diag set to Diag 
 */

static int int_rmatrix_diagcre(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspRMatrix *Diag,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((Diag = GetRMat(stack,1)) == NULLRMAT) return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (Res =nsp_rmatrix_create_diag(Diag,k1)) == NULLRMAT ) return RET_BUG ;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 *nsp_rmatrix_resize: Changes NspRMatrix dimensions
 * Warning : this routine only enlarges the array 
 * of the NspRMatrix storage so as to contain mxn 
 * elements : the previous datas are not moved and 
 * occupy the first array cells 
 * The NspRMatrix is changed 
 * return 0 on failure 
 */

static int int_rmatrix_resize(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  NspRMatrix  *HMat;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (HMat=GetRMatCopy(stack,1))== NULLRMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if ( nsp_rmatrix_resize(HMat,m1,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}


/*
 *nsp_rmatrix_enlarge(A,m,n) 
 *  changes A to B= [ A , 0; 0,0 ]  where 0 stands for "." strings
 *  in such a way that B (max(A->m,m)xmax(A->n,n));
 *  The result is stored in A 
 * WARNING : no copy 
 */

static int int_rmatrix_enlarge(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *A;
  int m1,n1;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetRMat(stack,1)) == NULLRMAT) return RET_BUG;
  if (GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if ( nsp_rmatrix_enlarge(A,m1,n1)== FAIL)  return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Operation leading to Boolean result 
 */

/* A < B */ 

static int int_rmatrix_lt(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRMat(stack,1)) == NULLRMAT) return RET_BUG;
  if ((B = GetRMat(stack,2)) == NULLRMAT) return RET_BUG;
  Res = nsp_rmatrix_comp(A,B,"<");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_rmatrix_le(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRMat(stack,1)) == NULLRMAT) return RET_BUG;
  if ((B = GetRMat(stack,2)) == NULLRMAT) return RET_BUG;
  Res = nsp_rmatrix_comp(A,B,"<=");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_rmatrix_neq(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRMat(stack,1)) == NULLRMAT) return RET_BUG;
  if ((B = GetRMat(stack,2)) == NULLRMAT) return RET_BUG;
  Res = nsp_rmatrix_comp(A,B,"<>");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_rmatrix_eq(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *A,*B;
  NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRMat(stack,1)) == NULLRMAT) return RET_BUG;
  if ((B = GetRMat(stack,2)) == NULLRMAT) return RET_BUG;
  Res = nsp_rmatrix_comp(A,B,"==");
  
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_rmatrix_gt(Stack stack, int rhs, int opt, int lhs)
{

  NspRMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRMat(stack,1)) == NULLRMAT) return RET_BUG;
  if ((B = GetRMat(stack,2)) == NULLRMAT) return RET_BUG;
  Res = nsp_rmatrix_comp(A,B,">");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}


static int int_rmatrix_ge(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRMat(stack,1)) == NULLRMAT) return RET_BUG;
  if ((B = GetRMat(stack,2)) == NULLRMAT) return RET_BUG;
  Res = nsp_rmatrix_comp(A,B,">=");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Same but returns a unique boolean 
 */

static int int_rmatrix_f_gen(Stack stack, int rhs, int opt, int lhs,char *op)
{
  int rep,err=0;
  NspRMatrix *A,*B; NspObject *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRMat(stack,1)) == NULLRMAT) return RET_BUG;
  if ((B = GetRMat(stack,2)) == NULLRMAT) return RET_BUG;
  rep = nsp_rmatrix_full_comp(A,B,op,&err);
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

static int int_rmatrix_flt(Stack stack, int rhs, int opt, int lhs)
{
  return int_rmatrix_f_gen(stack,rhs,opt,lhs,"<");
}

static int int_rmatrix_fle(Stack stack, int rhs, int opt, int lhs)
{
  return int_rmatrix_f_gen(stack,rhs,opt,lhs,"<=");
}


static int int_rmatrix_fneq(Stack stack, int rhs, int opt, int lhs)
{
  return int_rmatrix_f_gen(stack,rhs,opt,lhs,"<>");
}

static int int_rmatrix_feq(Stack stack, int rhs, int opt, int lhs)
{
  return int_rmatrix_f_gen(stack,rhs,opt,lhs,"==");
}

static int int_rmatrix_fgt(Stack stack, int rhs, int opt, int lhs)
{
  return int_rmatrix_f_gen(stack,rhs,opt,lhs,">");
}

static int int_rmatrix_fge(Stack stack, int rhs, int opt, int lhs)
{
  return int_rmatrix_f_gen(stack,rhs,opt,lhs,">=");
}


static int int_rmatrix_dprim_r(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *HMat1,*HMat2;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if (( HMat1 = GetRMat(stack,1)) == NULLRMAT) return RET_BUG;
  if (( HMat2 =nsp_rmatrix_transpose(HMat1))  == NULLRMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat2);
  return 1;
}

static int int_rmatrix_quote_r(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  NspRMatrix *HMat1,*HMat2;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if (( HMat1 = GetRMat(stack,1)) == NULLRMAT) return RET_BUG;
  if (( HMat2 =nsp_rmatrix_transpose(HMat1))  == NULLRMAT) return RET_BUG;
  for ( i = 0 ; i < HMat2->mn ; i++)
    {
      nsp_mat_conj(HMat2->S[i]->num);
      nsp_mat_conj(HMat2->S[i]->den);
    }
  MoveObj(stack,1,(NspObject *) HMat2);
  return 1;
}


static int int_rmatrix_conj(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  NspRMatrix *A;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((A = GetRMatCopy(stack,1)) == NULLRMAT) return RET_BUG;
  for ( i = 0 ; i < A->mn ; i++)
    {
      nsp_mat_conj(A->S[i]->num);
      nsp_mat_conj(A->S[i]->den);
    }
  NSP_OBJECT(A)->ret_pos =1;
  return 1;
}


#if 0
static int int_rmatrix_add(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *P,*Q,*R;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P= GetRMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetRMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_rmatrix_add(P,Q))== NULLRMAT)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_rmatrix_minus_r_r(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *P,*Q,*R;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P= GetRMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetRMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_rmatrix_minus(P,Q))== NULLRMAT)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_rmatrix_mult_m_r(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *P;
  NspRMatrix *Q,*R;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P= GetMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetRMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_rmatrix_mult_m_r(P,Q))== NULLRMAT)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_rmatrix_mult_r_m(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *P,*R;
  NspMatrix *Q;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P= GetRMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_rmatrix_mult_r_m(P,Q))== NULLRMAT)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

/* test : mult by fft */

static int int_rmatrix_mult_tt(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *P,*Q,*R;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetRMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetRMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_rmatrix_mult_tt(P,Q))== NULL)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_rmatrix_mult_tt_r_m(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *P,*R;
  NspMatrix *Q;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetRMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_rmatrix_mult_tt_r_m(P,Q))== NULL)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_rmatrix_mult_tt_m_r(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *P;
  NspRMatrix *Q,*R;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetRMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_rmatrix_mult_tt_m_r(P,Q))== NULL)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_rmatrix_div_tt_r_m(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *P,*R;
  NspMatrix *Q;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetRMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_rmatrix_div_tt_r_m(P,Q,FALSE))== NULL)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_rmatrix_dsl_tt_r_m(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *P,*R;
  NspMatrix *Q;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetRMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_rmatrix_div_tt_r_m(P,Q,TRUE))== NULL)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_rmatrix_div_tt_m_r(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *P;
  NspRMatrix *Q,*R;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetRMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_rmatrix_div_tt_m_r(P,Q))== NULL)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_rmatrix_mult_r_r(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *P,*Q,*R;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetRMat(stack,1))== NULL) return RET_BUG;
  if ((Q=GetRMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_rmatrix_mult_r_r(P,Q))== NULL)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}


static int int_rmatrix_horner(Stack stack, int rhs, int opt, int lhs)
{
  int i,flag = FALSE, ttmode = FALSE;
  NspCells *Res;
  NspRMatrix *P;
  NspMatrix *V;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  nsp_option opts[] ={{"vdim",s_bool,NULLOBJ,-1},
		      {"ttmode",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ((P=GetRMat(stack,1))== NULL) return RET_BUG;
  if ((V=GetMat(stack,2))== NULL) return RET_BUG;
  if ( get_optional_args(stack, rhs, opt, opts, &flag,&ttmode) == FAIL )
    return RET_BUG;
  if ( ttmode == TRUE ) 
    {
      /* term to term mode */
      NspMatrix *R=nsp_rmatrix_horner_tt(P,V);
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
	  Res->objs[i] =(NspObject *) nsp_rmatrix_horner(P,V,i);
	  if ( Res->objs[i] == NULL ) return RET_BUG;
	}
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

static int int_rmatrix_hornerm(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  NspCells *Res;
  NspRMatrix *P;
  NspMatrix *V;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetRMat(stack,1))== NULL) return RET_BUG;
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
      Res->objs[i] =(NspObject *) nsp_polynom_hornerm(P->S[i],V);
      if ( Res->objs[i] == NULL ) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


static int int_rmatrix_isreal(Stack stack, int rhs, int opt, int lhs)
{
  int i,strict = FALSE,ans=TRUE;
  NspRMatrix *P;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((P=GetRMat(stack,1))== NULL) return RET_BUG;
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


static int int_rmatrix_clean (Stack stack, int rhs, int opt, int lhs)
{
  int i;
  double epsr=DBL_EPSILON;
  double epsa=DBL_EPSILON;
  NspRMatrix *P;
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

  if ((P = GetRMatCopy (stack, 1)) == NULLRMAT)
    return RET_BUG;
  
  for ( i = 0 ; i < P->mn ; i++)
    {
      nsp_mat_clean(P->S[i],rhs, epsa, epsr);
      if ( nsp_polynom_resize(P->S[i]) == FAIL) 
	return RET_BUG;
    }
  NSP_OBJECT (P)->ret_pos = 1;
  return 1;
}

typedef void (Fmat)(NspMatrix *A);

static int int_rmatrix_map (Stack stack, int rhs, int opt, int lhs, Fmat f)
{
  NspRMatrix *P;
  int i;
  CheckStdRhs(1,1);
  CheckLhs (1, 1);
  if ((P = GetRMatCopy (stack, 1)) == NULLRMAT)
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

static int int_rmatrix_map_c (Stack stack, int rhs, int opt, int lhs, Fmatc f)
{
  NspRMatrix *P;
  int i;
  CheckStdRhs(1,1);
  CheckLhs (1, 1);
  if ((P = GetRMatCopy (stack, 1)) == NULLRMAT)
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


static int int_rmatrix_real (Stack stack, int rhs, int opt, int lhs)
{
  return int_rmatrix_map_c(stack, rhs, opt, lhs, nsp_mat_get_real);
}

static int int_rmatrix_imag (Stack stack, int rhs, int opt, int lhs)
{
  return int_rmatrix_map_c(stack, rhs, opt, lhs, nsp_mat_get_imag);
}

static int int_rmatrix_ceil (Stack stack, int rhs, int opt, int lhs)
{
  return int_rmatrix_map(stack, rhs, opt, lhs, nsp_mat_ceil);
}

static int int_rmatrix_int (Stack stack, int rhs, int opt, int lhs)
{
  return int_rmatrix_map(stack, rhs, opt, lhs, nsp_mat_int);
}

static int int_rmatrix_floor (Stack stack, int rhs, int opt, int lhs)
{
  return int_rmatrix_map(stack, rhs, opt, lhs, nsp_mat_floor);
}

static int int_rmatrix_round (Stack stack, int rhs, int opt, int lhs)
{
  return int_rmatrix_map(stack, rhs, opt, lhs, nsp_mat_round);
}

static int int_rmatrix_norm( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  NspRMatrix *P;
  double p=2.0;
  int id=1, i;
  const char *norm_table[] =       {"1","2","inf",NULL};
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ( (P=GetRMat(stack, 1)) == NULLRMAT ) return RET_BUG;
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

static int int_rmatrix_minus (Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *P;
  int i;
  CheckStdRhs(1,1);
  CheckLhs (1, 1);
  if ((P = GetRMatCopy (stack, 1)) == NULLRMAT)
    return RET_BUG;
  for ( i = 0 ; i < P->mn ; i++)
    {
      nsp_mat_minus(P->S[i]->num);
    }
  NSP_OBJECT (P)->ret_pos = 1;
  return 1;
}

static int int_rmatrix_dh_r_m (Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *P,*R;
  NspMatrix *M;
  CheckStdRhs(2,2);
  CheckLhs (1, 1);
  if ((P = GetRMat(stack, 1)) == NULLRMAT)    return RET_BUG;
  if ((M = GetRealMat(stack, 2)) == NULLMAT)    return RET_BUG;
  if ((R= nsp_rmatrix_dh_r_m(P,M)) == NULLRMAT)return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(R));
  return Max(lhs,1);
}


static int int_rmatrix_hat_r_m (Stack stack, int rhs, int opt, int lhs)
{
  double d;
  NspRMatrix *P,*R;
  CheckStdRhs(2,2);
  CheckLhs (1, 1);
  if ((P = GetRMat(stack, 1)) == NULLRMAT)    return RET_BUG;
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
  if ((R= nsp_rmatrix_hat_r_m(P,floor(d))) == NULLRMAT) 
    return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(R));
  return Max(lhs,1);
}

static int int_rmatrix_add_r_m(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *P,*R;
  NspMatrix *M;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetRMat(stack,1))== NULL) return RET_BUG;
  if ((M=GetMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_rmatrix_add_m(P,M)) == NULL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_rmatrix_add_m_r(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *P,*R;
  NspMatrix *M;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetRMat(stack,2))== NULL) return RET_BUG;
  if ((M=GetMat(stack,1))== NULL) return RET_BUG;
  if ((R= nsp_rmatrix_add_m(P,M)) == NULL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}



static int int_rmatrix_minus_r_m(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *P,*R;
  NspMatrix *M;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetRMat(stack,1))== NULL) return RET_BUG;
  if ((M=GetMat(stack,2))== NULL) return RET_BUG;
  if ((R= nsp_rmatrix_minus_m(P,M,TRUE)) == NULL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_rmatrix_minus_m_r(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *P,*R;
  NspMatrix *M;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  if ((P=GetRMat(stack,2))== NULL) return RET_BUG;
  if ((M=GetMat(stack,1))== NULL) return RET_BUG;
  if ((R= nsp_rmatrix_minus_m(P,M,FALSE)) == NULL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) R);
  return 1;
}

static int int_rmatrix_pdiv_r_r(Stack stack, int rhs, int opt, int lhs)
{
  NspRMatrix *A,*B;
  NspRMatrix *Q,*R;
  CheckRhs(2,2);
  CheckLhs(2,2);
  if ((A=GetRMat(stack,1))== NULL) return RET_BUG;
  if ((B=GetRMat(stack,2))== NULL) return RET_BUG;
  if (nsp_rmatrix_pdiv_tt(A,B,&Q,&R)== FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Q);
  MoveObj(stack,2,(NspObject *) R);
  return 2;
}


typedef NspRMatrix *(*SuProPM) (NspRMatrix * A, int dim);

static int int_rmatrix_sum_prod_gen (Stack stack, int rhs, int opt, int lhs, SuProPM F)
{
  int dim=0;
  NspRMatrix *Res, *HMat;
  CheckRhs(1, 2);
  CheckOptRhs(0, 1)
  CheckLhs(1, 1);

  if ((HMat = GetRMat (stack, 1)) == NULLRMAT)
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

  if ((Res = (*F) (HMat, dim)) == NULLRMAT)
    return RET_BUG;

  MoveObj (stack, 1, NSP_OBJECT(Res));
  return 1;
}

int int_rmatrix_sum (Stack stack, int rhs, int opt, int lhs)
{
  return (int_rmatrix_sum_prod_gen (stack, rhs, opt, lhs, nsp_rmatrix_sum));
}

/*
 * matprod : product of all elements of a
 * a is unchanged 
 */

int int_rmatrix_prod (Stack stack, int rhs, int opt, int lhs)
{
  return (int_rmatrix_sum_prod_gen (stack, rhs, opt, lhs, nsp_rmatrix_prod));
}

/*
 * matcusum : cumulative sum of all elements of a
 * a is unchanged 
 */

/*
  int int_rmatrix_cusum (Stack stack, int rhs, int opt, int lhs)
  {
  return (int_rmatrix_sum_prod_gen (stack, rhs, opt, lhs, nsp_rmatrix_cum_sum));
  }
  
*/
 
/*
 * matcuprod : cumulative prod of all elements of a
 * a is unchanged 
 */

/*
  int int_rmatrix_cuprod (Stack stack, int rhs, int opt, int lhs)
  {
  return (int_rmatrix_sum_prod_gen(stack, rhs, opt, lhs, nsp_rmatrix_cum_prod));
}

*/
#endif 

/*
 * nsp_rmatrix_triu: A=triu(a)
 */
  
int int_rmatrix_triu (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspRMatrix *HMat;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((HMat = GetRMatCopy (stack, 1)) == NULLRMAT)
    return RET_BUG;
  if ( nsp_rmatrix_triu (HMat, k1) == FAIL) return RET_BUG;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

/*
 * nsp_rmatrix_tril: A=Tril(A)
 * A is changed  
 */

int int_rmatrix_tril (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspRMatrix *HMat;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((HMat = GetRMatCopy (stack, 1)) == NULLRMAT)
    return RET_BUG;
  if ( nsp_rmatrix_tril (HMat, k1) == FAIL ) return RET_BUG;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

int int_rmatrix_simp (Stack stack, int rhs, int opt, int lhs)
{
  int i = 0;
  NspRMatrix *R;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((R = GetRMatCopy (stack, 1)) == NULLRMAT) return RET_BUG;
  for ( i = 0 ; i < R->mn ; i++ )
    {
      nsp_polynoms_simp(R->S[i]->num,R->S[i]->den);
    }
  NSP_OBJECT(R)->ret_pos = 1;
  return 1;
}

/*
 * The Interface for basic matrices operation 
 */

static OpTab RMatrix_func[]={
  /* specific */
  {"simp_r", int_rmatrix_simp},
  {"m2r", int_rmatrix_m2r},
  {"p2r", int_rmatrix_p2r},
  {"concatd_m_r",int_rmatrix_concatd_m_r},
  {"concatd_r_m",int_rmatrix_concatd_r_m},
  {"concatd_r_r",int_matint_concatd}, /*  int_rmatrix_concatd}, */
  {"concatdiag_r_r",int_matint_concat_diag},
  {"concatr_m_r",int_rmatrix_concatr_m_r},
  {"concatr_r_m",int_rmatrix_concatr_r_m},
  {"concatr_r_r", int_matint_concatr}, /* int_rmatrix_concatr}, */
  {"addcols_r_m",int_rmatrix_addcols},
  {"addrows_r",int_rmatrix_addrows},
  {"diag_r", int_rmatrix_diag},
  {"diag_r_m", int_rmatrix_diag},
  {"diagcre_r",int_rmatrix_diagcre},
  {"diage_r",int_rmatrix_diage},
  {"resize_r",int_rmatrix_resize},
  {"enlarge_r", int_rmatrix_enlarge },
  {"ge_r_r" ,  int_rmatrix_ge },
  {"gt_r_r" ,  int_rmatrix_gt },
  {"le_r_r" ,  int_rmatrix_le },
  {"lt_r_r" ,  int_rmatrix_lt },
  {"feq_r_r" ,  int_rmatrix_feq },
  {"fge_r_r" ,  int_rmatrix_fge },
  {"fgt_r_r" ,  int_rmatrix_fgt },
  {"fle_r_r" ,  int_rmatrix_fle },
  {"flt_r_r" ,  int_rmatrix_flt },  
  {"eq_r_r" ,  int_rmatrix_eq },
  {"ne_r_r" ,  int_rmatrix_neq },
  {"fneq_r_r" ,  int_rmatrix_fneq },
  {"dprim_r", int_rmatrix_dprim_r},
  {"quote_r", int_rmatrix_quote_r},
  {"conj_r", int_rmatrix_conj},
  {"tril_r", int_rmatrix_tril},
  {"triu_r", int_rmatrix_triu},
  {"ce2r",  int_rmatrix_ce2r},

  {"extract_r", int_matint_extract}, 
  {"extractcols_r", int_matint_extractcols}, 
  {"extractelts_r", int_matint_extractelts}, 
  {"extractrows_r", int_matint_extractrows_pointer}, 
  {"deletecols_r", int_matint_deletecols},
  {"deleteelts_r", int_matint_deleteelts},
  {"deleterows_r", int_matint_deleterows},
  {"matrix_r",int_matint_redim},
  {"isvector_r", int_matint_isvector},
  {"redim_r",int_matint_redim},
  {"reshape_r",int_matint_redim},
  {"resize2vect_r", int_matint_resize2vect},
  {"setrowscols_r", int_matint_setrowscols},
  {"tozero_r", int_matint_tozero},
  
#if 0
  {"roots_r",int_rmatrix_roots},
  {"round_r",      int_rmatrix_round},
  {"companion_m",int_rmatrix_companion_m},
  {"companion_r",int_rmatrix_companion_r},
  {"horner", int_rmatrix_horner},
  {"hornerm", int_rmatrix_hornerm},
  {"norm_r",      int_rmatrix_norm},
  {"pdiv", int_rmatrix_rdiv_r_r},
  {"rmat_create",int_rmatrix_create},
  {"roots_r",int_rmatrix_roots},
  /* standard */
  {"ceil_r", int_rmatrix_ceil},
  {"clean_r",  int_rmatrix_clean},
  {"dh_r_m",  int_rmatrix_dh_r_m},
  {"div_m_r",int_rmatrix_div_tt_m_r},
  {"div_r_m",int_rmatrix_div_tt_r_m},
  {"dsl_m_r",int_rmatrix_div_tt_m_r},
  {"dsl_r_m",int_rmatrix_dsl_tt_r_m},
  {"dst_m_r",int_rmatrix_mult_tt_m_r},
  {"dst_r_m",int_rmatrix_mult_tt_r_m},
  {"dst_r_r",int_rmatrix_mult_tt},
  {"floor_r",     int_rmatrix_floor},
  {"hat_r_m",  int_rmatrix_hat_r_m},
  {"imag_r", int_rmatrix_imag},
  {"int_r",    int_rmatrix_int},
  {"isreal_r", int_rmatrix_isreal},
  {"minus_m_r",  int_rmatrix_minus_m_r},
  {"minus_r", int_rmatrix_minus},
  {"minus_r_m",  int_rmatrix_minus_r_m},
  {"minus_r_r",int_rmatrix_minus_r_r},
  {"mult_m_r",int_rmatrix_mult_m_r},
  {"mult_r_m",int_rmatrix_mult_r_m},
  {"mult_r_r",int_rmatrix_mult_r_r},
  {"plus_m_r",int_rmatrix_add_m_r},
  {"plus_r_m",int_rmatrix_add_r_m},
  {"plus_r_r",int_rmatrix_add},
  {"real_r", int_rmatrix_real},
  {"sum_r_s", int_rmatrix_sum},
  {"sum_r", int_rmatrix_sum},
  /* 
  {"cumsum_r_s", int_rmatrix_cusum},
  {"cumsum_r", int_rmatrix_cusum},
  */
  {"prod_r_s", int_rmatrix_prod},
  {"prod_r", int_rmatrix_prod},
  /*
  {"cumprod_r_s", int_rmatrix_cuprod},
  {"cumprod_r", int_rmatrix_cuprod},
  */
#endif
  
  {(char *) 0, NULL}
};

int RMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(RMatrix_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) */

void RMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = RMatrix_func[i].name;
  *f = RMatrix_func[i].fonc;
}










