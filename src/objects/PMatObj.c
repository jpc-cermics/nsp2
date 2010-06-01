/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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

#define PMatrix_Private 
#include "nsp/object.h"

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/matint.h"

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


int PMatFullComp(NspPMatrix * A,NspPMatrix * B,char *op,int *err)
{
  Scierror("PMatFullComp: to be implemented \n");
  return FALSE;
}



int nsp_pmatrix_eq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_pmatrix_id) == FALSE) return FALSE ;
  rep = PMatFullComp((NspPMatrix *) A,(NspPMatrix *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

int nsp_pmatrix_neq(NspObject *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_pmatrix_id) == FALSE) return TRUE;
  rep = PMatFullComp((NspPMatrix *) A,(NspPMatrix *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * Mat == TRUE ? 
 *  if Mat != [] and all the elements of Mat are != 0
 * PMatIsTrue(NspPMatrix *M)
 */


/*
 * Save a Matrix in a file stream 
 */

static int nsp_pmatrix_xdr_save(XDR *xdrs, NspMatrix *M)
{
#if 1 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_pmatrix)) == FAIL) return FAIL;
#else
  if (nsp_xdr_save_i(xdrs, M->type->id) == FAIL)    return FAIL;
#endif 
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("pmat_xdr_save: to be implemented \n");
  return OK;
}

/*
 * Load a Matrix from a file stream 
 **/

static NspPMatrix *nsp_pmatrix_xdr_load(XDR *xdrs)
{
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLPMAT;
  Scierror("pmat_xdr_load: to be implemented \n");
  return NULLPMAT;
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
 */

static int int_matrix_to_pmatrix(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P; NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A=GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if (( P=nsp_matrix_to_polynom(A))== NULLPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) P);
  return 1;
}

/*
 * Creation of a PMatrix 
 */

static int int_pmatrix_create(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  doubleC def ={0,0} ;
  NspPMatrix *P; 
  CheckRhs(2,3);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if (rhs == 3) 
    {
      if (GetScalarDouble(stack,1,&def.r) == FAIL) return RET_BUG;
    }
  if ( (P =nsp_pmatrix_create(NVOID,m1,n1,&def,(rhs==3)? 1: 0)) == NULLPMAT)  return RET_BUG;
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

static int int_meth_shift(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P=self;
  int n,i;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ( GetScalarInt (stack, 1, &n) == FAIL ) return RET_BUG;
  if ( n <= 0 ) return 0;
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
  return 0;
}


static NspMethods pmatrix_methods[] = {
  { "degree", int_meth_degree},
  { "shift", int_meth_shift},
  { (char *) 0, NULL}
};

static NspMethods *pmatrix_get_methods(void) { return pmatrix_methods;};

/*----------------------------------
 * Interfaces 
 *----------------------------------*/

/*
 * Right Concatenation 
 * A= [A,B] 
 * return 0 on failure ( incompatible size or No more space )
 */

int int_pmatrix_concatr(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetPMat(stack,1))  == NULLPMAT) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  if ( HMat2->mn == 0)
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else
    {
      if ((HMat1 = GetPMatCopy(stack,1))  == NULLPMAT) return RET_BUG;
      if (nsp_pmatrix_concat_right(HMat1,HMat2)!= OK) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  return 1;
}

/*
 * Right Concatenation 
 * Res = [A,B]  when A is a scalar matrix 
 * usefull when A=[]
 */

int int_pmatrix_concatr_m_s(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *HMat2,*Res;
  NspMatrix * HMat1;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  if (( Res=nsp_matrix_to_pmatrix(HMat1,NULL,0)) == NULLPMAT) return RET_BUG;

  if ( HMat2->mn != 0)
    {
      if (nsp_pmatrix_concat_right(Res,HMat2)!= OK) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSMat on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

typedef NspPMatrix * (*FSconcat) (const NspPMatrix *,const NspPMatrix *);

int int_pmatrix__concat(Stack stack, int rhs, int opt, int lhs, FSconcat F)
{
  NspPMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetPMat(stack,1))  == NULLPMAT) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  if ( HMat2->mn == 0)
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else
    {
      NspPMatrix *HMat3;
      if (( HMat3 = (*F)(HMat1,HMat2)) == NULLPMAT)  return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  return 1;
}

int int_pmatrix_concatd(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_concat_down(stack,rhs,opt,lhs,(Fconcat_d)nsp_matint_concat_down);
  /* return int_pmatrix__concat(stack,rhs,opt,lhs,nsp_pmatrix_concat_down); */
}


/*
 * Down Concatenation 
 * Res = [A;B]  when A is a scalar matrix 
 * usefull when A=[]
 */

int int_pmatrix_concatd_m_s(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *HMat2,*Res;
  NspMatrix * HMat1;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  if (( Res=nsp_matrix_to_pmatrix(HMat1,NULL,0)) == NULLPMAT) return RET_BUG;

  if ( HMat2->mn != 0)
    {
      if ((Res=nsp_pmatrix_concat_down(Res,HMat2))== NULLPMAT ) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 *nsp_pmatrix_add_columns: add n cols of zero to NspPMatrix A 
 * A= [A,ones(m,n)] 
 * return 0 on failure ( No more space )
 * A is changed 
 */

int int_pmatrix_addcols(Stack stack, int rhs, int opt, int lhs)
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

int int_pmatrix_addrows(Stack stack, int rhs, int opt, int lhs)
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
 *nsp_pmatrix_resize: Changes NspPMatrix dimensions
 * Warning : this routine only enlarges the array 
 * of the NspPMatrix storage so as to contain mxn 
 * elements : the previous datas are not moved and 
 * occupy the first array cells 
 * The NspPMatrix is changed 
 * return 0 on failure 
 */

int int_pmatrix_resize(Stack stack, int rhs, int opt, int lhs)
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

int int_pmatrix_enlarge(Stack stack, int rhs, int opt, int lhs)
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

int int_pmatrix_lt(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = PMatCompOp(A,B,"<");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_pmatrix_le(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = PMatCompOp(A,B,"<=");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_pmatrix_neq(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = PMatCompOp(A,B,"<>");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_pmatrix_eq(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B;
  NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = PMatCompOp(A,B,"==");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_pmatrix_gt(Stack stack, int rhs, int opt, int lhs)
{

  NspPMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = PMatCompOp(A,B,">");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}


int int_pmatrix_ge(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = PMatCompOp(A,B,">=");
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
  rep = PMatFullComp(A,B,op,&err);
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

int int_pmatrix_flt(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,"<");
}

int int_pmatrix_fle(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,"<=");
}


int int_pmatrix_fneq(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,"<>");
}

int int_pmatrix_feq(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,"==");
}

int int_pmatrix_fgt(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,">");
}

int int_pmatrix_fge(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,">=");
}


/*
 * Res =nsp_pmatrix_copy(A) 
 * Creates a Copy of NspPMatrix A : A is not checked 
 */

int int_pmatrix_transpose(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *HMat1,*HMat2;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( HMat1 = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if (( HMat2 =nsp_pmatrix_transpose(HMat1))  == NULLPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat2);
  return 1;
}

/* companion 
 *
 */

int int_pmatrix_companion_p(Stack stack, int rhs, int opt, int lhs)
{
  nsp_polynom P;
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((P=GetPolynom(stack,1))== NULL) return RET_BUG;
  if ((A= nsp_matrix_companion((NspMatrix *) P))== NULLMAT)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}

int int_pmatrix_companion_m(Stack stack, int rhs, int opt, int lhs)
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

int int_pmatrix_roots(Stack stack, int rhs, int opt, int lhs)
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

int int_pmatrix_add(Stack stack, int rhs, int opt, int lhs)
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

int int_pmatrix_minus(Stack stack, int rhs, int opt, int lhs)
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

int int_pmatrix_mult_m_p(Stack stack, int rhs, int opt, int lhs)
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

/* test : mult by fft */

int int_pmatrix_mult_tt(Stack stack, int rhs, int opt, int lhs)
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

int int_pmatrix_mult_p_p(Stack stack, int rhs, int opt, int lhs)
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


int int_pmatrix_horner(Stack stack, int rhs, int opt, int lhs)
{
  int i,flag = FALSE;
  NspCells *Res;
  NspPMatrix *P;
  NspMatrix *V;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  nsp_option opts[] ={{"vdim",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ((P=GetPMat(stack,1))== NULL) return RET_BUG;
  if ((V=GetMat(stack,2))== NULL) return RET_BUG;
  if ( get_optional_args(stack, rhs, opt, opts, &flag) == FAIL )
    return RET_BUG;
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


int int_pmatrix_isreal(Stack stack, int rhs, int opt, int lhs)
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




/*
 * The Interface for basic matrices operation 
 */

static OpTab PMatrix_func[]={
  {"extract_p", int_matint_extract}, 
  {"extractelts_p", int_matint_extractelts}, 
  {"extractcols_p", int_matint_extractcols}, 
  {"extractrows_p", int_matint_extractrows_pointer}, 
  {"resize2vect_p", int_matint_resize2vect},
  {"setrowscols_p", int_matint_setrowscols},
  {"deleteelts_p", int_matint_deleteelts},
  {"deleterows_p", int_matint_deleterows},
  {"deletecols_p", int_matint_deletecols},
  {"tozero_p", int_matint_tozero},
  {"pmat_create",int_pmatrix_create},
  {"redim_p",int_matint_redim},
  {"matrix_p",int_matint_redim},
  {"reshape_p",int_matint_redim},
  {"concatr_p_p", int_matint_concatr}, /* int_pmatrix_concatr}, */
  {"concatr_m_p",int_pmatrix_concatr_m_s},
  {"addcols_p_m",int_pmatrix_addcols},
  {"concatd_p_p",int_matint_concatd}, /*  int_pmatrix_concatd}, */
  {"concatd_m_p",int_pmatrix_concatd_m_s},
  {"concatdiag_p_p",int_matint_concat_diag},
  {"isvector_p", int_matint_isvector},
  {"addrows_p",int_pmatrix_addrows},
  {"resize_p",int_pmatrix_resize},
  {"enlarge_p", int_pmatrix_enlarge },
  {"eq_p_p" ,  int_pmatrix_eq },
  {"feq_p_p" ,  int_pmatrix_feq },
  {"fge_p_p" ,  int_pmatrix_fge },
  {"fgt_p_p" ,  int_pmatrix_fgt },
  {"fle_p_p" ,  int_pmatrix_fle },
  {"flt_p_p" ,  int_pmatrix_flt },
  {"fneq_p_p" ,  int_pmatrix_fneq },
  {"ge_p_p" ,  int_pmatrix_ge },
  {"gt_p_p" ,  int_pmatrix_gt },
  {"le_p_p" ,  int_pmatrix_le },
  {"lt_p_p" ,  int_pmatrix_lt },
  {"ne_p_p" ,  int_pmatrix_neq },
  {"m2p", int_matrix_to_pmatrix},
  {"quote_p", int_pmatrix_transpose},
  {"companion_m",int_pmatrix_companion_m},
  {"companion_p",int_pmatrix_companion_p},
  {"roots_p",int_pmatrix_roots},
  {"plus_p_p",int_pmatrix_add},
  {"minus_p_p",int_pmatrix_minus},
  {"mult_m_p",int_pmatrix_mult_m_p},
  {"mult_p_p",int_pmatrix_mult_p_p},
  {"dst_p_p",int_pmatrix_mult_tt},
  {"horner", int_pmatrix_horner},
  {"isreal_p", int_pmatrix_isreal},
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










