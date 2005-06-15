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
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BMatrix_Private 
#include "nsp/object.h"
#include "nsp/datas.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/matint.h"

/*
 * NspBMatrix inherits from NspObject 
 */

int nsp_type_bmatrix_id=0;
NspTypeBMatrix *nsp_type_bmatrix=NULL;

NspTypeBMatrix *new_type_bmatrix(type_mode mode)
{
  NspTypeMatint *mati;/* interface */
  NspTypeBMatrix *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_bmatrix != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_bmatrix;
    }
  
  if ((type  =  malloc(sizeof(NspTypeBMatrix))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs =  NULL; /*bmatrix_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = bmatrix_get_methods; 
  type->new = (new_func *) new_bmatrix;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for bmatrix */ 

  top->pr = (print_func *)nsp_bmatrix_print;                    
  top->dealloc = (dealloc_func *)nsp_bmatrix_destroy;            
  top->copy  =  (copy_func *)nsp_bmatrix_copy;                   
  top->size  = (size_func *) bmatrix_size;                  
  top->s_type =  (s_type_func *) bmatrix_type_as_string;    
  top->sh_type = (sh_type_func *) bmatrix_type_short_string;
  top->info = (info_func *)nsp_bmatrix_info;                    
  top->is_true = (is_true_func  *) bmatrix_is_true;           
  top->loop =(loop_func *) bmatrix_loop;
  top->path_extract =  NULL;       
  top->get_from_obj = (get_from_obj_func *)  BMatObj ;  
  top->eq  = (eq_func *) bmatrix_eq;
  top->neq  = (eq_func *) bmatrix_neq;
  top->save  = (save_func *) bmatrix_xdr_save;
  top->load  = (load_func *) bmatrix_xdr_load;
  /* specific methods for bmatrix */

  type->init = (init_func *) init_bmatrix;
  /* 
   * BMatrix interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  /*
   * BMatrix implements Matint the matrix interface 
   * which is common to object that behaves like matrices.
   */

  mati = new_type_matint(T_DERIVED);
  mati->methods = matint_get_methods; 
  mati->redim = (matint_redim *) nsp_bmatrix_redim; 
  mati->resize = (matint_resize  *) nsp_bmatrix_resize;
  mati->free_elt = (matint_free_elt *) 0; /* nothing to do */
  mati->elt_size = (matint_elt_size *) nsp_bmatrix_elt_size ;

  type->interface = (NspTypeBase *) mati;
  
  if ( nsp_type_bmatrix_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeBMatrix called nsp_type_bmatrix
       */
      type->id =  nsp_type_bmatrix_id = nsp_new_type_id();
      nsp_type_bmatrix = type;
      if ( nsp_register_type(nsp_type_bmatrix) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_bmatrix(mode);
    }
  else 
    {
      type->id = nsp_type_bmatrix_id;
      return type;
    }
}

/*
 * initialize BMatrix instances 
 * locally and by calling initializer on parent class 
 */

static int init_bmatrix(NspBMatrix *o,NspTypeBMatrix *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of BMatrix 
 */

NspBMatrix *new_bmatrix() 
{
  NspBMatrix *loc; 
  /* type must exists */
  nsp_type_bmatrix = new_type_bmatrix(T_BASE);
  if ( (loc = malloc(sizeof(NspBMatrix)))== NULLBMAT) return loc;
  /* initialize object */
  if ( init_bmatrix(loc,nsp_type_bmatrix) == FAIL) return NULLBMAT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for BMatrix 
 *-----------------------------------------------*/

/*
 * size 
 */

static int bmatrix_size(NspBMatrix *Mat, int flag)
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
 * type as string 
 */

static char bmat_type_name[]="BMat";
static char bmat_short_type_name[]="b";

static char *bmatrix_type_as_string(void)
{
  return(bmat_type_name);
}

static char *bmatrix_type_short_string(void)
{
  return(bmat_short_type_name);
}

/* used in for x=y where y is a BMatrix **/

static NspObject *bmatrix_loop(char *str, NspObject *O, NspObject *O1, int i, int *rep)
{
  NspBMatrix *M= (NspBMatrix *) O1,*M1=NULLBMAT;
  if ( O == NULLOBJ ) 
    {
      if (( M1= BMatLoopCol(str,NULLBMAT,M,i,rep))==NULLBMAT) return NULLOBJ;
      if (( *rep == RET_ENDFOR )) return NULLOBJ;
      return (NspObject *) M1 ;
    }
  else
    {
      if (( M1 = BMatObj(O)) == NULLBMAT ) return NULLOBJ;
      M1= BMatLoopCol(str,M1,M,i,rep);
      if (( *rep == RET_ENDFOR )) return NULLOBJ;
      return O;
    }
}

static int bmatrix_eq(NspBMatrix *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_bmatrix_id) == FALSE) return FALSE ;
  rep =nsp_bmatrix_full_compare(A,(NspBMatrix *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

static int bmatrix_neq(NspBMatrix *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_bmatrix_id) == FALSE) return TRUE;
  rep =nsp_bmatrix_full_compare(A,(NspBMatrix *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}


/*
 * Mat == TRUE ? 
 *  if Mat != [] and all the elements of Mat are TRUE
 */

static int bmatrix_is_true(NspBMatrix *A)
{
  int i;
  if ( A->mn == 0) return FALSE;
  for ( i=0; i < A->mn ; i++ ) 
    {
      if ( A->B[i] == FALSE ) return FALSE ;
    }
  return(TRUE);
}

/*
 * save 
 */

static int bmatrix_xdr_save(NspFile  *F, NspBMatrix *M)
{
  if (nsp_xdr_save_i(F->xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(F->xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(F->xdrs,M->m) == FAIL) return FAIL;
  if (nsp_xdr_save_i(F->xdrs,M->n) == FAIL) return FAIL;
  if (nsp_xdr_save_array_i(F->xdrs,M->B,M->mn) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

static NspBMatrix  *bmatrix_xdr_load(NspFile  *F)
{
  int m,n;
  NspBMatrix *M;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(F->xdrs,name,NAME_MAXL) == FAIL) return NULLBMAT;
  if (nsp_xdr_load_i(F->xdrs,&m) == FAIL) return NULLBMAT;
  if (nsp_xdr_load_i(F->xdrs,&n) == FAIL) return NULLBMAT;
  if (( M=nsp_bmatrix_create(name,m,n)) == NULLBMAT ) return NULLBMAT;
  if (nsp_xdr_load_array_i(F->xdrs,M->B,M->mn) == FAIL) return NULLBMAT;
  return M;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for BMatrix objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspBMatrix   *BMatObj(NspObject *O)
{
  /* Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type **/
  if ( check_cast(O,nsp_type_bmatrix_id) == TRUE) return ((NspBMatrix *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_bmatrix));
  return(NULL);
}

int IsBMatObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_bmatrix_id);
}

int IsBMat(NspObject *O)
{
  return nsp_object_type(O , nsp_type_bmatrix_id);
}

NspBMatrix  *GetBMatCopy(Stack stack, int i)
{
  if (  GetBMat(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspBMatrix  *GetBMat(Stack stack, int i)
{
  NspBMatrix *M;
  if (( M = BMatObj(NthObj(i))) == NULLBMAT)
    ArgMessage(stack,i);
  return M;
}

int BoolScalar(NspObject *O, Boolean *val)
{
  static char mess[]="Argument should be an integer";
  NspBMatrix *A;
  if (( A= BMatObj(O)) == NULLBMAT
      || ( A->mn != 1 ))
    { Scierror(mess); return(FAIL);}
  *val = A->B[0];
  return(OK);
}

int GetScalarBool(Stack stack, int i, int *val)
{
  NspBMatrix *M;
  if (( M = BMatObj(NthObj(i))) == NULLBMAT 
      || ( M->mn != 1 )) 
    {
      Scierror("Error:\t%s", ArgPosition(i));
      ArgName(stack,i);
      Scierror(" of function %s should be an integer\n",stack.fname);
      return FAIL;
    }
  *val =M->B[0];
  return(OK);
}


/*-------------------------------------------------------------------
 * wrappers for the BMatrix 
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/*
 *  Creation of a NspBMatrix : created with true value 
 */

static int int_bmatrix_create(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *HMat;
  int m1,n1;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ( (HMat =nsp_bmatrix_create(NVOID,m1,n1)) == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat);
  return 1;
}


/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static NspMethods *bmatrix_get_methods(void) { return NULL;}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

/*
 * Res =nsp_bmatrix_copy(A) 
 * Creates a Copy of NspBMatrix A : A is not checked 
 */

static int int_bmatrix_copy(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *HMat1,*HMat2;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( HMat1 = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  if (( HMat2 =nsp_bmatrix_copy(HMat1)) == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat2);
  return 1;  
}

/*
 * A(i;j) = "A(i;j) and B(i;j)" : A is changed  B unchanged 
 *    A and B must have the same size except if A or B is scalar 
 *    or A and B are []
 */

typedef int (*MPM) (NspBMatrix *,const NspBMatrix*);

static int int_bmatrix__and_or(Stack stack, int rhs, int opt, int lhs, MPM F1, MPM F2)
{
  NspBMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetBMatCopy(stack,1)) == NULLBMAT) return RET_BUG;
  if ((HMat2 = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
  if ( HMat1->mn == 0) 
    {
      NSP_OBJECT(HMat2)->ret_pos = 1;
      return 1;
    }
  if ( HMat2->mn == 0) 
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  if ( HMat2->mn == 1) 
    {
      if ( (*F1)(HMat1,HMat2) != OK) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  else if ( HMat1->mn == 1 ) 
    {
      /* since Mat1 is scalar we store the result in Mat2 so we 
       *  must copy it 
       **/
      if ((HMat2 = GetBMatCopy(stack,2)) == NULLBMAT) return RET_BUG;
      if ( (*F1)(HMat2,HMat1) != OK) return RET_BUG;
      NSP_OBJECT(HMat2)->ret_pos = 1;
    }
  else 
    {
      if ( (*F2)(HMat1,HMat2) != OK) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  return 1;
}

static int int_bmatrix_and(Stack stack, int rhs, int opt, int lhs)
{
  return int_bmatrix__and_or(stack,rhs,opt,lhs,nsp_bmatrix_scalar_and,nsp_bmatrix_and);
}

/*
 * A(i;j) = "A(i;j) or B(i;j)" : A is changed  B unchanged 
 *    A and B must have the same size except if A or B is scalar 
 *    or A and B are []
 */

static int int_bmatrix_or(Stack stack, int rhs, int opt, int lhs)
{
  return int_bmatrix__and_or(stack,rhs,opt,lhs,nsp_bmatrix_scalar_or,nsp_bmatrix_or);
}

/*
 * res = " and A(i;j)"
 */

static int int_bmatrix_and1(Stack stack, int rhs, int opt, int lhs)
{
  int rep=2,i,j;
  NspBMatrix *HMat1,*HMat;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((HMat1 = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  if (rhs == 2)
    {
      char *and_opts[] = { "c", "r", "*", NULL };
      if ((rep = GetStringInArray (stack, 2,and_opts, 1)) == -1)
	return RET_BUG;
    }
  switch (rep) 
    {
    case 0:  /* column */
      if ((HMat =nsp_bmatrix_create(NVOID,HMat1->m,1)) == NULLBMAT) return RET_BUG;
      for ( i= 0 ; i < HMat1->m ; i++)
	{
	  HMat->B[i] = TRUE; 
	  for ( j = 0 ; j < HMat1->n ; j++) 
	    if ( HMat1->B[i+HMat1->m*j] == FALSE) {HMat->B[i] = FALSE;break;}
	}
      MoveObj(stack,1, (NspObject *)HMat);  
      break;
    case 1:   /* row */
      if ((HMat =nsp_bmatrix_create(NVOID,1,HMat1->n)) == NULLBMAT) return RET_BUG;
      for ( j= 0 ; j < HMat1->n ; j++)
	{
	  HMat->B[j] = TRUE; 
	  for ( i = 0 ; i < HMat1->m ; i++) 
	    if (  HMat1->B[i+HMat1->m*j]== FALSE ) { HMat->B[j] = FALSE; break;}
	}
      MoveObj(stack,1, (NspObject *)HMat);  
      break;
    case 2 : 
      if ((HMat =nsp_bmatrix_create(NVOID,1,1)) == NULLBMAT) return RET_BUG;
      HMat->B[0] = NSP_OBJECT(HMat1)->type->is_true(HMat1);
      MoveObj(stack,1, (NspObject *)HMat);  
      break;
    }
  return 1;
}

/*
 * res = " or A(i;j)"
 */

static int int_bmatrix_or1(Stack stack, int rhs, int opt, int lhs)
{
  int rep=2,i,j;
  NspBMatrix *HMat1,*HMat=NULLBMAT;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((HMat1 = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  if (rhs == 2)
    {
      char *and_opts[] = { "c", "r", "*", NULL };
      if ((rep = GetStringInArray (stack, 2,and_opts, 1)) == -1)
	return RET_BUG;
    }
  switch (rep) 
    {
    case 0:  /* column */
      if ((HMat =nsp_bmatrix_create(NVOID,HMat1->m,1)) == NULLBMAT) return RET_BUG;
      for ( i= 0 ; i < HMat1->m ; i++)
	{
	  HMat->B[i] = FALSE; 
	  for ( j = 0 ; j < HMat1->n ; j++) 
	    if ( HMat1->B[i+HMat1->m*j] == TRUE ){ HMat->B[i] = TRUE; break;}
	}
      break;
    case 1:   /* row */
      if ((HMat =nsp_bmatrix_create(NVOID,1,HMat1->n)) == NULLBMAT) return RET_BUG;
      for ( j= 0 ; j < HMat1->n ; j++)
	{
	  HMat->B[j] = FALSE; 
	  for ( i = 0 ; i < HMat1->m ; i++) 
	    if ( HMat1->B[i+HMat1->m*j]== TRUE ) {HMat->B[j] = TRUE; break;}
	}
      break;
    case 2 : 
      if ((HMat =nsp_bmatrix_create(NVOID,1,1)) == NULLBMAT) return RET_BUG;
      HMat->B[0] = FALSE;
      for ( i=0; i < HMat1->mn ; i++ ) 
	{
	  if ( HMat1->B[i] == TRUE ) {  HMat->B[0] = TRUE;   break;  }
	}
      break;
    }
  MoveObj(stack,1, (NspObject *)HMat);  
  return 1;
}


/*
 * A(i;j) = "not A(i;j)" : A is changed
 */

static int int_bmatrix_not(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *HMat1;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat1 = GetBMatCopy(stack,1)) == NULLBMAT) return RET_BUG;
  if (nsp_bmatrix_not(HMat1) == FAIL ) return RET_BUG;
  NSP_OBJECT(HMat1)->ret_pos = 1;
  return 1;
}

/*
 * returns in a Matrix the indices for which the Boolean Matrix B is true
 */

static int int_bmatrix_find(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *A;
  NspMatrix *Rc,*Rr;
  CheckRhs(1,1);
  CheckLhs(1,2);
  if ((A = GetBMat(stack,1)) == NULLBMAT)  return RET_BUG;
  if (nsp_bmatrix_find_2(A,Max(lhs,1),&Rr,&Rc) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Rr);
  if ( lhs == 2 )
    {
      NthObj(2) = (NspObject *) Rc ;
      NSP_OBJECT(NthObj(2))->ret_pos = 2;
      return 2;
    }
  return 1;
}

/*
 * BMatredim: changes NspBMatrix dimensions
 * but keeps m*n constant
 * WARNING: Object on stack is changed 
 */

static int int_bmatrix_redim(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  NspBMatrix  *HMat;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (HMat=GetBMat(stack,1))== NULLBMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if (nsp_bmatrix_redim(HMat,m1,n1) != OK) return RET_BUG;
  return 1;
}


/*
 * changes a copy of matrix stack object to column vector 
 */

static int int_bmatrix_to_vect (Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetBMatCopy (stack, 1)) == NULLBMAT)
    return RET_BUG;
  if (nsp_bmatrix_redim (HMat, HMat->mn, 1) != OK)
    return RET_BUG;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}


/*
 * Right Concatenation 
 * A= [A,B] 
 * return 0 on failure ( incompatible size or No more space )
 */

static int int_bmatrix_concatr(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetBMat(stack,1))  == NULLBMAT) return RET_BUG;
  if ((HMat2 = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
  if (HMat1->mn == 0)
    {
      /* this is a bit tricky since HMat1 and HMat2 may point 
       * to the same object 
       */
      if ( HMat1 == HMat2 ) 
	{
	  NthObj(2) = NULLOBJ;
	  NSP_OBJECT(HMat1)->ret_pos = 1;
	}
      else 
	{
	  NSP_OBJECT(HMat2)->ret_pos = 1;
	}
      return 1;
    }
  if (HMat2->mn == 0)
    {
      if ( HMat1 == HMat2 ) NthObj(2) = NULLOBJ;
    }
  else 
    {
      if ((HMat1 = GetBMatCopy(stack,1))  == NULLBMAT) return RET_BUG;
      if (nsp_bmatrix_concat_right(HMat1,HMat2)!= OK) return RET_BUG;
    }
  NSP_OBJECT(HMat1)->ret_pos = 1;
  return 1;
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLMAT on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

typedef NspBMatrix * (*FBconcat) (NspBMatrix *, NspBMatrix *);

static int int_bmatrix__concat(Stack stack, int rhs, int opt, int lhs, FBconcat F)
{
  NspBMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetBMat (stack, 1)) == NULLBMAT)
    return RET_BUG;
  if ((HMat2 = GetBMat (stack, 2)) == NULLBMAT)
    return RET_BUG;
  if (HMat1->mn == 0)
    {
      /* this is a bit tricky since HMat1 and HMat2 may point 
       * to the same object 
       */
      if ( HMat1 == HMat2 ) 
	{
	  NthObj(2) = NULLOBJ;
	  NSP_OBJECT(HMat1)->ret_pos = 1;
	}
      else 
	{
	  NSP_OBJECT(HMat2)->ret_pos = 1;
	}
      return 1;
    }

  if (HMat2->mn == 0)
    {
      /* this is a bit tricky since HMat1 and HMat2 may point 
       * to the same object 
       */
      if ( HMat1 == HMat2 ) NthObj(2) = NULLOBJ;
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else
    {
      NspBMatrix *HMat3;
      if (( HMat3 = (*F)(HMat1,HMat2)) == NULLBMAT)  return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  return 1;
}

static int int_bmatrix_concatd(Stack stack, int rhs, int opt, int lhs)
{
  return int_bmatrix__concat(stack,rhs,opt,lhs,nsp_bmatrix_concat_down);
}

/*
 * Diag Concatenation 
 * Res = [A,0;0,B] 
 * return NULLMAT on failure ( No more space )
 * A and B are left unchanged 
 */

static int int_bmatrix_concatdiag(Stack stack, int rhs, int opt, int lhs)
{
  return int_bmatrix__concat(stack,rhs,opt,lhs,nsp_bmatrix_concat_diag);
}


/*
 * BMatAddCols: add n cols of zero to NspBMatrix A 
 * A= [A,ones(m,n)] 
 * return 0 on failure ( no more space )
 */

static int int_bmatrix_addcols(Stack stack, int rhs, int opt, int lhs)
{
  int n1;
  NspBMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetBMatCopy(stack,1))== NULLBMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if (nsp_bmatrix_add_columns(HMat,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 * Add Rows: Add m rows of zero to a NspBMatrix A 
 * A = [A;ones(m,n)]
 * return NULLBMAT on failure 
 */

static int int_bmatrix_addrows(Stack stack, int rhs, int opt, int lhs)
{
  int m1;
  NspBMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetBMatCopy(stack,1))== NULLBMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if (nsp_bmatrix_add_rows(HMat,m1) != OK) return RET_BUG; ;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Size Compatibility is checked 
 *  WARNING: A is not Copied we want this routine to change A
 *  =======
 *  XXXXX: Attention cette routine ne peux pas changer le 
 *  premier argument qui est un objet nomme 
 */

int int_bmatrix_setrc(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *A,*B;
  NspMatrix *Rows,*Cols=NULLMAT;
  CheckRhs(3,4);
  CheckLhs(1,1);

  if ( IsBMatObj(stack,1) ) 
    {
      /* A is boolean **/
      if ((A = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
    }
  else 
    {
      Scierror("Error: A(...)= B, A and B must be of the same type\n");
      return RET_BUG;
    }

  if ( IsBMatObj(stack,2)  ) 
    {
      /* Rows is boolean: use find(Rows) **/
      NspBMatrix *BRows ;
      if ((BRows = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
      if ((Rows =nsp_bmatrix_find(BRows)) == NULLMAT) return RET_BUG;
    }
  else
    {
      /* Rows is a real matrix **/
      if ((Rows = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
    }
  if ( rhs == 4 )
    {
      /* Cols is boolean: use find(Cols) **/
      if ( IsBMatObj(stack,3) ) 
	{
	  NspBMatrix *BCols ;
	  if ((BCols = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
	  if ((Cols =nsp_bmatrix_find(BCols)) == NULLMAT) return RET_BUG;
	}  
      else
	{
	  if ((Cols = GetRealMat(stack,3)) == NULLMAT ) return RET_BUG;
	}
    }
  if ((B = GetBMat(stack,rhs)) == NULLBMAT) return RET_BUG;
  if ( B == A) 
    {
      if ((B = GetBMatCopy(stack,rhs)) == NULLBMAT) return RET_BUG;
    }
  if ( rhs == 3 )
    {  if (nsp_bmatrix_set_rows( A, Rows,B) != OK) return RET_BUG; }
  else
    {  if (nsp_bmatrix_set_submatrix( A, Rows,Cols,B) != OK) return RET_BUG;}

  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 * Res=BMatDeletecols(A,Cols)
 *     Cols unchanged  ( restored at end of function if necessary)
 *     WARNING: A must be changed by this routine
 */

typedef int (*delf) (NspBMatrix *M,NspMatrix *Elts);

static int int_bmatrix_deleteelts_gen(Stack stack, int rhs, int opt, int lhs, delf F)
{
  int alloc=FALSE;
  NspBMatrix *A,*BElts=NULLBMAT;
  NspMatrix *Elts;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  if ( IsBMatObj(stack,2)  ) 
    {
      /* Elts is boolean: use find(Elts) **/
      if ((BElts = GetBMat(stack,2)) == NULLBMAT) 
	return RET_BUG;
      if ((Elts =nsp_bmatrix_find(BElts)) == NULLMAT) 
	return RET_BUG;
      alloc=TRUE;
    }
  else
    {
      if ((Elts = GetRealMat(stack,2)) == NULLMAT) 
	return RET_BUG;
    }
  if ( (*F)( A, Elts) == FAIL )
    {
      if ( alloc ) nsp_matrix_destroy(Elts) ;
      return RET_BUG;
    }
  /* take care that A and Elts can be the same */
  if ( A == BElts ) NthObj(2)=NULLOBJ;
  NSP_OBJECT(A)->ret_pos = 1;
  if ( alloc ) nsp_matrix_destroy(Elts) ;
  return 1;
}

static int int_bmatrix_deletecols(Stack stack, int rhs, int opt, int lhs)
{
  return int_bmatrix_deleteelts_gen(stack,rhs,opt,lhs,
				    (delf) nsp_smatrix_delete_columns);
}

/*
 * Res=BMatDeleterows(A,Rows)
 *     Rows unchanged  ( restored at end of function if necessary)
 * WARNING: A must be changed by this routine
 */

static int int_bmatrix_deleterows(Stack stack, int rhs, int opt, int lhs)
{
  return int_bmatrix_deleteelts_gen(stack,rhs,opt,lhs,
				    (delf) nsp_smatrix_delete_rows);
}

/*
 * Res=BMatDeleteelts(A,Elts)
 *     Elts unchanged  ( restored at end of function if necessary)
 *     redirect to  nsp_smatrix_delete_elements which is generic now 
 */

static int int_bmatrix_deleteelts(Stack stack, int rhs, int opt, int lhs)
{
  return int_bmatrix_deleteelts_gen(stack,rhs,opt,lhs,
				    (delf) nsp_smatrix_delete_elements);
}

/*
 * Res=nsp_bmatrix_extract(A,Rows,Cols)
 * A unchanged, Rows and Cols are unchanged 
 * if Rows and Cols are to be kept they are restored at end of function 
 */

static int int_bmatrix_extract(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *A,*Res;
  NspMatrix *Rows,*Cols;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  if ((Rows = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ((Cols = GetMat(stack,3)) == NULLMAT) return RET_BUG;
  Res =nsp_bmatrix_extract( A, Rows,Cols);
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1, (NspObject *)Res);
  return 1;
}


/*
 * Res=nsp_bmatrix_extract_elements(Elts,A)
 * A unchanged, Elts
 */


/* generic function for elts extraction */

typedef NspBMatrix * (*extrf) (NspBMatrix *M,NspMatrix *Elts);

static int int_bmatrix_extractelts_gen(Stack stack, int rhs, int opt, int lhs, extrf F)
{
  NspBMatrix *A,*Res;
  NspMatrix *Elts;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;

  if ( IsBMatObj(stack,2)  ) 
    {
      /* Elts is boolean: use find(Elts) **/
      NspBMatrix *BElts;
      if ((BElts = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
      if ((Elts =nsp_bmatrix_find(BElts)) == NULLMAT) return RET_BUG;
    }
  else
    {
      /* Elts is a real matrix **/
      if ((Elts = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
    }

  if ((Res = (*F)( A, Elts)) == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_bmatrix_extractelts(Stack stack, int rhs, int opt, int lhs)
{
  return int_bmatrix_extractelts_gen(stack,rhs,opt,lhs,nsp_bmatrix_extract_elements);
}

/*
 * columns extraction  Cols A --> A(Cols)				   
 */

static int int_bmatrix_extractcols(Stack stack, int rhs, int opt, int lhs)
{
  return int_bmatrix_extractelts_gen(stack,rhs,opt,lhs,nsp_bmatrix_extract_columns);
}

/*
 * rows extraction 					   
 */

static int int_bmatrix_extractrows(Stack stack, int rhs, int opt, int lhs)
{
  return int_bmatrix_extractelts_gen(stack,rhs,opt,lhs,nsp_bmatrix_extract_rows);
}

/*
 * Returns the kthe diag of a NspBMatrix 
 */

static int int_bmatrix_diage(Stack stack, int rhs, int opt, int lhs)
{
  int k1;
  NspBMatrix *A,*Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
  if ((A = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  Res =nsp_bmatrix_extract_diag( A,k1);
  if ( Res == NULLBMAT)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Set the kth Diag of A to Diag 
 *  A is enlarged & comlexified if necessary 
 *  int nsp_bmatrix_create_diag(A,Diag,k)
 * WARNING: A is not copied we want this routine to change A
 */

static int int_bmatrix_diagset(Stack stack, int rhs, int opt, int lhs)
{
  int k1;
  NspBMatrix *A,*Diag;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  if ((Diag = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
  if ( GetScalarInt(stack,3,&k1) == FAIL) return RET_BUG;
  if (nsp_bmatrix_set_diag( A, Diag,k1) != OK) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 *  Creates a NspBMatrix with kth diag set to Diag 
 */

static int int_bmatrix_diagcre(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspBMatrix *Diag,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((Diag = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (Res =nsp_bmatrix_create_diag(Diag,k1)) == NULLBMAT ) return RET_BUG ;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 * BMatresize: changes NspBMatrix dimensions
 * m,n are changed and the arrays enlarged 
 */

static int int_bmatrix_resize(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  NspBMatrix  *HMat;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (HMat=GetBMatCopy(stack,1))== NULLBMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;      
  if (nsp_bmatrix_resize(HMat,m1,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}


/*
 * A'
 */

int
int_bmatrix_quote (Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *A, *B;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((A = GetBMat (stack, 1)) == NULLBMAT)
    return RET_BUG;
  if ((B = nsp_bmatrix_transpose (A)) == NULLBMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) B);
  return 1;
}


/*
 * BMat2LaTeXMat: writes BMat Objet on fd in tex language
 */

static int int_bmatrix_2latexmat(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  nsp_bmatrix_latex_print(HMat);
  return 0;
}

/*
 *  BMat2LaTeXTab: writes BMat Objet on fd in TeX language
 */

static int int_bmatrix_2latextab(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;    
  nsp_bmatrix_latex_tab_print(HMat);
  return 0;
}

/*
 * Usual matrix to boolean 
 */

static int int_bmatrix_m2b(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *BM;
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((M = GetMat(stack,1)) == NULLMAT) return RET_BUG;    
  if ((BM =nsp_matrix_to_bmatrix(M)) == NULLBMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) BM);
  return 1;
}

/*
 * boolean to Matrix 
 */

int int_bmatrix_b2m(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *BM;
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((BM = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;    
  if (( M =nsp_bmatrix_to_matrix(BM)) == NULLMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) M);
  return 1;
}

/*
 * == and <> 
 */

static int int_bmatrix_neq(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *A,*B, *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  if ((B = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
  Res =nsp_bmatrix_compare(A,B,"<>");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_bmatrix_eq(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *A,*B, *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  if ((B = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
  Res =nsp_bmatrix_compare(A,B,"==");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Same but returns a unique boolean 
 */

static int int_bmatrix_fneq(Stack stack, int rhs, int opt, int lhs)
{
  int rep,err;
  NspBMatrix *A,*B;
  NspObject *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  if ((B = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
  rep =nsp_bmatrix_full_compare(A,B,"<>",&err);
  if ( err == 1) 
    {
      Scierror("Error: operator %s , arguments with incompatible dimensions\n","<>");
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

static int int_bmatrix_feq(Stack stack, int rhs, int opt, int lhs)
{
  int rep,err;
  NspBMatrix *A,*B;
  NspObject *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  if ((B = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
  rep =nsp_bmatrix_full_compare(A,B,"==",&err);
  if ( err == 1) 
    {
      Scierror("Error: operator %s , arguments with incompatible dimensions\n","==");
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

/*
 * The Interface for basic matrices operation 
 */

static OpTab BMatrix_func[]={
  {"resize2vect_b", int_bmatrix_to_vect},
  {"latexmat_b",int_bmatrix_2latexmat},
  {"latextab_b",int_bmatrix_2latextab},
  {"addcols_b_m",int_bmatrix_addcols},
  {"addrows_b_m",int_bmatrix_addrows},
  {"and_b",int_bmatrix_and1},
  {"and_b_b",int_bmatrix_and},
  {"seq_and_b",int_bmatrix_and1},
  {"seq_and_b_b",int_bmatrix_and},
  {"b2m",int_bmatrix_b2m},
  {"concatd_b_b",int_bmatrix_concatd},
  {"concatr_b_b",int_bmatrix_concatr},
  {"concatdiag_b_b" ,  int_bmatrix_concatdiag },
  {"copy_b",int_bmatrix_copy},
  {"create_b",int_bmatrix_create},
  {"deletecols_b_b", int_bmatrix_deletecols},
  {"deletecols_b_m", int_bmatrix_deletecols},
  {"deleteelts_b_b", int_bmatrix_deleteelts},
  {"deleteelts_b_m", int_bmatrix_deleteelts},
  {"deleterows_b_b", int_bmatrix_deleterows},
  {"deleterows_b_m", int_bmatrix_deleterows},
  {"diagcre_b",int_bmatrix_diagcre},
  {"diage_b",int_bmatrix_diage},
  {"diagset_b",int_bmatrix_diagset},
  {"extract_b",int_bmatrix_extract},
  {"extractcols_b",int_bmatrix_extractcols},
  {"extractelts_b",int_bmatrix_extractelts},
  {"extractrows_b",int_bmatrix_extractrows},
  {"find_b",int_bmatrix_find},
  {"m2b",int_bmatrix_m2b},
  {"not_b",int_bmatrix_not},
  {"or_b",int_bmatrix_or1},
  {"or_b_b",int_bmatrix_or},
  {"seq_or_b",int_bmatrix_or1},
  {"seq_or_b_b",int_bmatrix_or},
  {"redim_b",int_bmatrix_redim},
  {"resize_b",int_bmatrix_resize},
  {"setrc_b",int_bmatrix_setrc},
  {"setrowscols_b",int_bmatrix_setrc},
  {"eq_b_b" ,  int_bmatrix_eq },
  {"ne_b_b" ,  int_bmatrix_neq },
  {"fneq_b_b" ,  int_bmatrix_fneq },
  {"feq_b_b" ,  int_bmatrix_feq },
  {"quote_b", int_bmatrix_quote},
  {(char *) 0, NULL}
};

int BMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(BMatrix_func[i].fonc))(stack,rhs,opt,lhs);
}


/* used to walk through the interface table 
   (for adding or removing functions) **/

void BMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = BMatrix_func[i].name;
  *f = BMatrix_func[i].fonc;
}
