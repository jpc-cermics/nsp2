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

#define  MaxpMatrix_Private 
#include "nsp/object.h"
#include "nsp/mpmatrix.h" /* to be added in object.h XXXXX */
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"
#include "nsp/smatrix-in.h"
#include "nsp/matutil.h"

/* 
 * NspMaxpMatrix inherits from NspObject 
 * Max Plus matrices 
 */

int nsp_type_mpmatrix_id=0;
NspTypeMaxpMatrix *nsp_type_mpmatrix=NULL;

/*
 * Type object for ClassA 
 * all the instance of NspTypeClassA share the same id. 
 * nsp_type_mpmatrix: is a an instance of NspTypeClassA 
 *    used for objects of NspClassA type (i.e built with new_mpmatrix) 
 * other instances are used for derived classes 
 */

NspTypeMaxpMatrix *new_type_mpmatrix(type_mode mode)
{
  NspTypeMaxpMatrix *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_mpmatrix != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_mpmatrix;
    }
  
  if ((type =  malloc(sizeof(NspTypeMaxpMatrix))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs =  NULL; /*matrix_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods =  mpmatrix_get_methods;
  type->new = (new_func *) new_mpmatrix;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for matrix */ 
  
  top->pr = (print_func *) nsp_mpmatrix_print;                    
  top->dealloc = (dealloc_func *) nsp_mpmatrix_destroy;
  top->copy  =  (copy_func *) nsp_mpmatrix_copy;                   
  top->size  = (size_func *) mpmatrix_size;                  
  top->s_type =  (s_type_func *) mpmatrix_type_as_string;    
  top->sh_type = (sh_type_func *) mpmatrix_type_short_string;
  top->info = (info_func *) nsp_mpmatrix_info ;                    
  top->is_true = (is_true_func  *) mpmatrix_is_true; 
  top->loop =(loop_func *) mpmatrix_loop_extract ; 
  top->path_extract = NULL ; /* (path_func *) mpmatrix_path_extract ; */
  top->get_from_obj = (get_from_obj_func *) mpmatrix_object;
  top->eq  = (eq_func *) mpmatrix_eq;
  top->neq  = (eq_func *) mpmatrix_neq;
  top->save  = (save_func *) mpmatrix_xdr_save;
  top->load  = (load_func *) mpmatrix_xdr_load;

  /* specific methods for matrix */
      
  type->init = (init_func *) init_mpmatrix;
      
  /* 
   * Matrix interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_mpmatrix_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMaxpMatrix called nsp_type_mpmatrix
       */
      type->id =  nsp_type_mpmatrix_id = nsp_new_type_id();
      nsp_type_mpmatrix = type;
      if ( nsp_register_type(nsp_type_mpmatrix) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_mpmatrix(mode);
    }
  else 
    {
      type->id = nsp_type_mpmatrix_id;
      return type;
    }
}

/*
 * initialize Matrix instances 
 * locally and by calling initializer on parent class 
 */

static int init_mpmatrix(NspMaxpMatrix *o,NspTypeMaxpMatrix *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Matrix 
 */

NspMaxpMatrix *new_mpmatrix() 
{
  NspMaxpMatrix *loc; 
  /* type must exists */
  nsp_type_mpmatrix = new_type_mpmatrix(T_BASE);
  if ( (loc = malloc(sizeof(NspMaxpMatrix)))== NULLMAXPMAT) return loc;
  /* initialize object */
  if ( init_mpmatrix(loc,nsp_type_mpmatrix) == FAIL) return NULLMAXPMAT;
  return loc;
}

/*
 * Object methods redefined for matrix 
 */

static int mpmatrix_size(NspMaxpMatrix *Mat, int flag)
{
  switch (flag) 
    {
    case 0: return Mat->mn;
    case 1: return Mat->m;
    case 2: return Mat->n;
    }
  return 0;
}

/* Type as char */

static char mat_type_name[]="MaxpMat";
static char mat_short_type_name[]="mp";

char *mpmatrix_type_as_string(void)
{
  return(mat_type_name);
}

char *mpmatrix_type_short_string(void)
{
  return(mat_short_type_name);
}

/* used in for x=Matrix ... **/

NspObject *mpmatrix_loop_extract(char *str, NspObject *O, NspObject *O1, int i, int *rep)
{
  NspMaxpMatrix *M= (NspMaxpMatrix *) O1,*M1=NULLMAXPMAT;
  if ( O == NULLOBJ ) 
    {
      if (( M1=MpMatLoopCol(str,NULLMAXPMAT,M,i,rep))==NULLMAXPMAT) return NULLOBJ;
      if (( *rep == RET_ENDFOR )) return NULLOBJ;
      return (NspObject *) M1 ;
    }
  else
    {
      if (( M1 = mpmatrix_object(O)) == NULLMAXPMAT ) return NULLOBJ;
      M1=MpMatLoopCol(str,M1,M,i,rep);
      if (( *rep == RET_ENDFOR )) return NULLOBJ;
      return O;
    }
}

int mpmatrix_eq(NspObject *A,NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_mpmatrix_id) == FALSE) return FALSE ;
  rep =nsp_mat_fullcomp((NspMatrix *) A,(NspMatrix *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

int mpmatrix_neq(NspObject *A,NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_mpmatrix_id) == FALSE) return TRUE;
  rep =nsp_mat_fullcomp((NspMatrix *) A,(NspMatrix *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * Mat == TRUE ? 
 *  if Mat != [] and all the elements of Mat are != 0
 */

static int mpmatrix_is_true(NspMaxpMatrix *M)
{
  int i;
  if ( M->mn == 0) return FALSE;
  if ( M->rc_type == 'r') 
    for ( i = 0 ; i < M->mn ; i++ ) 
      {
	if ( M->R[i] == 0.0) return FALSE;
      }
  else
    for ( i = 0 ; i < M->mn ; i++ ) 
      {
	if (  M->I[i].r == 0.0 && M->I[i].i == 0.0 ) 
	  return FALSE;
      }
  return TRUE;
}

/*
 * Save a Matrix in a file stream 
 */

static int mpmatrix_xdr_save(NspFile  *F, NspMaxpMatrix *M)
{
  if (nsp_xdr_save_i(F,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(F, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(F,M->m) == FAIL) return FAIL;
  if (nsp_xdr_save_i(F,M->n) == FAIL) return FAIL;
  if (nsp_xdr_save_c(F,M->rc_type) == FAIL) return FAIL;
  if ( M->rc_type == 'r') 
    { if (nsp_xdr_save_array_d(F,M->R,M->mn) == FAIL) return FAIL; }
  else
    { if (nsp_xdr_save_array_d(F,(double *) M->I,2*M->mn) == FAIL) return FAIL; }
  return OK;
}

/*
 * Load a Matrix from a file stream 
 **/

static NspMaxpMatrix *mpmatrix_xdr_load(NspFile  *F)
{
  char c;
  int m,n;
  NspMaxpMatrix *M;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(F,name,NAME_MAXL) == FAIL) return NULLMAXPMAT;
  if (nsp_xdr_load_i(F,&m) == FAIL) return NULLMAXPMAT;
  if (nsp_xdr_load_i(F,&n) == FAIL) return NULLMAXPMAT;
  if (nsp_xdr_load_c(F,&c) == FAIL) return NULLMAXPMAT;
  if (( M= nsp_mpmatrix_create(name,c,m,n)) == NULLMAXPMAT ) return NULLMAXPMAT;
  if ( M->rc_type == 'r') 
   {
     if (nsp_xdr_load_array_d(F,M->R,M->mn) == FAIL) return NULLMAXPMAT;
   }
  else
   {
     if (nsp_xdr_load_array_d(F,(double *)M->I,2*M->mn) == FAIL) return NULLMAXPMAT;
   }
  return M;
}

/*
 * A = MatObj(O);
 * checks that O is an object of type Matrix 
 * or a Hobj which points to an object of type Matrix
 *    if so, returns a pointer to that Matrix and else returns
 *    NULLMAXPMAT
 */

NspMaxpMatrix  *mpmatrix_object(NspObject *O)
{

  /* Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type **/
  if ( check_cast(O,nsp_type_mpmatrix_id) == TRUE) return ((NspMaxpMatrix *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_mpmatrix));
  return(NULLMAXPMAT);
}

/*
 * IsMpMatObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  Matrix 
 * or a Hobj which points to an object of type Matrix
 */

int IsMpMatObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_mpmatrix_id);
}

/*
 * IsMat(O)
 * only checks that object is an object of type  Matrix 
 * or a Hobj which points to an object of type Matrix
 */

int IsMpMat(NspObject *O)
{
  return nsp_object_type(O , nsp_type_mpmatrix_id);
}

/*
 * Checks that first+i object on the stack 
 * is a matrix and returns that matrix  
 * (internal)
 */

static NspMaxpMatrix *GetMpMat_G (Stack stack, int i)
{
  NspMaxpMatrix *M;
  if (( M = mpmatrix_object(NthObj(i))) == NULLMAXPMAT)
    ArgMessage(stack,i);
  return M;
}

/*
 * Checks that first+i object on the stack 
 * is a matrix and returns that matrix  
 * or a copy of that matrix if its name 
 * is != NVOID 
 * (internal function)
 * the object on the stack is replaced by the new copy 
 * 
 */

static NspMaxpMatrix *GetMpMatCopy_G(Stack stack, int i)
{
  if (  GetMpMat_G(stack,i) == NULL ) return NULL;;
  return MaybeObjCopy(&NthObj(i));
}

/*
 * GetMpMat_G and GetMpMatCopy_G and conversions 
 */

NspMaxpMatrix *GetMpMatCopy(Stack stack, int i)
{
  return MpMat2double(GetMpMatCopy_G(stack,i));
}

NspMaxpMatrix *GetMpMat(Stack stack, int i)
{
  return MpMat2double(GetMpMat_G(stack,i));
}

NspMaxpMatrix *GetMpMatCopyInt(Stack stack, int i)
{
  return MpMat2int(GetMpMatCopy_G(stack,i));
}

NspMaxpMatrix *GetMpMatInt(Stack stack, int i)
{
  return MpMat2int(GetMpMat_G(stack,i));
}

NspMaxpMatrix *GetMpMatCopyFloat(Stack stack, int i)
{
  return MpMat2float(GetMpMatCopy_G(stack,i));
}

NspMaxpMatrix *GetMpMatFloat(Stack stack, int i)
{
  return MpMat2float(GetMpMat_G(stack,i));
}

/*
 * Checks that first+i object on the stack 
 * is a matrix and returns that matrix  
 * or a copy of that matrix if its name 
 * is != NVOID 
 */

NspMaxpMatrix *GetRealMpMatCopy_G(Stack stack, int i)
{
  NspMaxpMatrix *M;
  if (( M = GetMpMatCopy_G(stack,i)) == NULLMAXPMAT) return NULLMAXPMAT;
  if ( M->rc_type == 'i' ) 
    {
      Scierror("\t%s", ArgPosition(i));
      ArgName(stack,i);
      Scierror(" of function %s should not be complex\n",stack.fname);
      return NULLMAXPMAT;
    }
  return M;
}

/*
 * Checks that first+i object on the stack 
 * is a matrix and returns that matrix  
 * The matrix is converted to double if necessary 
 */

NspMaxpMatrix *GetRealMpMat_G(Stack stack, int i)
{
  NspMaxpMatrix *M;
  if (( M = GetMpMat_G(stack,i)) == NULLMAXPMAT) return NULLMAXPMAT;
  if ( M->rc_type == 'i' ) 
    {
      Scierror("\t%s", ArgPosition(i));
      ArgName(stack,i);
      Scierror(" of function %s should not be complex\n",stack.fname);
      return NULLMAXPMAT;
    }
  return M;
}

/*
 * GetMpMat_G and GetMpMatCopy_G and conversions 
 */

NspMaxpMatrix *GetRealMpMatCopy(Stack stack, int i)
{
  return MpMat2double(GetRealMpMatCopy_G(stack,i));
}

NspMaxpMatrix *GetRealMpMat(Stack stack, int i)
{
  return MpMat2double(GetRealMpMat_G(stack,i));
}

NspMaxpMatrix *GetRealMpMatCopyInt(Stack stack, int i)
{
  return MpMat2int(GetRealMpMatCopy_G(stack,i));
}

NspMaxpMatrix *GetRealMpMatInt(Stack stack, int i)
{
  return MpMat2int(GetRealMpMat_G(stack,i));
}

NspMaxpMatrix *GetRealMpMatCopyFloat(Stack stack, int i)
{
  return MpMat2float(GetRealMpMatCopy_G(stack,i));
}

NspMaxpMatrix *GetRealMpMatFloat(Stack stack, int i)
{
  return MpMat2float(GetRealMpMat_G(stack,i));
}


/*
 * Utility function : Converts A to int 
 * on-place. returns max and min 
 * XXXXXX : doit-on mettre 1 et 1 comme valeur par defaut 
 */

integer *MaxpMatd2i(NspMaxpMatrix *A, integer *imin, integer *imax)
{
  int i;
  integer *Iloc;
  Iloc = (integer *) A->R;
  nsp_double2int(&A->mn,A->R,Iloc);
  *imax = 1;  *imin = 1;
  for ( i = 0 ; i < A->mn; i++) 
    {
      if ( Iloc[i] > *imax) 
	*imax = Iloc[i] ;
      else if ( Iloc[i] < *imin ) 
	*imin = Iloc[i];
    }
  return Iloc;
}


/*
 * Utility function : Converts back to double
 * if Matrix was previously converted to int or real
 * A is changed and its adress is also returned
 */

NspMaxpMatrix *MpMat2double(NspMaxpMatrix *A)
{
  int inc = -1;
  if ( A != NULLMAXPMAT && A->rc_type == 'r' ) 
    switch ( A->convert ) 
      {
      case 'i' : 
	nsp_int2double(&A->mn,(int *) A->R,&inc,A->R,&inc);
	A->convert = 'd';
	break;
      case 'f' : 
	nsp_float2double(&A->mn,(float *) A->R,&inc,A->R,&inc);
	A->convert = 'd';
	break;
      }
  return A;
}

/*
 * Utility function : Converts to int 
 */

NspMaxpMatrix * MpMat2int(NspMaxpMatrix *A)
{
  if ( A != NULLMAXPMAT &&  A->convert !=  'i' ) 
    {
      if ( A->rc_type == 'r' )
	{
	  if ( A->convert == 'd' ) 
	    nsp_double2int(&A->mn,A->R,(integer *) A->R);
	  else if ( A->convert == 'f' ) 
	    nsp_float2int(&A->mn,(float *) A->R,(integer *) A->R);
	  A->convert  = 'i' ;
	}
      else 
	{
	  Scierror("Error: Cannot convert a complex matrix to int\n");
	  return NULLMAXPMAT;
	}
    }
  return A;
}

/*
 * Utility function : Converts to float 
 */

NspMaxpMatrix *MpMat2float(NspMaxpMatrix *A)
{
  static int inc=-1;
  if ( A!= NULLMAXPMAT &&  A->convert != 'f' ) 
    {
      if ( A->rc_type == 'r' )
	{
	  if ( A->convert == 'd' ) 
	nsp_double2float(&A->mn,A->R,(float *) A->R);
	  else if ( A->convert == 'i' ) 
	nsp_int2float(&A->mn,(int *) A->R,&inc,(float*)A->R,&inc);
	  A->convert  = 'i' ;
	}
      else 
	{
	  Scierror("Error: Cannot convert a complex matrix to float\n");
	  return NULLMAXPMAT;
	}
    }
  return A;
}


/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static NspMethods *mpmatrix_get_methods(void) { return NULL;}


/*------------------------------------------------------
 * interfaced functions 
 *------------------------------------------------------*/

/*
 * Now the interfaced function for basic matrices operations
 */

/*
 * Creation of a Matrix 
 * returns NULLMAXPMAT on failure 
 * The matrix is created with no initial value 
 */

int int_mpcreate(Stack stack, int rhs, int opt, int lhs)
{
  integer m1,n1;
  NspMaxpMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ((HMat = nsp_mpmatrix_create(NVOID,'r',m1,n1) ) == NULLMAXPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)HMat);
  return 1;
}

/*
 * convert a matrix to a max plus matrix 
 */

int int_m2mp(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *B;
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if (( B = nsp_mpmatrix_copy((NspMaxpMatrix *) A)) == NULLMAXPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);   
  return 1;
}

/*
 * interface for operator : 
 */

int int_mpimpl(Stack stack, int rhs, int opt, int lhs)
{
  double dfirst,step=1.0E0,last;
  NspMaxpMatrix *M;
  CheckRhs(2,3);
  CheckLhs(1,1);
  if (GetScalarDouble(stack,1,&dfirst) == FAIL) return RET_BUG;
  if (GetScalarDouble(stack,2,&last) == FAIL) return RET_BUG;
  if ( rhs == 3 ) 
    {
      step = last;
      if (GetScalarDouble(stack,3,&last) == FAIL) return RET_BUG;
    }
  if ((M = nsp_mpmatrix_create_impl(dfirst,step,last) ) == NULLMAXPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)M);
  return 1;
}


/*
 * Copy of a Matrix 
 * The copy has  name NVOID
 * returns NULLMAXPMAT on failure 
 */

int int_mpcopy(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *M1,*M2;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( M1 = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if (( M2 = nsp_mpmatrix_copy(M1)) == NULLMAXPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)M2);
  return 1;  
}

/*
 * Change a matrix of Real type to Imaginary type 
 * The imag part is not initialized 
 */

int int_mpcomplexify(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat;
  double d=0.00;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT ) return RET_BUG;
  if ( rhs == 2 ) 
    if (GetScalarDouble(stack,2,&d) == FAIL) return RET_BUG;
  if (nsp_mat_complexify((NspMatrix *) HMat,d) != 0 ) { return RET_BUG;};
  return 1;  
}

/*
 * Returns real(A) 
 */

int int_mprealpart(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ( HMat->rc_type == 'i') 
    {
      if ((HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
    }
  if (nsp_mat_get_real((NspMatrix *) HMat) != OK  ) return RET_BUG;
  ((NspObject *) HMat)->ret_pos = 1;
  return 1;
}

/*
 * Returns imag(A), the Imaginary part of Matrix A 
 */

int int_mpimagpart(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ( (HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  if (nsp_mat_get_imag((NspMatrix *)HMat) !=  OK ) return RET_BUG; ;
  ((NspObject *) HMat)->ret_pos = 1;
  return 1;
}

/*
 * Returns a kroeneker product A.*.B 
 */

int int_mpkron(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat1,*HMat2,*M;
  NspMatrix *HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((HMat2 = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  if ((HMat3 =nsp_mat_kron((NspMatrix *) HMat1,(NspMatrix *) HMat2)) == NULLMAT) return RET_BUG;
  /* from matrix to maxplus matrix by moving data */
  if ((M = nsp_mp_matrix_from_m(NVOID,HMat3))  == NULLMAXPMAT) return RET_BUG;
  nsp_matrix_destroy(HMat3);
  MoveObj(stack,1,(NspObject *) M);
  return 1;
}


/*
 * MatSort 
 * [A_sorted,Index]=sort(A, 'r'| 'c' | 'g' | 'lr'| 'lc' ,'i'|'d')
 */

int int_mpsort(Stack stack, int rhs, int opt, int lhs)
{
  char *str1,*str2;
  NspMaxpMatrix *A;
  NspMatrix *Index;
  CheckRhs(1,3);
  CheckLhs(1,2);
  /* XXXX*/
  if ( IsSMatObj(stack,1))  return int_smxsort(stack,rhs,opt,lhs);
  if ((A=GetRealMpMatCopy(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ( rhs >= 2) 
    {
      if ((str1 = GetString(stack,2)) == (char*)0) return RET_BUG;
      if ( strcmp(str1,"c") != 0 && strcmp(str1,"r") != 0 
	   && strcmp(str1,"g") != 0 && strcmp(str1,"lr") != 0 
	   && strcmp(str1,"lc") != 0 ) 
	{
	  Scierror("Error: wrong second argument in function %s\n",stack.fname);
	  Scierror("\tonly 'g','c','r','lr','lc' are allowed\n");
	  return RET_BUG;
	}
    }
  else 
    { str1 = "g"; }
  if ( rhs == 3) 
    {
      if ((str2 = GetString(stack,3)) == (char*)0) return RET_BUG;
      if ( strcmp(str2,"i") != 0 && strcmp(str2,"d") != 0 )
	{
	  Scierror("Error: wrong third argument in function %s\n",stack.fname);
	  Scierror("\tonly 'i' or 'd' are allowed\n");
	  return RET_BUG;
	}
    }
  else 
    { str2 = "d"; }
  Index=nsp_mat_sort((NspMatrix *) A,lhs,str1,str2);
  NSP_OBJECT(A)->ret_pos = 1;
  if ( lhs == 2) 
    {
      if ( Index == NULLMAT ) return RET_BUG;
      MoveObj(stack,2,(NspObject *)Index);
      return 2;
    }
  else 
    {
      return 1;
    }
}

/*
 *nsp_mpmat_sum: sum=Sum(a[,b]) 
 * a is unchanged 
 */

typedef NspMatrix *(*SuPro) (NspMatrix *A,char *);

static int int_mp_sum(Stack stack, int rhs, int opt, int lhs, SuPro F)
{
  char *str;
  NspMaxpMatrix *HMat,*Res; 
  NspMatrix *M;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((HMat = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ( rhs == 2) 
    {
      if ((str = GetString(stack,2)) == (char*)0) return RET_BUG;
    }
  else 
    { str = "F"; }
  if ((M= (*F)((NspMatrix *) HMat,str)) == NULLMAT ) return RET_BUG;
  if ((Res = nsp_mp_matrix_from_m(NVOID,M))  == NULLMAXPMAT) return RET_BUG;
  nsp_matrix_destroy(M);
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_mpsum(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_mp_sum(stack,rhs,opt,lhs,nsp_mat_sum) );
}

/*
 * matprod : product of all elements of a
 * a is unchanged 
 */

int int_mpprod(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_mp_sum(stack,rhs,opt,lhs,nsp_mat_prod) );
}

/*
 * matcusum : cumulative sum of all elements of a
 * a is unchanged 
 */

int int_mpcusum(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_mp_sum(stack,rhs,opt,lhs,nsp_mat_cum_sum) );
}

/*
 * matcuprod : cumulative prod of all elements of a
 * a is unchanged 
 */

int int_mpcuprod(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_mp_sum(stack,rhs,opt,lhs,nsp_mat_cum_prod) );
}

/*
 *nsp_mpmat_maxi: Maxi(*HMat);
 * A is unchanged 
 */

typedef NspMatrix *(*MiMax) (NspMatrix *A,char *,NspMatrix **Imax,int lhs);
typedef int (*MiMax1) (NspMatrix *A, NspMatrix *B, NspMatrix *Ind, integer j,int flag);

static int int_mp_maxi(Stack stack, int rhs, int opt, int lhs, MiMax F, MiMax1 F1)
{
  char *str;
  NspMaxpMatrix *A,*M,*B; 
  NspMatrix *M1,*Imax;
  if ( rhs < 1) 
    { 
      Scierror("Error:\t Rhs must be >= 1 for function %s\n",stack.fname);
      return RET_BUG;
    }
  CheckLhs(1,2);
  if ( rhs == 1 || ( rhs == 2 && IsSMatObj(stack,2) ))
    {
      /* maxi(A) or maxi(A,'c' or 'r' or 'F') where A is a matrix **/
      /* idem for mini **/
      if ((A = GetRealMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
      if ( rhs == 2) 
	{
	  if ((str = GetString(stack,2)) == (char*)0) return RET_BUG;
	}
      else 
	{ str = "F"; }
      if (( M1= (*F)((NspMatrix *)A,str,&Imax,lhs)) == NULLMAT ) return RET_BUG;
      if ( lhs == 2)
	{
	  MoveObj(stack,2,(NspObject *)Imax);
	}
      if ((M = nsp_mp_matrix_from_m(NVOID,M1))  == NULLMAXPMAT) return RET_BUG;
      nsp_matrix_destroy(M1);
      MoveObj(stack,1,(NspObject *)M);
    }
  else
    {
      NspMatrix *Ind;
      int flag=0,i;
      /* Maxi(A1,A2,....,An) ***/
      if ((A = GetRealMpMatCopy(stack,1)) == NULLMAXPMAT) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
      if (lhs == 2) 
	{
	  /* intialize Ind matrix **/
	  flag =1;
	  if ((Ind = nsp_matrix_create(NVOID,'r',A->m,A->n)) == NULLMAT)  return RET_BUG;
	  nsp_mat_set_rval((NspMatrix *) Ind,1.0);
	}
      for ( i= 2 ; i <= rhs ; i++) 
	{
	  if ((B = GetRealMpMat(stack,i)) == NULLMAXPMAT) return RET_BUG;
	  if ((*F1)((NspMatrix *)A,(NspMatrix *)B,Ind,i,flag) == FAIL ) return RET_BUG;
	}
      if ( lhs == 2)
	{
	  MoveObj(stack,2,(NspObject *)Ind);
	}
    }
  return Max(lhs,1);
}


int int_mpmaxi(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_mp_maxi(stack,rhs,opt,lhs,nsp_mat_maxi,nsp_mat_maxitt1) );
}


/*
 *nsp_mpmat_mini: Mini(A)
 * A is unchanged 
 * rs and ri are set to the result 
 */

int int_mpmini(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_mp_maxi(stack,rhs,opt,lhs,nsp_mat_mini,nsp_mat_minitt1) );
}

/*
 *nsp_mpmat_triu: A=Triu(a)
 * A is changed  
 */

int int_mptriu(Stack stack, int rhs, int opt, int lhs)
{
  integer k1=0;
  NspMaxpMatrix *HMat;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ( rhs == 2) 
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  nsp_mat_triu((NspMatrix *) HMat,k1);
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 *nsp_mpmat_tril: A=Tril(A)
 * A is changed  
 */

int int_mptril(Stack stack, int rhs, int opt, int lhs)
{
  integer k1=0;
  NspMaxpMatrix  *HMat; 
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ( rhs == 2) 
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  nsp_mat_tril((NspMatrix *) HMat,k1);
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 *nsp_mpmat_ones: A=ones(m,n)
 * A is created , m,n no
 */

typedef NspMatrix * (*Mfunc) (int m,int n);

/* generic function for ones,rand,eyes **/
 
static int int_mp_gen(Stack stack, int rhs, int opt, int lhs, Mfunc F)
{
  integer m1,n1;
  NspMaxpMatrix *HMat;
  NspMatrix *M;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ( rhs == 2) 
    {
      if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
      if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
    }
  else 
    {
      if ( (HMat=GetMpMat(stack,1))== NULLMAXPMAT) return RET_BUG;
      m1= HMat->m;
      n1= HMat->n;
    }
  if ((M = (*F)(m1,n1) ) == NULLMAT) return RET_BUG;
  if ((HMat = nsp_mp_matrix_from_m(NVOID,M))  == NULLMAXPMAT) return RET_BUG;
  nsp_matrix_destroy(M);
  MoveObj(stack,1,(NspObject *)HMat);
  return 1;
}

int int_mpones(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen(stack,rhs,opt,lhs,nsp_mat_ones) ;
}



/*
 *nsp_mat_eye: A=Eye(m,n)
 * A is created  m,n no
 */

int int_mpeye(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen(stack,rhs,opt,lhs,nsp_mat_eye);
}

/*
 *nsp_mat_zeros: A=zeros(m,n)
 * A is created  m,n no
 */

int int_mpzeros(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen(stack,rhs,opt,lhs,nsp_mat_zeros);
}

/*
 * setr(A,d) <=> real(A) = d, im(A) unchanged 
 */

int int_mpsetr(Stack stack, int rhs, int opt, int lhs)
{
  double dval;
  NspMaxpMatrix *HMat; 
  CheckRhs(2,2);
  CheckLhs(1,1);
  if (GetScalarDouble(stack,2,&dval) == FAIL) return RET_BUG;
  /* xxxxx verifier que la matrice a un nom **/
  if ( (HMat=GetMpMat(stack,1))== NULLMAXPMAT) return RET_BUG;
  nsp_mat_set_rval((NspMatrix *) HMat,dval);
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 * seti(A,d) <=> imag(A) = d, real(A) unchanged 
 */

int int_mpseti(Stack stack, int rhs, int opt, int lhs)
{
  double dval;
  NspMaxpMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if (GetScalarDouble(stack,2,&dval) == FAIL) return RET_BUG;
  /* xxxxx verifier que la matrice a un nom **/
  if ( (HMat=GetMpMat(stack,1))== NULLMAXPMAT) return RET_BUG;
  if (nsp_mat_set_ival((NspMatrix *) HMat,dval) != OK ) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 * A = Matclean(a [,epsa,[epsr]]) clean A according to eps 
 * A is changed : 
 * XXXX A Changer : mettre le vrai eps 
 */

int int_mpclean(Stack stack, int rhs, int opt, int lhs)
{
  double epsa=1.e-8,epsr=1.e-8;
  NspMaxpMatrix *HMat;
  CheckRhs(1,3);
  CheckLhs(1,1);
  if ( rhs >= 2) 
    {
      if ( GetScalarDouble(stack,2,&epsa) == FAIL) return RET_BUG;
    }
  if ( rhs >= 3) 
    {
      if ( GetScalarDouble(stack,3,&epsr) == FAIL) return RET_BUG;
    }
  if ( (HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  nsp_mat_clean((NspMatrix *) HMat,rhs,epsa,epsr);
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 * Operation leading to Boolean result 
 */

/* A < B */ 
int int_mplt(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRealMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((B = GetRealMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  Res = nsp_mat_comp((NspMatrix *)A,(NspMatrix *)B,"<");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_mple(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRealMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((B = GetRealMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  Res =nsp_mat_comp((NspMatrix *)A,(NspMatrix *)B,"<=");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_mpneq(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((B = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  Res =nsp_mat_comp((NspMatrix *)A,(NspMatrix *)B,"<>");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_mpeq(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((B = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  Res =nsp_mat_comp((NspMatrix *)A,(NspMatrix *)B,"==");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_mpgt(Stack stack, int rhs, int opt, int lhs)
{

  NspMaxpMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRealMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((B = GetRealMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  Res =nsp_mat_comp((NspMatrix *)A,(NspMatrix *)B,">");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}


int int_mpge(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRealMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((B = GetRealMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  Res =nsp_mat_comp((NspMatrix *)A,(NspMatrix *)B,">=");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Same but returns a unique boolean 
 */

static int int_mpf_gen(Stack stack, int rhs, int opt, int lhs,char *op)
{
  int rep,err;
  NspMaxpMatrix *A,*B;
  NspObject *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetRealMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((B = GetRealMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  rep =nsp_mat_fullcomp((NspMatrix *)A,(NspMatrix *)B,op,&err);
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

int int_mpflt(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpf_gen(stack,rhs,opt,lhs,"<");
}

int int_mpfle(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpf_gen(stack,rhs,opt,lhs,"<=");
}

int int_mpfneq(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpf_gen(stack,rhs,opt,lhs,"<>");
}

int int_mpfeq(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpf_gen(stack,rhs,opt,lhs,"==");
}

int int_mpfgt(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpf_gen(stack,rhs,opt,lhs,">");
}

int int_mpfge(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpf_gen(stack,rhs,opt,lhs,">=");
}


/*
 * NspMaxpMatrix : Restack a copy of matrix A
 * keeps m*n constant
 */

int int_mpmatrix(Stack stack, int rhs, int opt, int lhs)
{
  integer m1,n1;
  NspMaxpMatrix  *A,*B;
  CheckRhs(2,3);
  CheckLhs(1,1);
  if ( (A=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  if ( rhs == 3) 
    {
      if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
      if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
    }
  else if ( rhs == 2) 
    {
      if ((B = GetRealMpMat(stack,2))== NULLMAXPMAT) return RET_BUG;
      if ( B->mn != 2) 
	{
	  Scierror("Error:\t second argument of function %s\n",stack.fname);
	  Scierror("\texpecting a vector of size 2\n");
	  return RET_BUG;
	}
      m1= (int) B->R[0];
      n1= (int) B->R[1];
    }
  if ( nsp_mpmatrix_redim(A,m1,n1) != OK) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Matredim : changes Matrix dimensions
 * but keeps m*n constant
 * WARNING : Object on stack is changed 
 * =======
 */

int int_mpredim(Stack stack, int rhs, int opt, int lhs)
{
  integer m1,n1;
  NspMaxpMatrix  *HMat;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (HMat=GetMpMat(stack,1))== NULLMAXPMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if ( nsp_mpmatrix_redim(HMat,m1,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 * changes a copy of matrix stack object to column vector 
 */

int int_mpmat2vect(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix  *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  if ( nsp_mpmatrix_redim(HMat,HMat->mn,1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/* xxxx ecrire le Enlarge ***/

/*
 * Right Concatenation 
 * A= [A,B] 
 */

int int_mpconcatr(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMpMat(stack,1))  == NULLMAXPMAT) return RET_BUG;
  if ( HMat1->mn == 0) 
    {
      /* return 2 */
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  if ( HMat2->mn == 0) 
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else
    {
      if ((HMat1 = GetMpMatCopy(stack,1))  == NULLMAXPMAT) return RET_BUG;
      if (nsp_mpmatrix_concat_right(HMat1,HMat2)!= OK) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  return 1;
}

/*
 * Right Concatenation Mat & BMat  
 * A= [A,B] 
 * we have to deal with [] 
 * if Mat<>[]  -->  [Mat,Bmat] -> [Mat,b2m(BMat)] 
 * if Mat==[]  -->  [Mat,Bmat] -> BMat
 */

int int_mpconcatr_mb(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( IsMatObj(stack,1)  ) 
    {
      if ((HMat = GetMpMat(stack,1))  == NULLMAXPMAT) return RET_BUG;
      if ( HMat->mn == 0) 
	{
	  /* [[],B] --> B **/
	  NSP_OBJECT(NthObj(2))->ret_pos = 1;
	  return 1;
	}
      else 
	{
	  /* [A,B] --> [A, b2m(B)] **/
	  stack.first +=1;
	  if ( int_bmatrix_b2m(stack,1,0,1) < 0 ) return RET_BUG;
	  stack.first -=1;
	  return int_mpconcatr(stack,rhs,opt,lhs);
	}
    }
  else
    {
      /* [B,A] **/
      if ((HMat = GetMpMat(stack,2))  == NULLMAXPMAT) return RET_BUG;
      if ( HMat->mn == 0)  return 1; 
      if ( int_bmatrix_b2m(stack,1,0,1) < 0 ) return RET_BUG;
      return int_mpconcatr(stack,rhs,opt,lhs);
    }
}


/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLMAXPMAT on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

typedef NspMaxpMatrix * (*Fconcat) (const NspMaxpMatrix *,const NspMaxpMatrix *);

static int int_mp_concat(Stack stack, int rhs, int opt, int lhs, Fconcat F)
{
  NspMaxpMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMpMat(stack,1))  == NULLMAXPMAT) return RET_BUG;
  if ( HMat1->mn == 0) 
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  if ( HMat2->mn == 0) 
    {
      NSP_OBJECT(HMat1)->ret_pos =1;
      return 1;
    }
  else
    {
      NspMaxpMatrix *HMat3;
      if (( HMat3 = (*F)(HMat1,HMat2)) == NULLMAXPMAT)  return RET_BUG;
      MoveObj(stack,1,(NspObject *)HMat3);
    }
  return 1;
}

int int_mpconcatd(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_concat(stack,rhs,opt,lhs,nsp_mpmatrix_concat_down);
}

/*
 * Diag Concatenation 
 * Res = [A,0;0,B] 
 * return NULLMAXPMAT on failure ( No more space )
 * A and B are left unchanged 
 */

int int_mpconcatdiag(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_concat(stack,rhs,opt,lhs,nsp_mpmatrix_concat_diag);
}


/*
 * MatAddCols : add n cols of zero to Matrix A 
 * A= [A,ones(m,n)] 
 * A is changed 
 */

int int_mpaddcols(Stack stack, int rhs, int opt, int lhs)
{
  integer n1;
  NspMaxpMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ( nsp_mpmatrix_add_columns(HMat,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos =1;
  return 1;
}


/*
 * Add Rows : Add m rows of zero to a Matrix A 
 * A = [A;0*ones(m,n)]
 * return NULLMAXPMAT on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

int int_mpaddrows(Stack stack, int rhs, int opt, int lhs)
{
  integer m1;
  NspMaxpMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( nsp_mpmatrix_add_rows(HMat,m1) != OK) return RET_BUG; ;
  NSP_OBJECT(HMat)->ret_pos =1;
  return 1;
}

/*
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Size Compatibility is checked 
 *  WARNING : A is not Copied we want this routine to change A
 *  =======
 */

int int_mpsetrc(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A,*B;
  NspMatrix *Rows,*Cols=NULLMAT;
  CheckRhs(3,4);
  CheckLhs(1,1);
  if ( IsBMatObj(stack,rhs)) 
    return int_bmatrix_setrc(stack,rhs,opt,lhs);
  else if ( IsSMatObj(stack,rhs)) 
    return int_smxsetrc(stack,rhs,opt,lhs);
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ( IsBMatObj(stack,2) ) 
    {
      /* Rows is boolean : use find(Rows) **/
      NspBMatrix *BRows ;
      if ((BRows = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
      if ((Rows =nsp_bmatrix_find(BRows)) == NULLMAT) return RET_BUG;
    }
  else
    {
      /* Rows is a real matrix : make a copy if Rows == A */
      if ((Rows = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
    }
  if ( rhs == 4 )
    {
      /* Cols is boolean : use find(Cols) **/
      if ( IsBMatObj(stack,3)  ) 
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
  if ((B = GetMpMat(stack,rhs)) == NULLMAXPMAT ) return RET_BUG;
  if ( B == A ) 
    { if ((B = GetMpMatCopy(stack,rhs)) == NULLMAXPMAT ) return RET_BUG;}
  if ( rhs == 3 ) 
    { if ( nsp_mpmatrix_set_rows( A, Rows,B) == FAIL) return RET_BUG; }
  else 
    { if ( nsp_mpmatrix_set_submatrix( A, Rows,Cols,B) == FAIL )  return RET_BUG;} 
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 * Res=MatDeletecols(A,Cols)
 *     Cols unchanged  ( restored at end of function if necessary)
 * WARNING : A must be changed by this routine
 * =======
 */	

/* generic interface for elts, rows and columns deletion **/

typedef int (*mpdelf) (NspMaxpMatrix *M,NspMatrix *Elts);

static int int_mpdeleteelts_gen(Stack stack, int rhs, int opt, int lhs, mpdelf F)
{
  NspMaxpMatrix *A;
  NspMatrix *Elts;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ( IsBMatObj(stack,2)  ) 
    {
      /* Elts is boolean : use find(Elts) **/
      NspBMatrix *BElts;
      if ((BElts = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
      if ((Elts =nsp_bmatrix_find(BElts)) == NULLMAT) return RET_BUG;
    }
  else
    {
      int flag;
      if ((Elts = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
      /* we must get a copy of Elts if A== Elts or if Elts need to be sorted **/
      flag = mat_is_increasing(Elts);
      if ( flag == FAIL) nsp_mat_sort(Elts,1,"g","i");
    }
  if ( (*F)( A, Elts) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos =1;
  return 1;
}

int int_mpdeletecols(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpdeleteelts_gen(stack,rhs,opt,lhs,nsp_mpmatrix_delete_columns);
}

/*
 * Res=MatDeleterows(A,Rows)
 *     Rows unchanged  ( restored at end of function if necessary)
 * WARNING : A must be changed by this routine
 */	

int int_mpdeleterows(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpdeleteelts_gen(stack,rhs,opt,lhs,nsp_mpmatrix_delete_rows);
}

/*
 * Res=MatDeleteelts(A,Elts)
 *     Elts unchanged  ( restored at end of function if necessary)
 * WARNING : A must be changed by this routine
 */	

int int_mpdeleteelts(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpdeleteelts_gen(stack,rhs,opt,lhs,nsp_mpmatrix_delete_elements);
}

/*
 * Res=nsp_mpmatrix_extract(Rows,Cols,A)
 * A unchanged, Rows and Cols are unchanged 
 * if Rows and Cols are to be kept they are restored at end of function 
 * WARNING note that on the stack we have Rows,Cols,A 
 */	

int int_mpextract(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A,*Res;
  NspMatrix *Rows,*Cols;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((Rows = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  if ((Cols = GetRealMat(stack,3)) == NULLMAT) return RET_BUG;
  Res =nsp_mpmatrix_extract( A, Rows,Cols);
  if ( Res == NULLMAXPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Res=nsp_mpmatrix_extract_elements(Elts,A)
 * A unchanged, Elts
 */	

/* generic function for elts extraction */

typedef NspMaxpMatrix *(*extrf) (const NspMaxpMatrix *M,const NspMatrix *Elts);

static int int_mpextractelts_gen(Stack stack, int rhs, int opt, int lhs, extrf F)
{
  NspMaxpMatrix *A,*Res;
  NspMatrix *Elts;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;

  if ( IsBMatObj(stack,2)  ) 
    {
      /* Elts is boolean : use find(Elts) **/
      NspBMatrix *BElts;
      if ((BElts = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
      if ((Elts =nsp_bmatrix_find(BElts)) == NULLMAT) return RET_BUG;
    }
  else
    {
      /* Elts is a real matrix  **/
      if ((Elts = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
    }

  if ((Res = (*F)( A, Elts)) == NULLMAXPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_mpextractelts(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpextractelts_gen(stack,rhs,opt,lhs,nsp_mpmatrix_extract_elements);
}

/*
 * columns extraction  Cols A --> A(Cols)
 */	

int int_mpextractcols(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpextractelts_gen(stack,rhs,opt,lhs,nsp_mpmatrix_extract_columns);
}

/*
 * rows extraction 					   
 */	

int int_mpextractrows(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpextractelts_gen(stack,rhs,opt,lhs,nsp_mpmatrix_extract_rows);
}

/*
 * Scilab diag function 
 */

int int_mpdiag(Stack stack, int rhs, int opt, int lhs)
{
  integer k1=0;
  NspMaxpMatrix *A,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ( rhs == 2) 
    { if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;}
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ( A->m == 1 || A->n == 1) 
    Res =nsp_mpmatrix_create_diag(A,k1);
  else
    Res =nsp_mpmatrix_extract_diag( A,k1);
  if ( Res == NULLMAXPMAT)  return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}


/*
 * Returns the kthe diag of a Matrix 
 */

int int_mpdiage(Stack stack, int rhs, int opt, int lhs)
{
  integer k1=0;
  NspMaxpMatrix *A,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ( rhs == 2) 
    { if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;}
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  Res =nsp_mpmatrix_extract_diag( A,k1);
  if ( Res == NULLMAXPMAT)  return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Set the kth Diag of A to Diag 
 *  A is enlarged & comlexified if necessary 
 *  int nsp_mpmatrix_create_diag(A,Diag,k)
 * WARNING : A is not copied we want this routine to change A
 */

int int_mpdiagset(Stack stack, int rhs, int opt, int lhs)
{
  integer k1;
  NspMaxpMatrix *A,*Diag;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((Diag = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  if ( GetScalarInt(stack,3,&k1) == FAIL) return RET_BUG;
  if ( nsp_mpmatrix_set_diag( A, Diag,k1) != OK) return RET_BUG;
  NSP_OBJECT(A)->ret_pos =1;
  return 1;
}

/*
 *  Creates a Matrix with kth diag set to Diag 
 */

int int_mpdiagcre(Stack stack, int rhs, int opt, int lhs)
{
  integer k1=0;
  NspMaxpMatrix *Diag,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((Diag = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (Res =nsp_mpmatrix_create_diag(Diag,k1)) == NULLMAXPMAT ) return RET_BUG ;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Matresize : changes Matrix dimensions
 *             m,n are changed and the arrays enlarged or schrinked 
 * 
 */

int int_mpresize(Stack stack, int rhs, int opt, int lhs)
{
  integer m1,n1;
  NspMaxpMatrix  *HMat;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;      
  if ( nsp_mpmatrix_resize(HMat,m1,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos =1;
  return 1;
}

/*
 * A'
 */

int int_mpquote(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix  *A,*B;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetMpMat(stack,1))== NULLMAXPMAT) return RET_BUG;
  if ((B=nsp_mpmatrix_transpose(A)) ==  NULLMAXPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)B);
  return 1;
}

/*
 * _
 * A'
 */

int int_mpdquote(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix  *A,*B;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetMpMat(stack,1))== NULLMAXPMAT) return RET_BUG;
  if ((B=nsp_mpmatrix_transpose(A)) ==  NULLMAXPMAT) return RET_BUG;
  nsp_mat_conj((NspMatrix *) B);
  MoveObj(stack,1,(NspObject *)B);
  return 1;
}

/*
 * Delete the Matrix Mat 
 */

int int_mpdestroy(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(0,0);
  if ((HMat = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  nsp_object_destroy(&NthObj(1));
  return 0;
}

/*
 * Matinfo : display info on Matrix Mat 
 */

int int_mpinfo(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat; 
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  nsp_mpmatrix_info(HMat,0);
  return 0;
}

/*
 * nsp_mpmatrix_latex_print: writes Mat Objet on fd in tex language
 */

int int_mp2latexmat(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  nsp_mpmatrix_latex_print(HMat);
  return 0;
}

/*
 *  Mat2LaTeXTab: writes Mat Objet on fd in TeX language
 */

int int_mp2latextab(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;    
  nsp_mpmatrix_latex_tab_print(HMat);
  return 0;
}

/*
 *  A=op(A) 
 */

typedef int (*M11) (NspMatrix *A);

/* generic function for ones,rand,eyes */
 
static int int_mp_gen11(Stack stack, int rhs, int opt, int lhs, M11 F)
{
  NspMaxpMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat=GetMpMat(stack,1))== NULLMAXPMAT) return RET_BUG;
  if ( HMat->mn == 0) return 1;
  if ((HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  if (( (*F)((NspMatrix *) HMat)) < 0) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

typedef void (*VM11) (NspMatrix *A); 

static int int_mp_genv11(Stack stack, int rhs, int opt, int lhs, VM11 F)
{
  NspMaxpMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat=GetMpMat(stack,1))== NULLMAXPMAT) return RET_BUG;
  if ( HMat->mn == 0) return 1;
  if ((HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  (*F)((NspMatrix *) HMat);
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}


/*
 * A=Abs(A), absolue value or module of each element 
 */

int int_mpabs(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_abs);
}

/*
 * A=Erf(A)
 */

int int_mperf(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_erf);
}

/*
 * A=Erfc(A),  * A is changed 
 */

int int_mperfc(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_erfc);
}

/*
 * A=Arg(A),  * A is changed 
 */

int int_mparg(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_arg);
}

/*
 *nsp_mat_cos: A=Cos(A)
 * A is changed  
 * return 0 if error 
 */

int int_mpcos(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_genv11(stack,rhs,opt,lhs,nsp_mat_cos);
}

/*
 *nsp_mat_cosh: A=Cosh(A)
 * A is changed  
 * return 0 if error 
 */

int int_mpcosh(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_genv11(stack,rhs,opt,lhs,nsp_mat_cosh);
}

/*
 * MatExpl : Exponentiation terme a term 
 * A is changed 
 */

int int_mpexpel(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_genv11(stack,rhs,opt,lhs,nsp_mat_expel);
}

/*
 * MatLog : A=LogEl(A) 
 */

int int_mplogel(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_logel);
}


/*
 *nsp_mat_sin: A=Sin(A)
 * A is changed  
 * return 0 if error 
 */

int int_mpsin(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_genv11(stack,rhs,opt,lhs,nsp_mat_sin);
}

/*
 *nsp_mat_sinh: A=Sinh(A)
 * A is changed  
 * return 0 if error 
 */


int int_mpsinh(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_genv11(stack,rhs,opt,lhs,nsp_mat_sinh);
}


/*
 *nsp_mat_sqrtel: A=SqrtEl(A)  term to term square root
 * A is changed  
 * return 0 if error 
 */

int int_mpsqrtel(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_sqrtel);
}

/*
 *nsp_mat_acos: A=Acos(A)
 * A is changed  
 * return 0 if error 
 */

int int_mpacos(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_acos);
}

/*
 *nsp_mat_acosh: A=Acosh(A)
 * A is changed  
 * return 0 if error 
 */

int int_mpacosh(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_acosh);
}

/*
 *nsp_mat_asin: A=Asin(A)
 * A is changed  
 * return 0 if error 
 */

int int_mpasin(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_asin);
}

/*
 *nsp_mat_asinh: A=Asinh(A)
 * A is changed  
 * return 0 if error 
 */


int int_mpasinh(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_asinh);
}

/*
 * MatATan : res= arctang(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

int int_mpatan(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_atan);
}

/*
 * Atan2(A,B), 
 */

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

int int_mpatan2(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A,*B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A=GetRealMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  if ( A->mn == 0) return 1;
  if ((B=GetRealMpMat(stack,2))== NULLMAXPMAT) return RET_BUG;
  CheckSameDims(stack.fname,1,2,A,B);
  if (nsp_mat_atan2((NspMatrix *) A,(NspMatrix *)B) == FAIL ) return RET_BUG;
  return 1;
}

/*
 * MatArcTangH : res= atanh(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

int int_mpatanh(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_atanh);
}

/*
 *nsp_mat_ceil: A=Ceil(A)
 * A is changed  
 */

int int_mpceil(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_genv11(stack,rhs,opt,lhs,nsp_mat_ceil);
}

/*
 *nsp_mat_int: A=Int(A)
 * A is changed  
 */

int int_mpint(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_genv11(stack,rhs,opt,lhs,nsp_mat_int);
}

/*
 *nsp_mat_floor: A=Floor(A)
 * A is changed  
 */
 
int int_mpfloor(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_genv11(stack,rhs,opt,lhs,nsp_mat_floor);
}

/*
 *nsp_mat_round: A=Round(A)
 * A is changed  
 */
 
int int_mpround(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_genv11(stack,rhs,opt,lhs,nsp_mat_round);
}

/*
 *nsp_mat_sign: A=Sign(A)
 * A is changed  
 * return 0 if error 
 */
 
int int_mpsign(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_sign);
}

/*
 *nsp_mat_tan: A=Tan(A)
 * A is changed  
 * return 0 if error 
 */
 
int int_mptan(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_tan);
}

/*
 *nsp_mat_tanh: A=Tanh(A)
 * A is changed  
 * return 0 if error 
 */
 
int int_mptanh(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_tanh);
}

/*
 *nsp_mat_minus: A=-(A)
 * A is changed  
 * return 0 if error 
 */
 
int int_mpminus(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen11(stack,rhs,opt,lhs,nsp_mat_minus);
}


/*
 * A=Polar(A,B),  * A is changed 
 */


int int_mppolar(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A,*B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  if ( A->mn == 0) return 1;
  if ((B=GetMpMat(stack,2))== NULLMAXPMAT) return RET_BUG;
  if ( SameDim(A,B) ) 
    {
      if (nsp_mat_polar((NspMatrix *)A,(NspMatrix *)B) < 0 ) return RET_BUG;
    }
  else 
    {
      Scierror("Error: %s Mat1 & Mat2 don't have same size \n",stack.fname);
      return RET_BUG;
    }
  return 1;
}

/*
 * A=iand(A,B)
 */

int int_mpiand(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A,*B;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((A=GetRealMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  if ( A->mn == 0) return 1;
  if ( rhs == 2) 
    {
      if ((B=GetRealMpMat(stack,2))== NULLMAXPMAT) return RET_BUG;
      if ( SameDim(A,B) ) 
	{
	  if (nsp_mat_iand((NspMatrix *)A,(NspMatrix *)B) == FAIL ) return RET_BUG;
	}
      else 
	{
	  Scierror("Error: %s Mat1 & Mat2 don't have same size \n",stack.fname);
	  return RET_BUG;
	}
    }
  else 
    {
      unsigned res;
      if (nsp_mat_iandu((NspMatrix *)A,&res)  == FAIL )  return RET_BUG;       
      if ( nsp_mpmatrix_resize(A,1,1) == FAIL) return(FAIL);
      A->R[0] = res;
    }
  return 1;
}

/*
 * A= ior(A,B)
 */

int int_mpior(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A,*B;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((A=GetRealMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  if ( A->mn == 0) return 1;
  if ( rhs == 2) 
    {
      if ((B=GetRealMpMat(stack,2))== NULLMAXPMAT) return RET_BUG;
      if ( SameDim(A,B) ) 
	{
	  if (nsp_mat_ior((NspMatrix *)A,(NspMatrix *)B) == FAIL ) return RET_BUG;
	}
      else 
	{
	  Scierror("Error: %s Mat1 & Mat2 don't have same size \n",stack.fname);
	  return RET_BUG;
	}
    }
  else 
    {
      unsigned res;
      if (nsp_mat_ioru((NspMatrix *)A,&res)  == FAIL )  return RET_BUG;       
      if ( nsp_mpmatrix_resize(A,1,1) == FAIL) return(FAIL);
      A->R[0] = res;
    }
  return 1;
}

/*
 *nsp_mat_conj: A=real(A)-i*Imag(A)
 * A is changed  if imaginary not changed if real 
 */

int int_mpconj(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat=GetMpMat(stack,1))== NULLMAXPMAT) return RET_BUG;
  if ( HMat->mn == 0 || HMat->rc_type == 'r' ) 
    { 
      NSP_OBJECT(HMat)->ret_pos = 1;
      return 1;
    }
  if ((HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  nsp_mat_conj((NspMatrix *)HMat);
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}


/*
 *nsp_mat_modulo: A=Modulo(A) remainder in integer division 
 * A is changed  
 */

int int_mpmodulo(Stack stack, int rhs, int opt, int lhs)
{
  int n;
  NspMaxpMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&n) == FAIL) return RET_BUG;
  nsp_mat_modulo((NspMatrix *)HMat,n);
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 * Mat : A= Idiv(A,b) quotient in integer division
 * A is changed   A(i)=A(i)/b 
 */

int int_mpidiv(Stack stack, int rhs, int opt, int lhs)
{
  int n;
  NspMaxpMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,2);
  if ((HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&n) == FAIL) return RET_BUG;
  if ( HMat->mn != 0 )nsp_mat_idiv((NspMatrix *)HMat,n);
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}



/*
 * A generic function fo  A op B with 
 * special case for [] and A or B scalar 
 *  [] op A ---> F4(A)  (only usefull for []-A )
 *  A op [] ---> A 
 *  A op scalar --->  F1(A,scalar)
 *  A op B      --->  F2(A,B) 
 *  scalar op A --->  F3(A,scalar) 
 */

typedef int (*MPM) (NspMatrix *,NspMatrix*);

int MpMatNoOp(NspMatrix *A)
{
  return OK;
}

static int int_mp_mopscal(Stack stack, int rhs, int opt, int lhs, MPM F1, MPM F2, MPM F3, M11 F4, int flag)
{
  NspMaxpMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMpMatCopy(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ( HMat1->mn == 0) 
    {
      if ( flag == 1) 
	{
	  /* flag == 1 ==> [] op A  returns [] **/
	  NSP_OBJECT(HMat1)->ret_pos = 1;
	  return 1;
	}
      else
	{
	  /* flag == 1 ==> [] op A  returns F4(A) **/
	  if ( F4 != MpMatNoOp)
	    {
	      if ((HMat2 = GetMpMatCopy(stack,2)) == NULLMAXPMAT) 
		return RET_BUG;
	      if ( (*F4)((NspMatrix *) HMat2)== FAIL)  return RET_BUG;
	      NSP_OBJECT(HMat2)->ret_pos = 1;
	    }
	  else 
	    {
	      if ((HMat2 = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
	      NSP_OBJECT(HMat2)->ret_pos = 1;
	    }
	  return 1;
	}
    }
  if ((HMat2 = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  if ( HMat2->mn == 0) 
    {
      if ( flag == 1) 
	{
	  /* flag == 1 ==> A op [] returns [] **/
	  NSP_OBJECT(HMat2)->ret_pos = 1;
	  return 1;
	}
      else
	{
	  /* flag == 1 ==> A op [] returns A **/
	  NSP_OBJECT(HMat1)->ret_pos = 1;
	  return 1;
	}
    }
  if ( HMat2->mn == 1) 
    {
      if ( (*F1)((NspMatrix *) HMat1,(NspMatrix *) HMat2) != OK) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  else if ( HMat1->mn == 1 ) 
    {
      /* since Mat1 is scalar we store the result in Mat2 so we 
	must copy it **/
      if ((HMat2 = GetMpMatCopy(stack,2)) == NULLMAXPMAT) return RET_BUG;
      if ( (*F3)((NspMatrix *)HMat2,(NspMatrix *)HMat1) != OK) return RET_BUG;
      NSP_OBJECT(HMat2)->ret_pos = 1;
    }
  else 
    {
      if ( (*F2)((NspMatrix *)HMat1,(NspMatrix *)HMat2) != OK) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  return 1;
}

/*
 * term to term addition (i.e max )
 * with special cases Mat + [] and Mat + scalar 
 */

int int_mpdadd(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_mopscal (stack, rhs, opt, lhs,
			 nsp_mat_mult_scalar, nsp_mat_mult_el,
			 nsp_mat_mult_scalar, MpMatNoOp, 1);
}

/*
 * term to term substraction 
 * with special cases Mat - [] and Mat - scalar
 *  XXXXX Attention le cas F3 est faux scalar - Mat --> Mat -scalar  
 */

int int_mpdsub(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_mopscal(stack,rhs,opt,lhs,
			nsp_mat_sub_scalar,nsp_mat_dsub,nsp_mat_subs_calarm,nsp_mat_minus,0);
}

/*
 * A=nsp_mat_pow_el(A,B), A.^ B 
 * with special cases Mat.^[]  and Mat.^scalar
 *                    [].^Mat ans scalar.^Mat
 */

int int_mppowel(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_mopscal(stack,rhs,opt,lhs,
			nsp_mat_pow_scalar,nsp_mat_pow_el,nsp_mat_pow_scalarm,MpMatNoOp,1);
}

/*
 * A=DivEl(A,B),  A ./ B 
 */

int int_mpdivel(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_mopscal(stack,rhs,opt,lhs,
			nsp_mat_div_scalar,nsp_mat_div_el,nsp_mat_bdiv_scalar,MpMatNoOp,1);
}


/*
 * A=BackDivEl(A,B),  A .\ B 
 */

int int_mpbackdivel(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_mopscal(stack,rhs,opt,lhs,
			nsp_mat_bdiv_scalar,nsp_mat_bdiv_el,nsp_mat_div_scalar,MpMatNoOp,1);
}


/*
 * A=MultEl(A,B),  A .* B -> a + b in max plus 
 */

int int_mpmultel(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_mopscal(stack,rhs,opt,lhs,
			nsp_mat_add_scalar,nsp_mat_dadd,nsp_mat_add_scalar,MpMatNoOp,1);
}


/*
 * NspMaxpMatrix multiplication  Res= A*B  
 * with special cases Mat * [] and Mat * scalar
 * very similar to mopscal but MatMult returns a new matrix 
 */



static int int_mpmult(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat1,*HMat2,*HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      if ( HMat1 == HMat2 ) NthObj(2) = NULLOBJ;
      NSP_OBJECT (HMat1)->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  if ( HMat2->mn == 0) 
    {
      if ( HMat1 == HMat2 ) 
	{
	  NthObj(2) = NULLOBJ;
	  NSP_OBJECT (HMat1)->ret_pos = 1;
	}
      else 
	{
	  /* flag == 1 ==> A op [] returns [] * */
	  NSP_OBJECT (HMat2)->ret_pos = 1;
	}
      return 1;
    }
  if ( HMat2->mn == 1) 
    {
      if ((HMat1 = GetMpMatCopy(stack,1)) == NULLMAXPMAT) return RET_BUG;
      if (nsp_mat_mult_scalar((NspMatrix *)HMat1,(NspMatrix *)HMat2) != OK) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  else if ( HMat1->mn == 1 ) 
    {
      /* since Mat1 is scalar we store the result in Mat2 so we 
       * must copy it 
       */
      if ((HMat2 = GetMpMatCopy(stack,2)) == NULLMAXPMAT) return RET_BUG;
      if (nsp_mat_mult_scalar((NspMatrix *)HMat2,(NspMatrix *)HMat1) != OK) return RET_BUG;
      NSP_OBJECT(HMat2)->ret_pos = 1;
    }
  else 
    {
      NspMatrix *M;
      if ((M=nsp_mat_maxplus_mult((NspMatrix *)HMat1,(NspMatrix *)HMat2)) == NULLMAT) return RET_BUG;
      if ((HMat3 = nsp_mp_matrix_from_m(NVOID,M))  == NULLMAXPMAT) return RET_BUG;
      nsp_matrix_destroy(M);
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  return 1;
}


/*
 * A / B 
 * just implemented for scalars XXXXX 
 * result stored in A 
 */

static int int_mpdiv(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((HMat2 = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  if ( HMat2->mn <= 1 )
    {
      return int_mp_mopscal(stack,rhs,opt,lhs,
			nsp_mat_div_scalar,nsp_mat_div_el,nsp_mat_bdiv_scalar,MpMatNoOp,1);
    }
  else 
    {
      Scierror("%s: / not implemented for non 1x1 matrices\n",stack.fname);
      return RET_BUG;
    }
  return 1;
}


/*
 * returns in a Matrix the indices for which the Matrix is true 
 */

int int_mpfind(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A;
  NspMatrix *Rc,*Rr;
  CheckRhs(1,1);
  CheckLhs(1,2);
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT)  return RET_BUG;
  if (nsp_mat_find((NspMatrix *) A,Max(lhs,1),&Rr,&Rc) == FAIL) return RET_BUG;
  MoveObj(stack,1, (NspObject *)Rr);
  if ( lhs == 2 )
    {
      NthObj(2) = (NspObject *) Rc;
      NSP_OBJECT(NthObj(2))->ret_pos = 2;
      return 2;
    }
  return 1;
}


/*
 * isinf 
 */

int int_mp_isinf(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A; 
  NspBMatrix *B;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT)  return RET_BUG;
  if ((B=nsp_mat_isinf((NspMatrix *) A)) == NULLBMAT)  return RET_BUG;
  MoveObj(stack,1, (NspObject *)B);
  return 1;
}

/*
 * isnan 
 */

int int_mp_isnan(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A; 
  NspBMatrix *B;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT)  return RET_BUG;
  if ((B=nsp_mat_isnan((NspMatrix *)A)) == NULLBMAT)  return RET_BUG;
  MoveObj(stack,1, (NspObject *)B);
  return 1;
}

/*
 * finite 
 */

int int_mp_finite(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A; 
  NspBMatrix *B;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT)  return RET_BUG;
  if ((B= nsp_mat_finite((NspMatrix *)A)) == NULLBMAT)  return RET_BUG;
  MoveObj(stack,1, (NspObject *)B);
  return 1;
}

/*
 * The Interface for basic matrices operation 
 */

static OpTab Matrix_func[]={

  {"m2mp",int_m2mp},
  {"maxplus",int_m2mp},
  {"resize2vect_mp",int_mpmat2vect},
  {"extractcols_mp",int_mpextractcols},
  {"extractrows_mp",int_mpextractrows},
  {"extract_mp",int_mpextract},
  {"extractelts_mp",int_mpextractelts},
  {"deletecols_mp_mp", int_mpdeletecols},
  {"deleterows_mp_mp", int_mpdeleterows},
  {"deleteelts_mp_mp", int_mpdeleteelts},
  {"deletecols_m_b", int_mpdeletecols},
  {"deleterows_m_b", int_mpdeleterows},
  {"deleteelts_m_b", int_mpdeleteelts},
  {"setrowscols_mp",int_mpsetrc},
  {"impl",int_mpimpl},
  {"addcols_mp_mp" ,  int_mpaddcols },
  {"addrows_mp_mp" ,  int_mpaddrows },
  {"clean_mp" ,  int_mpclean },
  {"complexify_mp" ,  int_mpcomplexify },
  {"concatd_mp_mp" ,  int_mpconcatd },
  {"concatr_mp_mp" ,  int_mpconcatr },
  {"concatr_b_mp" ,  int_mpconcatr_mb },
  {"concatr_m_b" ,  int_mpconcatr_mb },
  {"copy_mp" ,  int_mpcopy },
  {"mpcreate_m_m" ,  int_mpcreate },
  {"dadd_mp_mp" ,   int_mpdadd },
  {"destroy_mp" ,  int_mpdestroy },
  {"concatdiag_mp_mp" ,  int_mpconcatdiag },
  {"diag_mp",int_mpdiag},
  {"diag_mp_mp",int_mpdiag},
  {"diagcre_mp" ,  int_mpdiagcre },
  {"diagcre_mp_mp" ,  int_mpdiagcre },
  {"diage_mp" ,  int_mpdiage },
  {"diage_mp_mp" ,  int_mpdiage },
  {"diagset_mp" ,  int_mpdiagset },
  {"eq_mp_mp" ,  int_mpeq },
  {"eye_mp_mp" ,  int_mpeye },
  {"ones_mp_mp" ,  int_mpones},
  {"zeros_mp_mp", int_mpzeros },
  {"feq_mp_mp" ,  int_mpfeq },
  {"fge_mp_mp" ,  int_mpfge },
  {"fgt_mp_mp" ,  int_mpfgt },
  {"fle_mp_mp" ,  int_mpfle },
  {"flt_mp_mp" ,  int_mpflt },
  {"fneq_mp_mp" ,  int_mpfneq },
  {"ge_mp_mp" ,  int_mpge },
  {"gt_mp_mp" ,  int_mpgt },
  {"imag_mp" ,  int_mpimagpart },
  {"info_mp" ,  int_mpinfo },
  {"dstd_mp_mp" ,  int_mpkron }, /* operator:  .*. */
  {"latexmat_mp" ,  int_mp2latexmat },
  {"latextab_mp" ,  int_mp2latextab },
  {"le_mp_mp" ,  int_mple },
  {"lt_mp_mp" ,  int_mplt },
  {"max_mp" ,  int_mpmaxi}, 
  {"max" ,  int_mpmaxi },
  {"min_mp" ,  int_mpmini },
  {"min_mp" ,  int_mpmini },
  {"sum_mp_s" ,  int_mpsum },
  {"sum_mp" ,  int_mpsum },
  {"cumsum_mp_s" ,  int_mpcusum },
  {"cumsum_mp" ,  int_mpcusum },
  {"prod_mp_s" ,  int_mpprod },
  {"prod_mp" ,  int_mpprod },
  {"cumprod_mp_s" ,  int_mpcuprod },
  {"cumprod_mp" ,  int_mpcuprod },
  {"ne_mp_mp" ,  int_mpneq },
  {"real_mp" ,  int_mprealpart },
  {"redim_mp" ,  int_mpredim },
  {"resize_mp_mp" ,  int_mpresize },
  {"seti_mp_mp" ,  int_mpseti },
  {"setr_mp_mp" ,  int_mpsetr },
  {"setrc_mp_mp" ,  int_mpsetrc },
  {"sort_mp" ,  int_mpsort },
  {"gsort_mp" ,  int_mpsort },
  {"tril_mp" ,  int_mptril },
  {"triu_mp" ,  int_mptriu },
  {"matrix_mp",int_mpmatrix},
  {"quote_mp",int_mpquote},
  {"dprim_mp",int_mpdquote},

  {"abs_mp",int_mpabs},
  {"erf_mp",int_mperf},
  {"erfc_mp",int_mperfc},
  {"arg_mp",int_mparg},
  {"cos_mp",int_mpcos},
  {"cosh_mp",int_mpcosh},
  {"exp_mp",int_mpexpel},
  {"log_mp",int_mplogel},
  {"sin_mp",int_mpsin},
  {"sinh_mp",int_mpsinh},
  {"sqrt_mp",int_mpsqrtel},
  {"acos_mp",int_mpacos},
  {"acosh_mp",int_mpacosh},
  {"asin_mp",int_mpasin},
  {"asinh_mp",int_mpasinh},
  {"atan_mp",int_mpatan},
  {"atan_mp_mp",int_mpatan2},
  {"atanh_mp",int_mpatanh},
  {"ceil_mp",int_mpceil},
  {"modulo_mp_mp",int_mpmodulo},
  {"idiv_mp_mp",int_mpidiv},
  {"int_mp",int_mpint},
  {"floor_mp",int_mpfloor},
  {"round_mp",int_mpround},
  {"sign_mp",int_mpsign},
  {"tan_mp",int_mptan},
  {"tanh_mp",int_mptanh},
  {"polar",int_mppolar},
  {"iand_mp_mp",int_mpiand},
  {"ior_mp_mp",int_mpior},
  {"conj_mp",int_mpconj},
  {"dh",int_mppowel},
  {"dsl",int_mpdivel},
  {"dbs",int_mpbackdivel},
  {"dst_mp_mp",int_mpmultel},
  {"plus_mp_mp",   int_mpdadd},
  {"minus_mp_mp",   int_mpdsub},
  {"minus_mp", int_mpminus},	
  {"mult_mp_mp" ,  int_mpmult},
  {"div_mp_mp" ,  int_mpdiv},
  {"find_mp", int_mpfind},
  {(char *) 0, NULL}
};


int MpMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Matrix_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) **/

void MpMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Matrix_func[i].name;
  *f = Matrix_func[i].fonc;
}








