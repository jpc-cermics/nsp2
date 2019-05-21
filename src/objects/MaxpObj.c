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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define  MaxpMatrix_Private 
#include <nsp/object.h> 
#include <nsp/matrix.h> 
#include <nsp/mpmatrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/smatrix.h> 
#include <nsp/imatrix.h> 
#include <nsp/matint.h> 
#include <nsp/hobj.h> 
#include <nsp/file.h> 
#include <nsp/type.h> 

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"
#include "nsp/smatrix-in.h"
#include "nsp/matutil.h"
#include "nsp/matint.h"
#include "nsp/gsort-p.h"
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
  NspTypeMatint *mati;/* interface */
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
  type->gtk_methods = FALSE;
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
  top->full_copy  =  (copy_func *) nsp_mpmatrix_copy;                   
  /* specific methods for matrix */
      
  type->init = (init_func *) init_mpmatrix;
      
  /* 
   * Matrix interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */


  /*
   * Matrix implements Matint the matrix interface 
   * which is common to object that behaves like matrices.
   */

  mati = new_type_matint(T_DERIVED);
  mati->methods = matint_get_methods; 
  /* mati->redim = (matint_redim *) nsp_mpmatrix_redim; use default value */
  mati->resize = (matint_resize  *) nsp_mpmatrix_resize;
  mati->free_elt = (matint_free_elt *) 0; /* nothing to do */
  mati->elt_size = (matint_elt_size *) nsp_matrix_elt_size ;/* same as for matrix */
  mati->clone = (matint_clone *) nsp_mpmatrix_clone ;
  mati->copy_elt = (matint_copy_elt *) 0; /* nothing to do */
  mati->enlarge = (matint_enlarge *) nsp_mpmatrix_enlarge;
  mati->canonic =  nsp_matint_canonic;
  mati->copy_ind = nsp_matint_basic_copy_mat;
  type->interface = (NspTypeBase *) mati;

  
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

char *mpmatrix_type_short_string(NspObject *v)
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
  if ( ! ( ((NspMaxpMatrix *) A)->m == ((NspMaxpMatrix *) B)->m 
	   && ((NspMaxpMatrix *) A)->n == ((NspMaxpMatrix *) B)->n)) return FALSE;
  rep = nsp_mat_fullcomp ((NspMatrix *) A, (NspMatrix *) B, "==", &err);
  if (err == TRUE)
    return FALSE;
  return rep;
}

int mpmatrix_neq(NspObject *A,NspObject *B)
{
  return ( mpmatrix_eq(A,B) == TRUE ) ? FALSE : TRUE ;
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
	if (  M->C[i].r == 0.0 && M->C[i].i == 0.0 ) 
	  return FALSE;
      }
  return TRUE;
}

/*
 * Save a Matrix in a file stream 
 */

static int mpmatrix_xdr_save(XDR *xdrs, NspMaxpMatrix *M)
{
#if 1 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_mpmatrix)) == FAIL) return FAIL;
#else
  if (nsp_xdr_save_i(xdrs, M->type->id) == FAIL)    return FAIL;
#endif 
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->m) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->n) == FAIL) return FAIL;
  if (nsp_xdr_save_c(xdrs,M->rc_type) == FAIL) return FAIL;
  if ( M->rc_type == 'r') 
    { if (nsp_xdr_save_array_d(xdrs,M->R,M->mn) == FAIL) return FAIL; }
  else
    { if (nsp_xdr_save_array_d(xdrs,(double *) M->C,2*M->mn) == FAIL) return FAIL; }
  return OK;
}

/*
 * Load a Matrix from a file stream 
 **/

static NspMaxpMatrix *mpmatrix_xdr_load(XDR *xdrs)
{
  char c;
  int m,n;
  NspMaxpMatrix *M;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLMAXPMAT;
  if (nsp_xdr_load_i(xdrs,&m) == FAIL) return NULLMAXPMAT;
  if (nsp_xdr_load_i(xdrs,&n) == FAIL) return NULLMAXPMAT;
  if (nsp_xdr_load_c(xdrs,&c) == FAIL) return NULLMAXPMAT;
  if (( M= nsp_mpmatrix_create(name,c,m,n)) == NULLMAXPMAT ) return NULLMAXPMAT;
  if ( M->rc_type == 'r') 
    {
      if (nsp_xdr_load_array_d(xdrs,M->R,M->mn) == FAIL) return NULLMAXPMAT;
    }
  else
    {
      if (nsp_xdr_load_array_d(xdrs,(double *)M->C,2*M->mn) == FAIL) return NULLMAXPMAT;
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
  HOBJ_GET_OBJECT(O,NULL);
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
  if ( M->rc_type == 'c' ) 
    {
      Scierror("\t%s", ArgPosition(i));
      ArgName(stack,i);
      Scierror(" of function %s should not be complex\n",NspFname(stack));
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
  if ( M->rc_type == 'c' ) 
    {
      Scierror("\t%s", ArgPosition(i));
      ArgName(stack,i);
      Scierror(" of function %s should not be complex\n",NspFname(stack));
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

int *MaxpMatd2i(NspMaxpMatrix *A, int *imin, int *imax)
{
  int i;
  int *Iloc;
  Iloc = (int *) A->R;
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
  int inc = -1,i;
  if ( A != NULLMAXPMAT && A->rc_type == 'r' ) 
    switch ( A->convert ) 
      {
	case 'u':
	  A->R =nsp_alloc_doubles(A->mn);
	  if ( A->mn != 0 )
	    {
	      A->R[0] = A->impl[0];
	      for ( i = 1 ; i < A->mn ; i++ )
		A->R[i] = A->R[i-1] + (double) A->impl[1];
	    }
	  A->convert = 'd';
	  break;
	case 'i':
	  nsp_int2double (&A->mn, (int *) A->R, &inc, A->R, &inc);
	  A->convert = 'd';
	  break;
	case 'f':
	  nsp_float2double (&A->mn, (float *) A->R, &inc, A->R, &inc);
	  A->convert = 'd';
	  break;
      }
  return A;
}

/*
 * Utility function : Converts to int 
 */

NspMaxpMatrix *MpMat2int(NspMaxpMatrix *A)
{
  int i;
  if ( A != NULLMAXPMAT &&  A->convert !=  'i' ) 
    {
      if ( A->rc_type == 'r' )
	{
	  switch ( A->convert) 
	    {
	    case 'd':  nsp_double2int (&A->mn, A->R, (int *) A->R); break;
	    case 'f':  nsp_float2int (&A->mn, (float *) A->R, (int *) A->R);break;
	    case 'u':  
	      A->R =nsp_alloc_doubles(A->mn);
	      if ( A->mn != 0 )
		{
		  A->I[0] = A->impl[0];
		  for ( i = 1 ; i < A->mn ; i++ )
		    A->I[i] = A->I[i-1] + A->impl[1];
		}
	      break;
	    }
	  A->convert = 'i';
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
  int inc=-1,i;
  if ( A!= NULLMAXPMAT &&  A->convert != 'f' ) 
    {
      if ( A->rc_type == 'r' )
	{
	  switch ( A->convert) 
	    {
	    case 'd':  nsp_double2float (&A->mn, A->R, (float *) A->R);break;
	    case 'i':  nsp_int2float (&A->mn, (int *) A->R, &inc, (float *) A->R, &inc);break;
	    case 'u':  
	      A->R =nsp_alloc_doubles(A->mn);
	      if ( A->mn != 0 )
		{
		  A->F[0] = A->impl[0];
		  for ( i = 1 ; i < A->mn ; i++ )
		    A->F[i] = A->F[i-1] + A->impl[1];
		}
	      break;
	    }
	  A->convert = 'f';
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

static int int_mpcreate(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
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

static int int_m2mp(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *B;
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ( IsMpMatObj(stack,1)) 
    {
      /* Note that the result will be boolean */
      NthObj (1)->ret_pos = 1;
      return 1;
    }
  if (( A = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if (( B = nsp_mpmatrix_copy((NspMaxpMatrix *) A)) == NULLMAXPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);   
  return 1;
}

/*
 * convert a max plus matrix to a  matrix 
 */

static int int_mp2m(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *B;
  NspMaxpMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if (( B = nsp_matrix_copy((NspMatrix *) A)) == NULLMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);   
  return 1;
}


/*
 * Change a matrix of Real type to Imaginary type 
 * The imag part is not initialized 
 */

static int int_mpcomplexify(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat;
  double d=0.00;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT ) return RET_BUG;
  if ( rhs == 2 ) 
    if (GetScalarDouble(stack,2,&d) == FAIL) return RET_BUG;
  if (nsp_mat_complexify((NspMatrix *) HMat,d) == FAIL ) { return RET_BUG;};
  return 1;  
}

/*
 * Returns: real(A) 
 */

static int int_mprealpart(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ( HMat->rc_type == 'c') 
    {
      if ((HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
    }
  if (nsp_mat_get_real((NspMatrix *) HMat) != OK  ) return RET_BUG;
  ((NspObject *) HMat)->ret_pos = 1;
  return 1;
}

/*
 * Returns: imag(A), the Imaginary part of Matrix A 
 */

static int int_mpimagpart(Stack stack, int rhs, int opt, int lhs)
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
 * Returns: a kroeneker product A.*.B 
 */

static int int_mpkron(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat1,*HMat2,*M;
  NspMatrix *HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((HMat2 = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  if ((HMat3 =nsp_mat_kron((NspMatrix *) HMat1,(NspMatrix *) HMat2)) == NULLMAT) return RET_BUG;
  /* from matrix to maxplus matrix by moving data */
  M = nsp_matrix_cast_to_mpmatrix(HMat3);
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}

/*
 * MatSort 
 * [A_sorted,Index]=sort(A, type,dir ) 
 *  type = "g"| "gs"| "gm"| "c"| "r"| "lr" | "lc" | "ldc"| "ldr"|"gb"|"gd"
 *  dir =  "i"| "d";
 */

static int int_mpmatrix_sort(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *M=NULL;
  NspObject *Index=NULLOBJ;
  const char *type_possible_choices[]={ "g", "gs", "gm", "c", "r", "lr" , "lc" , "ldc", "ldr","gb","gd", NULL };
  char *type=NULL;
  const char *dir_possible_choices[]={ "i", "d",  NULL };
  char *dir=NULL;
  const char *ind_type_possible_choices[]={ "double", "int",  NULL };
  char *ind_type=NULL;
  int iflag = FALSE;
  char direction = 'd', itype = 'd';
  int rep_type= sort_g, rep_dir, rep_ind_type;

  CheckRhs(1,4);
  CheckOptRhs(0,3)
  CheckLhs(0,2);

  if ((M=GetMpMatCopy(stack,1)) == NULLMAXPMAT ) return RET_BUG;

  if ( rhs > 1 )
    {
      if ( rhs - opt >= 2  && opt >= 1 )
	{
	  Scierror ("Error:\t don't use both usual and named optional arguments (in function %s)\n", NspFname(stack));
	  return RET_BUG;
	}

      if ( opt == 0 )
	{
	  if ((type = GetString(stack,2)) == NULLSTRING) return RET_BUG; 
	  if (rhs >= 3) 
	    {
	      if ((dir = GetString(stack,3)) == NULLSTRING) return RET_BUG; 
	      if ( rhs >= 4 )
		{
		  if ((ind_type = GetString(stack,4)) == NULLSTRING) return RET_BUG; 
		}
	    }  
	}
      else
	{
	  nsp_option opts[] ={{"type",string,NULLOBJ,-1},
			      {"dir",string,NULLOBJ,-1},
			      {"ind_type",string,NULLOBJ,-1},
			      { NULL,t_end,NULLOBJ,-1}};
	  if ( get_optional_args(stack, rhs, opt, opts, &type, &dir, &ind_type) == FAIL )
	    return RET_BUG;
	}

      /* verify optional arg*/
      if ( type != NULL )
	{
	  if ( (rep_type= is_string_in_array(type, type_possible_choices,1)) == -1 )
	    {
	      string_not_in_array(stack, type, type_possible_choices, "optional argument type");
	      return RET_BUG; 
	    }
	}
      if ( dir != NULL )
	{
	  if ( (rep_dir= is_string_in_array(dir, dir_possible_choices,1)) == -1 ) 
	    {
	      string_not_in_array(stack, dir, dir_possible_choices, "optional argument dir");
	      return RET_BUG;
	    } 
	  direction = dir_possible_choices[rep_dir][0];
	}
      if ( ind_type != NULL )
	{
	  if ( (rep_ind_type= is_string_in_array(ind_type, ind_type_possible_choices,1)) == -1 ) 
	    {
	      string_not_in_array(stack, ind_type, ind_type_possible_choices, "optional argument ind_type");
	      return RET_BUG;
	    } 
	  itype = ind_type_possible_choices[rep_ind_type][0];
	}
    }

  if ( lhs  == 2 || rep_type == sort_gs )  /* force index allocation for stable quick sort */
    {
      iflag = TRUE;
    }

  switch ( rep_type  )
    {
    case sort_g : 
    case sort_gs: 
    case sort_gm: 
    case sort_gb: 
    case sort_gd: 
      nsp_matrix_sort((NspMatrix *) M,&Index,iflag,direction,rep_type,itype);break;
      break;
    case sort_c:
      /* take care that c -> row */
      nsp_matrix_row_sort((NspMatrix *)M,&Index,iflag,direction,itype);break;
    case sort_r:
      nsp_matrix_column_sort((NspMatrix *)M,&Index,iflag,direction,itype);break;
    case sort_lr:
      nsp_matrix_lexical_row_sort((NspMatrix *)M,&Index,iflag,direction,'i',itype);break;
    case sort_lc:
      nsp_matrix_lexical_column_sort((NspMatrix *)M,&Index,iflag,direction,'i',itype);break;
    case sort_ldr:
      nsp_matrix_lexical_row_sort((NspMatrix *)M,&Index,iflag,direction,'d',itype);
      break;
    case sort_ldc:
      nsp_matrix_lexical_column_sort((NspMatrix *)M,&Index,iflag,direction,'d',itype);  break;
    }

  if ( iflag == TRUE && Index == NULL) return RET_BUG;
  NSP_OBJECT(M)->ret_pos = 1;

  if ( lhs == 2 ) 
    {
      if ( itype == 'd' ) /* back convert */
	Index = (NspObject *) Mat2double( (NspMatrix *) Index);
      MoveObj(stack,2,Index);
    }

  return Max(lhs,1);
} 


/*
 *nsp_mpmat_sum: sum=Sum(a[,b]) 
 * a is unchanged 
 */

typedef NspMatrix *(*SuPro) (NspMatrix *A,int dim);

static int getdimfromstring(char *str)
{
  switch(str[0])
    {
    default :
      Sciprintf("\nInvalid flag '%c' assuming flag='*'\n",str[0]);
    case 'f': case 'F': case '*':
      return 0;
      break;
    case 'r': case 'R':
      return 1;
      break ;
    case 'c': case 'C':
      return 2;
      break;
    }
}

static int int_mp_sum (Stack stack, int rhs, int opt, int lhs, SuPro F)
{
  char *str;
  int dim=0;
  NspMaxpMatrix *HMat,*Res; 
  NspMatrix *M;
  CheckRhs (1, 2);
  CheckLhs (1, 1);

  if ((HMat = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;

  if (rhs == 2)
    {
      if ( IsSMatObj(stack, 2) )
	{
	  if ((str = GetString (stack, 2)) == (char *) 0)
	    return RET_BUG;
	  dim = getdimfromstring(str);
	}
      else
	{
	  if ( GetScalarInt(stack, 2, &dim) == FAIL )
	    return RET_BUG;
	}
    }
      
  if ((M= (*F)((NspMatrix *) HMat,dim)) == NULLMAT ) return RET_BUG;
  Res= nsp_matrix_cast_to_mpmatrix(M);
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}

static int int_mpsum(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_mp_sum(stack,rhs,opt,lhs,nsp_mat_sum) );
}

/*
 * matprod : product of all elements of a in the tropical sense 
 * i.e it is a sum 
 * a is unchanged 
 */

/*
static int int_mpprod(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_mp_sum(stack,rhs,opt,lhs,nsp_mat_prod) );
}
*/

/*
 * matcusum : tropical cumulative sum of all elements of a
 * i.e a cumulative max 
 * a is unchanged 
 */

static NspMatrix *nsp_mat_cum_max(NspMatrix *A, int dim)
{
  return (NspMatrix *) nsp_mpmatrix_cum_max((NspMaxpMatrix *)A, dim);
}

static int int_mpcusum(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_mp_sum(stack,rhs,opt,lhs,nsp_mat_cum_max) );
}

/*
 * matcuprod : tropical cumulative prod of all elements of a
 * i.e a standard cumulative sum.
 * a is unchanged 
 */

static int int_mpcuprod(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_mp_sum(stack,rhs,opt,lhs,nsp_mat_cum_sum) );
}

/* mp_maxi: a generic interface for computing max or min in 
 * the usual sense. This interface is used for the tropical sum 
 * operation.
 */

typedef NspMatrix *(*MiMax) (NspMatrix *A,int,NspMatrix **Imax,int lhs);
typedef int (*MiMax1) (NspMatrix *A, NspMatrix *B, NspMatrix *Ind, int j,int flag);

static int int_mp_maxi(Stack stack, int rhs, int opt, int lhs, MiMax F, MiMax1 F1)
{
  int dim=0;
  NspMaxpMatrix *A,*M,*B; 
  NspMatrix *M1,*Imax;
  if ( rhs < 1) 
    { 
      Scierror("Error:\t Rhs must be >= 1 for function %s\n",NspFname(stack));
      return RET_BUG;
    }
  CheckLhs(1,2);
  if (rhs == 1 || (rhs - opt ) == 1 || ( rhs == 2 && IsSMatObj(stack,2)) )
    {
      /* maxi(A), or maxi(A,str) or maxi(A,dim=options) idem for mini */
      if ((A = GetRealMpMat(stack,1)) == NULLMAXPMAT) 
	return RET_BUG;
      if ( rhs == 2) 
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
	      Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n",
			NspFname(stack));
	      return RET_BUG;
	    }
	  if ( dim == -2 )  /* matlab compatibility flag */
	    dim = GiveMatlabDimFlag(A);
	}

      if (( M1= (*F)((NspMatrix *)A,dim,&Imax,lhs)) == NULLMAT ) return RET_BUG;
      if ( lhs == 2)
	{
	  MoveObj(stack,2,(NspObject *)Imax);
	}
      M= nsp_matrix_cast_to_mpmatrix(M1);
      MoveObj(stack,1,NSP_OBJECT(M));
    }
  else
    {
      NspMatrix *Ind=NULLMAT;
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


static int int_mpmaxi(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_mp_maxi(stack,rhs,opt,lhs,nsp_mat_maxi,nsp_mat_maxitt1) );
}


/*
 *nsp_mpmat_mini: Mini(A)
 * A is unchanged 
 * rs and ri are set to the result 
 */

static int int_mpmini(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_mp_maxi(stack,rhs,opt,lhs,nsp_mat_mini,nsp_mat_minitt1) );
}

/*
 *nsp_mpmat_triu: A=Triu(a)
 * A is changed  
 */

static int int_mptriu(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspMaxpMatrix *HMat;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ( rhs == 2) 
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  nsp_mpmatrix_triu( HMat,k1);
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 *nsp_mpmat_tril: A=Tril(A)
 * A is changed  
 */

static int int_mptril(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspMaxpMatrix  *HMat; 
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ( rhs == 2) 
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (HMat=GetMpMatCopy(stack,1))== NULLMAXPMAT) return RET_BUG;
  nsp_mpmatrix_tril( HMat,k1);
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
  int m1,n1;
  NspMaxpMatrix *HMat,*Res;
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
  Res = nsp_matrix_cast_to_mpmatrix(M);
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}

static int int_mpones_deprecated(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen(stack,rhs,opt,lhs,nsp_mat_ones) ;
}



/*
 *nsp_mat_eye: A=Eye(m,n)
 * A is created  m,n no
 */

static int int_mpeye(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen(stack,rhs,opt,lhs,nsp_mat_eye);
}

/*
 *nsp_mat_zeros: A=zeros(m,n)
 * A is created  m,n no
 */

static int int_mpzeros(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_gen(stack,rhs,opt,lhs,nsp_mat_zeros);
}

/*
 * setr(A,d) <=> real(A) = d, im(A) unchanged 
 */

static int int_mpsetr(Stack stack, int rhs, int opt, int lhs)
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

static int int_mpseti(Stack stack, int rhs, int opt, int lhs)
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

static int int_mpclean(Stack stack, int rhs, int opt, int lhs)
{
  double epsr=DBL_EPSILON;
  double epsa=DBL_EPSILON;
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
static int int_mplt(Stack stack, int rhs, int opt, int lhs)
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

static int int_mple(Stack stack, int rhs, int opt, int lhs)
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

static int int_mpneq(Stack stack, int rhs, int opt, int lhs)
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

static int int_mpeq(Stack stack, int rhs, int opt, int lhs)
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

static int int_mpgt(Stack stack, int rhs, int opt, int lhs)
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


static int int_mpge(Stack stack, int rhs, int opt, int lhs)
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
  if ( err == TRUE) 
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

static int int_mpflt(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpf_gen(stack,rhs,opt,lhs,"<");
}

static int int_mpfle(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpf_gen(stack,rhs,opt,lhs,"<=");
}

static int int_mpfneq(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpf_gen(stack,rhs,opt,lhs,"<>");
}

static int int_mpfeq(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpf_gen(stack,rhs,opt,lhs,"==");
}

static int int_mpfgt(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpf_gen(stack,rhs,opt,lhs,">");
}

static int int_mpfge(Stack stack, int rhs, int opt, int lhs)
{
  return int_mpf_gen(stack,rhs,opt,lhs,">=");
}


/*
 * NspMaxpMatrix : Restack a copy of matrix A
 * keeps m*n constant
 */
#if 0 
static int int_mpmatrix(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
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
	  Scierror("Error:\t second argument of function %s\n",NspFname(stack));
	  Scierror("\texpecting a vector of size 2\n");
	  return RET_BUG;
	}
      m1= (int) B->R[0];
      n1= (int) B->R[1];
    }
  if ( nsp_matint_redim(NSP_OBJECT(A),m1,n1) != OK) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}
#endif 



/*
 * Right and Down Concatenation in mixed operations with Mat and MaxpMat
 * The result should always be a maxp matrix 
 */

static int int_mxconcat_mixed_mmp (Stack stack, int rhs, int opt, int lhs,function F)
{
  NspMatrix *A;
  NspMaxpMatrix /* *Amp,*/ *B;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if (IsMatObj (stack, 1))
    {
      if ((A = GetMat(stack, 1)) == NULLMAT) return RET_BUG;
      if ( A->mn == 0)
	{
	  /* [[],B] --> B : Note that result is maxp  */
	  NSP_OBJECT (NthObj (2))->ret_pos = 1;
	  return 1;
	}
      else
	{
	  /* Copy A if necessary  */
	  if ((A = GetMatCopy(stack, 1)) == NULLMAT) return RET_BUG;
	  /* cast A to Maxp */
	  /* Amp =*/ nsp_matrix_cast_to_mpmatrix(A);
	  if ((B = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;    
	  return F(stack, rhs, opt, lhs);
	}
    }
  else
    {
      /* [B,A]-> [ B,maxp(A)]  */
      if ((A = GetMat(stack, 2)) == NULLMAT) return RET_BUG;
      if (A->mn == 0)
	{
	  /* Note that the result will be boolean */
	  NthObj (1)->ret_pos = 1;
	  return 1;
	}
      /* Copy A if necessary  */
      if ((A = GetMatCopy(stack,2)) == NULLMAT) return RET_BUG;
      /* cast A to Maxp */
      /* Amp =*/ nsp_matrix_cast_to_mpmatrix(A);
      if ((B = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;    
      return F(stack, rhs, opt, lhs);
    }
}


static int int_mpconcatr_mpm (Stack stack, int rhs, int opt, int lhs)
{
  return int_mxconcat_mixed_mmp (stack,rhs,opt,lhs,int_matint_concatr);
}

static int int_mpconcatd_mpm (Stack stack, int rhs, int opt, int lhs)
{
  return int_mxconcat_mixed_mmp (stack,rhs,opt,lhs,int_matint_concatd);
}

typedef NspMaxpMatrix * (*Fconcat) (const NspMaxpMatrix *,const NspMaxpMatrix *);

/* mixed diag concatenation */

static int int_mpconcatdiag_mpm(Stack stack, int rhs, int opt, int lhs)
{
  return int_mxconcat_mixed_mmp (stack,rhs,opt,lhs,int_matint_concat_diag);
}




/*
 * MatAddCols : add n cols of zero to Matrix A 
 * A= [A,ones(m,n)] 
 * A is changed 
 */

static int int_mpaddcols(Stack stack, int rhs, int opt, int lhs)
{
  int n1;
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

static int int_mpaddrows(Stack stack, int rhs, int opt, int lhs)
{
  int m1;
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
 * Scilab diag function 
 */

static int int_mpdiag(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
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
 * Returns: the kthe diag of a Matrix 
 */

static int int_mpdiage(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
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
 *  Creates a Matrix with kth diag set to Diag 
 */

static int int_mpdiagcre(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
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

static int int_mpresize(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
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

static int int_mpquote(Stack stack, int rhs, int opt, int lhs)
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

static int int_mpdquote(Stack stack, int rhs, int opt, int lhs)
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
 * nsp_mpmatrix_latex_print: writes Mat Objet on fd in tex language
 */

static int int_mp2latexmat(Stack stack, int rhs, int opt, int lhs)
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

static int int_mp2latextab(Stack stack, int rhs, int opt, int lhs)
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

/*
 * Atan2(A,B), 
 */

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

static int int_mpatan2(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A,*B,*Cmp;
  NspMatrix *C;
  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((A=GetRealMpMat(stack,1))== NULLMAXPMAT) return RET_BUG;

  if ((B=GetRealMpMat(stack,2))== NULLMAXPMAT) return RET_BUG;

  if ( A->mn != 1 && B->mn != 1 )
    CheckSameDims (NspFname(stack), 1, 2, A, B);

  if ( (C = nsp_mat_atan2 ((NspMatrix *) A,(NspMatrix *) B)) == NULLMAT)
    return RET_BUG;

  /* from matrix to maxplus matrix by moving data */
  Cmp = nsp_matrix_cast_to_mpmatrix(C);
  MoveObj(stack,1,(NspObject *) Cmp);
  return 1;
}

/*
 * A=Polar(A,B),  * A is changed 
 */


static int int_mppolar(Stack stack, int rhs, int opt, int lhs)
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
      Scierror("Error: %s Mat1 & Mat2 don't have same size \n",NspFname(stack));
      return RET_BUG;
    }
  return 1;
}

/*
 * A=iand(A,B)
 */

static int int_mpiand(Stack stack, int rhs, int opt, int lhs)
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
	  Scierror("Error: %s Mat1 & Mat2 don't have same size \n",NspFname(stack));
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

static int int_mpior(Stack stack, int rhs, int opt, int lhs)
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
	  Scierror("Error: %s Mat1 & Mat2 don't have same size \n",NspFname(stack));
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

static int int_mpconj(Stack stack, int rhs, int opt, int lhs)
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
 *nsp_mat_modulo: A=Modulo(A) remainder in int division 
 * A is changed  
 */

static int int_mpmodulo(Stack stack, int rhs, int opt, int lhs)
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
 * Mat : A= Idiv(A,b) quotient in int division
 * A is changed   A(i)=A(i)/b 
 */

static int int_mpidiv(Stack stack, int rhs, int opt, int lhs)
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

static int nsp_mp_max(NspMatrix *A, NspMatrix *B);

static int int_mpdadd(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_mopscal (stack, rhs, opt, lhs,
			 nsp_mp_max, nsp_mp_max,
			 nsp_mp_max, MpMatNoOp, 1);
}

/* utility for previous function: Mat2 can be scalar here but Mat1 no */

static int nsp_mp_max(NspMatrix *A, NspMatrix *B)
{
  return nsp_mat_maxitt1(A,B,NULL,0,0);
}


/*
 * term to term substraction 
 *  with special cases Mat - [] and Mat - scalar
 * FIXME: nsp_mat_dsub and nsp_mat_subs_calarm are to be done 
 * 
 */

static int int_mpdsub(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_mopscal(stack,rhs,opt,lhs,
			nsp_mat_sub_scalar_maxplus,nsp_mat_dsub,
			nsp_mat_subs_calarm,nsp_mat_minus_maxplus,0);
}

/*
 * A=nsp_mat_pow_el(A,B), A.^ B 
 * with special cases Mat.^[]  and Mat.^scalar
 *                    [].^Mat ans scalar.^Mat
 * Note: B here is a standard Matrix 
 */

static int int_mppowel(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A;
  NspMatrix *B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMpMatCopy(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if (( B = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( B->mn == 0) 
    {
      NspMaxpMatrix *Res;
      /* since B is to be returned we make a copy if necessary */
      if (( Res = nsp_mpmatrix_copy((NspMaxpMatrix *) B)) == NULLMAXPMAT) return RET_BUG;
      /* return Res */
      MoveObj(stack,1,NSP_OBJECT(Res));
      return 1;
    }
  if ( B->mn == 1) 
    {
      if ( nsp_mat_mult_scalar((NspMatrix *) A,(NspMatrix *) B) != OK) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
    }
  else if ( A->mn == 1 ) 
    {
      NspMaxpMatrix *Res;
      /* since Mat1 is scalar we store the result in Mat2 so we 
       * must copy it and change it to a Max Plus Matrix;
       */
      if (( Res = nsp_mpmatrix_copy((NspMaxpMatrix *) B)) == NULLMAXPMAT) return RET_BUG;
      if (  nsp_mat_mult_scalar((NspMatrix *)Res,(NspMatrix *)A) != OK) return RET_BUG;
      /* return Res */
      MoveObj(stack,1,NSP_OBJECT(Res));
    }
  else 
    {
      if ( nsp_mat_mult_el((NspMatrix *)A,(NspMatrix *)B) != OK) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
    }
  return 1;
}



/*
 * A=DivEl(A,B),  A ./ B 
 */

static int int_mpdivel(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_mopscal(stack,rhs,opt,lhs,
			nsp_mat_div_scalar,nsp_mat_div_el,nsp_mat_bdiv_scalar,MpMatNoOp,1);
}


/*
 * A=BackDivEl(A,B),  A .\ B 
 */

static int int_mpbackdivel(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_mopscal(stack,rhs,opt,lhs,
			nsp_mat_bdiv_scalar,nsp_mat_bdiv_el,nsp_mat_div_scalar,MpMatNoOp,1);
}


/*
 * A=MultEl(A,B),  A .* B -> a + b in max plus 
 */


static int int_mpmultel(Stack stack, int rhs, int opt, int lhs)
{
  return int_mp_mopscal(stack,rhs,opt,lhs,
			nsp_mat_add_scalar_maxplus,nsp_mat_dadd_maxplus,nsp_mat_add_scalar_maxplus,MpMatNoOp,1);
}


/*
 * NspMaxpMatrix multiplication  Res= A*B  
 * with special cases Mat * [] and Mat * scalar
 * very similar to mopscal but MatMult returns a new matrix 
 */

static int int_mpmult(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A,*B,*Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((B = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  if ( A->mn == 0)
    {
      if ( A == B ) NthObj(2) = NULLOBJ;
      NSP_OBJECT (A)->ret_pos = 1;
      return 1;
    }
  if ( B->mn == 0) 
    {
      if ( A == B ) 
	{
	  NthObj(2) = NULLOBJ;
	  NSP_OBJECT (A)->ret_pos = 1;
	}
      else 
	{
	  /* flag == 1 ==> A op [] returns [] * */
	  NSP_OBJECT (B)->ret_pos = 1;
	}
      return 1;
    }
  if ( B->mn == 1) 
    {
      if ((A = GetMpMatCopy(stack,1)) == NULLMAXPMAT) return RET_BUG;
      if (nsp_mat_add_scalar_maxplus((NspMatrix *)A,(NspMatrix *)B) != OK) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
    }
  else if ( A->mn == 1 ) 
    {
      /* since Mat1 is scalar we store the result in Mat2 so we 
       * must copy it
       */
      if ((B = GetMpMatCopy(stack,2)) == NULLMAXPMAT) return RET_BUG;
      if (nsp_mat_add_scalar_maxplus((NspMatrix *)B,(NspMatrix *)A) != OK) return RET_BUG;
      NSP_OBJECT(B)->ret_pos = 1;
    }
  else 
    {
      NspMatrix *M;
      if ((M=nsp_mat_maxplus_mult((NspMatrix *)A,(NspMatrix *)B)) == NULLMAT) return RET_BUG;
      Res= nsp_matrix_cast_to_mpmatrix(M);
      MoveObj(stack,1,NSP_OBJECT(Res));
    }
  return 1;
}


/*
 * Res = A / B 
 * 
 */

static int int_mpdiv(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((HMat2 = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  if ( HMat2->mn == 1 )
    {
      NspMaxpMatrix *Res,*B;
      /* HMat1 - (HMat2)' */
      if ((B=nsp_mpmatrix_transpose(HMat2)) ==  NULLMAXPMAT) return RET_BUG;
      nsp_mat_conj((NspMatrix *) B);
      nsp_mat_minus((NspMatrix *) B);
      if ((Res= nsp_mpmatrix_copy(HMat1)) == NULLMAXPMAT) return RET_BUG;
      nsp_mat_add_scalar_maxplus((NspMatrix *) Res,(NspMatrix *) B);
      nsp_mpmatrix_destroy(B);
      MoveObj(stack,1,(NspObject *) Res);
    }
  else 
    {
      NspMaxpMatrix *Res,*B;
      NspMatrix *C;
      /* transconjugate HMat2 */
      if ((B=nsp_mpmatrix_transpose(HMat2)) ==  NULLMAXPMAT) return RET_BUG;
      nsp_mat_conj((NspMatrix *) B);
      nsp_mat_minus((NspMatrix *) B);
      if ((C=nsp_mat_minplus_mult((NspMatrix *)HMat1,(NspMatrix *)B)) == NULLMAT) return RET_BUG;
      nsp_mpmatrix_destroy(B);
      Res= nsp_matrix_cast_to_mpmatrix(C);
      MoveObj(stack,1,NSP_OBJECT(Res));
    }
  return 1;
}

/*
 * Res = A \ B 
 * 
 */

static int int_mpbdiv(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMpMat(stack,1)) == NULLMAXPMAT) return RET_BUG;
  if ((HMat2 = GetMpMat(stack,2)) == NULLMAXPMAT) return RET_BUG;
  if ( HMat1->mn == 1 )
    {
      NspMaxpMatrix *Res,*A;
      /* - HMat1' + (HMat2) */
      if ((A=nsp_mpmatrix_transpose(HMat1)) ==  NULLMAXPMAT) return RET_BUG;
      nsp_mat_conj((NspMatrix *) A);
      nsp_mat_minus((NspMatrix *) A);
      if ((Res= nsp_mpmatrix_copy(HMat2)) == NULLMAXPMAT) return RET_BUG;
      nsp_mat_add_scalar_maxplus((NspMatrix *) Res,(NspMatrix *) A);
      nsp_mpmatrix_destroy(A);
      MoveObj(stack,1,(NspObject *) Res);
    }
  else 
    {
      NspMaxpMatrix *Res,*A;
      NspMatrix *C;
      /* transconjugate HMat1 */
      if ((A=nsp_mpmatrix_transpose(HMat1)) ==  NULLMAXPMAT) return RET_BUG;
      nsp_mat_conj((NspMatrix *) A);
      nsp_mat_minus((NspMatrix *) A);
      if ((C=nsp_mat_minplus_mult((NspMatrix *)A,(NspMatrix *)HMat2)) == NULLMAT) return RET_BUG;
      nsp_mpmatrix_destroy(A);
      Res= nsp_matrix_cast_to_mpmatrix(C);
      MoveObj(stack,1,NSP_OBJECT(Res));
    }
  return 1;
}


/*
 * returns in a Matrix the indices for which the Matrix is true 
 */

static int int_mpfind(Stack stack, int rhs, int opt, int lhs)
{
  NspMaxpMatrix *A;
  NspMatrix *Rc,*Rr;
  CheckRhs(1,1);
  CheckLhs(1,2);
  if ((A = GetMpMat(stack,1)) == NULLMAXPMAT)  return RET_BUG;
  if (nsp_mat_find((NspMatrix *) A,Max(lhs,1),(NspObject **)&Rr,(NspObject **)&Rc,'d') == FAIL) return RET_BUG;
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

static int int_mp_isinf(Stack stack, int rhs, int opt, int lhs)
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

static int int_mp_isnan(Stack stack, int rhs, int opt, int lhs)
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

static int int_mp_finite(Stack stack, int rhs, int opt, int lhs)
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
 * test of casts 
 * FIXME : just a test 
 */

static int int_mp_cast(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetMat(stack,1)) == NULLMAT)  return RET_BUG;
  nsp_matrix_cast_to_mpmatrix(A);
  return 0;
}

/* wrapper for functions  B=f(A)
 * call the matrix interface after a cast 
 */

static int int_mp_wrap1(Stack stack, int rhs, int opt, int lhs,function *f)
{
  int rep;
  register NspMaxpMatrix *Mp;
  CheckRhs(1,1);
  if ((Mp=GetMpMat(stack,1))== NULLMAXPMAT) return RET_BUG;
  /* cast without copy : */
  nsp_mpmatrix_cast_to_matrix(Mp);
  /* call same interface for matrix */
  rep = (*f)(stack,rhs,opt,lhs);
  /* always recast back even if an error occured */
  nsp_matrix_cast_to_mpmatrix((NspMatrix *) NthObj(1));
  /* we have to reset Mp to mpmatrix */
  nsp_matrix_cast_to_mpmatrix((NspMatrix *) Mp);
  return rep;
}

/*
 * The Interface for basic matrices operation 
 */

static OpWrapTab Matrix_func[]={
  {"extract_mp", int_matint_extract}, 
  {"extractelts_mp", int_matint_extractelts}, 
  {"extractcols_mp", int_matint_extractcols}, 
  {"extractrows_mp", int_matint_extractrows_mat}, 
  {"resize2vect_mp", int_matint_resize2vect},
  {"setrowscols_mp", int_matint_setrowscols},
  {"deleteelts_mp", int_matint_deleteelts},
  {"deleterows_mp", int_matint_deleterows},
  {"deletecols_mp", int_matint_deletecols},
  {"tozero_mp", int_matint_tozero},
  {"isvector_mp", int_matint_isvector},
  {"abs_mp",int_mxabs,int_mp_wrap1},
  {"acos_mp",int_mxacos,int_mp_wrap1},
  {"acosh_mp",int_mxacosh,int_mp_wrap1},
  {"addcols_mp_m",int_mpaddcols ,NULL},
  {"addrows_mp_m",int_mpaddrows ,NULL},
  {"and_mp_mp",int_mpmini,NULL},
  {"arg_mp",int_mxangle,int_mp_wrap1},
  {"angle_mp",int_mxangle,int_mp_wrap1},
  {"asin_mp",int_mxasin,int_mp_wrap1},
  {"asinh_mp",int_mxasinh,int_mp_wrap1},
  {"atan_mp",int_mxatan,int_mp_wrap1},
  {"atan_mp_mp",int_mpatan2,NULL},
  {"atanh_mp",int_mxatanh,int_mp_wrap1},
  {"bdiv_mp_mp",int_mpbdiv,NULL},
  {"cast",int_mp_cast,NULL},
  {"ceil_mp",int_mxceil,int_mp_wrap1},
  {"clean_mp" ,  int_mpclean ,NULL},
  {"complexify_mp" ,  int_mpcomplexify ,NULL},
  {"concatd_m_mp" ,  int_mpconcatd_mpm ,NULL},
  {"concatd_mp_m" ,  int_mpconcatd_mpm ,NULL},
  {"concatd_mp_mp" , int_matint_concatd ,NULL},
  {"concatdiag_m_mp" ,  int_mpconcatdiag_mpm ,NULL},
  {"concatdiag_mp_m" ,  int_mpconcatdiag_mpm ,NULL},
  {"concatdiag_mp_mp" , int_matint_concat_diag ,NULL},
  {"concatr_m_mp" ,  int_mpconcatr_mpm,NULL},
  {"concatr_mp_m" ,  int_mpconcatr_mpm ,NULL},
  {"concatr_mp_mp" , int_matint_concatr   ,NULL},
  {"conj_mp",int_mpconj,NULL},
  {"cos_mp",int_mxcos,int_mp_wrap1},
  {"cosh_mp",int_mxcosh,int_mp_wrap1},
  {"cumprod_mp" ,  int_mpcuprod ,NULL},
  {"cumprod_mp_s" ,  int_mpcuprod ,NULL},
  {"cumsum_mp" ,  int_mpcusum ,NULL},
  {"cumsum_mp_s" ,  int_mpcusum ,NULL},
  {"dadd_mp_mp" ,   int_mpdadd ,NULL},
  {"dh_mp_m",int_mppowel,NULL},
  {"diag_mp",int_mpdiag,NULL},
  {"diag_mp_mp",int_mpdiag,NULL},
  {"diagcre_mp" ,  int_mpdiagcre ,NULL},
  {"diagcre_mp_mp" ,  int_mpdiagcre ,NULL},
  {"diage_mp" ,  int_mpdiage ,NULL},
  {"diage_mp_mp" ,  int_mpdiage ,NULL},
  {"div_mp_mp" ,  int_mpdiv,NULL},
  {"dprim_mp",int_mpdquote,NULL},
  {"dst_mp_mp",int_mpmultel,NULL},
  {"dstd_mp_mp" ,  int_mpkron ,NULL}, /* operator:  .*. */
  {"eq_mp_mp" ,  int_mpeq ,NULL},
  {"erf_mp",int_mxerf,int_mp_wrap1},
  {"erfc_mp",int_mxerfc,int_mp_wrap1},
  {"expel_mp",int_mxexpel,int_mp_wrap1},
  {"eye_mp_mp" ,  int_mpeye ,NULL},
  {"eye_mp" ,  int_mpeye ,NULL},
  {"feq_mp_mp" ,  int_mpfeq ,NULL},
  {"fge_mp_mp" ,  int_mpfge ,NULL},
  {"fgt_mp_mp" ,  int_mpfgt ,NULL},
  {"find_mp", int_mpfind,NULL},
  {"fle_mp_mp" ,  int_mpfle ,NULL},
  {"floor_mp",int_mxfloor,int_mp_wrap1},
  {"flt_mp_mp" ,  int_mpflt ,NULL},
  {"fneq_mp_mp" ,  int_mpfneq ,NULL},
  {"ge_mp_mp" ,  int_mpge ,NULL},
  {"gsort_mp" , int_mpmatrix_sort ,NULL},
  {"gt_mp_mp" ,  int_mpgt ,NULL},
  {"iand_mp_mp",int_mpiand,NULL},
  {"idiv_mp_mp",int_mpidiv,NULL},
  {"imag_mp" ,  int_mpimagpart ,NULL},
  {"int_mp",int_mxint,int_mp_wrap1},
  {"ior_mp_mp",int_mpior,NULL},
  {"latexmat_mp" ,  int_mp2latexmat ,NULL},
  {"latextab_mp" ,  int_mp2latextab ,NULL},
  {"le_mp_mp" ,  int_mple ,NULL},
  {"log_mp",int_mxlogel,int_mp_wrap1},
  {"logel_mp",int_mxlogel,int_mp_wrap1},
  {"lt_mp_mp" ,  int_mplt ,NULL},
  {"m2mp",int_m2mp,NULL},
  {"max_mp" ,  int_mpmaxi,NULL},
  {"maxplus",int_m2mp,NULL},
  {"min_mp" ,  int_mpmini ,NULL},
  {"minus_mp",int_mxminus,int_mp_wrap1},
  {"minus_mp_mp",   int_mpdsub,NULL},
  {"modulo_mp_mp",int_mpmodulo,NULL},
  {"mp2m",int_mp2m,NULL},
  {"mpcreate_m_m" ,  int_mpcreate ,NULL},
  {"mult_mp_mp" ,  int_mpmult,NULL},
  {"ne_mp_mp" ,  int_mpneq ,NULL},
  {"ones_mp_mp" ,  int_mpones_deprecated,NULL},
  {"ones_deprecated_mp_mp" ,  int_mpones_deprecated,NULL},
  {"plus_mp_mp",   int_mpdadd,NULL},
  {"prod_mp" ,  int_mpsum ,NULL}, // should be a sum
  {"prod_mp_s" ,  int_mpsum ,NULL},
  {"quote_mp",int_mpquote,NULL},
  {"real_mp" ,  int_mprealpart ,NULL},
  {"redim_mp" ,  int_matint_redim ,NULL}, 
  {"matrix_mp", int_matint_redim, NULL},
  {"reshape_mp", int_matint_redim, NULL},
  {"resize_mp_mp" ,  int_mpresize ,NULL},
  {"round_mp",int_mxround,int_mp_wrap1},
  {"seti_mp_mp" ,  int_mpseti ,NULL},
  {"setr_mp_mp" ,  int_mpsetr ,NULL},
  {"sign_mp",int_mxsign,int_mp_wrap1},
  {"sin_mp",int_mxsin,int_mp_wrap1},
  {"sinh_mp",int_mxsinh,int_mp_wrap1},
  {"sort_mp" , int_mpmatrix_sort ,NULL},
  {"sqrt_mp",int_mxsqrtel,int_mp_wrap1},
  {"sqrtel_mp",int_mxsqrtel,int_mp_wrap1},
  {"sum_mp" ,  int_mpmaxi ,NULL}, // sum is a max in Maxplus 
  {"tan_mp",int_mxtan,int_mp_wrap1},
  {"tanh_mp",int_mxtanh,int_mp_wrap1},
  {"tril_mp" ,  int_mptril ,NULL},
  {"triu_mp" ,  int_mptriu ,NULL},
  {"zeros_mp_mp", int_mpzeros ,NULL},
  {"isinf_mp", int_mp_isinf,NULL},
  {"isnan_mp", int_mp_isnan,NULL},
  {"finite_mp", int_mp_finite,NULL},
  {"polar_mp",int_mppolar,NULL},
  {"dsl_mp",int_mpdivel,NULL},
  {"dbs_mp",int_mpbackdivel,NULL},
  {(char *) 0, NULL,NULL},
};


int MpMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  if ( Matrix_func[i].wrapper != NULL) 
    return (*(Matrix_func[i].wrapper))(stack,rhs,opt,lhs,Matrix_func[i].fonc);
  else
    return (*(Matrix_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) **/

void MpMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Matrix_func[i].name;
  *f = Matrix_func[i].fonc;
}




