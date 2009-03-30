/* Nsp
 * Copyright (C) 2009 Jean-Philippe Chancelier Enpc/Cermics
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

#define IMatrix_Private 
#include "nsp/object.h"
#include "nsp/datas.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/matint.h"
#include <nsp/imatrix.h>

/**
 * SECTION:imatrix
 * @title: NspIMatrix
 * @short_description: An object used to implement boolean matrices.
 * @see_also: 
 *
 * <para>
 * A #NspIMatrix is used to represent a boolean matrix. 
 * It can be filled with %TRUE and %FALSE values and is 
 * implemented as an array of integer. It implement the 
 * matint interface which is used for generic matrices 
 * operations. When using the matint interface a #NspIMatrix 
 * can always be casted to a #NspSMatrix.
 * </para>
 **/

/*
 * NspIMatrix inherits from NspObject 
 */

int nsp_type_imatrix_id=0;
NspTypeIMatrix *nsp_type_imatrix=NULL;

NspTypeIMatrix *new_type_imatrix(type_mode mode)
{
  NspTypeMatint *mati;/* interface */
  NspTypeIMatrix *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_imatrix != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_imatrix;
    }
  
  if ((type  =  malloc(sizeof(NspTypeIMatrix))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs =  NULL; /*imatrix_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = imatrix_get_methods; 
  type->new = (new_func *) new_imatrix;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for imatrix */ 

  top->pr = (print_func *)nsp_imatrix_print;                    
  top->dealloc = (dealloc_func *)nsp_imatrix_destroy;            
  top->copy  =  (copy_func *)nsp_imatrix_copy;                   
  top->size  = (size_func *) imatrix_size;                  
  top->s_type =  (s_type_func *) imatrix_type_as_string;    
  top->sh_type = (sh_type_func *) imatrix_type_short_string;
  top->info = (info_func *)nsp_imatrix_info;                    
  top->is_true = (is_true_func  *) imatrix_is_true;           
  top->loop =(loop_func *) imatrix_loop;
  top->path_extract =  NULL;       
  top->get_from_obj = (get_from_obj_func *)  IMatObj ;  
  top->eq  = (eq_func *) imatrix_eq;
  top->neq  = (eq_func *) imatrix_neq;
  top->save  = (save_func *) imatrix_xdr_save;
  top->load  = (load_func *) imatrix_xdr_load;
  top->latex = (print_func *) nsp_imatrix_latex_print;
  top->as_index  = (get_index_vector_func *) nsp_imatrix_as_index;
  
  /* specific methods for imatrix */

  type->init = (init_func *) init_imatrix;
  /* 
   * IMatrix interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  /*
   * IMatrix implements Matint the matrix interface 
   * which is common to object that behaves like matrices.
   */

  mati = new_type_matint(T_DERIVED);
  mati->methods = matint_get_methods; 
  /* mati->redim = (matint_redim *) nsp_imatrix_redim; use default value */
  mati->resize = (matint_resize  *) nsp_imatrix_resize;
  mati->free_elt = (matint_free_elt *) 0; /* nothing to do */
  mati->elt_size = (matint_elt_size *) nsp_imatrix_elt_size ;
  mati->clone = (matint_clone *) nsp_imatrix_clone ;
  mati->copy_elt = (matint_copy_elt *) 0; /* nothing to do */
  mati->enlarge = (matint_enlarge *) nsp_imatrix_enlarge;
  mati->canonic =  nsp_matint_canonic;
  mati->copy_ind = nsp_matint_basic_copy;
  type->interface = (NspTypeBase *) mati;
  
  if ( nsp_type_imatrix_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeIMatrix called nsp_type_imatrix
       */
      type->id =  nsp_type_imatrix_id = nsp_new_type_id();
      nsp_type_imatrix = type;
      if ( nsp_register_type(nsp_type_imatrix) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_imatrix(mode);
    }
  else 
    {
      type->id = nsp_type_imatrix_id;
      return type;
    }
}

/*
 * initialize IMatrix instances 
 * locally and by calling initializer on parent class 
 */

static int init_imatrix(NspIMatrix *o,NspTypeIMatrix *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of IMatrix 
 */

NspIMatrix *new_imatrix() 
{
  NspIMatrix *loc; 
  /* type must exists */
  nsp_type_imatrix = new_type_imatrix(T_BASE);
  if ( (loc = malloc(sizeof(NspIMatrix)))== NULLIMAT) return loc;
  /* initialize object */
  if ( init_imatrix(loc,nsp_type_imatrix) == FAIL) return NULLIMAT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for IMatrix 
 *-----------------------------------------------*/

/*
 * size 
 */

static int imatrix_size(NspIMatrix *Mat, int flag)
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

static char imat_type_name[]="IMat";
static char imat_short_type_name[]="i";

static char *imatrix_type_as_string(void)
{
  return(imat_type_name);
}

static char *imatrix_type_short_string(NspObject *v)
{
  return(imat_short_type_name);
}

/* used in for x=y where y is a IMatrix **/

static NspObject *imatrix_loop(char *str, NspObject *O, NspObject *O1, int i, int *rep)
{
  NspIMatrix *M= (NspIMatrix *) O1,*M1=NULLIMAT;
  if ( O == NULLOBJ ) 
    {
      if (( M1= IMatLoopCol(str,NULLIMAT,M,i,rep))==NULLIMAT) return NULLOBJ;
      if (( *rep == RET_ENDFOR )) return NULLOBJ;
      return (NspObject *) M1 ;
    }
  else
    {
      if (( M1 = IMatObj(O)) == NULLIMAT ) return NULLOBJ;
      M1= IMatLoopCol(str,M1,M,i,rep);
      if (( *rep == RET_ENDFOR )) return NULLOBJ;
      return O;
    }
}

static int imatrix_eq(NspIMatrix *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_imatrix_id) == FALSE) return FALSE ;
  if ( ! ( ((NspIMatrix *) A)->m == ((NspIMatrix *) B)->m 
	   && ((NspIMatrix *) A)->n == ((NspIMatrix *) B)->n)) return FALSE;
  if ( ((NspIMatrix *) A)->itype != ((NspIMatrix *) B)->itype) return FALSE;
  rep = nsp_imatrix_full_compare (A, (NspIMatrix *) B, "==", &err);
  if (err == TRUE)
    return FALSE;
  return rep;
}

static int imatrix_neq(NspIMatrix *A, NspObject *B)
{
   return ( imatrix_eq(A,B) == TRUE ) ? FALSE : TRUE ;
}


/*
 * Mat == TRUE ? 
 *  if Mat != [] and all the elements of Mat are non null
 */

static int imatrix_is_true(NspIMatrix *A)
{
  int i;
  if ( A->mn == 0) return FALSE;
  switch (A->itype )
    {						
    case nsp_gint: for (i=0; i <  A->mn; i++) if ( A->Gint[i] == (gint) 0) return FALSE; 
    case nsp_guint: for (i=0; i <  A->mn; i++) if ( A->Guint[i] == (guint) 0) return FALSE; 
    case nsp_gshort:for (i=0; i <  A->mn; i++) if ( A->Gshort[i] == (gshort) 0) return FALSE; 
    case nsp_gushort:for (i=0; i <  A->mn; i++) if ( A->Gushort[i] == (gushort) 0) return FALSE; 
    case nsp_glong : for (i=0; i <  A->mn; i++) if ( A->Glong[i] == (glong) 0) return FALSE; 
    case nsp_gulong: for (i=0; i <  A->mn; i++) if ( A->Gulong[i] == (gulong) 0) return FALSE; 
    case nsp_gint8:for (i=0; i <  A->mn; i++) if ( A->Gint8[i] == (gint8) 0) return FALSE; 
    case nsp_guint8: for (i=0; i <  A->mn; i++) if ( A->Guint8[i] == (guint8) 0) return FALSE; 
    case nsp_gint16: for (i=0; i <  A->mn; i++) if ( A->Gint16[i] == (gint16) 0) return FALSE; 
    case nsp_guint16:for (i=0; i <  A->mn; i++) if ( A->Guint16[i] == (guint16) 0) return FALSE; 
    case nsp_gint32: for (i=0; i <  A->mn; i++) if ( A->Gint32[i] == (gint32) 0) return FALSE; 
    case nsp_guint32:for (i=0; i <  A->mn; i++) if ( A->Guint32[i] == (guint32) 0) return FALSE; 
    case nsp_gint64:for (i=0; i <  A->mn; i++) if ( A->Gint64[i] == (gint64) 0) return FALSE; 
    case nsp_guint64:for (i=0; i <  A->mn; i++) if ( A->Guint64[i] == (guint64) 0) return FALSE;
    }
  return(TRUE);
}

/*
 * save 
 */

static int imatrix_xdr_save(XDR *xdrs, NspIMatrix *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->m) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->n) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->itype) == FAIL) return FAIL;
  if (nsp_xdr_save_array_ixx(xdrs,M->Iv,M->itype,M->mn) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

static NspIMatrix  *imatrix_xdr_load(XDR *xdrs)
{
  int m,n,itype;
  NspIMatrix *M;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLIMAT;
  if (nsp_xdr_load_i(xdrs,&m) == FAIL) return NULLIMAT;
  if (nsp_xdr_load_i(xdrs,&n) == FAIL) return NULLIMAT;
  if (nsp_xdr_load_i(xdrs,&itype) == FAIL) return NULLIMAT;
  if (( M=nsp_imatrix_create(name,m,n,itype)) == NULLIMAT ) return NULLIMAT;
  if (nsp_xdr_load_array_ixx(xdrs,M->Iv,M->itype,M->mn) == FAIL) return NULLIMAT;
  return M;
}

/**
 * nsp_imatrix_bounds:
 * @A:  a #NspIMatrix supposed to be a vector of indices
 * @index: an #index_vector
 *
 * cast elements of A to integers in ind (minus 1 such that ind is "0-based")
 * and computes the min and max of A (here staying "1-based")   
 */

static int nsp_imatrix_bounds(const NspIMatrix *A,index_vector *index)
{
  NSP_ITYPE_NAMES(names);
  int i, ival, *ind = index->val;
  index->max = 1;
  index->min = 1;
  index->flag = FALSE;
  switch (  A->itype ) 
    {
    default:
      Scierror("Error:\tindices cannot be given by imatrix of %s type\n",names[A->itype]);
      index->error = index_wrong_value;
      return FAIL;
    case nsp_gint :
      for (i = 0; i < A->mn; i++)
	{
	  ival = A->Gint[i];
	  if (ival > index->max)
	    index->max = ival;
	  else if (ival < index->min)
	    index->min = ival;
	  ind[i] = ival-1;
	}
      break;
    }
  return OK;
}

/**
 * nsp_imatrix_as_index:
 * @M: a #NspIMatrix 
 * @index: an #index_vector
 * 
 * fills index vector @index with matrix values.
 *
 * Return value: %OK or %FAIL 
 **/

static int nsp_imatrix_as_index(NspIMatrix *M, index_vector *index)
{
  index->nval = M->mn;
  if ( nsp_get_index_vector_cache(index) == FALSE) return FAIL;
  if ( nsp_imatrix_bounds(M, index) == FAIL ) 
    {
      /* we assume that index->error is set by nsp_matint_bounds
       * free allocated memory and return 
       */
      if (  index->val  != nsp_get_index_vector_work(index->iwork) ) 
	FREE(index->val);
      return FAIL;
    }
  return OK;
}



/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for IMatrix objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

/**
 * IMatObj:
 * @O: a #NspObject 
 * 
 * checks if @O can be casted to a #IMatObj. 
 * 
 * Return value: %NULL or a #IMatObj.
 **/

NspIMatrix   *IMatObj(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_imatrix_id) == TRUE) return ((NspIMatrix *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_imatrix));
  return(NULL);
}

/**
 * IsIMatObj:
 * @stack: a #Stack
 * @i: an integer 
 * 
 * checks if #NspObject at position @i on the stack can be casted to a #IMatObj.
 * 
 * Return value: %TRUE or %FALSE
 **/

int IsIMatObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_imatrix_id);
}

/**
 * IsIMat:
 * @O: a #NspObject 
 * 
 * checks if object @O can be casted to a #IMatObj.
 * 
 * Return value:  %TRUE or %FALSE
 **/

int IsIMat(const NspObject *O)
{
  return nsp_object_type(O , nsp_type_imatrix_id);
}

/**
 * GetIMatCopy:
 * @stack: a #Stack
 * @i: an integer 
 * 
 * checks if #NspObject at position @i on the stack can be casted to a #IMatObj and
 * if true returns a copy of that boolean matrix.
 * 
 * Return value: %NULL or a #NspIMatrix
 **/

NspIMatrix  *GetIMatCopy(Stack stack, int i)
{
  if (  GetIMat(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/**
 * GetIMat:
 * @stack: a #Stack
 * @i: an integer 
 * 
 * checks if #NspObject at position @i on the stack can be casted to a #IMatObj and
 * if true returns the boolean matrix.
 * 
 * Return value:  %NULL or a #NspIMatrix
 **/

NspIMatrix  *GetIMat(Stack stack, int i)
{
  NspIMatrix *M;
  if (( M = IMatObj(NthObj(i))) == NULLIMAT)
    ArgMessage(stack,i);
  return M;
}


/*-------------------------------------------------------------------
 * wrappers for the IMatrix 
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/*
 *  Creation of a NspIMatrix : created with true value 
 */

static int int_imatrix_create(Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  NSP_ITYPE_NAMES(names);
  NspIMatrix *HMat;
  int m1,n1;
  nsp_itype itype;
  CheckRhs(2,3);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ( rhs == 3 ) 
    {
      if ((rep = GetStringInArray(stack, 3, names, 0)) == -1 ) return RET_BUG;
      itype = (nsp_itype) rep;
    }

  if ( (HMat =nsp_imatrix_create(NVOID,m1,n1,itype)) == NULLIMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat);
  return 1;
}


/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/


static int int_imatrix_meth_retype(NspObject *self, Stack stack, int rhs, int opt, int lhs) 
{
  nsp_itype itype = nsp_gint;
  int rep;
  NSP_ITYPE_NAMES(names);
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((rep = GetStringInArray(stack, 1, names, 0)) == -1 ) return RET_BUG;
  itype = (nsp_itype) rep;
  if (nsp_imatrix_change_itype((NspIMatrix *)self,itype)==FAIL) return RET_BUG;
  MoveObj(stack,1,self);
  return 1;
}


static NspMethods nsp_imatrix_methods[] = {
  {"retype",(nsp_method *) int_imatrix_meth_retype},
  { NULL, NULL}
};

static NspMethods *imatrix_get_methods(void) { return nsp_imatrix_methods;}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

/*
 * Res =nsp_imatrix_copy(A) 
 * Creates a Copy of NspIMatrix A : A is not checked 
 */

static int int_imatrix_copy(Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *HMat1,*HMat2;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( HMat1 = GetIMat(stack,1)) == NULLIMAT) return RET_BUG;
  if (( HMat2 =nsp_imatrix_copy(HMat1)) == NULLIMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat2);
  return 1;  
}


/*
 * returns in a Matrix the indices for which the IMatrix is true
 */

static int int_imatrix_find(Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *A;
  NspMatrix *Rc,*Rr;
  CheckRhs(1,1);
  CheckLhs(1,2);
  if ((A = GetIMat(stack,1)) == NULLIMAT)  return RET_BUG;
  if (nsp_imatrix_find_2(A,Max(lhs,1),&Rr,&Rc) == FAIL) return RET_BUG;
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
 * IMatAddCols: add n cols of zero to NspIMatrix A 
 * A= [A,ones(m,n)] 
 * return 0 on failure ( no more space )
 */

static int int_imatrix_addcols(Stack stack, int rhs, int opt, int lhs)
{
  int n1;
  NspIMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetIMatCopy(stack,1))== NULLIMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if (nsp_imatrix_add_columns(HMat,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 * Add Rows: Add m rows of zero to a NspIMatrix A 
 * A = [A;ones(m,n)]
 * return NULLIMAT on failure 
 */

static int int_imatrix_addrows(Stack stack, int rhs, int opt, int lhs)
{
  int m1;
  NspIMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetIMatCopy(stack,1))== NULLIMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if (nsp_imatrix_add_rows(HMat,m1) != OK) return RET_BUG; ;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}


/*
 * Returns the kthe diag of a NspIMatrix 
 */

static int int_imatrix_diage(Stack stack, int rhs, int opt, int lhs)
{
  int k1;
  NspIMatrix *A,*Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
  if ((A = GetIMat(stack,1)) == NULLIMAT) return RET_BUG;
  Res =nsp_imatrix_extract_diag( A,k1);
  if ( Res == NULLIMAT)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 *  Creates a NspIMatrix with kth diag set to Diag 
 */

static int int_imatrix_diagcre(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspIMatrix *Diag,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((Diag = GetIMat(stack,1)) == NULLIMAT) return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (Res =nsp_imatrix_create_diag(Diag,k1)) == NULLIMAT ) return RET_BUG ;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 * IMatresize: changes NspIMatrix dimensions
 * m,n are changed and the arrays enlarged 
 */

static int int_imatrix_resize(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  NspIMatrix  *HMat;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (HMat=GetIMatCopy(stack,1))== NULLIMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;      
  if (nsp_imatrix_resize(HMat,m1,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}


/*
 * A'
 */

int
int_imatrix_quote (Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *A, *B;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((A = GetIMat (stack, 1)) == NULLIMAT)
    return RET_BUG;
  if ((B = nsp_imatrix_transpose (A)) == NULLIMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) B);
  return 1;
}


/*
 * IMat2LaTeXMat: writes IMat Objet on fd in tex language
 */

static int int_imatrix_2latexmat(Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetIMat(stack,1)) == NULLIMAT) return RET_BUG;
  nsp_imatrix_latex_print(HMat);
  return 0;
}

/*
 *  IMat2LaTeXTab: writes IMat Objet on fd in TeX language
 */

static int int_imatrix_2latextab(Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetIMat(stack,1)) == NULLIMAT) return RET_BUG;    
  nsp_imatrix_latex_tab_print(HMat);
  return 0;
}

/*
 * Usual matrix to boolean 
 */

static int int_imatrix_m2i(Stack stack, int rhs, int opt, int lhs)
{
  nsp_itype itype = nsp_gint;
  int rep;
  NSP_ITYPE_NAMES(names);
  NspIMatrix *BM;
  NspMatrix *M;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((M = GetMat(stack,1)) == NULLMAT) return RET_BUG;    
  if ( rhs == 2 ) 
    {
      if ((rep = GetStringInArray(stack, 2, names, 0)) == -1 ) return RET_BUG;
      itype = (nsp_itype) rep;
    }
  if ((BM =nsp_matrix_to_imatrix(M,itype)) == NULLIMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) BM);
  return 1;
}

/*
 * boolean to Matrix 
 */

int int_imatrix_i2m(Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *BM;
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((BM = GetIMat(stack,1)) == NULLIMAT) return RET_BUG;    
  if ((M =nsp_imatrix_to_matrix(BM)) == NULLMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) M);
  return 1;
}

/*
 * == and <> 
 */

static int int_imatrix_neq(Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *A,*B;
  NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetIMat(stack,1)) == NULLIMAT) return RET_BUG;
  if ((B = GetIMat(stack,2)) == NULLIMAT) return RET_BUG;
  Res =nsp_imatrix_compare(A,B,"<>");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_imatrix_eq(Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *A,*B;
  NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetIMat(stack,1)) == NULLIMAT) return RET_BUG;
  if ((B = GetIMat(stack,2)) == NULLIMAT) return RET_BUG;
  Res =nsp_imatrix_compare(A,B,"==");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Same but returns a unique boolean 
 */

static int int_imatrix_fneq(Stack stack, int rhs, int opt, int lhs)
{
  int rep,err;
  NspIMatrix *A,*B;
  NspObject *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetIMat(stack,1)) == NULLIMAT) return RET_BUG;
  if ((B = GetIMat(stack,2)) == NULLIMAT) return RET_BUG;
  rep =nsp_imatrix_full_compare(A,B,"<>",&err);
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

static int int_imatrix_feq(Stack stack, int rhs, int opt, int lhs)
{
  int rep,err;
  NspIMatrix *A,*B;
  NspObject *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetIMat(stack,1)) == NULLIMAT) return RET_BUG;
  if ((B = GetIMat(stack,2)) == NULLIMAT) return RET_BUG;
  rep =nsp_imatrix_full_compare(A,B,"==",&err);
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

static OpTab IMatrix_func[]={
  {"extract_i", int_matint_extract}, 
  {"extractelts_i", int_matint_extractelts}, 
  {"extractcols_i", int_matint_extractcols}, 
  {"extractrows_i", int_matint_extractrows_gen}, 
  {"resize2vect_i", int_matint_resize2vect},
  {"setrowscols_i", int_matint_setrowscols},
  {"deleteelts_i", int_matint_deleteelts},
  {"deleterows_i", int_matint_deleterows},
  {"deletecols_i", int_matint_deletecols},
  {"tozero_i", int_matint_tozero},
  {"repmat_i_m", int_matint_repmat},
  {"latexmat_i",int_imatrix_2latexmat},
  {"latextab_i",int_imatrix_2latextab},
  {"addcols_i_m",int_imatrix_addcols},
  {"addrows_i_m",int_imatrix_addrows},
  {"i2m",int_imatrix_i2m},
  {"concatd_i_i", int_matint_concatd}, /* int_imatrix_concatd}, */
  {"concatr_i_i", int_matint_concatr}, /* int_imatrix_concatr},*/
  {"concatdiag_i_i" , int_matint_concat_diag},/* int_imatrix_concatdiag },*/
  {"isvector_i", int_matint_isvector},
  {"copy_i",int_imatrix_copy},
  {"imat_create",int_imatrix_create},
  {"diagcre_i",int_imatrix_diagcre},
  {"diage_i",int_imatrix_diage},
  {"find_i",int_imatrix_find},
  {"m2i",int_imatrix_m2i},
  {"redim_i",int_matint_redim}, 
  {"matrix_i", int_matint_redim},
  {"resize_i",int_imatrix_resize},
  {"eq_i_i" ,  int_imatrix_eq },
  {"ne_i_i" ,  int_imatrix_neq },
  {"fneq_i_i" ,  int_imatrix_fneq },
  {"feq_i_i" ,  int_imatrix_feq },
  {"quote_i", int_imatrix_quote},
  {(char *) 0, NULL}
};

int IMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(IMatrix_func[i].fonc))(stack,rhs,opt,lhs);
}


/* used to walk through the interface table 
   (for adding or removing functions) **/

void IMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = IMatrix_func[i].name;
  *f = IMatrix_func[i].fonc;
}


/* wrapper for functions  B=f(A)
 * call the matrix interface after a cast 
 */

static int int_bm_wrap(Stack stack, int rhs, int opt, int lhs,function *f)
{
  NspIMatrix *BM;
  NspMatrix *M;
  CheckRhs(1,2);
  if ( IsIMatObj(stack,1) ) 
    {
      if ((BM = GetIMat(stack,1)) == NULLIMAT) return RET_BUG;
      if ((M =nsp_imatrix_to_matrix(BM)) == NULLMAT ) return RET_BUG;
      MoveObj(stack,1,(NspObject *) M);
      /* we don't want M ret_pos to be set */
      NSP_OBJECT(M)->ret_pos = -1;
    }
  if (rhs == 2 &&  IsIMatObj(stack,2) )
    {
      if ((BM = GetIMat(stack,2)) == NULLIMAT) return RET_BUG;
      if ((M =nsp_imatrix_to_matrix(BM)) == NULLMAT ) return RET_BUG;
      MoveObj(stack,2,(NspObject *) M);
      /* we don't want M ret_pos to be set */
      NSP_OBJECT(M)->ret_pos = -1;
    }
  /* call same interface for matrix */
  return (*f)(stack,rhs,opt,lhs);
}

/*
 * Interface for Boolean/Matrix Operations 
 * which involves a conversion b2m + use of the matrix interface.
 */

#include "nsp/matrix-in.h"

static OpWrapTab B2mMatrix_func[]={
  {"dst_m_i", int_mxmultel, int_bm_wrap},
  {"dst_i_m", int_mxmultel, int_bm_wrap},
  {"dst_i_i", int_mxmultel, int_bm_wrap},
  {"dadd_i_i" ,   int_mxdadd , int_bm_wrap},
  {"dadd_i_m" ,   int_mxdadd , int_bm_wrap},
  {"dadd_m_i" ,   int_mxdadd , int_bm_wrap},
  {"div_i_i" ,  int_mxdiv, int_bm_wrap},
  {"div_i_m" ,  int_mxdiv, int_bm_wrap},
  {"div_m_i" ,  int_mxdiv, int_bm_wrap},
  {"dstd_i_i" ,  int_mxkron , int_bm_wrap}, /* operator:  .*. */
  {"dstd_i_m" ,  int_mxkron , int_bm_wrap}, /* operator:  .*. */
  {"dstd_m_i" ,  int_mxkron , int_bm_wrap}, /* operator:  .*. */
  {"minus_i",int_mxminus,int_bm_wrap},
  {"minus_i_i",   int_mxdsub, int_bm_wrap},
  {"minus_i_m",   int_mxdsub, int_bm_wrap},
  {"minus_m_i",   int_mxdsub, int_bm_wrap},
  {"mult_i_i" ,  int_mxmult, int_bm_wrap},
  {"mult_i_m" ,  int_mxmult, int_bm_wrap},
  {"mult_m_i" ,  int_mxmult, int_bm_wrap},
  {"plus_i_i",   int_mxdadd, int_bm_wrap},
  {"plus_i_m",   int_mxdadd, int_bm_wrap},
  {"plus_m_i",   int_mxdadd, int_bm_wrap},
  {"prod_i" ,  int_mxprod , int_bm_wrap},
  {"prod_i_s" ,  int_mxprod , int_bm_wrap},
  {"sum_i" ,  int_mxsum , int_bm_wrap},
  {"sum_i_s" ,  int_mxsum , int_bm_wrap},
  {(char *) 0, NULL,NULL},
};


int XXB2mMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(B2mMatrix_func[i].wrapper))(stack,rhs,opt,lhs,B2mMatrix_func[i].fonc);
}

/* used to walk through the interface table 
   (for adding or removing functions) **/

void XXB2mMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = B2mMatrix_func[i].name;
  *f = B2mMatrix_func[i].fonc;
}



