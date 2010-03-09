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
#include "nsp/gsort-p.h"

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

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
  top->full_copy  =  (copy_func *)nsp_imatrix_copy;                   
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
  rep = nsp_imatrix_fullcomp (A, (NspIMatrix *) B, "==", &err);
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
#define IMAT_ISTRUE(name,type,arg) for ( i=0 ; i < A->mn ; i++)	\
    {if ( A->name[i] == 0) return FALSE;}break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_ISTRUE,"");
#undef  IMAT_ISTRUE
  return(TRUE);
}

/*
 * save 
 */

static int imatrix_xdr_save(XDR *xdrs, NspIMatrix *M)
{
#if 1 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_imatrix)) == FAIL) return FAIL;
#else
  if (nsp_xdr_save_i(xdrs, M->type->id) == FAIL)    return FAIL;
#endif 
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
    case nsp_gint32:
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
 * if true returns the matrix.
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

/**
 * GetScalarIMat:
 * @stack: a #Stack
 * @i: an integer 
 * 
 * checks if #NspObject at position @i on the stack can be casted to a #IMatObj 
 * of size 1x1 if returns the matrix.
 * 
 * Return value:  %NULL or a #NspIMatrix
 **/

NspIMatrix  *GetScalarIMat(Stack stack, int i)
{
  NspIMatrix *M;
  if (( M = IMatObj(NthObj(i))) == NULLIMAT 
      || (M->mn != 1))
    {
      Scierror ("Error:\t%s", ArgPosition (i));
      ArgName (stack, i);
      Scierror (" of function %s should be a 1x1 imatrix \n", NspFname(stack));
      return NULL;
    }
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
  nsp_itype itype = nsp_gint;
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


static int int_meth_imatrix_add(void *a,Stack stack,int rhs,int opt,int lhs)
{
  NspIMatrix *B;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((B = GetIMat (stack, 1)) == NULLIMAT) return RET_BUG;
  if ( nsp_imatrix_add(a,B) == FAIL )  return RET_BUG;
  return 0;
}

static int int_meth_imatrix_scale_rows(void *self, Stack stack,int rhs,int opt,int lhs)
{
  NspIMatrix *A = self, *x;
  CheckLhs(0,0);
  CheckRhs(1,1);

  if ((x = GetIMat (stack, 1)) == NULLIMAT) return RET_BUG;
  CheckVector(NspFname(stack),1,x);
  if ( x->mn != A->m )
    { 
      Scierror("%s: the argument should have %d components \n",NspFname(stack),A->m);
      return RET_BUG;
    }

  if ( nsp_imatrix_scale_rows(A, x) == FAIL )
    return RET_BUG;

  return 0;
}

/* 
 *  scale_cols[x]
 *
 *    A <- A*diag(x)
 *
 *    A.scale_cols[x]
 */

static int int_meth_imatrix_scale_cols(void *self, Stack stack,int rhs,int opt,int lhs)
{
  NspIMatrix *A =  self, *x;
  CheckLhs(0,0);
  CheckRhs(1,1);

  if ((x = GetIMat (stack, 1)) == NULLIMAT) return RET_BUG;
  CheckVector(NspFname(stack),1,x);
  if ( x->mn != A->n )
    { 
      Scierror("%s: the argument should have %d components \n",NspFname(stack),A->n);
      return RET_BUG;
    }

  if ( nsp_imatrix_scale_cols(A, x) == FAIL )
    return RET_BUG;

  return 0;
}

/* 
 * get_nnz 
 */

static int int_meth_imatrix_get_nnz(void *self, Stack stack,int rhs,int opt,int lhs)
{
  CheckLhs(0,0);
  CheckRhs(0,0);
  if ( nsp_move_double(stack,1,nsp_imatrix_nnz((NspIMatrix *) self)) == FAIL) return RET_BUG;
  return 1;
}

/* this method is also implemented in matint for matrix 
 * this one is a short cut.
 */

static int int_meth_imatrix_set_diag(NspObject *self, Stack stack, int rhs, int opt, int lhs) 
{
  NspIMatrix *Diag;
  int k=0;
  CheckRhs (1,2);
  CheckLhs (0,0); 
  if ((Diag = GetIMat (stack, 1)) == NULLIMAT)   return RET_BUG;
  if ( rhs == 2 )
    {
      if (GetScalarInt (stack,2 , &k) == FAIL)   return RET_BUG;
    }
  if (nsp_imatrix_set_diag ((NspIMatrix *) self, Diag, k) != OK)
    return RET_BUG;
  return 0;
}


static int int_meth_imatrix_has(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *A = self, *x;
  NspBMatrix *B;
  NspMatrix *Ind,*Ind2;
  
  CheckRhs(1,1); 
  CheckLhs(1,3);

  if ((x = GetIMat (stack, 1)) == NULLIMAT) return RET_BUG;

  if ( (B = nsp_imatrix_has(A, x, lhs, &Ind, &Ind2)) == NULLBMAT )
    return RET_BUG;

  MoveObj(stack,1,NSP_OBJECT(B));
  if ( lhs >= 2 )
    {
      MoveObj(stack,2,NSP_OBJECT(Ind));
      if ( lhs == 3 )
	MoveObj(stack,3,NSP_OBJECT(Ind2));
    }

  return Max(lhs,1);
}


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


static int int_imatrix_meth_itype(NspObject *self, Stack stack, int rhs, int opt, int lhs) 
{
  NSP_ITYPE_NAMES(names);
  char *st= NSP_ITYPE_NAME(names,((NspIMatrix *)self)->itype);
  CheckRhs(0,0);
  CheckLhs(1,1);
  if ( nsp_move_string(stack,1,st,-1) ==FAIL) return RET_BUG;
  return 1;
}

static NspMethods nsp_imatrix_methods[] = {
  { "add", int_meth_imatrix_add},
  { "scale_rows",int_meth_imatrix_scale_rows}, 
  { "scale_cols",int_meth_imatrix_scale_cols}, 
  { "get_nnz", int_meth_imatrix_get_nnz},
  { "has", int_meth_imatrix_has},
  { "set_diag",(nsp_method *) int_meth_imatrix_set_diag}, /* preferred to generic matint method */
  { "retype",(nsp_method *) int_imatrix_meth_retype},
  { "itype",(nsp_method *) int_imatrix_meth_itype},
  { NULL, NULL}
};

static NspMethods *imatrix_get_methods(void) { return nsp_imatrix_methods;}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static int int_imatrix_impl (Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *M;
  CheckRhs (2, 3);
  CheckLhs (1, 1);
  NspIMatrix  *First, *Last, *Step= NULL; 
  if ((First= GetScalarIMat(stack,1))==  NULLIMAT ) return RET_BUG;
  if ((Last= GetScalarIMat(stack,2))==  NULLIMAT ) return RET_BUG;
  if ( rhs -opt == 3) 
    {
      Step = Last;
      if ((Last= GetScalarIMat(stack,3))==  NULLIMAT ) return RET_BUG;
    }
  if ( First->itype != Last->itype) 
    {
      Scierror ("Error: %s should be used with elements of the same subtype\n", NspFname(stack));
      return RET_BUG;
    }
  if ( Step != NULL && First->itype != Step->itype) 
    {
      Scierror ("Error: %s should be used with elements of the same subtype\n", NspFname(stack));
      return RET_BUG;
    }
  if ((M = nsp_imatrix_create_int_impl (First,Step,Last)) == NULLIMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) M);
  return 1;
}


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
 * Operation leading to Boolean result 
 */

static int _int_imatrix_comp_gen (Stack stack, int rhs, int opt, int lhs, char *op)
{
  NspIMatrix *A, *B;
  NspBMatrix *Res;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((A = GetIMat (stack, 1)) == NULLIMAT)
    return RET_BUG;
  if ((B = GetIMat (stack, 2)) == NULLIMAT)
    return RET_BUG;
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return RET_BUG;
    }
  Res = nsp_imatrix_comp (A, B, op);
  if (Res == NULLBMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

/* A < B */
int int_imatrix_lt (Stack stack, int rhs, int opt, int lhs)
{
  return _int_imatrix_comp_gen(stack,rhs,opt,lhs,"<");
}

int int_imatrix_le (Stack stack, int rhs, int opt, int lhs)
{
  return _int_imatrix_comp_gen(stack,rhs,opt,lhs,"<=");
}

int int_imatrix_neq (Stack stack, int rhs, int opt, int lhs)
{
  return _int_imatrix_comp_gen(stack,rhs,opt,lhs,"<>");
}

int int_imatrix_eq (Stack stack, int rhs, int opt, int lhs)
{
  return _int_imatrix_comp_gen(stack,rhs,opt,lhs,"==");
}

int int_imatrix_gt (Stack stack, int rhs, int opt, int lhs)
{
  return _int_imatrix_comp_gen(stack,rhs,opt,lhs,">");
}


int int_imatrix_ge (Stack stack, int rhs, int opt, int lhs)
{
  return _int_imatrix_comp_gen(stack,rhs,opt,lhs,">=");
}

/*
 * Same but returns a unique boolean 
 */

static int
int_imatrix_f_gen (Stack stack, int rhs, int opt, int lhs, char *op)
{
  int rep, err;
  NspIMatrix *A, *B;
  NspObject *Res;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((A = GetIMat (stack, 1)) == NULLIMAT)
    return RET_BUG;
  if ((B = GetIMat (stack, 2)) == NULLIMAT)
    return RET_BUG;
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return RET_BUG;
    }

  rep = nsp_imatrix_fullcomp (A, B, op, &err);
  if (err == TRUE)
    {
      Scierror
	("Error: operator %s , arguments with incompatible dimensions\n", op);
      return RET_BUG;
    }
  if (rep == TRUE)
    {
      if ((Res = nsp_create_true_object (NVOID)) == NULLOBJ)
	return RET_BUG;
    }
  else
    {
      if ((Res = nsp_create_false_object (NVOID)) == NULLOBJ)
	return RET_BUG;
    }
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

int
int_imatrix_flt (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_f_gen (stack, rhs, opt, lhs, "<");
}

int
int_imatrix_fle (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_f_gen (stack, rhs, opt, lhs, "<=");
}

int
int_imatrix_fne (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_f_gen (stack, rhs, opt, lhs, "<>");
}

int
int_imatrix_feq (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_f_gen (stack, rhs, opt, lhs, "==");
}

int
int_imatrix_fgt (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_f_gen (stack, rhs, opt, lhs, ">");
}

int
int_imatrix_fge (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_f_gen (stack, rhs, opt, lhs, ">=");
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
 *  diag function 
 */

static int int_imatrix_diag (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspIMatrix *A, *Res;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((A = GetIMat (stack, 1)) == NULLIMAT)
    return RET_BUG;
  if (A->m == 1 || A->n == 1)
    Res = nsp_imatrix_create_diag (A, k1);
  else
    Res = nsp_imatrix_extract_diag (A, k1);
      
  if (Res == NULLIMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
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
  if (1) 
    {
      /* cast */
      if ((BM =nsp_matrix_to_imatrix(M,itype)) == NULLIMAT ) return RET_BUG;
    }
  else
    {
      /* min/max */
      if ((BM =nsp_matrix_to_imatrix_with_bounds(M,itype)) == NULLIMAT ) return RET_BUG;
    }
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
  rep =nsp_imatrix_fullcomp(A,B,"<>",&err);
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




/*
 * Returns a kroeneker product A.*.B 
 */

int
int_imatrix_kron (Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *HMat1, *HMat2, *HMat3;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((HMat1 = GetIMat (stack, 1)) == NULLIMAT)
    return RET_BUG;
  if ((HMat2 = GetIMat (stack, 2)) == NULLIMAT)
    return RET_BUG;
  if ((HMat3 = nsp_imatrix_kron (HMat1, HMat2)) == NULLIMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) HMat3);
  return 1;
}


/*
 * MatSort 
 * [A_sorted,Index]=sort(A, type,dir ) 
 *  type = "g"| "gs"| "gm"| "c"| "r"| "lr" | "lc" | "ldc"| "ldr"|"gb"|"gd"
 *  dir =  "i"| "d";
 */

static int int_imatrix_sort(Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *M=NULL;
  NspObject *Index=NULL;
  char *type_possible_choices[]={ "g", "gs", "gm", "c", "r", "lr" , "lc" , "ldc", "ldr","gb","gd", NULL };
  char *type=NULL;
  char *dir_possible_choices[]={ "i", "d",  NULL };
  char *dir=NULL;
  char *ind_type_possible_choices[]={ "double", "int",  NULL };
  char *ind_type=NULL;
  int iflag = FALSE;
  char direction = 'd', itype = 'd';
  int rep_type= sort_g, rep_dir, rep_ind_type;

  CheckRhs(1,4);
  CheckOptRhs(0,3)
  CheckLhs(0,2);

  if ((M=GetIMatCopy(stack,1)) == NULLIMAT ) return RET_BUG;

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
      nsp_imatrix_sort(M,&Index,iflag,direction,rep_type,itype);
      break;
    case sort_c:
      /* take care that c -> row */
      nsp_imatrix_row_sort(M,&Index,iflag,direction,itype);break;
    case sort_r:
      nsp_imatrix_column_sort(M,&Index,iflag,direction,itype);break;
    case sort_lr:
      nsp_imatrix_lexical_row_sort(M,&Index,iflag,direction,'i',itype);break;
    case sort_lc:
      nsp_imatrix_lexical_column_sort(M,&Index,iflag,direction,'i',itype);break;
    case sort_ldr:
      nsp_imatrix_lexical_row_sort(M,&Index,iflag,direction,'d',itype);
      break;
    case sort_ldc:
      nsp_imatrix_lexical_column_sort(M,&Index,iflag,direction,'d',itype);  break;
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
 * nsp_mat_sum: s=sum(a[,dim_flag])  or s = sum(a, dim=dim_flag)  
 * a is unchanged 
 */

typedef NspIMatrix *(*SuPro) (NspIMatrix * A, int dim);

static int
int_imatrix_sum_gen (Stack stack, int rhs, int opt, int lhs, SuPro F)
{
  int dim=0;
  NspIMatrix *Res, *HMat;
  CheckRhs(1, 2);
  CheckOptRhs(0, 1)
  CheckLhs(1, 1);

  if ((HMat = GetIMat (stack, 1)) == NULLIMAT)
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

  if ((Res = (*F) (HMat, dim)) == NULLIMAT)
    return RET_BUG;

  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

int
int_imatrix_sum (Stack stack, int rhs, int opt, int lhs)
{
  return (int_imatrix_sum_gen (stack, rhs, opt, lhs, nsp_imatrix_sum));
}

/*
 * matprod : product of all elements of a
 * a is unchanged 
 */

int
int_imatrix_prod (Stack stack, int rhs, int opt, int lhs)
{
  return (int_imatrix_sum_gen (stack, rhs, opt, lhs, nsp_imatrix_prod));
}

/*
 * matcusum : cumulative sum of all elements of a
 * a is unchanged 
 */

int
int_imatrix_cusum (Stack stack, int rhs, int opt, int lhs)
{
  return (int_imatrix_sum_gen (stack, rhs, opt, lhs, nsp_imatrix_cum_sum));
}

/*
 * matcuprod : cumulative prod of all elements of a
 * a is unchanged 
 */

int
int_imatrix_cuprod (Stack stack, int rhs, int opt, int lhs)
{
  return (int_imatrix_sum_gen (stack, rhs, opt, lhs, nsp_imatrix_cum_prod));
}


/*
 * A= ior(A,B) or ior(A,dim) or ior(A,dim=)
 */

int
int_imatrix_ior (Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *A, *B;
  CheckStdRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs - opt == 2)
    {
      if ( IsIMatObj(stack,2) )
	{
	  /* ior(A,B) */
	  if ((A = GetIMatCopy (stack, 1)) == NULLIMAT)
	    return RET_BUG;
	  NSP_OBJECT (A)->ret_pos = 1;
	  if ((B = GetIMat (stack, 2)) == NULLIMAT)
	    return RET_BUG;
	  if (SameDim (A, B))
	    {
	      if (nsp_imatrix_ior (A, B) == FAIL)
		return RET_BUG;
	    }
	  else
	    {
	      Scierror ("Error: %s Mat1 & Mat2 don't have same size \n",
			NspFname(stack));
	      return RET_BUG;
	    }
	  return 1;
	}
    }
  else
    {
      return (int_imatrix_sum_gen (stack, rhs, opt, lhs, nsp_imatrix_ior_unary));
    }
  return 1;
}


/*
 * A= iand(A,B) or iand(A,dim) or iand(A,dim=)
 */

int
int_imatrix_iand (Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *A, *B;
  CheckStdRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs - opt == 2)
    {
      if ( IsIMatObj(stack,2) )
	{
	  /* iand(A,B) */
	  if ((A = GetIMatCopy (stack, 1)) == NULLIMAT)
	    return RET_BUG;
	  NSP_OBJECT (A)->ret_pos = 1;
	  if ((B = GetIMat (stack, 2)) == NULLIMAT)
	    return RET_BUG;
	  if (SameDim (A, B))
	    {
	      if (nsp_imatrix_iand (A, B) == FAIL)
		return RET_BUG;
	    }
	  else
	    {
	      Scierror ("Error: %s Mat1 & Mat2 don't have same size \n",
			NspFname(stack));
	      return RET_BUG;
	    }
	  return 1;
	}
    }
  else
    {
      return (int_imatrix_sum_gen (stack, rhs, opt, lhs, nsp_imatrix_iand_unary));
    }
  return 1;
}



/*
 * diff
 *
 */
static int
int_imatrix_diff (Stack stack, int rhs, int opt, int lhs)
{
  int dim=0;
  int order=1;
  NspIMatrix *Res, *HMat;
  CheckRhs (1,3);
  CheckOptRhs(0,2)
  CheckLhs (1, 1);

  if ((HMat = GetIMat (stack, 1)) == NULLIMAT)
    return RET_BUG;

  if ( rhs > 1 )
    {
      if ( rhs == 3 && opt == 1 )
	{
	  Scierror ("Error:\t don't use both usual and named optional arguments (in function %s)\n", NspFname(stack));
	  return RET_BUG;
	}

      if ( opt == 0 )
	{
	  if ( GetScalarInt(stack, 2, &order) == FAIL )
	    return RET_BUG;
	  CheckNonNegative(NspFname(stack),order,2);

	  if ( rhs == 3 )
	    {
	      if ( GetDimArg(stack, 3, &dim) == FAIL )
		return RET_BUG;
	    }
	}
      else
	{
	  nsp_option opts[] ={{"dim",dim_arg,NULLOBJ,-1},
			      {"order",s_int,NULLOBJ,-1},
			      { NULL,t_end,NULLOBJ,-1}};
	  if ( get_optional_args(stack, rhs, opt, opts, &dim, &order) == FAIL )
	    return RET_BUG;
	}
    }

  if ( order < 0 )
    {
      Scierror ("Error:\t order should be non negative (in function %s)\n", NspFname(stack));
      return RET_BUG;
    }

  if ( dim == -1 )
    {
      Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
      return RET_BUG;
    }
  if ( dim == -2 )  /* matlab compatibility flag */
    dim = GiveMatlabDimFlag(HMat);
  
  
  if ((Res = nsp_imatrix_diff(HMat, order, dim)) == NULLIMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

/*
 *nsp_imatrix_maxi: Maxi(*HMat);
 * A is unchanged 
 */

typedef NspIMatrix *(*MiMax) (NspIMatrix * A, int dim, NspMatrix ** Imax,
			     int lhs);
typedef int (*MiMax1) (NspIMatrix * A, NspIMatrix * B, NspMatrix * Ind,
		       int j, int flag);

static int
int_imatrix_maxi_gen (Stack stack, int rhs, int opt, int lhs, MiMax F, MiMax1 F1)
{
  int dim = 0;
  NspIMatrix *A, *M, *B;
  NspMatrix *Imax;
  if (rhs < 1)
    {
      Scierror ("Error:\t Rhs must be >= 1 for function %s\n", NspFname(stack));
      return RET_BUG;
    }
  CheckLhs (1, 2);
  if (rhs == 1 || (rhs - opt ) == 1 || ( rhs == 2 && IsSMatObj(stack,2)) )
    {
      /* maxi(A), or maxi(A,str) or maxi(A,dim=options) idem for mini */
      if ((A = GetIMat (stack, 1)) == NULLIMAT)
	return RET_BUG;
      if (rhs == 2 )
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
	    dim = GiveMatlabDimFlag(A);
	}

      if ((M = (*F) (A, dim, &Imax, lhs)) == NULLIMAT)
	return RET_BUG;
      if (lhs == 2)
	{
	  MoveObj (stack, 2, (NspObject *) Imax);
	}
      MoveObj (stack, 1, (NspObject *) M);
    }
  else        
    {
      /* Maxi(A1,A2,....,An)   */
      NspMatrix *Ind=NULL;
      int flag = 0, i;

      if ( opt > 0 ) 
	{
	  Scierror ("Error:\t named optional argument not supported in this form of %s\n", NspFname(stack));
	  return RET_BUG;
	}

      if ((A = GetIMatCopy (stack, 1)) == NULLIMAT)
	return RET_BUG;
      NSP_OBJECT (A)->ret_pos = 1;
      if (lhs == 2)
	{
	  /* intialize Ind matrix * */
	  flag = 1;
	  if ((Ind = nsp_matrix_create (NVOID, 'r', A->m, A->n)) == NULLMAT)
	    return RET_BUG;
	  nsp_mat_set_rval (Ind, 1.0);
	}
      for (i = 2; i <= rhs; i++)
	{
	  if ((B = GetIMat (stack, i)) == NULLIMAT)
	    return RET_BUG;
	  if ((*F1) (A, B, Ind, i, flag) == FAIL)
	    return RET_BUG;
	}
      if (lhs == 2)
	{
	  MoveObj (stack, 2, (NspObject *) Ind);
	}
    }
  return Max (lhs, 1);
}


int
int_imatrix_maxi (Stack stack, int rhs, int opt, int lhs)
{
  return (int_imatrix_maxi_gen (stack, rhs, opt, lhs, nsp_imatrix_maxi, nsp_imatrix_maxitt1));
}


/*
 * nsp_imatrix_mini: Mini(A)
 * A is unchanged 
 * rs and ri are set to the result 
 */

int
int_imatrix_mini (Stack stack, int rhs, int opt, int lhs)
{
  return (int_imatrix_maxi_gen (stack, rhs, opt, lhs, nsp_imatrix_mini, nsp_imatrix_minitt1));
}


/* 
 *  [amin, amax, imin, imax] = minmax(A) or minmax(A,dir) or minmax(A,dim=dir)
 *  with dir = 'c','r','F','*' or 0, 1, 2
 *  to compute min and max at same time
 */

static int
int_imatrix_minmax(Stack stack, int rhs, int opt, int lhs)
{
  int dim = 0;
  NspIMatrix *A, *Amin, *Amax;
  NspMatrix *Imin, *Imax;
  CheckRhs(1,2);
  CheckOptRhs(0,1);
  CheckLhs (2, 4);

  if ((A = GetIMat (stack, 1)) == NULLIMAT)
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
	  Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n",
		    NspFname(stack));
	  return RET_BUG;
	}
      if ( dim == -2 )  /* matlab compatibility flag */
	dim = GiveMatlabDimFlag(A);
    }

  if ( nsp_imatrix_minmax(A, dim, &Amin, &Imin, &Amax, &Imax, lhs) == FAIL )
    return RET_BUG;
  
  MoveObj (stack, 1, (NspObject *) Amin);
  MoveObj (stack, 2, (NspObject *) Amax);
  if ( lhs >= 3 ) 
    {
      MoveObj (stack, 3, (NspObject *) Imin);
      if ( lhs == 4 )
	MoveObj (stack, 4, (NspObject *) Imax);
      else
	nsp_matrix_destroy(Imax);   /* if lhs >= 3 both Imin and Imax are allocated */
    }

  return Max (lhs, 1);
}

/*
 *nsp_imatrix_triu: A=Triu(a)
 * A is changed  
 */

int
int_imatrix_triu (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspIMatrix *HMat;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((HMat = GetIMatCopy (stack, 1)) == NULLIMAT)
    return RET_BUG;
  nsp_imatrix_triu (HMat, k1);
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

/*
 *nsp_imatrix_tril: A=Tril(A)
 * A is changed  
 */

int
int_imatrix_tril (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspIMatrix *HMat;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((HMat = GetIMatCopy (stack, 1)) == NULLIMAT)
    return RET_BUG;
  nsp_imatrix_tril (HMat, k1);
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

/*
 *nsp_imatrix_ones: A=ones(m,n)
 * A is created , m,n no
 */

typedef NspIMatrix *(*Mfunc) (int m, int n, nsp_itype itype);

/* generic function for ones,rand,eyes */

static int
int_imatrix_gen (Stack stack, int rhs, int opt, int lhs, Mfunc F)
{
  nsp_itype itype = nsp_gint;
  int m1, n1, last=rhs;
  NspIMatrix *HMat;
  CheckRhsMin(1);
  CheckLhs (1, 1);
  if ( IsSMatObj(stack,rhs) )
    {
      /* is last element an int type 
       */
      int rep;
      NSP_ITYPE_NAMES(names);
      if ((rep = GetStringInArray(stack, rhs, names, 0)) == -1 ) return RET_BUG;
      itype = (nsp_itype) rep;
      last = rhs-1;
    }
  if (last == 1)
    {
      NspMatrix *M;
      if ((M = GetRealMat (stack, 1)) == NULLMAT)
	return RET_BUG; 
      switch ( M->mn ) 
	{
	case 0: 
	  Scierror("Error: in %s, size vector cannot be an empty matrix\n",NspFname(stack));
	  return RET_BUG; 
	case 1: m1 = n1 = (int) M->R[0]; break;
	case 2: m1 = (int) M->R[0]; n1 = (int) M->R[1]; break;
	default :
	  Scierror("Error: in %s, size vector is too long\n",NspFname(stack));
	  return RET_BUG; 
	}
    }
  else 
    {
      if ( last > 2 ) 
	{
	  Scierror("Error: in %s, n-arrays are not implemented, max dimensions is two\n",
		   NspFname(stack));
	  return RET_BUG; 
	}
      if (GetScalarInt (stack, 1, &m1) == FAIL)
	return RET_BUG;
      CheckNonNegative(NspFname(stack),m1,1);
      if (last == 2 ) 
	{
	  if (GetScalarInt (stack, 2, &n1) == FAIL)
	    return RET_BUG;
	  CheckNonNegative(NspFname(stack),n1,2);
	}
      else 
	{
	  n1 = m1;
	}
    }
  if ((HMat = (*F) (m1, n1,itype)) == NULLIMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) HMat);
  return 1;
}


int
int_imatrix_iones (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_gen(stack, rhs, opt, lhs, nsp_imatrix_ones);
}

/*
 *nsp_imatrix_eye: A=Eye(m,n)
 * A is created  m,n no
 */


int
int_imatrix_ieye (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_gen(stack, rhs, opt, lhs, nsp_imatrix_eye);
}

/*
 *nsp_imatrix_zeros: A=zeros(m,n)
 * A is created  m,n no
 */


int
int_imatrix_izeros (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_gen(stack, rhs, opt, lhs, nsp_imatrix_zeros);
}


/*
 *nsp_imatrix_irand:
 * A=rand(m,n [,type])
 */

int
int_imatrix_irand (Stack stack, int rhs, int opt, int lhs)
{
  static char *Table_R[] = { "uniform", "normal", NULL };
  char *str;
  int m, n, m1, tkp, tk;
  NspIMatrix *A;
  NspObject *O;
  CheckRhs (0, 3);
  CheckLhs (1, 1);
  switch (rhs)
    {
    case 0:

      if ((A = nsp_imatrix_rand (1, 1,nsp_gint)) == NULLIMAT)
	return RET_BUG;
      MoveObj (stack, 1, (NspObject *) A);
      return 1;
    case 1:

      m = nsp_object_get_size (NthObj (1), 1);
      n = nsp_object_get_size (NthObj (1), 2);
      /* XXXX */
      if (IsSMatObj (stack, 1))
	{
	  int ind, type;
	  double seed;
	  static char *Table[] =
	    { "info", "seed", "uniform", "normal", NULL };
	  if ((ind = GetStringInArray (stack, 1, Table, 0)) == -1)
	    return RET_BUG;
	  switch (ind)
	    {
	    case 0:		/* rand('info'); * */
	      type = nsp_get_urandtype ();
	      if ((O = nsp_create_object_from_str(NVOID, type == 0 ? "uniform" : "normal")) ==
		  NULLOBJ)
		return RET_BUG;
	      MoveObj (stack, 1, O);
	      return 1;
	    case 1:		/* rand('seed') * */
	      seed = nsp_get_urandseed ();
	      if ((O =
		   nsp_create_object_from_double (NVOID, seed)) == NULLOBJ)
		return RET_BUG;
	      MoveObj (stack, 1, O);
	      return 1;
	    case 2:		/* rand('uniform') * */
	      nsp_set_urandtype (0);
	      return 0;
	    case 3:		/* rand('normal') * */
	      nsp_set_urandtype (1);
	      return 0;
	    }
	}
      else if (IsMatObj (stack, 1))
	{
	  /* rand(a); * */
	  if ((O = (NspObject *) nsp_imatrix_rand (m, n, nsp_gint)) == NULLOBJ)
	    return RET_BUG;
	  MoveObj (stack, 1, O);
	  return 1;
	}
      else
	{
	  Scierror
	    ("Error\t: function %s, First argument must be a Matrix or a String\n",
	     NspFname(stack));
	  return RET_BUG;
	}
      break;
    case 2:

      if (IsSMatObj (stack, 1))
	{
	  /* rand('seed',s) * */
	  if ((str = GetString (stack, 1)) == (char *) 0)
	    return RET_BUG;
	  if (strcmp (str, "seed") != 0)
	    {
	      Scierror ("rand:\t wrong first argument %s\n", str);
	      return RET_BUG;
	    }
	  if (GetScalarInt (stack, 2, &m1) == FAIL)
	    return RET_BUG;
	  nsp_set_urandseed (m1);
	  return 0;
	}
      else
	{
	  /* rand(m,n) * */
	  if (GetScalarInt (stack, 1, &m) == FAIL)
	    return RET_BUG;
	  CheckNonNegative(NspFname(stack),m,1);
	  if (GetScalarInt (stack, 2, &n) == FAIL)
	    return RET_BUG;
	  CheckNonNegative(NspFname(stack),n,2);
	  if ((A = nsp_imatrix_rand (m, n,nsp_gint)) == NULLIMAT)
	    return RET_BUG;
	  MoveObj (stack, 1, (NspObject *) A);
	  return 1;
	}
    default:

      /* rhs == 3 * */
      /* rand(m,n,type); * */
      if (GetScalarInt (stack, 1, &m) == FAIL)
	return RET_BUG;
      CheckNonNegative(NspFname(stack),m,1);
      if (GetScalarInt (stack, 2, &n) == FAIL)
	return RET_BUG;
      CheckNonNegative(NspFname(stack),n,2);
      tkp = nsp_get_urandtype ();
      /* locally change rand type * */
      if ((tk = GetStringInArray (stack, 3, Table_R, 0)) == -1)
	return RET_BUG;
      nsp_set_urandtype (tk);
      if ((O = (NspObject *) nsp_imatrix_rand (m, n,nsp_gint)) == NULLOBJ)
	return RET_BUG;
      MoveObj (stack, 1, O);
      /* restore rand type * */
      nsp_set_urandtype (tkp);
      return 1;
    }
  return 0;
}


/*
 *  A=op(A) 
 */

typedef int (*M11) (NspIMatrix * A);

/* generic function for ones,rand,eyes */

static int
int_imatrix_gen11 (Stack stack, int rhs, int opt, int lhs, M11 F)
{
  NspIMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetIMat (stack, 1)) == NULLIMAT)
    return RET_BUG;
  if (HMat->mn == 0)
    {
      NSP_OBJECT (HMat)->ret_pos = 1;
      return 1;
    }
  if ((HMat = GetIMatCopy (stack, 1)) == NULLIMAT)
    return RET_BUG;
  if (((*F) (HMat)) < 0)
    return RET_BUG;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

typedef void (*VM11) (NspIMatrix * A);

#if 0
static int
int_imatrix_genv11 (Stack stack, int rhs, int opt, int lhs, VM11 F)
{
  NspIMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetIMat (stack, 1)) == NULLIMAT)
    return RET_BUG;
  if (HMat->mn == 0)
    {
      NSP_OBJECT (HMat)->ret_pos = 1;
      return 1;
    }
  if ((HMat = GetIMatCopy (stack, 1)) == NULLIMAT)
    return RET_BUG;
  (*F) (HMat);
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}
#endif

/*
 * A=Abs(A), absolue value or module of each element 
 */

int
int_imatrix_abs (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_gen11 (stack, rhs, opt, lhs, nsp_imatrix_abs);
}

/*
 *nsp_mat_sign: A=Sign(A)
 * A is changed  
 * return 0 if error 
 */

int
int_imatrix_sign (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_gen11 (stack, rhs, opt, lhs, nsp_imatrix_sign);
}

/*
 *nsp_mat_minus: A=-(A)
 * A is changed  
 * return 0 if error 
 */

int
int_imatrix_minus (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_gen11 (stack, rhs, opt, lhs, nsp_imatrix_minus);
}





/*
 * A=ishift(A,n,['r'|'l'])
 */

int int_imatrix_ishift (Stack stack, int rhs, int opt, int lhs)
{
  int shift;
  char dir='l';
  NspIMatrix *A;
  CheckRhs(2,3);
  CheckLhs(1, 1);
  if ((A = GetIMatCopy (stack, 1)) == NULLIMAT)
    return RET_BUG;
  NSP_OBJECT (A)->ret_pos = 1;
 if (A->mn == 0)
    {
      NSP_OBJECT (A)->ret_pos = 1;
      return 1;
    }
  if (GetScalarInt (stack, 2, &shift) == FAIL) 
    return RET_BUG;
  if (rhs >= 3)
    {
      int rep;
      char *shift_options1[] = { "r", "l", NULL };
      if ((rep = GetStringInArray (stack, 3, shift_options1, 1)) == -1)
	return RET_BUG;
      dir = shift_options1[rep][0];
    }
  if ( nsp_imatrix_ishift(A,shift,dir) == FAIL)
    return RET_BUG;
  return 1;
}



/*
 * nsp_imatrix_mod: z = mod(x,y) x or y is changed 
 */

int
int_imatrix_mod(Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *x, *y;
  CheckRhs (2, 2);
  CheckLhs (1, 1);

  if ((x = GetIMat(stack, 1)) == NULLIMAT)
    return RET_BUG;

  if ((y = GetIMat(stack, 2)) == NULLIMAT)
    return RET_BUG;

  if ( x->mn == 1  &&  y->mn > 1 )
    {
      if ((y = GetIMatCopy(stack, 2)) == NULLIMAT)
	return RET_BUG;
      NSP_OBJECT(y)->ret_pos = 1;
    }
  else if ( y->mn != 1 && y->mn != x->mn )
    {
      Scierror ("Error: %s arguments have incompatible sizes \n", NspFname(stack));
      return RET_BUG;
    }
  else
    {
      if ((x = GetIMatCopy(stack, 1)) == NULLIMAT)
	return RET_BUG;
      NSP_OBJECT(x)->ret_pos = 1;
    }

  nsp_imatrix_mod(x, y);
  return 1;
}

/*
 *nsp_imatrix_modulo: A=Modulo(A) remainder in int division 
 * A is changed  
 */

int
int_imatrix_modulo (Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *A,*B;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((A = GetIMatCopy (stack, 1)) == NULLIMAT)
    return RET_BUG;
  if ((B = GetIMat(stack, 2)) == NULLIMAT)
    return RET_BUG;
  if ( B->mn != A->mn || B->mn != 1) 
    {
      Scierror("Error: second argument of %s should be 1x1 or %dx%d\n",
	       NspFname(stack),A->m,A->n);
      return RET_BUG;

    }
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: the two arguments of %s must have the same integer type\n",
	       NspFname(stack));
      return RET_BUG;
    }
  nsp_imatrix_modulo (A,B);
  NSP_OBJECT (A)->ret_pos = 1;
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

typedef int (*MPM) (NspIMatrix *, NspIMatrix *);

int
IMatNoOp (NspIMatrix * A)
{
  return OK;
}


static int
int_imatrix_mopscal (Stack stack, int rhs, int opt, int lhs, MPM F1, MPM F2,
		MPM F3, M11 F4, int flag)
{
  NspIMatrix *HMat1, *HMat2;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((HMat1 = GetIMatCopy (stack, 1)) == NULLIMAT)
    return RET_BUG;
  if (HMat1->mn == 0)
    {
      if (flag == 1)
	{
	  /* flag == 1 ==> [] op A  returns [] * */
	  NSP_OBJECT (HMat1)->ret_pos = 1;
	  return 1;
	}
      else
	{
	  /* flag == 1 ==> [] op A  returns F4(A) * */
	  if (F4 != IMatNoOp)
	    {
	      if ((HMat2 = GetIMatCopy (stack, 2)) == NULLIMAT)
		return RET_BUG;
	      if ((*F4) (HMat2) == FAIL)
		return RET_BUG;
	      NSP_OBJECT (HMat2)->ret_pos = 1;
	    }
	  else
	    {
	      if ((HMat2 = GetIMat (stack, 2)) == NULLIMAT)
		return RET_BUG;
	      NSP_OBJECT (HMat2)->ret_pos = 1;
	    }
	  return 1;
	}
    }
  if ((HMat2 = GetIMat (stack, 2)) == NULLIMAT)
    return RET_BUG;
  if (HMat2->mn == 0)
    {
      if (flag == 1)
	{
	  /* flag == 1 ==> A op [] returns [] * */
	  NSP_OBJECT (HMat2)->ret_pos = 1;
	  return 1;
	}
      else
	{
	  /* flag == 1 ==> A op [] returns A * */
	  NSP_OBJECT (HMat1)->ret_pos = 1;
	  return 1;
	}
    }
  if (HMat2->mn == 1)
    {
      if ((*F1) (HMat1, HMat2) != OK)
	return RET_BUG;
      NSP_OBJECT (HMat1)->ret_pos = 1;
    }
  else if (HMat1->mn == 1)
    {
      /* since Mat1 is scalar we store the result in Mat2 so we 
         must copy it * */
      if ((HMat2 = GetIMatCopy (stack, 2)) == NULLIMAT)
	return RET_BUG;
      if ((*F3) (HMat2, HMat1) != OK)
	return RET_BUG;
      NSP_OBJECT (HMat2)->ret_pos = 1;
    }
  else
    {
      if ((*F2) (HMat1, HMat2) != OK)
	return RET_BUG;
      NSP_OBJECT (HMat1)->ret_pos = 1;
    }
  return 1;
}

/*
 *
 *  A op scalar --->  F1(A,scalar)  result is of same dim than A
 *  A op B      --->  F2(A,B)       for A and B with same dims
 *  scalar op A --->  F3(A,scalar)  result is of same dim than A
 */

static int
int_imatrix_mopscal_mtlb(Stack stack, int rhs, int opt, int lhs, MPM F1, MPM F2, MPM F3)
{
  NspIMatrix *HMat1, *HMat2, *HMat3;
  int HMat1_has_no_name, HMat2_has_no_name;
  CheckRhs (2, 2);
  CheckLhs (1, 1);

  if ( (HMat1 =GetIMat(stack, 1)) == NULLIMAT )
    return RET_BUG;
  HMat1_has_no_name = Ocheckname(HMat1,NVOID);
  
  if ( (HMat2 =GetIMat(stack, 2)) == NULLIMAT )
    return RET_BUG;
  HMat2_has_no_name = Ocheckname(HMat2,NVOID);

  if ( HMat1->mn == 1 )
    {
      if ( HMat2->mn == 1 )
	{
	  if ( HMat1_has_no_name )
	    {
	      if ( (*F1)(HMat1, HMat2) == FAIL )
		return RET_BUG;
	      NSP_OBJECT (HMat1)->ret_pos = 1;
	    }
	  else if ( HMat2_has_no_name )
	    {
	      if ( (*F3)(HMat2, HMat1) == FAIL )
		return RET_BUG;
	      NSP_OBJECT (HMat2)->ret_pos = 1;
	    }
	  else
	    {
	      if ( (HMat3 =nsp_imatrix_copy(HMat1)) == NULLIMAT )
		return RET_BUG;
	      if ( (*F1)(HMat3, HMat2) == FAIL )
		return RET_BUG;
	      MoveObj(stack, 1, (NspObject *) HMat3);
	    }
	}
      else /* HMat2 is not a scalar */
	{
	  if ( HMat2_has_no_name )
	    {
	      if ( (*F3)(HMat2, HMat1) == FAIL )
		return RET_BUG;
	      NSP_OBJECT (HMat2)->ret_pos = 1;
	    }
	  else
	    {
	      if ( (HMat3 =nsp_imatrix_copy(HMat2)) == NULLIMAT )
		return RET_BUG;
	      if ( (*F3)(HMat3, HMat1) == FAIL )
		return RET_BUG;
	      MoveObj(stack, 1, (NspObject *) HMat3);
	    }
	}
    }
  else if ( HMat2->mn == 1 )
    {
      if ( HMat1_has_no_name )
	{
	  if ( (*F1)(HMat1, HMat2) == FAIL )
	    return RET_BUG;
	  NSP_OBJECT (HMat1)->ret_pos = 1;
	}
      else
	{
	  if ( (HMat3 =nsp_imatrix_copy(HMat1)) == NULLIMAT )
	    return RET_BUG;
	  if ( (*F1)(HMat3, HMat2) == FAIL )
	    return RET_BUG;
	  MoveObj(stack, 1, (NspObject *) HMat3);
	}
    }
  else
    {
      if ( HMat1_has_no_name )
	{
	  if ( (*F2)(HMat1, HMat2) == FAIL )
	    return RET_BUG;
	  NSP_OBJECT (HMat1)->ret_pos = 1;
	}
      else
	{
	  if ( (HMat3 =nsp_imatrix_copy(HMat1)) == NULLIMAT )
	    return RET_BUG;
	  if ( (*F2)(HMat3, HMat2) == FAIL )
	    return RET_BUG;
	  MoveObj(stack, 1, (NspObject *) HMat3);
	}
    }
  return 1;
}


/*
 * term to term addition 
 * with special cases Mat + [] and Mat + scalar
 */

int
int_imatrix_dadd (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_mopscal_mtlb(stack, rhs, opt, lhs,
			     nsp_imatrix_add_scalar_bis, nsp_imatrix_add_mat, 
			     nsp_imatrix_add_scalar_bis);

}


/*
 * term to term substraction 
 * with special cases Mat - [] and Mat - scalar
 *  XXXXX Attention le cas F3 est faux scalar - Mat --> Mat -scalar  
 */

int
int_imatrix_dsub (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_mopscal_mtlb(stack, rhs, opt, lhs,
			     nsp_imatrix_sub_scalar_bis, nsp_imatrix_sub_mat, 
			     nsp_scalar_sub_imatrix_bis);

}



/*
 * A=nsp_imatrix_pow_el(A,B), A.^ B 
 */
#if 0
int
int_imatrix_powel (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_mopscal_mtlb (stack, rhs, opt, lhs,
			      nsp_imatrix_pow_scalar, nsp_imatrix_pow_el,
			      nsp_imatrix_pow_scalarm);
}
#endif 

/*
 * A=DivEl(A,B),  A ./ B 
 */

int
int_imatrix_divel (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_mopscal_mtlb (stack, rhs, opt, lhs,
			      nsp_imatrix_div_scalar, nsp_imatrix_div_el,
			      nsp_imatrix_bdiv_scalar);
}


/*
 * A=BackDivEl(A,B),  A .\ B 
 */

int
int_imatrix_backdivel (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_mopscal_mtlb (stack, rhs, opt, lhs,
			 nsp_imatrix_bdiv_scalar, nsp_imatrix_bdiv_el,
			 nsp_imatrix_div_scalar);
}


/*
 * A=MultEl(A,B),  A .* B 
 */

int
int_imatrix_multel (Stack stack, int rhs, int opt, int lhs)
{
  return int_imatrix_mopscal_mtlb(stack, rhs, opt, lhs,
			     nsp_imatrix_mult_scalar_bis, nsp_imatrix_mult_el, 
			     nsp_imatrix_mult_scalar_bis);
}


/*
 * NspMatrix multiplication  Res= A*B  
 * very similar to mopscal but MatMult returns a new matrix 
 */

int int_imatrix_mult (Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *HMat1, *HMat2, *HMat3;
  CheckRhs (2, 2);
  CheckLhs (1, 1);

  if ( (HMat1 =GetIMat(stack, 1)) == NULLIMAT )
    return RET_BUG;
  if ( (HMat2 = GetIMat(stack, 2)) == NULLIMAT )
    return RET_BUG;

  if ( HMat1->mn == 1 )
    {
      if ( (HMat2 = GetIMatCopy(stack, 2)) == NULLIMAT )
	return RET_BUG;
      if ( nsp_imatrix_mult_scalar_bis(HMat2, HMat1) == FAIL )
	return RET_BUG;
      NSP_OBJECT(HMat2)->ret_pos = 1;
    }
  else if ( HMat2->mn == 1 )
    {
      if ( (HMat1 = GetIMatCopy(stack, 1)) == NULLIMAT )
	return RET_BUG;
      if ( nsp_imatrix_mult_scalar_bis(HMat1, HMat2) == FAIL )
	return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  else
    {
      if ( (HMat3 = nsp_imatrix_mult(HMat1, HMat2, 0)) == NULLIMAT )
	return RET_BUG;
      MoveObj(stack, 1, (NspObject *) HMat3);
    }
  return 1;
}

/*
 * NspMatrix special multiplication  Res= A*B or A'*B or A*B' or A'*B'   
 *   pmult(A,B [,flag])   flag is optional and should be 0, 1, 2 or 3
 *   with default 1
 */

int int_imatrix_pmult (Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *HMat1, *HMat2, *HMat3;
  int flag=1;

  CheckRhs (2, 3);
  CheckLhs (1, 1);

  if ( (HMat1 =GetIMat(stack, 1)) == NULLIMAT )
    return RET_BUG;
  if ( (HMat2 = GetIMat(stack, 2)) == NULLIMAT )
    return RET_BUG;

  if ( rhs == 3 )
    if (GetScalarInt (stack, 3, &flag) == FAIL) return RET_BUG;

  if ( flag < 0 || flag > 3 )
    {
      Scierror ("%s: third argument should be an integer in [0,3]\n", NspFname(stack));
      return RET_BUG;
    }

  if ( (HMat3 = nsp_imatrix_mult(HMat1, HMat2, flag)) == NULLIMAT )
    return RET_BUG;

  MoveObj(stack, 1, (NspObject *) HMat3);
  return 1;
}



/*
 * A / B 
 * just implemented for scalars XXXXX 
 * result stored in A 
 */

int int_imatrix_div (Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *HMat1, *HMat2;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((HMat1 = GetIMat (stack, 1)) == NULLIMAT)
    return RET_BUG;
  if ((HMat2 = GetIMat (stack, 2)) == NULLIMAT)
    return RET_BUG;
  if (HMat2->mn <= 1)
    {
      return int_imatrix_mopscal (stack, rhs, opt, lhs,
				       nsp_imatrix_div_scalar, nsp_imatrix_div_el,
				       nsp_imatrix_bdiv_scalar, IMatNoOp, 1);
    }
  else
    {
      Scierror ("%s: / not implemented for non 1x1 matrices\n", NspFname(stack));
      return RET_BUG;
    }
  return 1;
}


/*
 * returns in a Matrix the indices for which the Matrix is true 
 */

int
int_imatrix_find (Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *A=NULLIMAT, *val=NULLIMAT;;
  NspObject *Rc=NULLOBJ,*Rr=NULLOBJ;
  nsp_option opts[] ={{"ind_type",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  char *ind_type_possible_choices[]={ "double", "int",  NULL };
  char *ind_type=NULL;
  char itype = 'd';
  int rep;

  CheckRhs(1, 2);
  CheckOptRhs(0, 1)
  CheckLhs(1, 3);

  if ((A = GetIMat (stack, 1)) == NULLIMAT)
    return RET_BUG;

  if ( rhs == 2 )
    {
      if ( opt == 0 )
	{
	  if ( (ind_type = GetString(stack, 2)) == NULLSTRING )
	    return RET_BUG;
	}
      else
	{
	  if ( get_optional_args(stack, rhs, opt, opts, &ind_type) == FAIL )
	    return RET_BUG;
	}
      rep = is_string_in_array(ind_type, ind_type_possible_choices, 1);
      if ( rep < 0 )
	{
	  string_not_in_array(stack, ind_type, ind_type_possible_choices, "optional argument ind_type");
	  return RET_BUG;
	}
      itype = ind_type_possible_choices[rep][0];
    }

  if (nsp_imatrix_find (A, Max (lhs, 1), &Rr, &Rc, itype) == FAIL)
    return RET_BUG;

  if ( lhs >= 3 )
    {
      int i, *indr, *indc;
      if ((val = nsp_imatrix_create(NVOID,((NspMatrix *)Rc)->m,((NspMatrix *)Rc)->n,A->itype))== NULLIMAT)
	{
	  nsp_object_destroy(&Rr); 
	  nsp_object_destroy(&Rc);
	  return RET_BUG; 
	}

      if ( itype == 'd' )
	{
	  indr = ((NspMatrix *)Rr)->I;
	  indc = ((NspMatrix *)Rc)->I;
	}
      else
	{
	  indr = ((NspIMatrix *)Rr)->Gint;
	  indc = ((NspIMatrix *)Rc)->Gint;
	}

#define IMAT_FIND(name,type,arg)				\
      for ( i = 0 ; i < ((NspMatrix *)Rr)->mn; i++)		\
	val->name[i]= A->name[indr[i]-1 + A->m*(indc[i]-1)];	\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_FIND,"");
#undef IMAT_FIND
    }

  if ( itype == 'd' ) /* back convert */
    Rr = (NspObject *) Mat2double( (NspMatrix *) Rr);
  MoveObj (stack, 1, Rr);

  if ( lhs >= 2 )
    {
      if ( itype == 'd' ) /* back convert */
	Rc = (NspObject *) Mat2double( (NspMatrix *) Rc);
      MoveObj(stack,2, Rc);

      if ( lhs >= 3 )
	MoveObj(stack,3, (NspObject *)val);
    }

  return Max(lhs, 1) ;
}

/*
 *  multiple find (mfind)
 *
 *  [ind1,...,indk,indk+1] = mfind( x, op1, sc1, ...., opk, sck ) 
 *
 *    opj is a string defining a comparizon operator "<", "<=", ">", ">=", "==", "~=" or "<>"
 *
 *    x is a real matrix, scj a real scalar
 */
#if 0 
int
int_imatrix_mfind (Stack stack, int rhs, int opt, int lhs)
{
  NspIMatrix *x;
  NspMatrix **ind = NULL;
  int i, j, m;
  double *scalars = NULL;
  const char **ops = NULL;
  CheckRhs (3, 13);   /* limited to 6 tests */

  if ( (rhs - 1) % 2 != 0 )
    {
      Scierror ("%s: bad number of input arguments\n", NspFname(stack));
      return RET_BUG;
    }

  m = (rhs-1)/2;
  
  if ( (scalars = malloc(m*sizeof(double))) == NULL )
    goto err;
  if ( (ops = malloc(m*sizeof(char *))) == NULL )
    goto err;

  if ( (x = GetIMat(stack, 1)) == NULLIMAT )
    goto err;

  for ( i = 2, j = 0 ; i <= rhs ; i+=2, j++ )
    {
      if ( (ops[j] = GetString(stack, i)) == NULL )
	goto err;
      if ( GetScalarDouble(stack, i+1, &scalars[j]) == FAIL )
	goto err;
    } 

  if ( (ind = malloc((m+1)*sizeof(NspMatrix *))) == NULL )
    goto err;
  for ( i = 0 ; i <= m ; i++ ) ind[i] = NULLIMAT;

  if ( nsp_imatrix_mfind(x, m, ops, scalars, ind) == FAIL )
    goto err;

  for ( j = 0 ; j <= m ; j++ )
    MoveObj (stack, j+1, (NspObject *) ind[j]);

  free(scalars);
  free(ops);
  free(ind);
  return m+1;

 err:
  free(scalars);
  free(ops);
  free(ind);
  return RET_BUG;
}
#endif 

/*
 *  ndind2ind
 *
 *  [ind] = ndind2ind( dims, ind1, ind2, ..., indk)
 *
 *  dims must be a vector of length k
 */

#if 0
int
int_ndind2ind (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Dims, **ndind = NULL, *ind;
  int nd, *dims=NULL, i;
  CheckRhsMin (2);

  if ( (Dims = GetIMat(stack, 1)) == NULLIMAT )
    return RET_BUG;

  nd = Dims->mn; 

  if ( rhs - 1 != nd )
    {
      Scierror ("%s: waiting for %d index vectors, got %d\n", NspFname(stack), nd, rhs-1);
      return RET_BUG;
    }

  /* parse Dims */
  if ( (dims = malloc(nd*sizeof(int))) == NULL )
    {
      Scierror ("%s: running out of memory\n", NspFname(stack));
      return RET_BUG;
    }
  for ( i = 0 ; i < nd ; i++ )
    {
      dims[i] = (int) Dims->R[i];
      if ( dims[i] < 0 )
	{
	  Scierror ("%s: length in the %d th dimension is negative\n", NspFname(stack), i+1);
	  goto err;
	}
    }

  if ( (ndind = malloc((rhs-1)*sizeof(NspMatrix *))) == NULL )
    {
      Scierror ("%s: running out of memory\n", NspFname(stack));
      goto err;
    }

  for ( i = 2 ; i <= rhs ; i++ )
    {
      if ( (ndind[i-2] =  GetIMat(stack, i)) == NULLIMAT )
	goto err;
    } 

  if ( nsp_imatrix_ndind2ind(dims, nd, ndind, &ind) == FAIL )
    goto err;

  MoveObj (stack, 1, (NspObject *) ind);

  free(dims);
  free(ndind);
  return 1;

 err:
  free(dims);
  free(ndind);
  return RET_BUG;
}
#endif 


/*
 */

int int_imatrix_intmax (Stack stack, int rhs, int opt, int lhs)
{
  nsp_itype itype = nsp_gint;
  nsp_int_union val;
  NspIMatrix *R;
  CheckRhs (1, 1);
  CheckLhs (0, 1);
  if ( IsSMatObj(stack,1)) 
    {
      int rep;
      NSP_ITYPE_NAMES(names);
      if ((rep = GetStringInArray(stack,1, names, 0)) == -1 ) return RET_BUG;
      itype = (nsp_itype) rep;
    }
  else
    {
      NspIMatrix *A;
      if ((A = GetIMat (stack, 1)) == NULLIMAT) 
	return RET_BUG;
      itype = A->itype;
    }
  NSP_MAX_ITYPE(val,itype);
  if ((R = nsp_imatrix_create(NVOID,1,1,itype))== NULLIMAT)
    return RET_BUG;
#define IMAT_INTM(name,type,arg) R->name[0] = val.name;break; 
  NSP_ITYPE_SWITCH(itype,IMAT_INTM,"");
#undef IMAT_INTM
  MoveObj(stack,1,NSP_OBJECT(R));
  return Max(lhs, 1) ;
}

int
int_imatrix_intmin (Stack stack, int rhs, int opt, int lhs)
{
  nsp_itype itype = nsp_gint;
  nsp_int_union val;
  NspIMatrix *R;
  CheckRhs (1, 1);
  CheckLhs (0, 1);
  if ( IsSMatObj(stack,1)) 
    {
      int rep;
      NSP_ITYPE_NAMES(names);
      if ((rep = GetStringInArray(stack,1, names, 0)) == -1 ) return RET_BUG;
      itype = (nsp_itype) rep;
    }
  else
    {
      NspIMatrix *A;
      if ((A = GetIMat (stack, 1)) == NULLIMAT) 
	return RET_BUG;
      itype = A->itype;
    }
  NSP_MIN_ITYPE(val,itype);
  if ((R = nsp_imatrix_create(NVOID,1,1,itype))== NULLIMAT)
    return RET_BUG;
#define IMAT_INTM(name,type,arg) R->name[0] = val.name;break; 
  NSP_ITYPE_SWITCH(itype,IMAT_INTM,"");
#undef IMAT_INTM
  MoveObj(stack,1,NSP_OBJECT(R));
  return Max(lhs, 1) ;
}


static int int_unique( Stack stack, int rhs, int opt, int lhs)
{ 
  Boolean first_ind;
  NspIMatrix *x;
  NspMatrix *occ, **Occ=NULL;
  NspObject *ind, **Ind=NULL;
  int_types T[] = {imatcopy,new_opts,t_end} ;
  nsp_option opts[] ={{ "first_ind",s_bool,NULLOBJ,-1},
		      { "ind_type",string,NULLOBJ,-1},
                      { "which",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  char *ind_type=NULL, itype='d', *ind_type_possible_choices[]={ "double", "int",  NULL };
  char *which=NULL, iwhich='e', *which_possible_choices[]={ "elements", "rows", "columns", NULL };
  int rep_ind_type, rep_which;

  if ( GetArgs(stack,rhs,opt,T,&x,&opts,&first_ind,&ind_type, &which) == FAIL ) 
    return RET_BUG;

  if ( opts[0].obj == NULLOBJ) first_ind = FALSE;
  
  if ( ind_type != NULL )
    {
      if ( (rep_ind_type= is_string_in_array(ind_type, ind_type_possible_choices,1)) == -1 ) 
	{
	  string_not_in_array(stack, ind_type, ind_type_possible_choices, "optional argument ind_type");
	  return RET_BUG;
	} 
      itype = ind_type_possible_choices[rep_ind_type][0];
    }
  
  if ( which != NULL )
    {
      if ( (rep_which= is_string_in_array(which, which_possible_choices,0)) == -1 ) 
	{
	  string_not_in_array(stack, which, which_possible_choices, "optional argument which");
	  return RET_BUG;
	} 
      iwhich = which_possible_choices[rep_which][0];
    }

  CheckLhs(1,3);

  if ( lhs >= 2 )
    {
      Ind = &ind;
      if ( lhs == 3 ) Occ = &occ;
    }

  if ( iwhich == 'e' )
    {
      if ( nsp_imatrix_unique(x, Ind, Occ, first_ind, itype) == FAIL )
	return RET_BUG;
    }
  else if ( iwhich == 'r' )
    {
      if ( nsp_imatrix_unique_rows(x, Ind, Occ, itype) == FAIL )
	return RET_BUG;
    }
  else /*  iwhich == 'c'  */
    {
      if ( nsp_imatrix_unique_columns(x, Ind, Occ, itype) == FAIL )
	return RET_BUG;
    }

  NSP_OBJECT(x)->ret_pos = 1; 
  if ( lhs >= 2 ) 
    {
      MoveObj(stack,2,NSP_OBJECT(ind));
      if ( lhs >= 3 )
	MoveObj(stack,3,NSP_OBJECT(occ));
    }

  return Max(lhs,1);
}


nsp_string nsp_dec2base( guint64 n,const char *str_base )
{
  nsp_string str=NULL;
  char basic[32]; 
  int base = strlen(str_base), count=0, i;
  /* check that str_base contains unique symbols ? */
  while (1)
    {
      int rem = n % base;
      basic[count]= str_base[rem];
      n /= base; 
      count++;
      if ( n == 0) break;
    }
  if ((str = new_nsp_string_n(count))==NULL) return NULL;
  for ( i = 0 ; i < count ; i++ )   str[i]= basic[count-i-1];
  str[count]='\0';
  return str;
}

int int_dec2base(Stack stack, int rhs, int opt, int lhs)
{
  nsp_string str=NULL;
  const char tab[]="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  const char base_def[]="01";
  const char *base=base_def;
  char *bbase = NULL;
  int i;
  NspSMatrix *M=NULL;
  NspMatrix  *A=NULL;
  NspIMatrix  *IA=NULL;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ( IsIMatObj(stack,1) )
    {
      if (( IA = GetIMat(stack,1))  == NULLIMAT) return RET_BUG;
      if ( IA->itype != nsp_guint64) 
	{
	  Scierror ("Error: integer matrix argument for %s should be of uint64 subtype\n", NspFname(stack));
	  return RET_BUG;
	}
    }
  else 
    {
      if ((A = GetMat(stack,1))  == NULLMAT) return RET_BUG;
    }
  
  if ( rhs == 2 )
    {
      if ( IsMatObj(stack,2) )
	{
	  int ibase; 
	  if (GetScalarInt(stack,2,&ibase) == FAIL) return RET_BUG;
	  if ( !(ibase >= 2 && ibase <= 36 )) 
	    {
	      Scierror("Error: base should be in [2,36]\n");
	      return RET_BUG;
	    }
	  if ((bbase = new_nsp_string_n(ibase))==NULL) return RET_BUG;
	  strncpy(bbase,tab,ibase);
	  bbase[ibase]='\0';
	  base = bbase;
	}
      else if ( IsSMatObj(stack,2))
	{
	  if ((str = GetString(stack,2)) == (char*)0) return RET_BUG;
	  if ( strlen(str) <= 1) return RET_BUG;
	  base = str;
	}
      else
	{
	  Scierror("Error: expecting an integer or a string for base\n");
	  return RET_BUG;
	}
    }

  if ( A != NULL ) 
    {
      if ( ( M =nsp_smatrix_create_with_length(NVOID,A->m,A->n,-1)) 
	   == NULLSMAT) return RET_BUG;
      for ( i = 0 ; i < A->mn ; i++ )
	{
	if ((str =nsp_dec2base((guint64) A->R[i],base)) == NULL) 
	  {
	    nsp_smatrix_destroy(M);
	    return RET_BUG;
	  }
	M->S[i]= str ;
	}
    }
  else
    {
      if ( ( M =nsp_smatrix_create_with_length(NVOID,IA->m,IA->n,-1)) 
	   == NULLSMAT) return RET_BUG;
      for ( i = 0 ; i < IA->mn ; i++ )
	{
	  if ((str =nsp_dec2base( IA->Guint64[i],base)) == NULL) 
	    {
	      nsp_smatrix_destroy(M);
	      return RET_BUG;
	    }
	  M->S[i]= str ;
	}
    }
  MoveObj(stack,1,NSP_OBJECT(M));
  if ( bbase != NULL ) nsp_string_destroy(&bbase);
  return 1;
}

/**
 * nsp_base2dec:
 * @n: 
 * @base: 
 * 
 * 
 * 
 * Returns: 
 **/

double nsp_base2dec(const char *n, int base)
{
  int len = strlen(n), i, code;
  double res = 0;
  for ( i = 0 ; i < len  ; i++ )
    {
      code = n[i];
      if ( code >= '0' && code <= '9') 
	code -= '0';
      else if ( code >= 'a' && code <= 'z') 
	code -= 'a' - 10;
      else if ( code >= 'A' && code <= 'Z') 
	code -= 'A' - 10;
      else 
	code = 0;
      res = base*res + code;
    }
  return res;
}

double nsp_strbase2dec(const char *n, const char *base)
{
  int len = strlen(n), i,j;
  int lenb=strlen(base);
  double res = 0;
  for ( i = 0 ; i < len  ; i++ )
    {
      int ok=0;
      for ( j = 0 ; j < lenb ; j++)
	if ( n[i]== base[j]) 
	  {
	    res = lenb*res + j;
	    ok = 1;
	  }
      if ( ok == 0 ) return 0.0/0.0; 
    }
  return res;
}


int int_base2dec(Stack stack, int rhs, int opt, int lhs)
{
  char *strbase = NULL;
  int i,base=2;
  NspSMatrix *M=NULL;
  NspMatrix  *A;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((M = GetSMat(stack,1))  == NULLSMAT) return RET_BUG;
  if ( rhs == 2 )
    {
      if ( IsMatObj(stack,2) )
	{
	  if (GetScalarInt(stack,2,&base) == FAIL) return RET_BUG;
	  if ( base < 2 || base > 36 ) 
	    {
	      Scierror("base should be in [2,36]\n");
	      return RET_BUG;
	    }
	}
      else if ( IsSMatObj(stack,2))
	{
	  if ((strbase = GetString(stack,2)) == (char*)0) return RET_BUG;
	  if ( strlen(strbase) <= 1) return RET_BUG;
	}
      else
	{
	  Scierror("Error: expecting an integer or a string for base\n");
	  return RET_BUG;
	}
    }
  if ((A = nsp_matrix_create(NVOID,'r',M->m,M->n)) == NULLMAT)
    return RET_BUG;
  if ( strbase != NULL )
    for ( i = 0 ; i < A->mn ; i++) 
      A->R[i]= nsp_strbase2dec(M->S[i],strbase);
  else
    for ( i = 0 ; i < A->mn ; i++) 
      A->R[i]= nsp_base2dec(M->S[i], base);
  MoveObj(stack,1,NSP_OBJECT(A));
  return 1;
}




/*
 * The Interface for basic matrices operation 
 */

static OpTab IMatrix_func[]={
  {"impl_i", int_imatrix_impl},
  {"dec2base", int_dec2base},
  {"base2dec", int_base2dec},
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
  {"diag_i", int_imatrix_diag},
  {"diag_i_m", int_imatrix_diag},
  {"diagcre_i",int_imatrix_diagcre},
  {"diage_i",int_imatrix_diage},
  {"find_i",int_imatrix_find},
  {"m2i",int_imatrix_m2i},
  {"redim_i",int_matint_redim}, 
  {"reshape_i",int_matint_redim}, 
  {"matrix_i", int_matint_redim},
  {"resize_i",int_imatrix_resize},
  {"eq_i_i" ,  int_imatrix_eq },
  {"ne_i_i" ,  int_imatrix_neq },
  {"fneq_i_i" ,  int_imatrix_fneq },
  {"feq_i_i" ,  int_imatrix_feq },
  {"quote_i", int_imatrix_quote},
  {"fge_i_i", int_imatrix_fge},
  {"fgt_i_i", int_imatrix_fgt},
  {"fle_i_i", int_imatrix_fle},
  {"flt_i_i", int_imatrix_flt},
  {"fne_i_i", int_imatrix_fne},
  {"ge_i_i", int_imatrix_ge},
  {"gt_i_i", int_imatrix_gt},
  {"le_i_i", int_imatrix_le},
  {"lt_i_i", int_imatrix_lt},
  {"find_i", int_imatrix_find},
  {"sort_i", int_imatrix_sort},
  {"gsort_i", int_imatrix_sort},
  {"diff_i", int_imatrix_diff},
  {"minmax_i", int_imatrix_minmax},  
  {"max_i", int_imatrix_maxi},
  {"max_i_i", int_imatrix_maxi},
  {"min_i", int_imatrix_mini},
  {"min_i_i", int_imatrix_mini},
  {"sum_i_s", int_imatrix_sum},
  {"sum_i", int_imatrix_sum},
  {"cumsum_i_s", int_imatrix_cusum},
  {"cumsum_i", int_imatrix_cusum},
  {"prod_i_s", int_imatrix_prod},
  {"prod_i", int_imatrix_prod},
  {"cumprod_i_s", int_imatrix_cuprod},
  {"cumprod_i", int_imatrix_cuprod},
  {"tril_i", int_imatrix_tril},
  {"triu_i", int_imatrix_triu},
  {"mult_i_i", int_imatrix_mult},
  {"pmult_i_i", int_imatrix_pmult},
  {"iand_i", int_imatrix_iand},
  {"ior_i", int_imatrix_ior},
  {"ishift_i", int_imatrix_ishift},
  /* XXX */
  {"ieye", int_imatrix_ieye},
  {"iones", int_imatrix_iones},
  {"izeros", int_imatrix_izeros},
  {"intmax", int_imatrix_intmax},
  {"intmin", int_imatrix_intmin},
  {"abs_i", int_imatrix_abs},
  {"modulo_i_i", int_imatrix_modulo},
  {"mod_i_i", int_imatrix_mod},
  {"dsl_i_i", int_imatrix_divel},
  {"dprim_i", int_imatrix_quote},
  {"unique_i", int_unique}, 
  {"plus_i_i", int_imatrix_dadd},
  {"minus_i_i", int_imatrix_dsub},
  {"minus_i", int_imatrix_minus},

#if 0
  {"dst_i_i", int_imatrix_iultel}, 
  {"dstd_i_i", int_imatrix_kron},	/* operator:  .*. */
  {"idiv_i_i", int_imatrix_idiv},
  {"bdiv_i_i", int_imatrix_bdiv},
  {"int_i", int_imatrix_int},
  {"sign_i", int_imatrix_sign},
  {"hat_i_i", int_imatrix_pow},
  {"dh_i_i", int_imatrix_powel},
  {"dbs_i_i", int_imatrix_backdivel},
  {"div_i_i", int_imatrix_div},
  {"mfind_i", int_imatrix_mfind},
  {"nnz_i",  int_matrix_nnz},
  {"cross_i_i", int_imatrix_cross},
  {"dot_i_i", int_imatrix_dot},
  {"issorted_i", int_imatrix_issorted},
  {"scale_rows_i_i", int_imatrix_scale_rows},
  {"scale_cols_i_i", int_imatrix_scale_cols},
#endif 

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



