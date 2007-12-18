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

/**
 * SECTION:bmatrix
 * @title: NspBMatrix
 * @short_description: An object used to implement boolean matrices.
 * @see_also: 
 *
 * <para>
 * A #NspBMatrix is used to represent a boolean matrix. 
 * It can be filled with %TRUE and %FALSE values and is 
 * implemented as an array of integer. It implement the 
 * matint interface which is used for generic matrices 
 * operations. When using the matint interface a #NspBMatrix 
 * can always be casted to a #NspSMatrix.
 * </para>
 **/

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
  top->latex = (print_func *) nsp_bmatrix_latex_print;
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
  /* mati->redim = (matint_redim *) nsp_bmatrix_redim; use default value */
  mati->resize = (matint_resize  *) nsp_bmatrix_resize;
  mati->free_elt = (matint_free_elt *) 0; /* nothing to do */
  mati->elt_size = (matint_elt_size *) nsp_bmatrix_elt_size ;
  mati->clone = (matint_clone *) nsp_bmatrix_clone ;
  mati->copy_elt = (matint_copy_elt *) 0; /* nothing to do */
  mati->enlarge = (matint_enlarge *) nsp_bmatrix_enlarge;
  mati->canonic =  nsp_matint_canonic;

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

static char *bmatrix_type_short_string(NspObject *v)
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
  if ( ! ( ((NspBMatrix *) A)->m == ((NspBMatrix *) B)->m 
	   && ((NspBMatrix *) A)->n == ((NspBMatrix *) B)->n)) return FALSE;
  rep = nsp_bmatrix_full_compare (A, (NspBMatrix *) B, "==", &err);
  if (err == TRUE)
    return FALSE;
  return rep;
}

static int bmatrix_neq(NspBMatrix *A, NspObject *B)
{
   return ( bmatrix_eq(A,B) == TRUE ) ? FALSE : TRUE ;
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

static int bmatrix_xdr_save(XDR *xdrs, NspBMatrix *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->m) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->n) == FAIL) return FAIL;
  if (nsp_xdr_save_array_i(xdrs,M->B,M->mn) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

static NspBMatrix  *bmatrix_xdr_load(XDR *xdrs)
{
  int m,n;
  NspBMatrix *M;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLBMAT;
  if (nsp_xdr_load_i(xdrs,&m) == FAIL) return NULLBMAT;
  if (nsp_xdr_load_i(xdrs,&n) == FAIL) return NULLBMAT;
  if (( M=nsp_bmatrix_create(name,m,n)) == NULLBMAT ) return NULLBMAT;
  if (nsp_xdr_load_array_i(xdrs,M->B,M->mn) == FAIL) return NULLBMAT;
  return M;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for BMatrix objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

/**
 * BMatObj:
 * @O: a #NspObject 
 * 
 * checks if @O can be casted to a #BMatObj. 
 * 
 * Return value: %NULL or a #BMatObj.
 **/

NspBMatrix   *BMatObj(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_bmatrix_id) == TRUE) return ((NspBMatrix *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_bmatrix));
  return(NULL);
}

/**
 * IsBMatObj:
 * @stack: a #Stack
 * @i: an integer 
 * 
 * checks if #NspObject at position @i on the stack can be casted to a #BMatObj.
 * 
 * Return value: %TRUE or %FALSE
 **/

int IsBMatObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_bmatrix_id);
}

/**
 * IsBMat:
 * @O: a #NspObject 
 * 
 * checks if object @O can be casted to a #BMatObj.
 * 
 * Return value:  %TRUE or %FALSE
 **/

int IsBMat(const NspObject *O)
{
  return nsp_object_type(O , nsp_type_bmatrix_id);
}

/**
 * GetBMatCopy:
 * @stack: a #Stack
 * @i: an integer 
 * 
 * checks if #NspObject at position @i on the stack can be casted to a #BMatObj and
 * if true returns a copy of that boolean matrix.
 * 
 * Return value: %NULL or a #NspBMatrix
 **/

NspBMatrix  *GetBMatCopy(Stack stack, int i)
{
  if (  GetBMat(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/**
 * GetBMat:
 * @stack: a #Stack
 * @i: an integer 
 * 
 * checks if #NspObject at position @i on the stack can be casted to a #BMatObj and
 * if true returns the boolean matrix.
 * 
 * Return value:  %NULL or a #NspBMatrix
 **/

NspBMatrix  *GetBMat(Stack stack, int i)
{
  NspBMatrix *M;
  if (( M = BMatObj(NthObj(i))) == NULLBMAT)
    ArgMessage(stack,i);
  return M;
}

/**
 * BoolScalar:
 * @O: a #NspObject 
 * @val: a pointer to #Boolean
 * 
 * checks if object @O is a boolean scalar and if true returns 
 * the boolean value in @val.
 * 
 * Return value: %OK or %FAIL 
 **/

int BoolScalar(NspObject *O, Boolean *val)
{
  static char mess[]="Argument should be a boolean";
  NspBMatrix *A;
  if (( A= BMatObj(O)) == NULLBMAT
      || ( A->mn != 1 ))
    { Scierror(mess); return(FAIL);}
  *val = A->B[0];
  return(OK);
}

/**
 * GetScalarBool:
 * @stack: a #Stack
 * @i: an integer 
 * @val: an int pointer 
 * 
 * checks if #NspObject at position @i on the stack can be casted to a
 * boolean scalar and returns that boolean value in @val.
 * 
 * Return value:  %OK or %FAIL 
 **/
int GetScalarBool(Stack stack, int i, int *val)
{
  NspBMatrix *M;
  if (( M = BMatObj(NthObj(i))) == NULLBMAT 
      || ( M->mn != 1 )) 
    {
      Scierror("Error:\t%s", ArgPosition(i));
      ArgName(stack,i);
      Scierror(" of function %s should be an integer\n",NspFname(stack));
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
  int rep = RET_BUG;
  NspMatrix *Mat2 = NULLMAT;
  NspBMatrix *HMat1=NULLBMAT,*HMat2=NULLBMAT,*B=NULLBMAT;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetBMatCopy(stack,1)) == NULLBMAT) goto bug;
  if (0 &&  IsMatObj(stack,2) )
    {
      /* remove 0 above to accept Matrix */
      if ((Mat2 = GetMat(stack,2)) == NULLMAT) goto bug;
      if ((HMat2 = B = nsp_matrix_to_bmatrix(Mat2)) == NULLBMAT ) goto bug;
    }
  else 
    {
      if ((HMat2 = GetBMat(stack,2)) == NULLBMAT) goto bug;
    }
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
      if ( (*F1)(HMat1,HMat2) != OK) goto bug;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  else if ( HMat1->mn == 1 ) 
    {
      /* since Mat1 is scalar we store the result in Mat2 so we 
       * must copy it. If B is non null Mat2 was already previously created. 
       */
      if ( B== NULL && (HMat2 = GetBMatCopy(stack,2)) == NULLBMAT) goto bug;
      if ( (*F1)(HMat2,HMat1) != OK) goto bug;
      NSP_OBJECT(HMat2)->ret_pos = 1;
    }
  else 
    {
      if ( (*F2)(HMat1,HMat2) != OK) goto bug;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  rep = 1;
 bug: 
  /* destroy B if it is not returned */
  if ( B != NULLBMAT && NSP_OBJECT(B)->ret_pos != 1) nsp_bmatrix_destroy(B);
  return rep;
}

/*
 * and(A,B)
 * and(A,dim=) 
 * A and B must have the same size except if A or B is scalar 
 * or A and B are []
 */

static int int_bmatrix_and(Stack stack, int rhs, int opt, int lhs)
{
  int dim=0,i,j;
  NspObject *Obj = NULLOBJ;
  NspBMatrix *HMat1,*HMat;
  nsp_option opts[] ={{"dim",obj,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,2);
  CheckLhs(1,1);
  if ((HMat1 = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  if (rhs - opt == 2)
    {
      /* or(A,B); */
      return int_bmatrix__and_or(stack,rhs,opt,lhs,nsp_bmatrix_scalar_and,nsp_bmatrix_and);
    }
  /* and(A,dim=) */
  if ( get_optional_args(stack, rhs, opt, opts, &Obj) == FAIL )
    return RET_BUG;
  if ( Obj != NULL) 
    {
      if ( GetDimArg(stack, opts[0].position, &dim) == FAIL ) return RET_BUG;
      if ( dim == -1 )
	{
	  Scierror ("Error:\t dim flag -1 or '.' not supported for function %s\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( dim == -2 )  /* matlab compatibility flag */
	dim = GiveMatlabDimFlag(HMat1);
    }
  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n", dim);
    case 0 : 
      if ((HMat =nsp_bmatrix_create(NVOID,1,1)) == NULLBMAT) return RET_BUG;
      HMat->B[0] = NSP_OBJECT(HMat1)->type->is_true(HMat1);
      break;
    case 1:   /* row */
      if ((HMat =nsp_bmatrix_create(NVOID,1,HMat1->n)) == NULLBMAT) return RET_BUG;
      for ( j= 0 ; j < HMat1->n ; j++)
	{
	  HMat->B[j] = TRUE; 
	  for ( i = 0 ; i < HMat1->m ; i++) 
	    if (  HMat1->B[i+HMat1->m*j]== FALSE ) { HMat->B[j] = FALSE; break;}
	}
      break;
    case 2:  /* column */
      if ((HMat =nsp_bmatrix_create(NVOID,HMat1->m,1)) == NULLBMAT) return RET_BUG;
      for ( i= 0 ; i < HMat1->m ; i++)
	{
	  HMat->B[i] = TRUE; 
	  for ( j = 0 ; j < HMat1->n ; j++) 
	    if ( HMat1->B[i+HMat1->m*j] == FALSE) {HMat->B[i] = FALSE;break;}
	}
      break;
    }

  MoveObj(stack,1, (NspObject *)HMat);  
  return 1;
}

/*
 * or(A,B)
 * or(A,dim=) 
 * A and B must have the same size except if A or B is scalar 
 * or A and B are []
 */

static int int_bmatrix_or(Stack stack, int rhs, int opt, int lhs)
{
  int dim=0,i,j;
  NspBMatrix *HMat1,*HMat=NULLBMAT;
  nsp_option opts[] ={{"dim",dim_arg,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,2);
  CheckLhs(1,1);
  if ((HMat1 = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  if (rhs - opt == 2)
    {
      /* or(A,B); */
      return int_bmatrix__and_or(stack,rhs,opt,lhs,nsp_bmatrix_scalar_or,nsp_bmatrix_or);
    }
  /* or(A,dim=) */
  if ( get_optional_args(stack, rhs, opt, opts, &dim) == FAIL )
    return RET_BUG;

  if ( dim == -1 )
    {
      Scierror ("Error:\t dim flag -1 or '.' not supported for function %s\n", NspFname(stack));
      return RET_BUG;
    }
  if ( dim == -2 )  /* matlab compatibility flag */
    dim = GiveMatlabDimFlag(HMat1);

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n", dim);
    case 0 : 
      if ((HMat =nsp_bmatrix_create(NVOID,1,1)) == NULLBMAT) return RET_BUG;
      HMat->B[0] = FALSE;
      for ( i=0; i < HMat1->mn ; i++ ) 
	{
	  if ( HMat1->B[i] != FALSE ) {  HMat->B[0] = TRUE;   break;  }
	}
      break;
      
    case 1:   /* row */
      if ((HMat =nsp_bmatrix_create(NVOID,1,HMat1->n)) == NULLBMAT) return RET_BUG;
      for ( j= 0 ; j < HMat1->n ; j++)
	{
	  HMat->B[j] = FALSE; 
	  for ( i = 0 ; i < HMat1->m ; i++) 
	    if ( HMat1->B[i+HMat1->m*j]!= FALSE ) {HMat->B[j] = TRUE; break;}
	}
      break;

    case 2:  /* column */
      if ((HMat =nsp_bmatrix_create(NVOID,HMat1->m,1)) == NULLBMAT) return RET_BUG;
      for ( i= 0 ; i < HMat1->m ; i++)
	{
	  HMat->B[i] = FALSE; 
	  for ( j = 0 ; j < HMat1->n ; j++) 
	    if ( HMat1->B[i+HMat1->m*j] != FALSE ){ HMat->B[i] = TRUE; break;}
	}
      break;
    }
  MoveObj(stack,1, (NspObject *)HMat);  
  return 1;
}


/* static int int_bmatrix_or(Stack stack, int rhs, int opt, int lhs) */
/* { */
/*   int dim=0,i,j; */
/*   NspBMatrix *HMat1,*HMat=NULLBMAT; */
/*   NspObject *Obj=NULLOBJ; */
/*   nsp_option opts[] ={{"dim",obj,NULLOBJ,-1}, */
/* 		      { NULL,t_end,NULLOBJ,-1}}; */
/*   CheckStdRhs(1,2); */
/*   CheckLhs(1,1); */
/*   if ((HMat1 = GetBMat(stack,1)) == NULLBMAT) return RET_BUG; */
/*   if (rhs - opt == 2) */
/*     { */
/*       /\* or(A,B); *\/ */
/*       return int_bmatrix__and_or(stack,rhs,opt,lhs,nsp_bmatrix_scalar_or,nsp_bmatrix_or); */
/*     } */
/*   /\* or(A,dim=) *\/ */
/*   if ( get_optional_args(stack, rhs, opt, opts, &Obj) == FAIL ) */
/*     return RET_BUG; */
/*   if ( Obj != NULL)  */
/*     { */
/*       if ( GetDimArg(stack, opts[0].position, &dim) == FAIL ) return RET_BUG; */
/*       if ( dim == -1 ) */
/* 	{ */
/* 	  Scierror ("Error:\t dim flag -1 or '.' not supported for function %s\n", NspFname(stack)); */
/* 	  return RET_BUG; */
/* 	} */
/*       if ( dim == -2 )  /\* matlab compatibility flag *\/ */
/* 	dim = GiveMatlabDimFlag(HMat1); */
/*     } */

/*   switch (dim)  */
/*     { */
/*     default :  */
/*       Sciprintf("Invalid dim flag '%d' assuming 0\n", dim); */
/*     case 0 :  */
/*       if ((HMat =nsp_bmatrix_create(NVOID,1,1)) == NULLBMAT) return RET_BUG; */
/*       HMat->B[0] = FALSE; */
/*       for ( i=0; i < HMat1->mn ; i++ )  */
/* 	{ */
/* 	  if ( HMat1->B[i] != FALSE ) {  HMat->B[0] = TRUE;   break;  } */
/* 	} */
/*       break; */
      
/*     case 1:   /\* row *\/ */
/*       if ((HMat =nsp_bmatrix_create(NVOID,1,HMat1->n)) == NULLBMAT) return RET_BUG; */
/*       for ( j= 0 ; j < HMat1->n ; j++) */
/* 	{ */
/* 	  HMat->B[j] = FALSE;  */
/* 	  for ( i = 0 ; i < HMat1->m ; i++)  */
/* 	    if ( HMat1->B[i+HMat1->m*j]!= FALSE ) {HMat->B[j] = TRUE; break;} */
/* 	} */
/*       break; */

/*     case 2:  /\* column *\/ */
/*       if ((HMat =nsp_bmatrix_create(NVOID,HMat1->m,1)) == NULLBMAT) return RET_BUG; */
/*       for ( i= 0 ; i < HMat1->m ; i++) */
/* 	{ */
/* 	  HMat->B[i] = FALSE;  */
/* 	  for ( j = 0 ; j < HMat1->n ; j++)  */
/* 	    if ( HMat1->B[i+HMat1->m*j] != FALSE ){ HMat->B[i] = TRUE; break;} */
/* 	} */
/*       break; */
/*     } */
/*   MoveObj(stack,1, (NspObject *)HMat);   */
/*   return 1; */
/* } */


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
  {"extract_b", int_matint_extract}, 
  {"extractelts_b", int_matint_extractelts}, 
  {"extractcols_b", int_matint_extractcols}, 
  {"extractrows_b", int_matint_extractrows}, 
  {"resize2vect_b", int_matint_resize2vect},
  {"setrowscols_b", int_matint_setrowscols},
  {"deleteelts_b", int_matint_deleteelts},
  {"deleterows_b", int_matint_deleterows},
  {"deletecols_b", int_matint_deletecols},
  {"tozero_b", int_matint_tozero},
  {"repmat_b_m", int_matint_repmat},
  {"latexmat_b",int_bmatrix_2latexmat},
  {"latextab_b",int_bmatrix_2latextab},
  {"addcols_b_m",int_bmatrix_addcols},
  {"addrows_b_m",int_bmatrix_addrows},
  {"and_b",int_bmatrix_and},
  {"and_b_b",int_bmatrix_and},
  {"seq_and_b",int_bmatrix_and},
  {"seq_and_b_b",int_bmatrix_and},
  {"b2m",int_bmatrix_b2m},
  {"concatd_b_b", int_matint_concatd}, /* int_bmatrix_concatd}, */
  {"concatr_b_b", int_matint_concatr}, /* int_bmatrix_concatr},*/
  {"concatdiag_b_b" , int_matint_concat_diag},/* int_bmatrix_concatdiag },*/
  {"copy_b",int_bmatrix_copy},
  {"bmat_create_m",int_bmatrix_create},
  {"diagcre_b",int_bmatrix_diagcre},
  {"diage_b",int_bmatrix_diage},
  {"diagset_b",int_bmatrix_diagset},
  {"find_b",int_bmatrix_find},
  {"m2b",int_bmatrix_m2b},
  {"not_b",int_bmatrix_not},
  {"or_b",int_bmatrix_or},
  {"or_b_b",int_bmatrix_or},
  {"seq_or_b",int_bmatrix_or},
  {"seq_or_b_b",int_bmatrix_or},
  {"redim_b",int_matint_redim}, 
  {"matrix_b", int_matint_redim},
  {"resize_b",int_bmatrix_resize},
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


/* wrapper for functions  B=f(A)
 * call the matrix interface after a cast 
 */

static int int_bm_wrap(Stack stack, int rhs, int opt, int lhs,function *f)
{
  NspBMatrix *BM;
  NspMatrix *M;
  CheckRhs(1,2);
  if ( IsBMatObj(stack,1) ) 
    {
      if ((BM = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
      if ((M =nsp_bmatrix_to_matrix(BM)) == NULLMAT ) return RET_BUG;
      MoveObj(stack,1,(NspObject *) M);
      /* we don't want M ret_pos to be set */
      NSP_OBJECT(M)->ret_pos = -1;
    }
  if (rhs == 2 &&  IsBMatObj(stack,2) )
    {
      if ((BM = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
      if ((M =nsp_bmatrix_to_matrix(BM)) == NULLMAT ) return RET_BUG;
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
  {"dst_m_b", int_mxmultel, int_bm_wrap},
  {"dst_b_m", int_mxmultel, int_bm_wrap},
  {"dst_b_b", int_mxmultel, int_bm_wrap},
  {"dadd_b_b" ,   int_mxdadd , int_bm_wrap},
  {"dadd_b_m" ,   int_mxdadd , int_bm_wrap},
  {"dadd_m_b" ,   int_mxdadd , int_bm_wrap},
  {"div_b_b" ,  int_mxdiv, int_bm_wrap},
  {"div_b_m" ,  int_mxdiv, int_bm_wrap},
  {"div_m_b" ,  int_mxdiv, int_bm_wrap},
  {"dstd_b_b" ,  int_mxkron , int_bm_wrap}, /* operator:  .*. */
  {"dstd_b_m" ,  int_mxkron , int_bm_wrap}, /* operator:  .*. */
  {"dstd_m_b" ,  int_mxkron , int_bm_wrap}, /* operator:  .*. */
  {"minus_b",int_mxminus,int_bm_wrap},
  {"minus_b_b",   int_mxdsub, int_bm_wrap},
  {"minus_b_m",   int_mxdsub, int_bm_wrap},
  {"minus_m_b",   int_mxdsub, int_bm_wrap},
  {"mult_b_b" ,  int_mxmult, int_bm_wrap},
  {"mult_b_m" ,  int_mxmult, int_bm_wrap},
  {"mult_m_b" ,  int_mxmult, int_bm_wrap},
  {"plus_b_b",   int_mxdadd, int_bm_wrap},
  {"plus_b_m",   int_mxdadd, int_bm_wrap},
  {"plus_m_b",   int_mxdadd, int_bm_wrap},
  {"prod_b" ,  int_mxprod , int_bm_wrap},
  {"prod_b_s" ,  int_mxprod , int_bm_wrap},
  {"sum_b" ,  int_mxsum , int_bm_wrap},
  {"sum_b_s" ,  int_mxsum , int_bm_wrap},
  {(char *) 0, NULL,NULL},
};


int B2mMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(B2mMatrix_func[i].wrapper))(stack,rhs,opt,lhs,B2mMatrix_func[i].fonc);
}

/* used to walk through the interface table 
   (for adding or removing functions) **/

void B2mMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = B2mMatrix_func[i].name;
  *f = B2mMatrix_func[i].fonc;
}



