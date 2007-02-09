/* Nsp
 * Copyright (C) 1998-2006 Jean-Philippe Chancelier Enpc/Cermics
 * Copyright (C) 2005-2006 Bruno Pinçon Esial/Iecn
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

#define  Matrix_Private
#include "nsp/object.h"
#include "nsp/pr-output.h"
#include "nsp/interf.h"
#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"
#include "nsp/smatrix-in.h"
#include "nsp/matutil.h"
#include "nsp/matint.h"
#include "nsp/gsort-p.h"
#include "nsp/nsp_lapack.h"
#include "nsp/gsort-p.h"
#include <nsp/blas.h>

/* 
 * NspMatrix inherits from NspObject 
 */

int nsp_type_matrix_id = 0;
NspTypeMatrix *nsp_type_matrix = NULL;

/*
 * Type object for ClassA 
 * all the instance of NspTypeClassA share the same id. 
 * nsp_type_matrix: is a an instance of NspTypeClassA 
 *    used for objects of NspClassA type (i.e built with new_matrix) 
 * other instances are used for derived classes 
 */

NspTypeMatrix *
new_type_matrix (type_mode mode)
{
  NspTypeMatint *mati;/* interface */
  NspTypeMatrix *type = NULL;
  NspTypeObject *top;
  if (nsp_type_matrix != 0 && mode == T_BASE)
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_matrix;
    }

  if ((type = malloc (sizeof (NspTypeMatrix))) == NULL)
    return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object (T_DERIVED);
  if (type->surtype == NULL)
    return NULL;
  type->attrs = NULL;		/*matrix_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = matrix_get_methods;
  type->new = (new_func *) new_matrix;

  top = NSP_TYPE_OBJECT (type->surtype);
  while (top->surtype != NULL)
    top = NSP_TYPE_OBJECT (top->surtype);

  /* object methods redefined for matrix */

  top->pr = (print_func *) nsp_matrix_print;
  top->dealloc = (dealloc_func *) nsp_matrix_destroy;
  top->copy = (copy_func *) nsp_matrix_copy;
  top->size = (size_func *) matrix_size;
  top->s_type = (s_type_func *) matrix_type_as_string;
  top->sh_type = (sh_type_func *) matrix_type_short_string;
  top->info = (info_func *) nsp_matrix_info;
  top->is_true = (is_true_func *) matrix_is_true;
  top->loop = (loop_func *) matrix_loop_extract;
  top->path_extract = NULL;	/* (path_func *) matrix_path_extract ; */
  top->get_from_obj = (get_from_obj_func *) matrix_object;
  top->eq = (eq_func *) matrix_eq;
  top->neq = (eq_func *) matrix_neq;
  top->save = (save_func *) matrix_xdr_save;
  top->load = (load_func *) matrix_xdr_load;
  top->latex = (print_func *) nsp_matrix_latex_print;

  /* specific methods for matrix */

  type->init = (init_func *) init_matrix;

  /* 
   * Matrix interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   */

  /*
   * Matrix implements Matint the matrix interface 
   * which is common to object that behaves like matrices.
   */

  mati = new_type_matint(T_DERIVED);
  mati->methods = matint_get_methods; 
  /* mati->redim = (matint_redim *) nsp_matrix_redim; use default value of matint */
  mati->resize = (matint_resize  *) nsp_matrix_resize;
  mati->free_elt = (matint_free_elt *) 0; /* nothing to do */
  mati->elt_size = (matint_elt_size *) nsp_matrix_elt_size ;
  mati->clone = (matint_clone *) nsp_matrix_clone ;
  mati->copy_elt = (matint_copy_elt *) 0; /* nothing to do */
  mati->enlarge = (matint_enlarge *) nsp_matrix_enlarge;
  mati->canonic = (matint_canonic *) Mat2double;
  /* mati->copy = (matint_copy *) nsp_matrix_copy_area; */


  type->interface = (NspTypeBase *) mati;

  if (nsp_type_matrix_id == 0)
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_matrix
       */
      type->id = nsp_type_matrix_id = nsp_new_type_id ();
      nsp_type_matrix = type;
      if (nsp_register_type (nsp_type_matrix) == FALSE)
	return NULL;
      return (mode == T_BASE) ? type : new_type_matrix (mode);
    }
  else
    {
      type->id = nsp_type_matrix_id;
      return type;
    }
}

/*
 * initialize Matrix instances 
 * locally and by calling initializer on parent class 
 */

static int
init_matrix (NspMatrix * o, NspTypeMatrix * type)
{
  /* to be done always */
  if (type->surtype->init (&o->father, type->surtype) == FAIL)
    return FAIL;
  o->type = type;
  NSP_OBJECT (o)->basetype = (NspTypeBase *) type;
  /* specific */
  return OK;
}

/*
 * new instance of Matrix 
 */

NspMatrix *
new_matrix ()
{
  NspMatrix *loc;
  /* type must exists */
  nsp_type_matrix = new_type_matrix (T_BASE);
  if ((loc = malloc (sizeof (NspMatrix))) == NULLMAT)
    return loc;
  /* initialize object */
  if (init_matrix (loc, nsp_type_matrix) == FAIL)
    return NULLMAT;
  return loc;
}

/*
 * Object methods redefined for matrix 
 */

static int
matrix_size (NspMatrix * Mat, int flag)
{
  switch (flag)
    {
    case 0:
      return Mat->mn;
    case 1:
      return Mat->m;
    case 2:
      return Mat->n;
    }
  return 0;
}

/* Type as char */

static char mat_type_name[] = "Mat";
static char mat_short_type_name[] = "m";

char *
matrix_type_as_string (void)
{
  return (mat_type_name);
}

char *
matrix_type_short_string (void)
{
  return (mat_short_type_name);
}

/* used in for x=Matrix ... **/

NspObject *
matrix_loop_extract (char *str, NspObject * O, NspObject * O1, int i,
		     int *rep)
{
  NspMatrix *M = (NspMatrix *) O1, *M1 = NULLMAT;
  if (O == NULLOBJ)
    {
      if ((M1 = MatLoopCol (str, NULLMAT, M, i, rep)) == NULLMAT)
	return NULLOBJ;
      if ((*rep == RET_ENDFOR))
	return NULLOBJ;
      return (NspObject *) M1;
    }
  else
    {
      if ((M1 = matrix_object (O)) == NULLMAT)
	return NULLOBJ;
      M1 = MatLoopCol (str, M1, M, i, rep);
      if ((*rep == RET_ENDFOR))
	return NULLOBJ;
      return O;
    }
}

/* A is <<equal>> to B i.e they are copies */

int
matrix_eq (NspObject * A, NspObject * B)
{
  int err, rep;
  if (check_cast (B, nsp_type_matrix_id) == FALSE)     return FALSE;
  if ( ! ( ((NspMatrix *) A)->m == ((NspMatrix *) B)->m 
	   && ((NspMatrix *) A)->n == ((NspMatrix *) B)->n)) return FALSE;
  rep = nsp_mat_fullcomp ((NspMatrix *) A, (NspMatrix *) B, "==", &err);
  if (err == TRUE)
    return FALSE;
  return rep;
}

/* A is not <<equal>> to B i.e they are not copies */

int
matrix_neq (NspObject * A, NspObject * B)
{
  return ( matrix_eq(A,B) == TRUE ) ? FALSE : TRUE ;
}

/*
 * Mat == TRUE ? 
 *  if Mat != [] and all the elements of Mat are != 0
 */

static int
matrix_is_true (NspMatrix * M)
{
  int i;
  if (M->mn == 0)
    return FALSE;
  if (M->rc_type == 'r')
    for (i = 0; i < M->mn; i++)
      {
	if (M->R[i] == 0.0)
	  return FALSE;
      }
  else
    for (i = 0; i < M->mn; i++)
      {
	if (M->C[i].r == 0.0 && M->C[i].i == 0.0)
	  return FALSE;
      }
  return TRUE;
}

/*
 * Save a Matrix in a file stream 
 */

static int
matrix_xdr_save (XDR *xdrs, NspMatrix * M)
{
  if (nsp_xdr_save_i(xdrs, M->type->id) == FAIL)
    return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT (M)->name) == FAIL)
    return FAIL;
  if (nsp_xdr_save_i(xdrs, M->m) == FAIL)
    return FAIL;
  if (nsp_xdr_save_i(xdrs, M->n) == FAIL)
    return FAIL;
  if (nsp_xdr_save_c(xdrs, M->rc_type) == FAIL)
    return FAIL;
  if (M->rc_type == 'r')
    {
      if (nsp_xdr_save_array_d(xdrs, M->R, M->mn) == FAIL)
	return FAIL;
    }
  else
    {
      if (nsp_xdr_save_array_d(xdrs, (double *) M->C, 2 * M->mn) == FAIL)
	return FAIL;
    }
  return OK;
}

/*
 * Load a Matrix from a file stream 
 **/

static NspMatrix *matrix_xdr_load(XDR *xdrs)
{
  char c;
  int m, n;
  NspMatrix *M;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs, name, NAME_MAXL) == FAIL)
    return NULLMAT;
  if (nsp_xdr_load_i(xdrs, &m) == FAIL)
    return NULLMAT;
  if (nsp_xdr_load_i(xdrs, &n) == FAIL)
    return NULLMAT;
  if (nsp_xdr_load_c(xdrs, &c) == FAIL)
    return NULLMAT;
  if ((M = nsp_matrix_create (name, c, m, n)) == NULLMAT)
    return NULLMAT;
  if (M->rc_type == 'r')
    {
      if (nsp_xdr_load_array_d(xdrs, M->R, M->mn) == FAIL)
	return NULLMAT;
    }
  else
    {
      if (nsp_xdr_load_array_d(xdrs, (double *) M->C, 2 * M->mn) == FAIL)
	return NULLMAT;
    }
  return M;
}

/*
 * A = MatObj(O);
 * checks that O is an object of type Matrix 
 * or a Hobj which points to an object of type Matrix
 *    if so, returns a pointer to that Matrix and else returns
 *    NULLMAT
 */

NspMatrix *
matrix_object (NspObject * O)
{
  /* Follow pointer  if necessary */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type * */
  if (check_cast (O, nsp_type_matrix_id) == TRUE)
    return ((NspMatrix *) O);
  else
    Scierror ("Error:\tArgument should be a %s\n",
	      type_get_name (nsp_type_matrix));
  return (NULLMAT);
}

/*
 * IsMatObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  Matrix 
 * or a Hobj which points to an object of type Matrix
 */

int
IsMatObj (Stack stack, int i)
{
  return nsp_object_type (NthObj (i), nsp_type_matrix_id);
}

/*
 * IsMat(O)
 * only checks that object is an object of type  Matrix 
 * or a Hobj which points to an object of type Matrix
 */

int
IsMat (const NspObject * O)
{
  return nsp_object_type (O, nsp_type_matrix_id);
}

/*
 * Checks that first+i object on the stack 
 * is a matrix and returns that matrix  
 * (internal)
 */

static NspMatrix *
GetMat_G (Stack stack, int i)
{
  NspMatrix *M;
  if ((M = matrix_object (NthObj (i))) == NULLMAT)
    ArgMessage (stack, i);
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

static NspMatrix *
GetMatCopy_G (Stack stack, int i)
{
  if (GetMat_G (stack, i) == NULL)
    return NULL;;
  return MaybeObjCopy (&NthObj (i));
}

#ifndef HAVE_INLINE 
/*
 * GetMat and GetMatCopy and conversions 
 */

NspMatrix *
GetMatCopy (Stack stack, int i)
{
  return Mat2double (GetMatCopy_G (stack, i));
}

NspMatrix *
GetMat (Stack stack, int i)
{
  return Mat2double (GetMat_G (stack, i));
}
#endif 

/*
 * GetMat and GetMatCopy and conversions 
 */

NspMatrix *
GetMtlbMatCopy (Stack stack, int i)
{
  return Mat2mtlb_cplx (GetMatCopy_G (stack, i));
}

NspMatrix *
GetMtlbMat (Stack stack, int i)
{
  return Mat2mtlb_cplx (GetMat_G (stack, i));
}


NspMatrix *
GetMatCopyInt (Stack stack, int i)
{
  return Mat2int (GetMatCopy_G (stack, i));
}

NspMatrix *
GetMatInt (Stack stack, int i)
{
  return Mat2int (GetMat_G (stack, i));
}

NspMatrix *
GetMatCopyFloat (Stack stack, int i)
{
  return Mat2float (GetMatCopy_G (stack, i));
}

NspMatrix *
GetMatFloat (Stack stack, int i)
{
  return Mat2float (GetMat_G (stack, i));
}

/*
 * Checks that first+i object on the stack 
 * is a matrix and returns that matrix  
 * or a copy of that matrix if its name 
 * is != NVOID 
 */

NspMatrix *
GetRealMatCopy_G (Stack stack, int i)
{
  NspMatrix *M;
  if ((M = GetMatCopy_G (stack, i)) == NULLMAT)
    return NULLMAT;
  if (M->rc_type == 'c')
    {
      Scierror ("\t%s", ArgPosition (i));
      ArgName (stack, i);
      Scierror (" of function %s should not be complex\n", NspFname(stack));
      return NULLMAT;
    }
  return M;
}

/*
 * Checks that first+i object on the stack 
 * is a matrix and returns that matrix  
 * The matrix is converted to double if necessary 
 */

NspMatrix *
GetRealMat_G (Stack stack, int i)
{
  NspMatrix *M;
  if ((M = GetMat_G (stack, i)) == NULLMAT)
    return NULLMAT;
  if (M->rc_type == 'c')
    {
      Scierror ("\t%s", ArgPosition (i));
      ArgName (stack, i);
      Scierror (" of function %s should not be complex\n", NspFname(stack));
      return NULLMAT;
    }
  return M;
}

/*
 * GetMat_G and GetMatCopy_G and conversions 
 */

NspMatrix *
GetRealMatCopy (Stack stack, int i)
{
  return Mat2double (GetRealMatCopy_G (stack, i));
}

NspMatrix *
GetRealMat (Stack stack, int i)
{
  return Mat2double (GetRealMat_G (stack, i));
}

NspMatrix *
GetRealMatCopyInt (Stack stack, int i)
{
  return Mat2int (GetRealMatCopy_G (stack, i));
}

NspMatrix *
GetRealMatInt (Stack stack, int i)
{
  return Mat2int (GetRealMat_G (stack, i));
}

NspMatrix *
GetRealMatCopyFloat (Stack stack, int i)
{
  return Mat2float (GetRealMatCopy_G (stack, i));
}

NspMatrix *
GetRealMatFloat (Stack stack, int i)
{
  return Mat2float (GetRealMat_G (stack, i));
}

/*
 * Utilities for interface 
 * Checks that objects is of scalar type 
 * and returns its value as an int 
 */

int
IntScalar (NspObject * O, int * val)
{
  static char mess[] = "Argument should be an integer\n";
  NspMatrix *A;
  if ((A = matrix_object (O)) == NULLMAT || (A->mn != 1 || A->rc_type != 'r'))
    {
      Scierror (mess);
      return (FAIL);
    }
  switch ( A->convert ) 
    {
    case 'i' :  *val = A->I[0]; break;
    case 'f' :  *val = (int) aint( A->F[0]); break;
    case 'd' :  
    default : 
      *val = (int) aint (A->R[0]); break;
    }
  return (OK);
}

/*
 * Checks that first+i object on the stack 
 * is of scalar type 
 * and returns its value as an int 
 */

int
GetScalarInt (Stack stack, int i, int * val)
{
  NspMatrix *M;
  if ((M = matrix_object (NthObj (i))) == NULLMAT
      || (M->mn != 1 || M->rc_type != 'r'))
    {
      Scierror ("Error:\t%s", ArgPosition (i));
      ArgName (stack, i);
      Scierror (" of function %s should be an integer\n", NspFname(stack));
      return FAIL;
    }
  switch ( M->convert ) 
    {
    case 'i' :  *val = M->I[0]; break;
    case 'f' :  *val = (int) aint( M->F[0]); break;
    case 'd' :  
    default : 
      *val = (int) aint (M->R[0]); break;
    }
  return (OK);
}

/*
 * Checks that objects is of scalar type 
 * and returns its value as a double 
 */

int
DoubleScalar (NspObject * O, double *val)
{
  static char mess[] = "Argument should be a double";
  NspMatrix *A;
  if ((A = matrix_object (O)) == NULLMAT)
    return FAIL;
  if (A->mn != 1 || A->rc_type != 'r')
    {
      Scierror (mess);
      return (FAIL);
    }
  switch ( A->convert ) 
    {
    case 'i' :  *val = (double) A->I[0]; break;
    case 'f' :  *val = (double) A->F[0]; break;
    case 'd' :  
    default : 
      *val = A->R[0]; break;
    }
  return (OK);
}

/*
 * Checks that first+i objects on the stack 
 * is of scalar type 
 * and returns its value as a double 
 */

int
GetScalarDouble (Stack stack, int i, double *val)
{
  NspMatrix *M;
  if ((M = matrix_object (NthObj (i))) == NULLMAT
      || (M->mn != 1 || M->rc_type != 'r'))
    {
      Scierror ("Error:\t%s", ArgPosition (i));
      ArgName (stack, i);
      Scierror (" of function %s should be a double\n", NspFname(stack));
      return FAIL;
    }
  switch ( M->convert ) 
    {
    case 'i' :  *val = M->I[0]; break;
    case 'f' :  *val = M->F[0]; break;
    case 'd' :  
    default : 
      *val = M->R[0]; break;
    }
  return (OK);
}

/*
 * Utility function : Converts A to int 
 * on-place. returns max and min 
 * XXXXXX : doit-on mettre 1 et 1 comme valeur par defaut 
 */

int *
Matd2i (NspMatrix * A, int * imin, int * imax)
{
  int i;
  int *Iloc;
  Iloc = (int *) A->R;
  nsp_double2int (&A->mn, A->R, Iloc);
  *imax = 1;
  *imin = 1;
  for (i = 0; i < A->mn; i++)
    {
      if (Iloc[i] > *imax)
	*imax = Iloc[i];
      else if (Iloc[i] < *imin)
	*imin = Iloc[i];
    }
  return Iloc;
}

/*
 * Utility function : Bounds 
 * returns max and min 
 */

void
Bounds (const NspMatrix * A, int * imin, int * imax)
{
  int i;
  *imax = 1;
  *imin = 1;
  for (i = 0; i < A->mn; i++)
    {
      int ival = (int) A->R[i];
      if (ival > *imax)
	*imax = ival;
      else if (ival < *imin)
	*imin = ival;
    }
}


void
nsp_matrix_boundster(const NspMatrix *A, int *ind, int *imin, int *imax)
{
  int i, ival;

  *imax = 1;
  *imin = 1;
  for (i = 0; i < A->mn; i++)
    {
      ival = (int) A->R[i];
      if (ival > *imax)
	*imax = ival;
      else if (ival < *imin)
	*imin = ival;
      ind[i] = ival-1;
    }
}


/*
 * Utility function : Converts back to double
 * if Matrix was previously converted to int or real
 * A is changed and its adress is also returned
 */

NspMatrix *
Mat2double (NspMatrix * A)
{
  int inc = -1;
  if (A == NULLMAT ) return A;
  if ( A->rc_type == 'r')
    {
      switch (A->convert)
	{
	case 'i':
	  nsp_int2double (&A->mn, (int *) A->R, &inc, A->R, &inc);
	  A->convert = 'd';
	  break;
	case 'f':
	  nsp_float2double (&A->mn, (float *) A->R, &inc, A->R, &inc);
	  A->convert = 'd';
	  break;
	}
    }
  else 
    {
      if ( A->convert == 'c') 
	{
	  nsp_double2complex(A->R, A->mn);
	  A->convert = 'd';
	}
    }
  return A;
}

/*
 * Utility function : 
 * If the matrix is complex it is converted to 
 * old style complex mode (used in mexlib)
 */

NspMatrix *
Mat2mtlb_cplx (NspMatrix * A)
{
  int inc = -1;
  if (A == NULLMAT ) return A;
  if ( A->rc_type == 'r')
    {
      switch (A->convert)
	{
	case 'i':
	  nsp_int2double (&A->mn, (int *) A->R, &inc, A->R, &inc);
	  A->convert = 'd';
	  break;
	case 'f':
	  nsp_float2double (&A->mn, (float *) A->R, &inc, A->R, &inc);
	  A->convert = 'd';
	  break;
	}
    }
  else 
    {
      if ( A->convert == 'd') 
	{
	  nsp_complex2double(A->R, A->mn);
	  A->convert = 'c';
	}
    }
  return A;
}

/*
 * Utility function : Converts to int 
 */

NspMatrix *
Mat2int (NspMatrix * A)
{
  if (A != NULLMAT && A->convert != 'i')
    {
      if (A->rc_type == 'r')
	{
	  if (A->convert == 'd')
	    nsp_double2int (&A->mn, A->R, (int *) A->R);
	  else if (A->convert == 'f')
	    nsp_float2int (&A->mn, (float *) A->R, (int *) A->R);
	  A->convert = 'i';
	}
      else
	{
	  Scierror ("Error: Cannot convert a complex matrix to int\n");
	  return NULLMAT;
	}
    }
  return A;
}

/*
 * Utility function : Converts to float 
 */

NspMatrix *
Mat2float (NspMatrix * A)
{
  static int inc = -1;
  if (A != NULLMAT && A->convert != 'f')
    {
      if (A->rc_type == 'r')
	{
	  if (A->convert == 'd')
	    nsp_double2float (&A->mn, A->R, (float *) A->R);
	  else if (A->convert == 'i')
	    nsp_int2float (&A->mn, (int *) A->R, &inc, (float *) A->R, &inc);
	  A->convert = 'f';
	}
      else
	{
	  Scierror ("Error: Cannot convert a complex matrix to float\n");
	  return NULLMAT;
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

static int int_meth_matrix_add(void *a,Stack stack,int rhs,int opt,int lhs)
{
  NspMatrix *B;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((B = GetMat (stack, 1)) == NULLMAT) return RET_BUG;
  if ( nsp_mat_add(a,B) == FAIL )  return RET_BUG;
  return 0;
}

/* 
 *  blas 1 operation : 
 *
 *    y <- y + alpha x
 *
 *    A.blas_axpy[alpha, x [i1,i2 [,j1,j2]]]
 */

static int int_meth_matrix_axpy(void *self, Stack stack,int rhs,int opt,int lhs)
{
  NspMatrix *alpha, *y = (NspMatrix *) self, *x;
  int one = 1, i1, i2, j1, j2, mn, m, n, j;
  CheckLhs(1,1);

  if ( rhs != 2  &&  rhs != 4  && rhs != 6 )
    { 
      Scierror("Error: %d arguments is incorrect for method %s\n",rhs,stack.fname);
      return RET_BUG;
    }

  if ((alpha = GetMat (stack, 1)) == NULLMAT) return RET_BUG;
  CheckScalar(stack.fname,1,alpha);

  if ((x = GetMat (stack, 2)) == NULLMAT) return RET_BUG;

  if ( alpha->rc_type != y->rc_type || x->rc_type != y->rc_type )
    { 
      Scierror("Error: sorry the arguments must be of same type than self arg %s\n",stack.fname);
      return RET_BUG;
    }

  if ( rhs == 2 )
    {
      if ( x->m != y->m || x->n != y->n ) 
	{ 
	  Scierror("%s: argument %d should have size %d x %d \n",stack.fname, 2, y->m, y->n );
	  return RET_BUG;
	} 
      
      if ( y->rc_type == 'r' )
	C2F(daxpy)(&y->mn, alpha->R, x->R, &one, y->R, &one);
      else
	C2F(zaxpy)(&y->mn, alpha->C, x->C, &one, y->C, &one);
      return 0;
    }

  if ( GetScalarInt (stack, 3, &i1) == FAIL ) return RET_BUG;
  if ( GetScalarInt (stack, 4, &i2) == FAIL ) return RET_BUG;

  if ( i1 <= 0  || i2 <= 0 )
    { 
      Scierror("%s: argument 3 and 4 must be positive integer \n",stack.fname);
      return RET_BUG;
    } 

  if ( rhs == 4 )
    {
      mn = i2-i1+1;
      if ( x->mn != mn )
	{ 
	  Scierror("%s: argument %d should have length %d \n",stack.fname, 2, i2-i1+1);
	  return RET_BUG;
	} 

      if ( i2 > y->mn )
	{ 
	  Scierror("%s: last index (%d) outside (self) array length (%d) \n",stack.fname, i2, y->mn);
	  return RET_BUG;
	} 

      if ( y->rc_type == 'r' )
	C2F(daxpy)(&mn, alpha->R, x->R, &one, &y->R[i1-1], &one);
      else
	C2F(zaxpy)(&mn, alpha->C, x->C, &one, &y->C[i1-1], &one);
      return 0;
    }


  if ( GetScalarInt (stack, 5, &j1) == FAIL ) return RET_BUG;
  if ( GetScalarInt (stack, 6, &j2) == FAIL ) return RET_BUG;

  if ( j1 <= 0  || j2 <= 0 )
    { 
      Scierror("%s: argument 5 and 6 must be positive integer \n",stack.fname);
      return RET_BUG;
    } 

  m = i2-i1+1; n = j2-j1+1;
  if ( x->m != m || x->n != n )
    { 
      Scierror("%s: argument %d should have size %d x %d \n",stack.fname, 2, m, n);
      return RET_BUG;
    } 

  if ( i2 > y->mn )
    { 
      Scierror("%s: last indices (%d,%d) outside (self) array sizes (%d) \n",stack.fname, i2, j2, y->m, y->n);
      return RET_BUG;
    }
 
  if ( y->rc_type == 'r' )
    for ( j = 0 ; j < n ; j++ )
      C2F(daxpy)(&m, alpha->R, &x->R[m*j], &one, &y->R[i1-1+(j+j1-1)*y->m], &one);
  else
    for ( j = 0 ; j < n ; j++ )
      C2F(zaxpy)(&m, alpha->C, &x->C[m*j], &one, &y->C[i1-1+(j+j1-1)*y->m], &one);
  return 0;
}


/* 
 *  blas 2 operation : 
 *
 *    A <- A + alpha x*y'  on a "contiguous" subpart [i1,i2]x[j1,j2] of A 
 *
 *    A.blas_ger[alpha, x, y [,i1,i2,j1,j2]]
 */
static int int_meth_matrix_ger(void *self,Stack stack,int rhs,int opt,int lhs)
{
  NspMatrix *A = (NspMatrix *) self;
  NspMatrix *alpha, *x, *y;
  int i1, i2, j1, j2, mm, nn, one=1, k;
  char *flag=NULL, cflag;
  nsp_option opts[] ={{"flag",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( rhs-opt != 3 && rhs-opt != 7 ) 
    { 
      Scierror("Error: %d arguments is incorrect for method %s\n",rhs,stack.fname);
      return RET_BUG;
    }
  CheckLhs(1,1);

  if ((alpha = GetMat (stack, 1)) == NULLMAT) return RET_BUG;
  CheckScalar(stack.fname,1,alpha);
  if ((x = GetMat (stack, 2)) == NULLMAT) return RET_BUG;
  CheckVector(stack.fname,2,x);
  if ((y = GetMat (stack, 3)) == NULLMAT) return RET_BUG;
  CheckVector(stack.fname,3,y);

  if ( alpha->rc_type != A->rc_type || x->rc_type != A->rc_type || y->rc_type != A->rc_type )
    { 
      Scierror("Error: sorry the 3 first arguments must be of same type than self arg %s\n",stack.fname);
      return RET_BUG;
    }

  if ( get_optional_args(stack, rhs, opt, opts, &flag) == FAIL )
    return RET_BUG;
  cflag = ( opts[0].obj == NULLOBJ) ? '*' : flag[0]; 


  if ( rhs-opt == 7 )
    {
      if ( GetScalarInt (stack, 4, &i1) == FAIL ) return RET_BUG;
      if ( GetScalarInt (stack, 5, &i2) == FAIL ) return RET_BUG;
      if ( GetScalarInt (stack, 6, &j1) == FAIL ) return RET_BUG;
      if ( GetScalarInt (stack, 7, &j2) == FAIL ) return RET_BUG;
      k = (j1-1)*A->m + i1 -1;
    }
  else
    {
      i1 = 1; i2 = A->m; j1 = 1; j2 = A->n;
      k = 0;
    }

  mm =  i2-i1+1; nn = j2-j1+1;
  if ( mm != x->mn  ||  nn != y->mn  ||  i1 < 1  ||  j1 < 1  ||  i2 > A->m  ||  j2 > A->n )
    { 
      Scierror("Error: incompatible dimensions %s\n",stack.fname);
      return RET_BUG;
    }

  if ( A->rc_type == 'r' )
    C2F(dger)(&mm, &nn, alpha->R, x->R, &one, y->R, &one, &A->R[k], &A->m);
  else
    {
      if ( cflag == 't' || cflag == 'T' )
	C2F(zgeru)(&mm, &nn, alpha->C, x->C, &one, y->C, &one, &A->C[k], &A->m);
      else
	C2F(zgerc)(&mm, &nn, alpha->C, x->C, &one, y->C, &one, &A->C[k], &A->m);
    }
  return 0;
}

/* 
 *  scale_rows[x]
 *
 *    A <- diag(x)*A
 *
 *    A.scale_rows[x]
 */

static int int_meth_matrix_scale_rows(void *self, Stack stack,int rhs,int opt,int lhs)
{
  NspMatrix *A = (NspMatrix *) self, *x;
  CheckLhs(0,0);
  CheckRhs(1,1);

  if ((x = GetMat (stack, 1)) == NULLMAT) return RET_BUG;
  CheckVector(stack.fname,1,x);
  if ( x->mn != A->m )
    { 
      Scierror("%s: the argument should have %d components \n",stack.fname,A->m);
      return RET_BUG;
    }

  if ( nsp_mat_scale_rows(A, x) == FAIL )
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

static int int_meth_matrix_scale_cols(void *self, Stack stack,int rhs,int opt,int lhs)
{
  NspMatrix *A = (NspMatrix *) self, *x;
  CheckLhs(0,0);
  CheckRhs(1,1);

  if ((x = GetMat (stack, 1)) == NULLMAT) return RET_BUG;
  CheckVector(stack.fname,1,x);
  if ( x->mn != A->n )
    { 
      Scierror("%s: the argument should have %d components \n",stack.fname,A->n);
      return RET_BUG;
    }

  if ( nsp_mat_scale_cols(A, x) == FAIL )
    return RET_BUG;

  return 0;
}

static NspMethods matrix_methods[] = {
  { "add", int_meth_matrix_add},
  { "blas_axpy", int_meth_matrix_axpy},  /* possible other name:  add_scal_time_mat  */
  { "blas_ger", int_meth_matrix_ger},    /* possible other name:  updt_rk1  */
  { "scale_rows",int_meth_matrix_scale_rows}, 
  { "scale_cols",int_meth_matrix_scale_cols}, 
  { (char *) 0, NULL}
};

static NspMethods *matrix_get_methods(void) { return matrix_methods;};

/*
 * Now the interfaced function for basic matrices operations
 */

/*
 * Creation of a Matrix 
 * returns NULLMAT on failure 
 * The matrix is created with no initial value 
 */

int
int_mxcreate (Stack stack, int rhs, int opt, int lhs)
{
  int m1, n1;
  NspMatrix *HMat;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if (GetScalarInt (stack, 1, &m1) == FAIL)
    return RET_BUG;
  CheckNonNegative(NspFname(stack),m1,1);
  if (GetScalarInt (stack, 2, &n1) == FAIL)
    return RET_BUG;
  CheckNonNegative(NspFname(stack),n1,2);
  if ((HMat = nsp_matrix_create (NVOID, 'r', m1, n1)) == NULLMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) HMat);
  return 1;
}

/*
 * interface for operator : 
 */

int
int_mximpl (Stack stack, int rhs, int opt, int lhs)
{
  double dfirst, step = 1.0E0, last;
  NspMatrix *M;
  CheckRhs (2, 3);
  CheckLhs (1, 1);
  if (GetScalarDouble (stack, 1, &dfirst) == FAIL)
    return RET_BUG;
  if (GetScalarDouble (stack, 2, &last) == FAIL)
    return RET_BUG;
  if (rhs == 3)
    {
      step = last;
      if (GetScalarDouble (stack, 3, &last) == FAIL)
	return RET_BUG;
    }
  if ((M = nsp_matrix_create_impl (dfirst, step, last)) == NULLMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) M);
  return 1;
}

/*
 * interface for operator linspace 
 */

int
int_mxlinspace (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *first, *last, *M;
  int n;
  CheckRhs (3, 3);
  CheckLhs (1, 1);
  if ((first = GetRealMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((last = GetRealMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  CheckSameDims (NspFname(stack), 1, 2, first, last);
  if (GetScalarInt (stack, 3, &n) == FAIL)
    return RET_BUG;
  if ((M =
       nsp_matrix_create_linspace (first->R, last->R, first->mn,
				   n)) == NULLMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) M);
  return 1;
}

/*
 * interface for operator logspace 
 */


int
int_mxlogspace (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *first, *last, *M;
  int n;
  CheckRhs (3, 3);
  CheckLhs (1, 1);
  if ((first = GetRealMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((last = GetRealMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  CheckSameDims (NspFname(stack), 1, 2, first, last);
  if (GetScalarInt (stack, 3, &n) == FAIL)
    return RET_BUG;
  if ((M =
       nsp_matrix_create_logspace (first->R, last->R, first->mn,
				   n)) == NULLMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) M);
  return 1;
}

/*
 * testmatrix('name',size)
 *    name = 'magic','franck','franck^-1','hilbert'
 */

int
int_mxtestmatrix (Stack stack, int rhs, int opt, int lhs)
{
  static char *Strings[] =
    { "magic", "franck", "franck^-1", "hilbert", "hilbert^-1",
      (char *) NULL };
  int ind, n1;
  NspMatrix *A=NULLMAT;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((ind = GetStringInArray (stack, 1, Strings, 0)) == -1)
    return RET_BUG;
  if (GetScalarInt (stack, 2, &n1) == FAIL)
    return RET_BUG;
  CheckNonNegative(NspFname(stack),n1,2);
  switch (ind)
    {
    case 0:
      A = nsp_mat_magic (n1);
      break;
    case 1:
      A = nsp_mat_franck (n1, 0);
      break;
    case 2:
      A = nsp_mat_franck (n1, 1);
      break;
    case 3:
      A = nsp_mat_hilbert (n1, 0);
      break;
    case 4:
      A = nsp_mat_hilbert (n1, 1);
      break;
    }
  if (A == NULLMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) A);
  return 1;
}


/*
 * Change a matrix of Real type to Imaginary type 
 * The imag part is not initialized 
 */

int
int_mxcomplexify (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat;
  double d = 0.00;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if ((HMat = GetMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (rhs == 2)
    if (GetScalarDouble (stack, 2, &d) == FAIL)
      return RET_BUG;
  if (nsp_mat_complexify (HMat, d) != 0)
    {
      return RET_BUG;
    };
  NSP_OBJECT (HMat)->ret_pos= 1;
  return 1;
}

/*
 * Returns real(A) 
 */

int
int_mxrealpart (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (HMat->rc_type == 'c')
    {
      if ((HMat = GetMatCopy (stack, 1)) == NULLMAT)
	return RET_BUG;
    }
  if (nsp_mat_get_real (HMat) != OK)
    return RET_BUG;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

/*
 * Returns imag(A), the Imaginary part of Matrix A 
 */

int
int_mximagpart (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (nsp_mat_get_imag (HMat) != OK)
    return RET_BUG;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

/*
 * Returns a kroeneker product A.*.B 
 */

int
int_mxkron (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat1, *HMat2, *HMat3;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((HMat1 = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((HMat2 = GetMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  if ((HMat3 = nsp_mat_kron (HMat1, HMat2)) == NULLMAT)
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

static int int_matrix_sort(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M=NULL,*Index=NULL;
  char *type_sort[]={ "g", "gs", "gm", "c", "r", "lr" , "lc" , "ldc", "ldr","gb","gd", NULL };
  char *dir_sort[]={ "i", "d",  NULL };
  int iflag = FALSE;
  char direction = 'd';
  int rep_type= sort_g,rep_dir;

  CheckRhs(1,3);
  if ((M=GetRealMatCopy(stack,1)) == NULLMAT ) return RET_BUG;

  if ( rhs >= 2) 
    {
      if ((rep_type= GetStringInArray(stack,2,type_sort,1)) == -1) return RET_BUG; 
    }

  if (rhs >= 3) 
    {
      if ((rep_dir= GetStringInArray(stack,3,dir_sort,1)) == -1) return RET_BUG; 
      direction = dir_sort[rep_dir][0];
    }

  if (lhs  == 2) 
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
      nsp_matrix_sort(M,&Index,iflag,direction,rep_type);
      break;
    case sort_c:
      /* take care that c -> row */
      nsp_matrix_row_sort(M,&Index,iflag,direction);break;
    case sort_r:
      nsp_matrix_column_sort(M,&Index,iflag,direction);break;
    case sort_lr:
      nsp_matrix_lexical_row_sort(M,&Index,iflag,direction,'i');break;
    case sort_lc:
      nsp_matrix_lexical_column_sort(M,&Index,iflag,direction,'i');break;
    case sort_ldr:
      nsp_matrix_lexical_row_sort(M,&Index,iflag,direction,'d');
      break;
    case sort_ldc:
      nsp_matrix_lexical_column_sort(M,&Index,iflag,direction,'d');  break;
    }
  if ( iflag == TRUE && Index == NULL) return RET_BUG;
  NSP_OBJECT(M)->ret_pos = 1;
  if ( lhs == 2 ) {
    /* back convert */
    Index = Mat2double(Index);
    MoveObj(stack,2, NSP_OBJECT(Index));
  }
  return Max(lhs,1);
} 

/*
 *nsp_mat_sum: sum=Sum(a[,b]) 
 * a is unchanged 
 */

typedef NspMatrix *(*SuPro) (NspMatrix * A, int dim);

static int
int_mx_sum (Stack stack, int rhs, int opt, int lhs, SuPro F)
{
  int dim=0;
  NspMatrix *Res, *HMat;
  CheckRhs (1, 2);
  CheckLhs (1, 1);

  if ((HMat = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;

  if (rhs == 2)
    if ( GetDimArg(stack, 2, &dim) == FAIL )
      return RET_BUG;

  if ((Res = (*F) (HMat, dim)) == NULLMAT)
    return RET_BUG;

  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

int
int_mxsum (Stack stack, int rhs, int opt, int lhs)
{
  return (int_mx_sum (stack, rhs, opt, lhs, nsp_mat_sum));
}

/*
 * matprod : product of all elements of a
 * a is unchanged 
 */

int
int_mxprod (Stack stack, int rhs, int opt, int lhs)
{
  return (int_mx_sum (stack, rhs, opt, lhs, nsp_mat_prod));
}

/*
 * matcusum : cumulative sum of all elements of a
 * a is unchanged 
 */

int
int_mxcusum (Stack stack, int rhs, int opt, int lhs)
{
  return (int_mx_sum (stack, rhs, opt, lhs, nsp_mat_cum_sum));
}

/*
 * matcuprod : cumulative prod of all elements of a
 * a is unchanged 
 */

int
int_mxcuprod (Stack stack, int rhs, int opt, int lhs)
{
  return (int_mx_sum (stack, rhs, opt, lhs, nsp_mat_cum_prod));
}

static int
int_mxdiff (Stack stack, int rhs, int opt, int lhs)
{
  int dim=0;
  int order=1;
  NspMatrix *Res, *HMat;
  CheckRhs (1, 3);
  CheckLhs (1, 1);

  if ((HMat = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;

  if ( rhs > 1 )
    {
      if ( GetScalarInt(stack, 2, &order) == FAIL )
	return RET_BUG;
      CheckNonNegative(NspFname(stack),order,2);

      if ( rhs == 3 )
	if ( GetDimArg(stack, 3, &dim) == FAIL )
	  return RET_BUG;
    }
  
  if ((Res = nsp_mat_diff(HMat, order, dim)) == NULLMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

/*
 *nsp_mat_maxi: Maxi(*HMat);
 * A is unchanged 
 */

typedef NspMatrix *(*MiMax) (NspMatrix * A, char *, NspMatrix ** Imax,
			     int lhs);
typedef int (*MiMax1) (NspMatrix * A, NspMatrix * B, NspMatrix * Ind,
		       int j, int flag);

static int
int_mx_maxi (Stack stack, int rhs, int opt, int lhs, MiMax F, MiMax1 F1)
{
  char *str;
  NspMatrix *A, *M, *Imax, *B;
  if (rhs < 1)
    {
      Scierror ("Error:\t Rhs must be >= 1 for function %s\n", NspFname(stack));
      return RET_BUG;
    }
  CheckLhs (1, 2);
  if (rhs == 1 || (rhs == 2 && IsSMatObj (stack, 2)))
    {
      /* maxi(A) or maxi(A,'c' or 'r' or 'F') where A is a matrix * */
      /* idem for mini * */
      if ((A = GetRealMat (stack, 1)) == NULLMAT)
	return RET_BUG;
      if (rhs == 2)
	{
	  if ((str = GetString (stack, 2)) == (char *) 0)
	    return RET_BUG;
	}
      else
	{
	  str = "F";
	}
      if ((M = (*F) (A, str, &Imax, lhs)) == NULLMAT)
	return RET_BUG;
      if (lhs == 2)
	{
	  MoveObj (stack, 2, (NspObject *) Imax);
	}
      MoveObj (stack, 1, (NspObject *) M);
    }
  else
    {
      NspMatrix *Ind=NULL;
      int flag = 0, i;
      /* Maxi(A1,A2,....,An) ** */
      if ((A = GetRealMatCopy (stack, 1)) == NULLMAT)
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
	  if ((B = GetRealMat (stack, i)) == NULLMAT)
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
int_mxmaxi (Stack stack, int rhs, int opt, int lhs)
{
  return (int_mx_maxi (stack, rhs, opt, lhs, nsp_mat_maxi, nsp_mat_maxitt1));
}


/*
 *nsp_mat_mini: Mini(A)
 * A is unchanged 
 * rs and ri are set to the result 
 */

int
int_mxmini (Stack stack, int rhs, int opt, int lhs)
{
  return (int_mx_maxi (stack, rhs, opt, lhs, nsp_mat_mini, nsp_mat_minitt1));
}


/* 
 *  [amin, amax, imin, imax] = minmax(A) or minmax(A,'c' or 'r' or 'F')
 *  to compute min and max at same time 3n/2 comparizons in place of 2n
 *  Routine introduced by Bruno Pinçon
 */
static int
int_mxminmax(Stack stack, int rhs, int opt, int lhs)
{
  char *str;
  NspMatrix *A, *Amin, *Imin, *Amax, *Imax;

  if ( rhs < 1  ||  rhs > 2 )
    {
      Scierror ("Error:\t Rhs must be 1 or 2 for function %s\n", NspFname(stack));
      return RET_BUG;
    }
  CheckLhs (2, 4);


  if ((A = GetRealMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (rhs == 2)
    {
      if ((str = GetString (stack, 2)) == (char *) 0)
	return RET_BUG;
    }
  else
    {
      str = "F";
    }

  if ( nsp_mat_minmax(A, str, &Amin, &Imin, &Amax, &Imax, lhs) == FAIL )
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
 *nsp_mat_triu: A=Triu(a)
 * A is changed  
 */

int
int_mxtriu (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspMatrix *HMat;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((HMat = GetMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  nsp_mat_triu (HMat, k1);
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

/*
 *nsp_mat_tril: A=Tril(A)
 * A is changed  
 */

int
int_mxtril (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspMatrix *HMat;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((HMat = GetMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  nsp_mat_tril (HMat, k1);
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

/*
 *nsp_mat_ones: A=ones(m,n)
 * A is created , m,n no
 */

typedef NspMatrix *(*Mfunc) (int m, int n);

/* generic function for ones,rand,eyes **/

static int
int_mx_gen (Stack stack, int rhs, int opt, int lhs, Mfunc F)
{
  int m1, n1;
  NspMatrix *HMat;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 1, &m1) == FAIL)
	return RET_BUG;
      CheckNonNegative(NspFname(stack),m1,1);
      if (GetScalarInt (stack, 2, &n1) == FAIL)
	return RET_BUG;
      CheckNonNegative(NspFname(stack),n1,2);
    }
  else
    {
      if ((HMat = GetMat (stack, 1)) == NULLMAT)
	return RET_BUG;
      m1 = HMat->m;
      n1 = HMat->n;
    }
  if ((HMat = (*F) (m1, n1)) == NULLMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) HMat);
  return 1;
}

int
int_mxones (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen (stack, rhs, opt, lhs, nsp_mat_ones);
}

/*
 *nsp_mat_rand:
 * A=rand(m,n [,type])
 */

int
int_mxrand (Stack stack, int rhs, int opt, int lhs)
{
  static char *Table_R[] = { "uniform", "normal", NULL };
  char *str;
  int m, n, m1, tkp, tk;
  NspMatrix *A;
  NspObject *O;
  CheckRhs (0, 3);
  CheckLhs (1, 1);
  switch (rhs)
    {
    case 0:

      if ((A = nsp_mat_rand (1, 1)) == NULLMAT)
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
	  if ((O = (NspObject *) nsp_mat_rand (m, n)) == NULLOBJ)
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
	  if ((A = nsp_mat_rand (m, n)) == NULLMAT)
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
      if ((O = (NspObject *) nsp_mat_rand (m, n)) == NULLOBJ)
	return RET_BUG;
      MoveObj (stack, 1, O);
      /* restore rand type * */
      nsp_set_urandtype (tkp);
      return 1;
    }
  return 0;
}


/*
 *nsp_mat_eye: A=Eye(m,n)
 * A is created  m,n no
 */

int
int_mxeye (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen (stack, rhs, opt, lhs, nsp_mat_eye);
}

/*
 *nsp_mat_zeros: A=zeros(m,n)
 * A is created  m,n no
 */

int
int_mxzeros (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen (stack, rhs, opt, lhs, nsp_mat_zeros);
}

/*
 * setr(A,d) <=> real(A) = d, im(A) unchanged 
 */

int
int_mxsetr (Stack stack, int rhs, int opt, int lhs)
{
  double dval;
  NspMatrix *HMat;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if (GetScalarDouble (stack, 2, &dval) == FAIL)
    return RET_BUG;
  /* xxxxx verifier que la matrice a un nom * */
  if ((HMat = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  nsp_mat_set_rval (HMat, dval);
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

/*
 * seti(A,d) <=> imag(A) = d, real(A) unchanged 
 */

int
int_mxseti (Stack stack, int rhs, int opt, int lhs)
{
  double dval;
  NspMatrix *HMat;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if (GetScalarDouble (stack, 2, &dval) == FAIL)
    return RET_BUG;
  /* xxxxx verifier que la matrice a un nom * */
  if ((HMat = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (nsp_mat_set_ival (HMat, dval) != OK)
    return RET_BUG;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

/*
 * A = Matclean(a [,epsa,[epsr]]) clean A according to eps 
 * A is changed : 
 * XXXX A Changer : mettre le vrai eps 
 */

int
int_mxclean (Stack stack, int rhs, int opt, int lhs)
{
  double epsa =1.e-10, epsr = 1.e-10;
  NspMatrix *HMat;
  CheckRhs (1, 3);
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
  if ((HMat = GetMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  nsp_mat_clean (HMat, rhs, epsa, epsr);
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

/*
 * Operation leading to Boolean result 
 */

/* A < B */
int
int_mxlt (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *B;
  NspBMatrix *Res;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((A = GetRealMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((B = GetRealMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  Res = nsp_mat_comp (A, B, "<");
  if (Res == NULLBMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

int
int_mxle (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *B;
  NspBMatrix *Res;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((A = GetRealMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((B = GetRealMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  Res = nsp_mat_comp (A, B, "<=");
  if (Res == NULLBMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

int
int_mxneq (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *B;
  NspBMatrix *Res;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((A = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((B = GetMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  Res = nsp_mat_comp (A, B, "<>");
  if (Res == NULLBMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

int
int_mxeq (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *B;
  NspBMatrix *Res;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((A = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((B = GetMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  Res = nsp_mat_comp (A, B, "==");
  if (Res == NULLBMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

int
int_mxgt (Stack stack, int rhs, int opt, int lhs)
{

  NspMatrix *A, *B;
  NspBMatrix *Res;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((A = GetRealMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((B = GetRealMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  Res = nsp_mat_comp (A, B, ">");
  if (Res == NULLBMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}


int
int_mxge (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *B;
  NspBMatrix *Res;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((A = GetRealMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((B = GetRealMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  Res = nsp_mat_comp (A, B, ">=");
  if (Res == NULLBMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

/*
 * Same but returns a unique boolean 
 */

static int
int_mxf_gen (Stack stack, int rhs, int opt, int lhs, char *op)
{
  int rep, err;
  NspMatrix *A, *B;
  NspObject *Res;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((A = GetRealMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((B = GetRealMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  rep = nsp_mat_fullcomp (A, B, op, &err);
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
int_mxflt (Stack stack, int rhs, int opt, int lhs)
{
  return int_mxf_gen (stack, rhs, opt, lhs, "<");
}

int
int_mxfle (Stack stack, int rhs, int opt, int lhs)
{
  return int_mxf_gen (stack, rhs, opt, lhs, "<=");
}

int
int_mxfne (Stack stack, int rhs, int opt, int lhs)
{
  return int_mxf_gen (stack, rhs, opt, lhs, "<>");
}

int
int_mxfeq (Stack stack, int rhs, int opt, int lhs)
{
  return int_mxf_gen (stack, rhs, opt, lhs, "==");
}

int
int_mxfgt (Stack stack, int rhs, int opt, int lhs)
{
  return int_mxf_gen (stack, rhs, opt, lhs, ">");
}

int
int_mxfge (Stack stack, int rhs, int opt, int lhs)
{
  return int_mxf_gen (stack, rhs, opt, lhs, ">=");
}


/*
 * Matredim : changes Matrix dimensions
 * but keeps m*n constant
 * WARNING : Object on stack is changed 
 * =======
 */

int
int_mxredim (Stack stack, int rhs, int opt, int lhs)
{
  int m1, n1;
  NspMatrix *HMat;
  CheckRhs (3, 3);
  CheckLhs (1, 1);
  if ((HMat = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (GetScalarInt (stack, 2, &m1) == FAIL)
    return RET_BUG;
  if (GetScalarInt (stack, 3, &n1) == FAIL)
    return RET_BUG;
  if (nsp_matint_redim (NSP_OBJECT(HMat), m1, n1) != OK)
    return RET_BUG;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

/* xxxx ecrire le Enlarge ***/

/*
 * Right Concatenation 
 * A= [A,B] 
 */

int int_mxconcatr(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat1, *HMat2;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((HMat1 = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((HMat2 = GetMat (stack, 2)) == NULLMAT)
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
    }
  else
    {
      if ((HMat1 = GetMatCopy (stack, 1)) == NULLMAT)	return RET_BUG;
      if (nsp_matrix_concat_right (HMat1, HMat2) != OK)	return RET_BUG;
    }
  NSP_OBJECT(HMat1)->ret_pos = 1;
  return 1;
}




/*
 * Right Concatenation Mat & BMat  
 * A= [A,B] 
 * we have to deal with [] 
 * if Mat<>[]  -->  [Mat,Bmat] -> [Mat,b2m(BMat)] 
 * if Mat==[]  -->  [Mat,Bmat] -> BMat
 */

int
int_mxconcatr_mb (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if (IsMatObj (stack, 1))
    {
      if ((HMat = GetMat (stack, 1)) == NULLMAT)
	return RET_BUG;
      if (HMat->mn == 0)
	{
	  /* [[],B] --> B * */
	  NSP_OBJECT (NthObj (2))->ret_pos = 1;
	  return 1;
	}
      else
	{
	  /* [A,B] --> [A, b2m(B)] * */
	  stack.first += 1;
	  if (int_bmatrix_b2m (stack, 1, 0, 1) < 0) return RET_BUG;
	  stack.first -= 1;
	  NSP_OBJECT (NthObj (2))->ret_pos = -1;
	  return int_mxconcatr (stack, rhs, opt, lhs);
	}
    }
  else
    {
      /* we can get here when called from boolean matrix interface 
       * [B,A]-> [ b2m(B),A]
       */
      if ((HMat = GetMat (stack, 2)) == NULLMAT)
	return RET_BUG;
      if (HMat->mn == 0)
	{
	  NthObj (1)->ret_pos = 1;
	  return 1;
	}
      if (int_bmatrix_b2m (stack, 1, 0, 1) < 0) return RET_BUG;
      return int_mxconcatr (stack, rhs, opt, lhs);
    }
}


/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLMAT on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

typedef NspMatrix *(*Fconcat) (const NspMatrix *, const NspMatrix *);

static int
int_mx_concat (Stack stack, int rhs, int opt, int lhs, Fconcat F)
{
  NspMatrix *HMat1, *HMat2;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((HMat1 = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((HMat2 = GetMat (stack, 2)) == NULLMAT)
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
      if ( HMat1 == HMat2 )  NthObj(2) = NULLOBJ;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  else
    {
      NspMatrix *HMat3;
      if ((HMat3 = (*F) (HMat1, HMat2)) == NULLMAT)
	return RET_BUG;
      MoveObj (stack, 1, (NspObject *) HMat3);
    }
  return 1;
}

int
int_mxconcatd_old (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_concat (stack, rhs, opt, lhs, nsp_matrix_concat_down);
}

/* The same but using the matint implementation. 
 *
 */

int int_mxconcatd (Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_concat_down(stack,rhs,opt,lhs,(Fconcat_d)nsp_matint_concat_down);
}

/*
 * MatAddCols : add n cols of zero to Matrix A 
 * A= [A,ones(m,n)] 
 * A is changed 
 */

int
int_mxaddcols (Stack stack, int rhs, int opt, int lhs)
{
  int n1;
  NspMatrix *HMat;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((HMat = GetMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (GetScalarInt (stack, 2, &n1) == FAIL)
    return RET_BUG;
  if (nsp_matrix_add_columns (HMat, n1) != OK)
    return RET_BUG;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}


/*
 * Add Rows : Add m rows of zero to a Matrix A 
 * A = [A;0*ones(m,n)]
 * return NULLMAT on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

int
int_mxaddrows (Stack stack, int rhs, int opt, int lhs)
{
  int m1;
  NspMatrix *HMat;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((HMat = GetMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (GetScalarInt (stack, 2, &m1) == FAIL)
    return RET_BUG;
  if (nsp_matrix_add_rows (HMat, m1) != OK)
    return RET_BUG;;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

/*
 * Scilab diag function 
 */

int
int_mxdiag (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspMatrix *A, *Res;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((A = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (A->m == 1 || A->n == 1)
    Res = nsp_matrix_create_diag (A, k1);
  else
    Res = nsp_matrix_extract_diag (A, k1);
  if (Res == NULLMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}


/*
 * Returns the kthe diag of a Matrix 
 */

int
int_mxdiage (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspMatrix *A, *Res;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((A = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  Res = nsp_matrix_extract_diag (A, k1);
  if (Res == NULLMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

/*
 * Set the kth Diag of A to Diag 
 *  A is enlarged & comlexified if necessary 
 *  int nsp_matrix_create_diag(A,Diag,k)
 * WARNING : A is not copied we want this routine to change A
 */

int
int_mxdiagset (Stack stack, int rhs, int opt, int lhs)
{
  int k1;
  NspMatrix *A, *Diag;
  CheckRhs (3, 3);
  CheckLhs (1, 1);
  if ((A = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((Diag = GetMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  if (GetScalarInt (stack, 3, &k1) == FAIL)
    return RET_BUG;
  if (nsp_matrix_set_diag (A, Diag, k1) != OK)
    return RET_BUG;
  NSP_OBJECT (A)->ret_pos = 1;
  return 1;
}

/*
 *  Creates a Matrix with kth diag set to Diag 
 */

int
int_mxdiagcre (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspMatrix *Diag, *Res;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if ((Diag = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((Res = nsp_matrix_create_diag (Diag, k1)) == NULLMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

/*
 * Matresize : changes Matrix dimensions
 *             m,n are changed and the arrays enlarged or schrinked 
 * 
 */

int
int_mxresize (Stack stack, int rhs, int opt, int lhs)
{
  int m1, n1;
  NspMatrix *HMat;
  CheckRhs (3, 3);
  CheckLhs (1, 1);
  if ((HMat = GetMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (GetScalarInt (stack, 2, &m1) == FAIL)
    return RET_BUG;
  if (GetScalarInt (stack, 3, &n1) == FAIL)
    return RET_BUG;
  if (nsp_matrix_resize (HMat, m1, n1) != OK)
    return RET_BUG;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

/*
 * A'
 */

int
int_mxquote (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *B;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((A = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((B = nsp_matrix_transpose (A)) == NULLMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) B);
  return 1;
}

/*
 * _
 * A'
 */

int
int_mxdquote (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *B;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((A = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((B = nsp_matrix_transpose (A)) == NULLMAT)
    return RET_BUG;
  nsp_mat_conj (B);
  MoveObj (stack, 1, (NspObject *) B);
  return 1;
}


/*
 * nsp_matrix_latex_print: writes Mat Objet on fd in tex language
 */

int
int_mx2latexmat (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  nsp_matrix_latex_print (HMat);
  return 0;
}

/*
 *  Mat2LaTeXTab: writes Mat Objet on fd in TeX language
 */

int
int_mx2latextab (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  nsp_matrix_latex_tab_print (HMat);
  return 0;
}

/*
 *  A=op(A) 
 */

typedef int (*M11) (NspMatrix * A);

/* generic function for ones,rand,eyes */

static int
int_mx_gen11 (Stack stack, int rhs, int opt, int lhs, M11 F)
{
  NspMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (HMat->mn == 0)
    {
      NSP_OBJECT (HMat)->ret_pos = 1;
      return 1;
    }
  if ((HMat = GetMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (((*F) (HMat)) < 0)
    return RET_BUG;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

typedef void (*VM11) (NspMatrix * A);

static int
int_mx_genv11 (Stack stack, int rhs, int opt, int lhs, VM11 F)
{
  NspMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (HMat->mn == 0)
    {
      NSP_OBJECT (HMat)->ret_pos = 1;
      return 1;
    }
  if ((HMat = GetMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  (*F) (HMat);
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}


/*
 * A=Abs(A), absolue value or module of each element 
 */

int
int_mxabs (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_abs);
}

/*
 * A=Erf(A)
 */

int
int_mxerf (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_erf);
}

/*
 * A=Erfc(A),  * A is changed 
 */

int
int_mxerfc (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_erfc);
}

/*
 * A=gamma(A)
 */

int
int_mxgamma(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_tgamma);
}

/*
 * A=gammaln(A),  * A is changed 
 */

int
int_mxgammaln (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_lgamma);
}


/*
 * A=Arg(A),  * A is changed 
 */

int
int_mxarg (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_arg);
}

/*
 *nsp_mat_cos: A=Cos(A)
 * A is changed  
 * return 0 if error 
 */

int
int_mxcos (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11 (stack, rhs, opt, lhs, nsp_mat_cos);
}

/*
 *nsp_mat_cosh: A=Cosh(A)
 * A is changed  
 * return 0 if error 
 */

int
int_mxcosh (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11 (stack, rhs, opt, lhs, nsp_mat_cosh);
}

/*
 * MatExpl : Exponentiation terme a term 
 * A is changed 
 */

int
int_mxexpel (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11 (stack, rhs, opt, lhs, nsp_mat_expel);
}

/*
 * MatLog : A=LogEl(A) 
 */

int
int_mxlogel (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_logel);
}


/*
 *nsp_mat_sin: A=Sin(A)
 * A is changed  
 * return 0 if error 
 */

int
int_mxsin (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11 (stack, rhs, opt, lhs, nsp_mat_sin);
}

/*
 *nsp_mat_sinh: A=Sinh(A)
 * A is changed  
 * return 0 if error 
 */


int
int_mxsinh (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11 (stack, rhs, opt, lhs, nsp_mat_sinh);
}


/*
 *nsp_mat_sqrtel: A=SqrtEl(A)  term to term square root
 * A is changed  
 * return 0 if error 
 */

int
int_mxsqrtel (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_sqrtel);
}

/*
 *nsp_mat_acos: A=Acos(A)
 * A is changed  
 * return 0 if error 
 */

int
int_mxacos (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_acos);
}

/*
 *nsp_mat_acosh: A=Acosh(A)
 * A is changed  
 * return 0 if error 
 */

int
int_mxacosh (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_acosh);
}

/*
 *nsp_mat_asin: A=Asin(A)
 * A is changed  
 * return 0 if error 
 */

int
int_mxasin (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_asin);
}

/*
 *nsp_mat_asinh: A=Asinh(A)
 * A is changed  
 * return 0 if error 
 */


int
int_mxasinh (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_asinh);
}

/*
 * MatATan : res= arctang(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

int
int_mxatan (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_atan);
}

/*
 * Atan2(A,B), 
 */

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

int
int_mxatan2 (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *B;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((A = GetRealMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  NSP_OBJECT (A)->ret_pos = 1;
  if (A->mn == 0)
    {
      NSP_OBJECT (A)->ret_pos = 1;
      return 1;
    }
  if ((B = GetRealMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  CheckSameDims (NspFname(stack), 1, 2, A, B);
  if (nsp_mat_atan2 (A, B) == FAIL)
    return RET_BUG;
  return 1;
}

/*
 * MatArcTangH : res= atanh(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

int
int_mxatanh (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_atanh);
}

/*
 *nsp_mat_ceil: A=Ceil(A)
 * A is changed  
 */

int
int_mxceil (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11 (stack, rhs, opt, lhs, nsp_mat_ceil);
}

/*
 *nsp_mat_int: A=Int(A)
 * A is changed  
 */

int
int_mxint (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11 (stack, rhs, opt, lhs, nsp_mat_int);
}

/*
 *nsp_mat_floor: A=Floor(A)
 * A is changed  
 */

int
int_mxfloor (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11 (stack, rhs, opt, lhs, nsp_mat_floor);
}

/*
 *nsp_mat_round: A=Round(A)
 * A is changed  
 */

int
int_mxround (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_genv11 (stack, rhs, opt, lhs, nsp_mat_round);
}

/*
 *nsp_mat_sign: A=Sign(A)
 * A is changed  
 * return 0 if error 
 */

int
int_mxsign (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_sign);
}

/*
 *nsp_mat_tan: A=Tan(A)
 * A is changed  
 * return 0 if error 
 */

int
int_mxtan (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_tan);
}

/*
 *nsp_mat_tanh: A=Tanh(A)
 * A is changed  
 * return 0 if error 
 */

int
int_mxtanh (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_tanh);
}

/*
 *nsp_mat_minus: A=-(A)
 * A is changed  
 * return 0 if error 
 */

int
int_mxminus (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_gen11 (stack, rhs, opt, lhs, nsp_mat_minus);
}


/*
 * A=Polar(A,B),  * A is changed 
 */


int
int_mxpolar (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *B;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((A = GetMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  NSP_OBJECT (A)->ret_pos = 1;
  if (A->mn == 0)
    {
      NSP_OBJECT (A)->ret_pos = 1;
      return 1;
    }
  if ((B = GetMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  if (SameDim (A, B))
    {
      if (nsp_mat_polar (A, B) < 0)
	return RET_BUG;
    }
  else
    {
      Scierror ("Error: %s Mat1 & Mat2 don't have same size \n", NspFname(stack));
      return RET_BUG;
    }
  return 1;
}

/*
 * A=iand(A,B)
 */

int
int_mxiand (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *B;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if ((A = GetRealMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  NSP_OBJECT (A)->ret_pos = 1;
  if (A->mn == 0)
    {
      NSP_OBJECT (A)->ret_pos = 1;
      return 1;
    }
  if (rhs == 2)
    {
      if ((B = GetRealMat (stack, 2)) == NULLMAT)
	return RET_BUG;
      if (SameDim (A, B))
	{
	  if (nsp_mat_iand (A, B) == FAIL)
	    return RET_BUG;
	}
      else
	{
	  Scierror ("Error: %s Mat1 & Mat2 don't have same size \n",
		    NspFname(stack));
	  return RET_BUG;
	}
    }
  else
    {
      unsigned res;
      if (nsp_mat_iandu (A, &res) == FAIL)
	return RET_BUG;
      if (nsp_matrix_resize (A, 1, 1) == FAIL)
	return (FAIL);
      A->R[0] = res;
    }
  return 1;
}

/*
 * A= ior(A,B)
 */

int
int_mxior (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *B;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if ((A = GetRealMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  NSP_OBJECT (A)->ret_pos = 1;
  if (A->mn == 0)
    {
      NSP_OBJECT (A)->ret_pos = 1;
      return 1;
    }
  if (rhs == 2)
    {
      if ((B = GetRealMat (stack, 2)) == NULLMAT)
	return RET_BUG;
      if (SameDim (A, B))
	{
	  if (nsp_mat_ior (A, B) == FAIL)
	    return RET_BUG;
	}
      else
	{
	  Scierror ("Error: %s Mat1 & Mat2 don't have same size \n",
		    NspFname(stack));
	  return RET_BUG;
	}
    }
  else
    {
      unsigned res;
      if (nsp_mat_ioru (A, &res) == FAIL)
	return RET_BUG;
      if (nsp_matrix_resize (A, 1, 1) == FAIL)
	return (FAIL);
      A->R[0] = res;
    }
  return 1;
}


/*
 * A=ishift(A,n,['r'|'l'])
 */

int int_mxishift (Stack stack, int rhs, int opt, int lhs)
{
  int shift;
  char dir='l';
  NspMatrix *A;
  CheckRhs(2,3);
  CheckLhs(1, 1);
  if ((A = GetRealMatCopy (stack, 1)) == NULLMAT)
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
  if ( nsp_mat_ishift(A,shift,dir) == FAIL)
    return RET_BUG;
  return 1;
}


/*
 *nsp_mat_conj: A=real(A)-i*Imag(A)
 * A is changed  if imaginary not changed if real 
 */

int
int_mxconj (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (HMat->mn == 0 || HMat->rc_type == 'r')
    {
      NSP_OBJECT (HMat)->ret_pos = 1;
      return 1;
    }
  if ((HMat = GetMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  nsp_mat_conj (HMat);
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}


/*
 * nsp_mat_mod: z = mod(x,y) x or y is changed 
 */
int
int_mxmod(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x, *y;
  CheckRhs (2, 2);
  CheckLhs (1, 1);

  if ((x = GetMat(stack, 1)) == NULLMAT)
    return RET_BUG;

  if ((y = GetMat(stack, 2)) == NULLMAT)
    return RET_BUG;

  if ( x->rc_type != 'r' ||  y->rc_type != 'r' )
    {
      Scierror ("Error: %s both arguments must be real \n", NspFname(stack));
      return RET_BUG;
    }

  if ( x->mn == 1  &&  y->mn > 1 )
    {
      if ((y = GetMatCopy(stack, 2)) == NULLMAT)
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
      if ((x = GetMatCopy(stack, 1)) == NULLMAT)
	return RET_BUG;
      NSP_OBJECT(x)->ret_pos = 1;
    }

  nsp_mat_mod(x, y);
  return 1;
}

/*
 *nsp_mat_modulo: A=Modulo(A) remainder in int division 
 * A is changed  
 */

int
int_mxmodulo (Stack stack, int rhs, int opt, int lhs)
{
  int n;
  NspMatrix *HMat;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((HMat = GetMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (GetScalarInt (stack, 2, &n) == FAIL)
    return RET_BUG;
  nsp_mat_modulo (HMat, n);
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}

/*
 * Mat : A= Idiv(A,b) quotient in int division
 * A is changed   A(i)=A(i)/b 
 */

int
int_mxidiv (Stack stack, int rhs, int opt, int lhs)
{
  int n;
  NspMatrix *HMat;
  CheckRhs (2, 2);
  CheckLhs (1, 2);
  if ((HMat = GetMatCopy (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (GetScalarInt (stack, 2, &n) == FAIL)
    return RET_BUG;
  if (HMat->mn != 0)
    nsp_mat_idiv (HMat, n);
  NSP_OBJECT (HMat)->ret_pos = 1;
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

typedef int (*MPM) (NspMatrix *, NspMatrix *);

int
MatNoOp (NspMatrix * A)
{
  return OK;
}

static int
int_mx_mopscal (Stack stack, int rhs, int opt, int lhs, MPM F1, MPM F2,
		MPM F3, M11 F4, int flag)
{
  NspMatrix *HMat1, *HMat2;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((HMat1 = GetMatCopy (stack, 1)) == NULLMAT)
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
	  if (F4 != MatNoOp)
	    {
	      if ((HMat2 = GetMatCopy (stack, 2)) == NULLMAT)
		return RET_BUG;
	      if ((*F4) (HMat2) == FAIL)
		return RET_BUG;
	      NSP_OBJECT (HMat2)->ret_pos = 1;
	    }
	  else
	    {
	      if ((HMat2 = GetMat (stack, 2)) == NULLMAT)
		return RET_BUG;
	      NSP_OBJECT (HMat2)->ret_pos = 1;
	    }
	  return 1;
	}
    }
  if ((HMat2 = GetMat (stack, 2)) == NULLMAT)
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
      if ((HMat2 = GetMatCopy (stack, 2)) == NULLMAT)
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

/* same than before but used for a more Matlab compliant behavior :
 *
 *  A op scalar --->  F1(A,scalar)  result is of same dim than A
 *  A op B      --->  F2(A,B)       for A and B with same dims
 *  scalar op A --->  F3(A,scalar)  result is of same dim than A
 */
#ifdef MTLB_MODE

static int
int_mx_mopscal_mtlb(Stack stack, int rhs, int opt, int lhs, MPM F1, MPM F2, MPM F3)
{
  NspMatrix *HMat1, *HMat2, *HMat3;
  int HMat1_has_no_name, HMat2_has_no_name;
  CheckRhs (2, 2);
  CheckLhs (1, 1);

  if ( (HMat1 =GetMat(stack, 1)) == NULLMAT )
    return RET_BUG;
  HMat1_has_no_name = Ocheckname(HMat1,NVOID);
  
  if ( (HMat2 =GetMat(stack, 2)) == NULLMAT )
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
	      if ( (HMat3 =nsp_matrix_copy(HMat1)) == NULLMAT )
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
	      if ( (HMat3 =nsp_matrix_copy(HMat2)) == NULLMAT )
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
	  if ( (HMat3 =nsp_matrix_copy(HMat1)) == NULLMAT )
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
	  if ( (HMat3 =nsp_matrix_copy(HMat1)) == NULLMAT )
	    return RET_BUG;
	  if ( (*F2)(HMat3, HMat2) == FAIL )
	    return RET_BUG;
	  MoveObj(stack, 1, (NspObject *) HMat3);
	}
    }
  return 1;
}
#endif


/*
 * term to term addition 
 * with special cases Mat + [] and Mat + scalar
 */

int
int_mxdadd (Stack stack, int rhs, int opt, int lhs)
{
#ifdef MTLB_MODE
  return int_mx_mopscal_mtlb(stack, rhs, opt, lhs,
			     nsp_mat_add_scalar_bis, nsp_mat_add_mat, 
			     nsp_mat_add_scalar_bis);

#else
  return int_mx_mopscal (stack, rhs, opt, lhs,
			 nsp_mat_add_scalar, nsp_mat_dadd, nsp_mat_add_scalar,
			 MatNoOp, 0);
#endif
}

/* FIXME: 
 *   just a test version 
 *
 */

NspMatrix *GetMatSafeCopy (Stack stack, int i)
{
  return Mat2double(MaybeObjCopy (&NthObj (i)));
}

NspMatrix *GetMatSafe(Stack stack, int i)
{
  NspObject *ob = NthObj(i);
  HOBJ_GET_OBJECT(ob,NULL);
  return Mat2double ((NspMatrix *) ob);
}

static int
int_mx_mopscal1(Stack stack, int rhs, int opt, int lhs, MPM F1, MPM F2,
		MPM F3, M11 F4, int flag)
{
  NspMatrix *HMat1, *HMat2;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((HMat1 = GetMatSafeCopy (stack, 1)) == NULLMAT)
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
	  if (F4 != MatNoOp)
	    {
	      if ((HMat2 = GetMatSafeCopy (stack, 2)) == NULLMAT)
		return RET_BUG;
	      if ((*F4) (HMat2) == FAIL)
		return RET_BUG;
	      NSP_OBJECT (HMat2)->ret_pos = 1;
	    }
	  else
	    {
	      if ((HMat2 = GetMatSafe (stack, 2)) == NULLMAT)
		return RET_BUG;
	      NSP_OBJECT (HMat2)->ret_pos = 1;
	    }
	  return 1;
	}
    }
  if ((HMat2 = GetMatSafe (stack, 2)) == NULLMAT)
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
      if ((HMat2 = GetMatSafeCopy (stack, 2)) == NULLMAT)
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


int
int_mxdadd1(Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_mopscal1 (stack, rhs, opt, lhs,
			 nsp_mat_add_scalar, nsp_mat_dadd, nsp_mat_add_scalar,
			 MatNoOp, 0);
}



/*
 * term to term substraction 
 * with special cases Mat - [] and Mat - scalar
 *  XXXXX Attention le cas F3 est faux scalar - Mat --> Mat -scalar  
 */

int
int_mxdsub (Stack stack, int rhs, int opt, int lhs)
{
#ifdef MTLB_MODE
  return int_mx_mopscal_mtlb(stack, rhs, opt, lhs,
			     nsp_mat_sub_scalar_bis, nsp_mat_sub_mat, 
			     nsp_scalar_sub_mat_bis);

#else
  return int_mx_mopscal (stack, rhs, opt, lhs,
			 nsp_mat_sub_scalar, nsp_mat_dsub,
			 nsp_mat_subs_calarm, nsp_mat_minus, 0);
#endif
}

/*
 * A=nsp_mat_pow(A,B), A^ B 
 */

int
int_mxpow (Stack stack, int rhs, int opt, int lhs)
{
  return int_mx_mopscal (stack, rhs, opt, lhs,
			 nsp_mat_pow_matscalar, nsp_mat_pow_matmat,
			 nsp_mat_pow_scalarmat, MatNoOp, 1);
}

/*
 * A=nsp_mat_pow_el(A,B), A.^ B 
 */

int
int_mxpowel (Stack stack, int rhs, int opt, int lhs)
{
#ifdef MTLB_MODE
  return int_mx_mopscal_mtlb (stack, rhs, opt, lhs,
			      nsp_mat_pow_scalar, nsp_mat_pow_el,
			      nsp_mat_pow_scalarm);
#else
  return int_mx_mopscal (stack, rhs, opt, lhs,
			 nsp_mat_pow_scalar, nsp_mat_pow_el,
			 nsp_mat_pow_scalarm, MatNoOp, 1);
#endif
}

/*
 * A=DivEl(A,B),  A ./ B 
 */

int
int_mxdivel (Stack stack, int rhs, int opt, int lhs)
{
#ifdef MTLB_MODE
  return int_mx_mopscal_mtlb (stack, rhs, opt, lhs,
			      nsp_mat_div_scalar, nsp_mat_div_el,
			      nsp_mat_bdiv_scalar);
#else
  return int_mx_mopscal (stack, rhs, opt, lhs,
			 nsp_mat_div_scalar, nsp_mat_div_el,
			 nsp_mat_bdiv_scalar, MatNoOp, 1);
#endif
}


/*
 * A=BackDivEl(A,B),  A .\ B 
 */

int
int_mxbackdivel (Stack stack, int rhs, int opt, int lhs)
{
#ifdef MTLB_MODE
  return int_mx_mopscal_mtlb (stack, rhs, opt, lhs,
			 nsp_mat_bdiv_scalar, nsp_mat_bdiv_el,
			 nsp_mat_div_scalar);
#else
  return int_mx_mopscal (stack, rhs, opt, lhs,
			 nsp_mat_bdiv_scalar, nsp_mat_bdiv_el,
			 nsp_mat_div_scalar, MatNoOp, 1);
#endif
}


/*
 * A=MultEl(A,B),  A .* B 
 */

int
int_mxmultel (Stack stack, int rhs, int opt, int lhs)
{
#ifdef MTLB_MODE
  return int_mx_mopscal_mtlb(stack, rhs, opt, lhs,
			     nsp_mat_mult_scalar_bis, nsp_mat_mult_el, 
			     nsp_mat_mult_scalar_bis);
#else
  return int_mx_mopscal (stack, rhs, opt, lhs,
			 nsp_mat_mult_scalar, nsp_mat_mult_el,
			 nsp_mat_mult_scalar, MatNoOp, 1);
#endif
}


/*
 * NspMatrix multiplication  Res= A*B  
 * very similar to mopscal but MatMult returns a new matrix 
 */
#ifdef MTLB_MODE
int
int_mxmult (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat1, *HMat2, *HMat3;
  CheckRhs (2, 2);
  CheckLhs (1, 1);

  if ( (HMat1 =GetMat(stack, 1)) == NULLMAT )
    return RET_BUG;
  if ( (HMat2 = GetMat(stack, 2)) == NULLMAT )
    return RET_BUG;

  if ( HMat1->mn == 1 )
    {
      if ( (HMat2 = GetMatCopy(stack, 2)) == NULLMAT )
	return RET_BUG;
      if ( nsp_mat_mult_scalar_bis(HMat2, HMat1) == FAIL )
	return RET_BUG;
      NSP_OBJECT(HMat2)->ret_pos = 1;
    }
  else if ( HMat2->mn == 1 )
    {
      if ( (HMat1 = GetMatCopy(stack, 1)) == NULLMAT )
	return RET_BUG;
      if ( nsp_mat_mult_scalar_bis(HMat1, HMat2) == FAIL )
	return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  else
    {
      if ( (HMat3 = nsp_mat_mult(HMat1, HMat2)) == NULLMAT )
	return RET_BUG;
      MoveObj(stack, 1, (NspObject *) HMat3);
    }
  return 1;
}
/* int */
/* int_mxmult (Stack stack, int rhs, int opt, int lhs) */
/* { */
/*   NspMatrix *HMat1, *HMat2, *HMat3; */
/*   CheckRhs (2, 2); */
/*   CheckLhs (1, 1); */

/*   if ( (HMat1 =GetMat(stack, 1)) == NULLMAT ) */
/*     return RET_BUG; */
/*   if ( (HMat2 = GetMat(stack, 2)) == NULLMAT ) */
/*     return RET_BUG; */

/*   if ( HMat1->mn == 1 ) */
/*     { */
/*       if ( HMat2->mn == 1 ) */
/* 	{ */
/* 	  if ( Ocheckname(HMat1,NVOID) ) */
/* 	    { */
/* 	      if ( nsp_mat_mult_scalar_bis(HMat1, HMat2) == FAIL ) */
/* 		return RET_BUG; */
/* 	      NSP_OBJECT(HMat1)->ret_pos = 1; */
/* 	    } */
/* 	  else if ( Ocheckname(HMat2,NVOID) ) */
/* 	    { */
/* 	      if ( nsp_mat_mult_scalar_bis(HMat2, HMat1) == FAIL ) */
/* 		return RET_BUG; */
/* 	      NSP_OBJECT(HMat2)->ret_pos = 1; */
/* 	    } */
/* 	  else */
/* 	    { */
/* 	      if ( (HMat3 =nsp_matrix_copy(HMat1)) == NULLMAT ) */
/* 		return RET_BUG; */
/* 	      if ( nsp_mat_mult_scalar_bis(HMat3, HMat2) == FAIL ) */
/* 		return RET_BUG; */
/* 	      MoveObj(stack, 1, (NspObject *) HMat3); */
/* 	    } */
/* 	} */
/*       else */
/* 	{ */
/* 	  if ( Ocheckname(HMat2,NVOID) ) */
/* 	    { */
/* 	      if ( nsp_mat_mult_scalar_bis(HMat2, HMat1) == FAIL ) */
/* 		return RET_BUG; */
/* 	      NSP_OBJECT(HMat2)->ret_pos = 1; */
/* 	    } */
/* 	  else */
/* 	    { */
/* 	      if ( (HMat3 =nsp_matrix_copy(HMat2)) == NULLMAT ) */
/* 		return RET_BUG; */
/* 	      if ( nsp_mat_mult_scalar_bis(HMat3, HMat1) == FAIL ) */
/* 		return RET_BUG; */
/* 	      MoveObj(stack, 1, (NspObject *) HMat3); */
/* 	    } */
/* 	} */
/*     } */
/*   else if ( HMat2->mn == 1 ) */
/*     { */
/*       if ( Ocheckname(HMat1,NVOID) ) */
/* 	{ */
/* 	  if ( nsp_mat_mult_scalar_bis(HMat1, HMat2) == FAIL ) */
/* 	    return RET_BUG; */
/* 	  NSP_OBJECT(HMat1)->ret_pos = 1; */
/* 	} */
/*       else */
/* 	{ */
/* 	  if ( (HMat3 =nsp_matrix_copy(HMat1)) == NULLMAT ) */
/* 	    return RET_BUG; */
/* 	  if ( nsp_mat_mult_scalar_bis(HMat3, HMat2) == FAIL ) */
/* 	    return RET_BUG; */
/* 	  MoveObj(stack, 1, (NspObject *) HMat3); */
/* 	} */
/*     } */
/*   else */
/*     { */
/*       if ( (HMat3 = nsp_mat_mult(HMat1, HMat2)) == NULLMAT ) */
/* 	return RET_BUG; */
/*       MoveObj(stack, 1, (NspObject *) HMat3); */
/*     } */
/*   return 1; */
/* } */
#else
int
int_mxmult (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat1, *HMat2, *HMat3;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((HMat1 = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((HMat2 = GetMat (stack, 2)) == NULLMAT)
    return RET_BUG;

  if (HMat1->mn == 0)
    {
      if ( HMat1 == HMat2 ) NthObj(2) = NULLOBJ;
      NSP_OBJECT (HMat1)->ret_pos = 1;
      return 1;
    }
  if (HMat2->mn == 0)
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
  if (HMat2->mn == 1)
    {
      if ((HMat1 = GetMatCopy (stack, 1)) == NULLMAT)
	return RET_BUG;
      if (nsp_mat_mult_scalar (HMat1, HMat2) != OK)
	return RET_BUG;
      NSP_OBJECT (HMat1)->ret_pos = 1;
    }
  else if (HMat1->mn == 1)
    {
      /* since Mat1 is scalar we store the result in Mat2 so we 
         must copy it * */
      if ((HMat2 = GetMatCopy (stack, 2)) == NULLMAT)
	return RET_BUG;
      if (nsp_mat_mult_scalar (HMat2, HMat1) != OK)
	return RET_BUG;
      NSP_OBJECT (HMat2)->ret_pos = 1;
    }
  else
    {
      if ((HMat3 = nsp_mat_mult (HMat1, HMat2)) == NULLMAT)
	return RET_BUG;
      MoveObj (stack, 1, (NspObject *) HMat3);
    }
  return 1;
}
#endif

/*
 * NspMatrix back division  Res= A\B  
 */

#ifdef MTLB_MODE
int
int_mxbdiv (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat1, *HMat2, *x, *A;
  char tri_type;
  int info, stat;
  double rcond, tol_rcond;

  CheckRhs (2, 2);
  CheckLhs (1, 1);

  if ((HMat1 = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((HMat2 = GetMat (stack, 2)) == NULLMAT)
    return RET_BUG;

  if ( HMat1->m != HMat2->m )  /* FIXME : must we treat the scalar case ? */
                               /* A\scalar => x = inv(A)*scalar or pinv(A)*scalar */
    {
      Scierror("Error:\tIncompatible dimensions (in %s)\n", NspFname(stack));
      return RET_BUG;
    }

  if ( HMat1->mn == 0 || HMat2->mn == 0 )  /* the special case */ 
    {
      if ( (x =nsp_matrix_create(NVOID, 'r', HMat1->n, HMat2->n)) == NULLMAT )
	return RET_BUG;
      nsp_mat_set_rval(x,0.0);
      MoveObj (stack, 1, (NspObject *) x);
      return 1;
    }

  if ((HMat2 = GetMatCopy (stack, 2)) == NULLMAT) return RET_BUG;

  tol_rcond = Max(HMat1->m,HMat1->n)*nsp_dlamch("eps");

  if ( HMat1->m == HMat1->n )  /* HMat1 is square */
    {
      /* test if HMat1 is triangular */
      if ( nsp_mat_is_upper_triangular(HMat1) ) 
	tri_type = 'u';
      else if ( nsp_mat_is_lower_triangular(HMat1) ) 
	tri_type = 'l';
      else 
	tri_type = 'n';

      if ( tri_type != 'n' )
	{
	  if ( nsp_mat_bdiv_triangular(HMat1, HMat2, tri_type, &info) == FAIL ) 
	    return RET_BUG;

	  if ( info != 0 )   
	    /* important note: in this case the rhs HMat2 have not been modified */
	    Sciprintf("\n Warning: matrix is singular => computes a lsq solution");
	  else
	    {
	      NSP_OBJECT (HMat2)->ret_pos = 1; 
	      return 1;
	    }
	}
      else
	{
	  /* use a LU factorization */
	  /* here we must be sure to use a real copy of HMat1 (because if the matrix */
	  /* is badly conditionned we must switch to the lsq solution) */
	  if ( (A = nsp_matrix_copy(HMat1)) == NULLMAT ) return RET_BUG;
	  stat = nsp_mat_bdiv_square(A, HMat2, &rcond, tol_rcond);
	  nsp_matrix_destroy(A);
	  if ( stat == FAIL )
	    return RET_BUG;
	  else if ( rcond <= tol_rcond )
	    {
	      Sciprintf("\n Warning: matrix is badly conditionned (rcond = %g)",rcond);
	      Sciprintf("\n          => computes a lsq solution\n",rcond);
	    }
	  else
	    {
	      NSP_OBJECT (HMat2)->ret_pos = 1; 
	      return 1;
	    }
	}
    }
  
  if ( (HMat1 = GetMatCopy(stack, 1)) == NULLMAT )
    return RET_BUG;

  if ( nsp_mat_bdiv_lsq(HMat1, HMat2, tol_rcond) == FAIL )
    return RET_BUG;

  NSP_OBJECT (HMat2)->ret_pos = 1; 

  return 1;
}
#else
int
int_mxbdiv (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat1, *HMat2, *A;
  char tri_type;
  int info, stat;
  double rcond, tol_rcond;

  CheckRhs (2, 2);
  CheckLhs (1, 1);

  if ((HMat1 = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((HMat2 = GetMat (stack, 2)) == NULLMAT)
    return RET_BUG;

  if (HMat1->mn == 0)
    {
      if ( HMat1 == HMat2 ) NthObj(2) = NULLOBJ;
      NSP_OBJECT (HMat1)->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  if (HMat2->mn == 0)
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

  if ( HMat1->m != HMat2->m )  /* FIXME : the scalar case must be treated one day */
    {
      Scierror("Error:\tIncompatible dimensions (in %s)\n", NspFname(stack));
      return RET_BUG;
    }

  if ((HMat2 = GetMatCopy (stack, 2)) == NULLMAT) return RET_BUG;

  tol_rcond = Max(HMat1->m,HMat1->n)*nsp_dlamch("eps");

  if ( HMat1->m == HMat1->n )  /* HMat1 is square */
    {
      /* test if HMat1 is triangular or diagonal */
      if ( nsp_mat_is_upper_triangular(HMat1) ) 
	{
	  if ( nsp_mat_is_lower_triangular(HMat1) )
	    tri_type = 'd';
	  else
	    tri_type = 'u';
	}
      else if ( nsp_mat_is_lower_triangular(HMat1) ) tri_type = 'l';
      else tri_type = 'n';

      if ( tri_type != 'n' )
	{
	  if ( tri_type == 'd' )  /* diagonal matrix */
	    {
	      if ( nsp_mat_bdiv_diagonal(HMat1, HMat2, &info) == FAIL ) 
		return RET_BUG;
	    }
	  else                    /* triangular matrix */
	    {
	      if ( nsp_mat_bdiv_triangular(HMat1, HMat2, tri_type, &info) == FAIL ) 
		return RET_BUG;
	    }
	  if ( info != 0 )   
	    /* important note: in this case the rhs HMat2 have not been modified */
	    Sciprintf("\n Warning: matrix is singular => computes a lsq solution");
	  else
	    {
	      NSP_OBJECT (HMat2)->ret_pos = 1; 
	      return 1;
	    }
	}
      else
	{
	  /* use a LU factorization */
	  /* here we must be sure to use a real copy of HMat1 (because if the matrix */
	  /* is badly conditionned we must switch to the lsq solution) */
	  if ( (A = nsp_matrix_copy(HMat1)) == NULLMAT ) return RET_BUG;
	  stat = nsp_mat_bdiv_square(A, HMat2, &rcond, tol_rcond);
	  nsp_matrix_destroy(A);
	  if ( stat == FAIL )
	    return RET_BUG;
	  else if ( rcond <= tol_rcond )
	    {
	      Sciprintf("\n Warning: matrix is badly conditionned (rcond = %g)",rcond);
	      Sciprintf("\n          => computes a lsq solution\n",rcond);
	    }
	  else
	    {
	      NSP_OBJECT (HMat2)->ret_pos = 1; 
	      return 1;
	    }
	}
    }
  
  if ( (HMat1 = GetMatCopy(stack, 1)) == NULLMAT )
    return RET_BUG;

  if ( nsp_mat_bdiv_lsq(HMat1, HMat2, tol_rcond) == FAIL )
    return RET_BUG;

  NSP_OBJECT (HMat2)->ret_pos = 1; 

  return 1;
}
#endif

/*
 * A / B 
 * just implemented for scalars XXXXX 
 * result stored in A 
 */

int
int_mxdiv (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat1, *HMat2;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((HMat1 = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((HMat2 = GetMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  if (HMat2->mn <= 1)
    {
      return int_mx_mopscal (stack, rhs, opt, lhs,
			     nsp_mat_div_scalar, nsp_mat_div_el,
			     nsp_mat_bdiv_scalar, MatNoOp, 1);
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
int_mxfind (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A, *Rc, *Rr;
  CheckRhs (1, 1);
  CheckLhs (1, 2);
  if ((A = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if (nsp_mat_find (A, Max (lhs, 1), &Rr, &Rc) == FAIL)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Rr);
  if (lhs == 2)
    {
      NthObj (2) = (NspObject *) Rc;
      NSP_OBJECT (NthObj (2))->ret_pos = 2;
      return 2;
    }
  return 1;
}


/*
 * isinf 
 */

int
int_mx_isinf (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  NspBMatrix *B;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((A = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((B = nsp_mat_isinf (A)) == NULLBMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) B);
  return 1;
}

/*
 * isnan 
 */

int
int_mx_isnan (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  NspBMatrix *B;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((A = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((B = nsp_mat_isnan (A)) == NULLBMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) B);
  return 1;
}

/*
 * finite 
 */

int
int_mx_finite (Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  NspBMatrix *B;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((A = GetMat (stack, 1)) == NULLMAT)
    return RET_BUG;
  if ((B = nsp_mat_finite (A)) == NULLBMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) B);
  return 1;
}

/* FIXME */
extern function int_nsp_grand;
extern function int_nsp_rand_discrete_guide;
extern function int_nsp_rand_discrete_alias;
extern double nsp_dlamch (char *cmach);

/*
 * constants from lapack dlamch 
 * and include files 
 * number_properties 
 *   
 *        number_properties("eps")    -> machine epsilon dlamch('e')
 *        number_properties("radix")  -> base  dlamch('b')
 *        number_properties("digits") -> number of digits for the mantissa dlamch('n')
 *        number_properties("minexp") -> emin dlamch('m')
 *        number_properties("maxexp") -> emax dlamch('l')
 *        number_properties("huge")   -> max positive float dlamch('o')
 *        number_properties("tiny")   -> min positive normalised float dlamch('u')
 *        number_properties("denorm") -> (boolean) true if denormalised number are used
 *        number_properties("tiniest")-> min positive denormalised float 
 * FIXME : add integer constants 
 */

typedef enum { nump_eps,nump_huge,nump_tiny,nump_radix,nump_digits,nump_minexp,nump_maxexp,nump_denorm,nump_tiniest  } nump_id;
static char *numbers_props[]={ "eps","huge","tiny","radix","digits","minexp","maxexp","denorm","tiniest" , NULL };

int int_number_properties(Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  double tiniest,b;
  NspObject *Ob=NULLOBJ;
  CheckRhs(1,1);
  CheckLhs(1,1);  
  /* we accept abbrevs here */
  if ((rep= GetStringInArray(stack,1,numbers_props,0)) == -1) return RET_BUG; 
  switch ( rep ) 
    {
    case nump_eps: Ob=nsp_new_double_obj(nsp_dlamch("e")); break;
    case nump_huge: Ob=nsp_new_double_obj(nsp_dlamch("o")); break;
    case nump_tiny: Ob=nsp_new_double_obj(nsp_dlamch("u")); break;
    case nump_radix: Ob=nsp_new_double_obj(nsp_dlamch("b")); break;
    case nump_digits: Ob=nsp_new_double_obj(nsp_dlamch("n")); break;
    case nump_minexp: Ob=nsp_new_double_obj(nsp_dlamch("m")); break;
    case nump_maxexp: Ob=nsp_new_double_obj(nsp_dlamch("l")); break;
    case nump_denorm: Ob = nsp_create_boolean_object(NVOID,(nsp_dlamch("u") / nsp_dlamch("b") ) > 0.0E0 ? TRUE : FALSE);break;
    case nump_tiniest: 
      b = nsp_dlamch("b"); tiniest = nsp_dlamch("u"); 
      if ( tiniest/b != 0.0 ) 
	{
	  int i;
	  for ( i = 1; i <= ((int)nsp_dlamch("n")) -1 ; i++) 
	    tiniest = tiniest/b;
	}
      Ob=nsp_new_double_obj(tiniest); break;
    }
  if ( Ob == NULLOBJ) return RET_BUG;
  NthObj(1) = Ob;
  NthObj(1)->ret_pos  = 1;
  return 1;
}


/*
 * push the marix  elements on the stack 
 */

static int int_mx_to_seq (Stack stack, int rhs, int opt, int lhs)
{
  int i,j,count=0;
  NspMatrix *M;
  CheckRhs (1, 1);
  if ((M = GetMat(stack, 1)) == NULLMAT ) return RET_BUG;
  if (M->rc_type == 'r')
    for ( i=0 ; i < M->mn ; i++)
      {  
	NthObj(i+2)= nsp_create_object_from_double(NVOID,M->R[i]);
	NthObj(i+2)->ret_pos = i+1;
	if ( NthObj(i+2) == NULLOBJ ) { count= i; goto bug;}
      }
  else 
    for ( i=0 ; i < M->mn ; i++)
      {  
	NthObj(i+2)=nsp_create_object_from_complex(NVOID,&M->C[i]);
	NthObj(i+2)->ret_pos = i+1;
	if ( NthObj(i+2) == NULLOBJ ) { count= i; goto bug;}
      }
  return M->mn ;
  bug: 
  for ( j= 2 ; j <= i +1  ; j++) 
    {
      M = (NspMatrix *) NthObj(j);
      nsp_matrix_destroy(M);
    }
  return RET_BUG;
}


/* internal tests 
 * FIXME : to be removed 
 */

#include "../interp/Eval.h"

int int_harmloop1(Stack stack, int rhs, int opt, int lhs)
{
  int n,i;
  NspMatrix *M,*M1=NULLMAT,*Ob1;
  NspObject *Ob=NULLOBJ;
  CheckRhs(1,1);
  CheckLhs(1,1);  
  if (GetScalarInt (stack, 1, &n) == FAIL) return RET_BUG;
  Ob = nsp_new_double_obj(1.0); 
  if ((M = nsp_matrix_create_impl (2, 1, n)) == NULLMAT) return RET_BUG;
  for ( i = 1 ; i <= M->n ; i++)
    {
      int rep;
      if ((M1 = MatLoopCol ("@",M1, M, i, &rep)) == NULLMAT) return RET_BUG;
      Ob1 =(NspMatrix *) nsp_new_double_obj(1.0); 
      NthObj(2)= NSP_OBJECT(Ob1);
      NthObj(3)= NSP_OBJECT(M1);
      if (( rep = nsp_eval_func(NULL,"div",2,stack,stack.first+1,2,0,1)) <0 )
	  return RET_BUG;
      NthObj(3)=NthObj(2);
      NthObj(2)=Ob;
      if (( rep = nsp_eval_func(NULL,"plus",2,stack,stack.first+1,2,0,1)) <0 )
	  return RET_BUG;
    }
  NthObj(2)->ret_pos  = 1;
  return 1;
}

int int_harmloop2(Stack stack, int rhs, int opt, int lhs)
{
  int n,i;
  NspMatrix *M,*M1=NULLMAT,*Ob1;
  NspObject *Ob=NULLOBJ;
  CheckRhs(1,1);
  CheckLhs(1,1);  
  if (GetScalarInt (stack, 1, &n) == FAIL) return RET_BUG;
  Ob = nsp_new_double_obj(1.0); 
  if ((M = nsp_matrix_create_impl (2, 1, n)) == NULLMAT) return RET_BUG;
  for ( i = 1 ; i <= M->n ; i++)
    {
      int rep;
      if ((M1 = MatLoopCol (NVOID,M1, M, i, &rep)) == NULLMAT) return RET_BUG;
      Ob1 =(NspMatrix *) nsp_new_double_obj(1.0); 
      nsp_mat_div_scalar(Ob1,M1);
      nsp_mat_add((NspMatrix *) Ob,Ob1);
      nsp_matrix_destroy(Ob1);
    }
  NthObj(1) = Ob;
  NthObj(1)->ret_pos  = 1;
  return 1;
}

int int_harmloop3(Stack stack, int rhs, int opt, int lhs)
{
  int n,i;
  NspMatrix *M,*M1=NULLMAT,*Ob1;
  NspObject *Ob=NULLOBJ;
  CheckRhs(1,1);
  CheckLhs(1,1);  
  if (GetScalarInt (stack, 1, &n) == FAIL) return RET_BUG;
  Ob = nsp_new_double_obj(1.0); 
  Ob1 =(NspMatrix *) nsp_new_double_obj(1.0); 
  if ((M = nsp_matrix_create_impl (2, 1, n)) == NULLMAT) return RET_BUG;
  for ( i = 1 ; i <= M->n ; i++)
    {
      int rep;
      if ((M1 = MatLoopCol (NVOID,M1, M, i, &rep)) == NULLMAT) return RET_BUG;
      Ob1->R[0]=1.0; 
      nsp_mat_div_scalar(Ob1,M1);
      nsp_mat_add((NspMatrix *) Ob,Ob1);
    }
  nsp_matrix_destroy(Ob1);
  NthObj(1) = Ob;
  NthObj(1)->ret_pos  = 1;
  return 1;
}

int int_harmloop4(Stack stack, int rhs, int opt, int lhs)
{
  double s=1;
  int n,i;
  NspMatrix *M;
  NspObject *Ob=NULLOBJ;
  CheckRhs(1,1);
  CheckLhs(1,1);  
  if (GetScalarInt (stack, 1, &n) == FAIL) return RET_BUG;
  if ((M = nsp_matrix_create_impl (2, 1, n)) == NULLMAT) return RET_BUG;
  for ( i = 1 ; i <= M->n ; i++)
    {
      s = s + 1/M->R[i];
    }
  Ob = nsp_new_double_obj(s); 
  nsp_matrix_destroy(M);
  NthObj(1) = Ob;
  NthObj(1)->ret_pos  = 1;
  return 1;
}

int int_alignement(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M, *A;
  CheckRhs(1,1);
  CheckLhs(1,1);  
  unsigned int a, p; 
  if ( (M = GetMat(stack, 1)) == NULLMAT ) return RET_BUG;
  if ( (A = nsp_matrix_create(NVOID, 'r', 1, 1)) == NULLMAT ) return RET_BUG;

  if ( M->rc_type == 'r' )
    p = (unsigned int) M->R;
  else
    p = (unsigned int) M->C;

  a = p & 0xF;
  A->R[0] = (double) a;
  MoveObj (stack, 1, (NspObject *) A);
  return 1;
}


/*
 * The Interface for basic matrices operation 
 */

static OpTab Matrix_func[] = {
  {"alignement_m",int_alignement},
  {"harmloop1_i",int_harmloop1},
  {"harmloop2_i",int_harmloop2},
  {"harmloop3_i",int_harmloop3},
  {"harmloop4_i",int_harmloop4},
  {"impl", int_mximpl},
  /*   {"impl_m", int_mximpl}, */
  {"impl_m_m", int_mximpl},
  {"addcols_m_m", int_mxaddcols},
  {"addrows_m_m", int_mxaddrows},
  {"clean", int_mxclean},
  {"complexify_m", int_mxcomplexify},
  {"concatd_m_m", int_matint_concatd},
  {"concatr_m_m", int_matint_concatr},
  {"concatr_b_m", int_mxconcatr_mb},
  {"concatr_m_b", int_mxconcatr_mb}, 
  {"mat_create_m", int_mxcreate},
  {"dadd_m_m", int_mxdadd},
  {"concatdiag_m_m",int_matint_concat_diag}, /* int_mxconcatdiag}, */
  {"diag_m", int_mxdiag},
  {"diag_m_m", int_mxdiag},
  {"diagcre_m", int_mxdiagcre},
  {"diagcre_m_m", int_mxdiagcre},
  {"diage_m", int_mxdiage},
  {"diage_m_m", int_mxdiage},
  {"diagset_m", int_mxdiagset},
  {"diff_m", int_mxdiff},
  {"eq_m_m", int_mxeq},
  {"extract_m", int_matint_extract}, 
  {"extractelts_m", int_matint_extractelts}, 
  {"extractcols_m", int_matint_extractcols}, 
  {"extractrows_m", int_matint_extractrows}, 
  {"resize2vect_m", int_matint_resize2vect},
  {"setrowscols_m", int_matint_setrowscols},
  {"deleteelts_m", int_matint_deleteelts},
  {"deleterows_m", int_matint_deleterows},
  {"deletecols_m", int_matint_deletecols},
  {"tozero_m", int_matint_tozero},
  {"repmat_m_m", int_matint_repmat},
  {"eye_m_m", int_mxeye},
  {"ones_m_m", int_mxones},
  {"zeros_m_m", int_mxzeros},
  {"feq_m_m", int_mxfeq},
  {"fge_m_m", int_mxfge},
  {"fgt_m_m", int_mxfgt},
  {"fle_m_m", int_mxfle},
  {"flt_m_m", int_mxflt},
  {"fne_m_m", int_mxfne},
  {"ge_m_m", int_mxge},
  {"gt_m_m", int_mxgt},
  {"imag_m", int_mximagpart},
  {"dstd_m_m", int_mxkron},	/* operator:  .*. */
  {"latexmat_m", int_mx2latexmat},
  {"latextab_m", int_mx2latextab},
  {"le_m_m", int_mxle},
  {"lt_m_m", int_mxlt},
  {"max_m", int_mxmaxi},
  {"max_m_m", int_mxmaxi},
  {"max", int_mxmaxi},
  {"min_m", int_mxmini},
  {"min", int_mxmini},
  {"minmax_m", int_mxminmax},  
  {"minmax", int_mxminmax},  
  {"sum_m_s", int_mxsum},
  {"sum_m", int_mxsum},
  {"cumsum_m_s", int_mxcusum},
  {"cumsum_m", int_mxcusum},
  {"prod_m_s", int_mxprod},
  {"prod_m", int_mxprod},
  {"cumprod_m_s", int_mxcuprod},
  {"cumprod_m", int_mxcuprod},
  {"ne_m_m", int_mxneq},
  {"rand", int_mxrand},
  {"grand", int_nsp_grand},	/* grand XXXX */
  /* {"rand_discrete_guide", int_nsp_rand_discrete_guide},*/	/*  XXXX */
  /* {"rand_discrete_alias", int_nsp_rand_discrete_alias},*//*  XXXX */
  {"real_m", int_mxrealpart},
  {"redim_m", int_matint_redim},
  {"resize_m_m", int_mxresize},
  {"seti_m_m", int_mxseti},
  {"setr_m_m", int_mxsetr},
  {"sort_m", int_matrix_sort},
  {"gsort_m", int_matrix_sort},
  {"new_sort", int_matrix_sort },
  {"tril", int_mxtril},
  {"triu", int_mxtriu},
  {"testm", int_mxtestmatrix},
  {"testmatrix", int_mxtestmatrix},
  {"matrix_m", int_matint_redim},
  {"quote_m", int_mxquote},
  {"dprim_m", int_mxdquote},
  {"abs_m", int_mxabs},
  {"erf_m", int_mxerf},
  {"erfc_m", int_mxerfc},
  {"gamma_m",int_mxgamma},
  {"gammaln_m",int_mxgammaln},
  {"arg_m", int_mxarg},
  {"cos_m", int_mxcos},
  {"cosh_m", int_mxcosh},
  {"exp_m", int_mxexpel},
  {"log_m", int_mxlogel},
  {"sin_m", int_mxsin},
  {"sinh_m", int_mxsinh},
  {"sqrt_m", int_mxsqrtel},
  {"acos_m", int_mxacos},
  {"acosh_m", int_mxacosh},
  {"asin_m", int_mxasin},
  {"asinh_m", int_mxasinh},
  {"atan_m", int_mxatan},
  {"atan_m_m", int_mxatan2},
  {"atanh_m", int_mxatanh},
  {"ceil_m", int_mxceil},
  {"modulo_m_m", int_mxmodulo},
  {"mod_m_m", int_mxmod},
  {"idiv_m_m", int_mxidiv},
  {"bdiv_m_m", int_mxbdiv},
  {"int_m", int_mxint},
  {"floor_m", int_mxfloor},
  {"round_m", int_mxround},
  {"sign_m", int_mxsign},
  {"tan_m", int_mxtan},
  {"tanh_m", int_mxtanh},
  {"polar", int_mxpolar},
  {"iand", int_mxiand},
  {"ior", int_mxior},
  {"ishift", int_mxishift},
  {"conj_m", int_mxconj},
  {"hat_m_m", int_mxpow},
  {"dh_m_m", int_mxpowel},
  {"dsl_m_m", int_mxdivel},
  {"dbs_m_m", int_mxbackdivel},
  {"dst_m_m", int_mxmultel},
  {"plus_m_m", int_mxdadd},
  {"plussafe_m_m", int_mxdadd1}, /* XXX experimental */
  {"minus_m_m", int_mxdsub},
  {"minus_m", int_mxminus},
  {"mult_m_m", int_mxmult},
  {"div_m_m", int_mxdiv},
  {"find_m", int_mxfind},
  {"isinf", int_mx_isinf},
  {"isnan", int_mx_isnan},
  {"finite", int_mx_finite},
  {"linspace", int_mxlinspace},
  {"logspace", int_mxlogspace},
  {"number_properties",int_number_properties},
  {"object2seq_m",int_mx_to_seq}, /* A{...} on rhs  */
  {(char *) 0, NULL}
};

int
Matrix_Interf (int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Matrix_func[i].fonc)) (stack, rhs, opt, lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) **/

void
Matrix_Interf_Info (int i, char **fname, function (**f))
{
  *fname = Matrix_func[i].name;
  *f = Matrix_func[i].fonc;
}

