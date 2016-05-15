/* Nsp
 * Copyright (C) 2016-2016 Jean-Philippe Chancelier Enpc/Cermics
 *
 * Some routines at the end are copied from SPQR/MATLAB Module.
 * They are copyrighted by Timothy A. Davis Version 1.2.  Copyright (C) 2005-2006,
 * under the GPL.
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
 *
 * Interface with the spqr library using a NspSpqr Object.
 * We use ZOMPLEX for sparse Matrice since passing sparse matrices
 * from Nsp to spqr uses Matlab sparse triplet (Sptriplet.c)
 * which are ZOMPLEX.
 * For full matrices COMPLEX is preferred since it is the default
 * in Nsp.
 *
 *
 */

#include <nsp/machine.h>

#ifdef WITH_SPQR

#include <cholmod.h>
#define  Spqr_Private
#include <nsp/object.h>
#include <nsp/cholmod.h>
#include <nsp/spqr.h>
#include <nsp/type.h>
#include <nsp/file.h>
#include <nsp/hobj.h>
#include <nsp/matrix.h>
#include <nsp/smatrix.h>
#include <nsp/bmatrix.h>
#include <nsp/spcolmatrix.h>
#include <nsp/sprowmatrix.h>
#include "nsp/interf.h"

#ifndef SPUMONI
#define SPUMONI 0
#endif

/*
 * NspSpqr inherits from NspObject
 */

int nsp_type_spqr_id=0;
NspTypeSpqr *nsp_type_spqr=NULL;

/*
 * Type object for Spqr
 * all the instance of NspTypeSpqr share the same id.
 * nsp_type_spqr: is an instance of NspTypeSpqr
 *    used for objects of NspSpqr type (i.e built with new_spqr)
 * other instances are used for derived classes
 */
NspTypeSpqr *new_type_spqr(type_mode mode)
{
  NspTypeSpqr *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_spqr != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_spqr;
    }
  if ((type =  malloc(sizeof(NspTypeSpqr))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = spqr_attrs ;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = spqr_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_spqr;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for spqr */

  top->pr = (print_func *) nsp_spqr_print;
  top->dealloc = (dealloc_func *) nsp_spqr_destroy;
  top->copy  =  (copy_func *) nsp_spqr_copy;
  top->size  = (size_func *) nsp_spqr_size;
  top->s_type =  (s_type_func *) nsp_spqr_type_as_string;
  top->sh_type = (sh_type_func *) nsp_spqr_type_short_string;
  top->info = (info_func *) nsp_spqr_info ;
  /* top->is_true = (is_true_func  *) nsp_spqr_is_true; */
  /* top->loop =(loop_func *) nsp_spqr_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_spqr_object;
  top->eq  = (eq_func *) nsp_spqr_eq;
  top->neq  = (eq_func *) nsp_spqr_neq;
  top->save  = (save_func *) nsp_spqr_xdr_save;
  top->load  = (load_func *) nsp_spqr_xdr_load;
  top->create = (create_func*) int_spqr_create;

  /* specific methods for spqr */

  type->init = (init_func *) init_spqr;

/*
 * Spqr interfaces can be added here
 * type->interface = (NspTypeBase *) new_type_b();
 * type->interface->interface = (NspTypeBase *) new_type_C()
 * ....
 */
  if ( nsp_type_spqr_id == 0 )
    {
      /*
       * the first time we get here we initialize the type id and
       * an instance of NspTypeSpqr called nsp_type_spqr
       */
      type->id =  nsp_type_spqr_id = nsp_new_type_id();
      nsp_type_spqr = type;
      if ( nsp_register_type(nsp_type_spqr) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_spqr(mode);
    }
  else
    {
       type->id = nsp_type_spqr_id;
       return type;
    }
}

/*
 * initialize Spqr instances
 * locally and by calling initializer on parent class
 */

static int init_spqr(NspSpqr *o,NspTypeSpqr *type)
{
  /* jump the first surtype */
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type;
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Spqr
 */

NspSpqr *new_spqr()
{
  NspSpqr *loc;
  /* type must exists */
  nsp_type_spqr = new_type_spqr(T_BASE);
  if ( (loc = malloc(sizeof(NspSpqr)))== NULLSPQR) return loc;
  /* initialize object */
  if ( init_spqr(loc,nsp_type_spqr) == FAIL) return NULLSPQR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Spqr
 *-----------------------------------------------*/
/*
 * size
 */

static int nsp_spqr_size(NspSpqr *Mat, int flag)
{
  return 0;
}

/*
 * type as string
 */

static char spqr_type_name[]="Spqr";
static char spqr_short_type_name[]="spqr";

static char *nsp_spqr_type_as_string(void)
{
  return(spqr_type_name);
}

static char *nsp_spqr_type_short_string(NspObject *v)
{
  return(spqr_short_type_name);
}

/*
 * A == B
 */

static int nsp_spqr_eq(NspSpqr *A, NspObject *B)
{
  NspSpqr *loc = (NspSpqr *) B;
  if ( check_cast(B,nsp_type_spqr_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  return FALSE;
}

/*
 * A != B
 */

static int nsp_spqr_neq(NspSpqr *A, NspObject *B)
{
  return ( nsp_spqr_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save
 */

static int nsp_spqr_xdr_save(XDR *xdrs, NspSpqr *M)
{
#if 1
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_spqr)) == FAIL) return FAIL;
#else
  if (nsp_xdr_save_i(xdrs, M->type->id) == FAIL)    return FAIL;
#endif
  Sciprintf("Warning: cannot save Spqr objects (cowardly not saving this object)\n");
  return OK;
}

/*
 * load
 */

static NspSpqr  *nsp_spqr_xdr_load(XDR *xdrs)
{
  NspSpqr *M = NULL;
  /* should never get there since spqr object are not saved */
  return M;
}

/*
 * delete
 */

void nsp_spqr_destroy(NspSpqr *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
     if ( H->obj->QR != NULL)
       {
	 SuiteSparseQR_C_free(&(H->obj->QR), &(H->obj->Common));
       }
     cholmod_l_free_sparse (&(H->obj->A), &(H->obj->Common)) ;
     cholmod_finish (&(H->obj->Common)) ;
     cholmod_print_common (" ",&(H->obj->Common)) ;
     FREE(H->obj);
   }
  FREE(H);
}

/*
 * info
 */

int nsp_spqr_info(NspSpqr *M, int indent,const char *name, int rec_level)
{
  int i;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if ( M == NULLSPQR || M->obj == NULL )
    {
      Sciprintf("Null Pointer Spqr \n");
      return TRUE;
    }
  Sciprintf("%s\t= [...]\t%s %c (%dx%d)\n",pname,nsp_spqr_type_short_string(NSP_OBJECT(M)),
	    (M->obj->A->xtype ==  CHOLMOD_ZOMPLEX) ? 'c' : 'r',
	    M->obj->A->nrow,M->obj->A->ncol);
  return TRUE;
}

/*
 * print
 */

int nsp_spqr_print(NspSpqr *Mat, int indent,char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  if (user_pref.pr_as_read_syntax)
    {
      Sciprintf("// Cannot print an spqr object using as_read=%%t option \n");
    }
  else
    {
      nsp_spqr_info(Mat,indent,pname,rec_level);
    }
  return TRUE;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces
 * for Spqr objects
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspSpqr   *nsp_spqr_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_spqr_id) == TRUE ) return ((NspSpqr *) O);
  else
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_spqr));
  return NULL;
}

int IsSpqrObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_spqr_id);
}

int IsSpqr(NspObject *O)
{
  return nsp_object_type(O,nsp_type_spqr_id);
}

NspSpqr  *GetSpqrCopy(Stack stack, int i)
{
  if (  GetSpqr(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspSpqr  *GetSpqr(Stack stack, int i)
{
  NspSpqr *M;
  if (( M = nsp_spqr_object(NthObj(i))) == NULLSPQR)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor
 * if type is non NULL it is a subtype which can be used to
 * create a NspClassB instance
 *-----------------------------------------------------*/

static NspSpqr *spqr_create_void(char *name,NspTypeBase *type)
{
 NspSpqr *H  = (type == NULL) ? new_spqr() : type->new();
 if ( H ==  NULLSPQR)
  {
   Sciprintf("No more memory\n");
   return NULLSPQR;
  }
  /* shared by all objects */
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULL)
   return NULLSPQR;
 NSP_OBJECT(H)->ret_pos = -1 ;
 H->obj = NULL;
 return H;
}

NspSpqr *spqr_create(char *name,NspTypeBase *type)
{
 NspSpqr *H  = spqr_create_void(name,type);
 if ( H ==  NULLSPQR) return NULLSPQR;
 if ((H->obj = calloc(1,sizeof(nsp_spqr))) == NULL) return NULL;
  H->obj->ref_count=1;
 return H;
}

/*
 * copy for gobject derived class
 */

NspSpqr *nsp_spqr_copy(NspSpqr *self)
{
  NspSpqr *H  =spqr_create_void(NVOID,(NspTypeBase *) nsp_type_spqr);
  if ( H ==  NULLSPQR) return NULLSPQR;
  H->obj = self->obj;
  self->obj->ref_count++;
 return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Spqr
 * i.e functions at Nsp level
 *-------------------------------------------------------------------*/

static int nsp_spqr_check_ordering(Stack stack, const char *ordering_s, int *ordering)
{
  if ( ordering_s != NULL)
    {
      const char *types[]={"fixed","natural","colamd","given","best","amd","metis","default",NULL};
      int modes[]={SPQR_ORDERING_FIXED, SPQR_ORDERING_NATURAL, SPQR_ORDERING_COLAMD,
		   SPQR_ORDERING_GIVEN, SPQR_ORDERING_CHOLMOD, SPQR_ORDERING_AMD,
		   SPQR_ORDERING_METIS, SPQR_ORDERING_DEFAULT};
      int rep = is_string_in_array(ordering_s, types,1);
      if ( rep < 0 ) 
	{
	  string_not_in_array(stack,ordering_s,types,"optional argument");
	  return FAIL;
	}
      *ordering = modes[rep];
    }
  return OK;
}

/* up and lo are ignored in qr factorization XXXX */

static int nsp_spqr_check_type(Stack stack, const char *sstype, int *stype, int *transpose, const NspObject *Obj)
{
  if ( sstype != NULL)
    {
      const char *types[]={ "row", "col", "sym", "lo", "up",  NULL };
      if ( !IsSpColMat(Obj))
	{
	  Scierror("Error: optional argument type= cannot be used when matrix is not sparse\n");
	  stype = 0;
	}
      int rep = is_string_in_array(sstype, types,1);
      if ( rep < 0 )
	{
	  string_not_in_array(stack,sstype,types,"optional argument");
	  return FAIL;
	}
      switch ( rep )
	{
	case 0:/* use all of A */ *stype = 0; break;
	case 1:/* use all of A */ *stype = 0; *transpose = TRUE; break;
	case 2:/* use only upper part of the matrix (stype=1) but verify symmetry */ 
	  *stype=1;
	  if ( IsSpColMat(Obj) && ( ! nsp_spcolmatrix_is_symmetric((NspSpColMatrix *) Obj) ))
	    {
	      Scierror("Error: matrix is not symmetric\n");
	      return FAIL;
	    }
	  break;
	case 3: /* use lower part but matrix must be square XXXX*/ *stype = -1; break;
	case 4: /* use upper part but matrix must be square XXXX */  *stype = 1; break;
	}
    }
  return OK;
}

int int_spqr_create(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;
  double dummy = 0;
  double tol = SPQR_DEFAULT_TOL;
  char *sstype=NULL;
  int stype=0;	
  int transpose= FALSE;
  int ordering= SPQR_ORDERING_DEFAULT;
  char *ordering_s = NULL;
  NspSpqr *H=NULL;
  nsp_option opts[] ={{"type",string,NULLOBJ,-1},
		      {"ordering",string,NULLOBJ,-1},
		      {"tol",s_double,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  /* Get a sparse matrix */
  CheckStdRhs(1,1);
  CheckLhs(0,2);
  /* now we can store the Numeric part */
  /* want to be sure that type spqr is initialized */
  nsp_type_spqr = new_type_spqr(T_BASE);
  if(( H = spqr_create(NVOID,(NspTypeBase *) nsp_type_spqr)) == NULLSPQR)
    goto err;

  if ((Obj = nsp_get_object(stack,1)) == NULL)   return RET_BUG;

  /* optional arguments */
  if ( get_optional_args(stack,rhs,opt,opts,&sstype,&ordering_s,&tol) == FAIL)
    goto err;

  /* checks the optional type argument */
  if ( nsp_spqr_check_ordering(stack,ordering_s, &ordering) == FAIL)
    goto err;

  if ( nsp_spqr_check_type(stack,sstype, &stype, &transpose, Obj) == FAIL)
    goto err;
    
  cholmod_l_start (&(H->obj->Common)) ;
  nsp_sputil_config (SPUMONI,&(H->obj->Common),FALSE) ;
  
  H->obj->A = cholmod_sparse_from_object(Obj, &dummy,stype, transpose,TRUE, TRUE, &(H->obj->Common));
  H->obj->ordering = ordering;
  H->obj->tol = tol;
  if (H->obj->A  == NULL) return RET_BUG;

  /* QR will be build latter if needed */
  H->obj->QR = NULL;
  
  /* we return H */
  MoveObj(stack,1,NSP_OBJECT(H));
  return Max(lhs,1);
 err:
  if ( H != NULL) nsp_spqr_destroy(H);
  return RET_BUG;
}


/* standard qr interface for sparses matrices (for compatibility) 
 */

int nsp_spqr_get_qr(NspSpqr *self,cholmod_sparse *Q,cholmod_sparse *R, long *E, long rk,
		    NspSpColMatrix **Qs, NspSpColMatrix **Rs, NspMatrix **Es, NspMatrix **Rk, int nargs)
{
  int nc = self->obj->A->ncol;
  *Qs = NULL; *Rs = NULL;*Es = NULL; *Rk=NULL;

  if ((*Qs = nsp_cholmod_to_spcol_sparse(&Q, &(self->obj->Common))) == NULL)
    goto err;
  if ( nargs >= 2 )
    {
      if ((*Rs = nsp_cholmod_to_spcol_sparse(&R, &(self->obj->Common))) == NULL)
	goto err;
    }
  if ( nargs >= 3)
    {
      int i ;
      if (( *Es= nsp_matrix_create(NVOID,'r',1,nc)) == NULLMAT ) goto err;
      /* size n column permutation, NULL if identity */
      if ( E == NULL)
	{
	  for (i = 0 ; i < nc ; i++) (*Es)->R[i]= (i+1);
	}
      else
	{
	  for (i = 0 ; i < nc ; i++) (*Es)->R[i]= (E[i]+1);
	  /* need to clean E */
	  cholmod_l_free (nc, sizeof (long), E, &(self->obj->Common)) ;
	}
    }
  if ( nargs >=4 )
    {
      if (( *Rk= nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT ) goto err;
      (*Rk)->R[0]=rk;
    }
  return OK;
 err:
  if ( *Qs != NULL) nsp_spcolmatrix_destroy(*Qs);
  if ( *Rs != NULL) nsp_spcolmatrix_destroy(*Rs);
  if ( *Es != NULL) nsp_matrix_destroy(*Es);
  if ( *Rk != NULL) nsp_matrix_destroy(*Rk);
  return FAIL;
}

int int_spqr_qr(Stack stack, int rhs, int opt, int lhs)
{
  cholmod_sparse *Q=NULL;   // m-by-e sparse matrix
  cholmod_sparse *R=NULL;     // e-by-n sparse matrix
  long *E=NULL;               // size n column permutation, NULL if identity
  long econ=-1;
  long rk;
  NspSpColMatrix *Qs = NULL,*Rs = NULL;
  NspMatrix *Es = NULL,*Rk = NULL;
  NspObject *Obj;
  double dummy = 0;
  double tol = SPQR_DEFAULT_TOL;
  char *sstype=NULL;
  int stype=0;	
  int transpose= FALSE;
  int ordering= SPQR_ORDERING_DEFAULT;
  char *ordering_s = NULL;
  char *mode_s = NULL;
  NspSpqr *H=NULL;
  nsp_option opts[] ={{"type",string,NULLOBJ,-1},
		      {"ordering",string,NULLOBJ,-1},
		      {"tol",s_double,NULLOBJ,-1},
		      {"mode",string,NULLOBJ,-1}, /* eco ? */
		      { NULL,t_end,NULLOBJ,-1}};
  /* Get a sparse matrix */
  CheckStdRhs(1,1);
  CheckLhs(0,4);
  /* now we can store the Numeric part */
  /* want to be sure that type spqr is initialized */
  nsp_type_spqr = new_type_spqr(T_BASE);
  if(( H = spqr_create(NVOID,(NspTypeBase *) nsp_type_spqr)) == NULLSPQR)
    goto err;

  if ((Obj = nsp_get_object(stack,1)) == NULL)   return RET_BUG;

  /* optional arguments */
  if ( get_optional_args(stack,rhs,opt,opts,&sstype,&ordering_s,&tol,&mode_s) == FAIL)
    goto err;

  /* checks the optional type argument */
  if ( nsp_spqr_check_ordering(stack,ordering_s, &ordering) == FAIL)
    goto err;

  if ( nsp_spqr_check_type(stack,sstype, &stype, &transpose, Obj) == FAIL)
    goto err;

  if ( lhs <= 2 ) ordering = SPQR_ORDERING_NATURAL; /* no ordering for [Q,R]=qr(A) */
    
  cholmod_l_start (&(H->obj->Common)) ;
  nsp_sputil_config (SPUMONI,&(H->obj->Common),FALSE) ;
  
  H->obj->A = cholmod_sparse_from_object(Obj, &dummy,stype, transpose,TRUE, TRUE, &(H->obj->Common));
  H->obj->ordering = ordering;
  H->obj->tol = tol;
  if (H->obj->A  == NULL) return RET_BUG;

  /* QR will be build latter if needed */
  H->obj->QR = NULL;
  if ( H->obj->QR  == NULL )
    {
      H->obj->QR = SuiteSparseQR_C_factorize( H->obj->ordering, H->obj->tol ,H->obj->A,&H->obj->Common);
      if ( H->obj->QR == NULL ) 
	{
	  Scierror("Error: QR factorization fails\n");
	  goto err;
	}
    }
  if ( mode_s != NULL && strcmp(mode_s,"e") == 0)
    econ = Min(H->obj->A->nrow,H->obj->A->ncol);
  else
    econ = H->obj->A->nrow;
      
  rk=SuiteSparseQR_C_QR(H->obj->ordering,tol,econ,H->obj->A,&Q,&R,&E,&(H->obj->Common));
  if ( rk == -1 )
    {
      goto err;
    }

  if ( nsp_spqr_get_qr(H, Q,R,E,rk, &Qs, &Rs, &Es, &Rk, lhs) == FAIL)
    {
      Scierror("Eror: failed to get QR factorization\n");
      goto err;
    }
  MoveObj(stack,1,NSP_OBJECT(Qs));
  if ( lhs >= 2 ) MoveObj(stack,2,NSP_OBJECT(Rs));
  if ( lhs >= 3 ) MoveObj(stack,3,NSP_OBJECT(Es));
  if ( lhs >= 4 ) MoveObj(stack,4,NSP_OBJECT(Rk));
  nsp_spqr_destroy(H);
  return Max(lhs,1);
 err:
  if ( H != NULL) nsp_spqr_destroy(H);
  return RET_BUG;
}

static int int_spqr_meth_isreal(NspSpqr *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;
  int ret;
  CheckRhs(0,0);
  CheckLhs(1,1);
  if ( self->obj == NULL || self->obj->A  == NULL)
    {
      Scierror("Error: cholmod object is not properly built\n");
      return RET_BUG;
    }
  ret =  (self->obj->A->xtype ==  CHOLMOD_ZOMPLEX) ?  FALSE :  TRUE;
  if ((Obj = nsp_new_boolean_obj(ret))==NULLOBJ) return RET_BUG;
  MoveObj(stack,1,Obj);
  return 1;
}


/* Solve an upper or lower triangular system using R from the QR factorization */
  
static int nsp_spqr_rsolve_mode( Stack stack,char *mode, int *imode)
{
  const char *types[]={ "R\\B", "E*(R\\B)",  "R'\\B", "R'\\(E'*B)" , NULL};
  int modes[]={SPQR_RX_EQUALS_B,SPQR_RETX_EQUALS_B,SPQR_RTX_EQUALS_B,SPQR_RTX_EQUALS_ETB};
  int rep = is_string_in_array(mode, types,1);
  if ( rep < 0 )
    {
      string_not_in_array(stack,mode,types,"optional argument");
      return FAIL;
    }
  *imode = modes[rep];
  return OK ;
}

static int int_spqr_meth_rsolve(NspSpqr *self, Stack stack, int rhs, int opt, int lhs)
{
  char *mode = NULL;
  double dummy = 0;
  cholmod_dense Bmatrix, *X ;
  nsp_option opts[] ={{"mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  int imode = 1;
  CheckStdRhs(1,1);
  CheckLhs(1,1);
  
  if ( self->obj == NULL || self->obj->A  == NULL ) 
    {
      Scierror("Error: cholmod object is not properly built\n");
      return RET_BUG;
    }

  if ( self->obj->QR  == NULL )
    {
      self->obj->QR = SuiteSparseQR_C_factorize( self->obj->ordering, self->obj->tol ,self->obj->A,&self->obj->Common);
      if ( self->obj->QR == NULL ) 
	{
	  Scierror("Error: QR factorization fails\n");
	  return RET_BUG;
	}
    }
  
  if ( get_optional_args(stack,rhs,opt,opts,&mode) == FAIL)
    return RET_BUG;
  /* checks the optional type argument */
  if ( mode != NULL)
    {
      if ( nsp_spqr_rsolve_mode(stack,mode,&imode) == FAIL) return RET_BUG;
    }

  if (IsMatObj(stack,1))
    {
      NspMatrix *B,*Res;
      if ( (B = GetMat(stack, 1)) == NULLMAT ) return RET_BUG;
      if ( imode == 0 || imode == 1)
	if ( B->m != self->obj->A->nrow )
	  {
	    Scierror("Error: argument should have %d rows\n",self->obj->A->nrow);
	    return RET_BUG;
	  }
      if ( imode == 2 || imode == 3)
	if ( B->m != self->obj->A->ncol )
	  {
	    Scierror("Error: argument should have %d rows\n",self->obj->A->ncol);
	    return RET_BUG;
	  }
      nsp_matrix_to_cholmod_dense(B,&Bmatrix,&dummy) ;
      X = SuiteSparseQR_C_solve(imode, self->obj->QR, &Bmatrix, &(self->obj->Common)) ;
      if ( X == NULL )
	{
	  Scierror("Error: failed to solve linear system\n");
	  return RET_BUG;
	}
      Res = nsp_cholmod_dense_to_matrix (&X, &(self->obj->Common)) ;
      if ( Res == NULL) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(Res));
      return 1;
    }
  return 0;
}


/* Applies Q in Householder form (as stored in the QR factorization object) */
//  method SPQR_QTX (0): Y = Q'*X
//  method SPQR_QX  (1): Y = Q*X
//  method SPQR_XQT (2): Y = X*Q'
//  method SPQR_XQ  (3): Y = X*Q

// returns Y of size m-by-n, or NULL on failure

static int nsp_spqr_qmult_mode( Stack stack,char *mode, int *imode)
{
  const char *types[]={ "Q'*X", "Q*X",  "X*Q'", "X*Q" , NULL};
  int modes[]={SPQR_QTX,SPQR_QX,SPQR_XQT,SPQR_XQ};
  int rep = is_string_in_array(mode, types,1);
  if ( rep < 0 )
    {
      string_not_in_array(stack,mode,types,"optional argument");
      return FAIL;
    }
  *imode = modes[rep];
  return OK ;
}

static int int_spqr_meth_qmult(NspSpqr *self, Stack stack, int rhs, int opt, int lhs)
{
  char *mode = NULL;
  double dummy = 0;
  cholmod_dense Bmatrix, *X ;
  nsp_option opts[] ={{"mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  int imode = 1;
  CheckStdRhs(1,1);
  CheckLhs(1,1);
  
  if ( self->obj == NULL || self->obj->A  == NULL )
    {
      Scierror("Error: cholmod object is not properly built\n");
      return RET_BUG;
    }

  if ( self->obj->QR  == NULL )
    {
      self->obj->QR = SuiteSparseQR_C_factorize( self->obj->ordering, self->obj->tol ,self->obj->A,&self->obj->Common);
      if ( self->obj->QR == NULL ) 
	{
	  Scierror("Error: QR factorization fails\n");
	  return RET_BUG;
	}
    }

  if ( get_optional_args(stack,rhs,opt,opts,&mode) == FAIL)
    return RET_BUG;
  /* checks the optional type argument */
  if ( mode != NULL)
    {
      if ( nsp_spqr_qmult_mode(stack,mode,&imode) == FAIL) return RET_BUG;
    }

  if (IsMatObj(stack,1))
    {
      NspMatrix *B,*Res;
      if ( (B = GetMat(stack, 1)) == NULLMAT ) return RET_BUG;
      if ( B->m != self->obj->A->nrow )
	{
	  Scierror("Error: argument should have %d rows\n",self->obj->A->nrow);
	  return RET_BUG;
	}
      nsp_matrix_to_cholmod_dense(B,&Bmatrix,&dummy) ;

      X = SuiteSparseQR_C_qmult(imode, self->obj->QR, &Bmatrix, &(self->obj->Common));
      Res = nsp_cholmod_dense_to_matrix (&X, &(self->obj->Common)) ;
      if ( Res == NULL) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(Res));
      return 1;
    }
  return 0;
}


static int int_spqr_meth_get_qr(NspSpqr *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Es,*Rk;
  double tol;
  long rk;
  long econ;  
  cholmod_sparse *Q=NULL;   // m-by-e sparse matrix
  cholmod_sparse *R=NULL;     // e-by-n sparse matrix
  long *E=NULL;               // size n column permutation, NULL if identity
  NspSpColMatrix *Qs,*Rs;

  CheckStdRhs(0,0);
  CheckLhs(1,4);
  
  if ( self->obj == NULL || self->obj->A  == NULL ) 
    {
      Scierror("Error: cholmod object is not properly built\n");
      return RET_BUG;
    }
  econ = self->obj->A->nrow;
  tol =  self->obj->tol;
  
  if ( self->obj->QR  == NULL )
    {
      self->obj->QR = SuiteSparseQR_C_factorize( self->obj->ordering, self->obj->tol ,self->obj->A,&self->obj->Common);
      if ( self->obj->QR == NULL ) 
	{
	  Scierror("Error: QR factorization fails\n");
	  return RET_BUG;
	}
    }
  
  rk=SuiteSparseQR_C_QR(self->obj->ordering,tol,econ,self->obj->A,&Q,&R,&E,&(self->obj->Common));
  if ( rk == -1 )
    {
      return RET_BUG;
    }

  if ( nsp_spqr_get_qr(self,Q,R,E,rk, &Qs, &Rs, &Es, &Rk, lhs) == FAIL)
    {
      Scierror("Eror: failed to get QR factorization\n");
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(Qs));
  if ( lhs >= 2 ) MoveObj(stack,2,NSP_OBJECT(Rs));
  if ( lhs >= 3 ) MoveObj(stack,3,NSP_OBJECT(Es));
  if ( lhs >= 4 ) MoveObj(stack,4,NSP_OBJECT(Rk));
  return Max(lhs,1);
}

static int int_spqr_meth_backslash(NspSpqr *self,Stack stack, int rhs, int opt, int lhs)
{
  double dummy = 0;

  CheckStdRhs(1,1);
  CheckLhs(1,1);
  
  if ( self->obj == NULL || self->obj->A  == NULL ) 
    {
      Scierror("Error: spqr object is not properly built\n");
      return RET_BUG;
    }
  
  if (IsMatObj(stack,1))
    {
      cholmod_dense *X ;
      cholmod_dense Bmatrix;
      NspMatrix *A,*Res;
      if ( (A = GetMat(stack, 1)) == NULLMAT ) return RET_BUG;
      if ( A->m != self->obj->A->nrow )
	{
	  Scierror("Error: argument should have %d rows\n",self->obj->A->nrow);
	  return RET_BUG;
	}
      if ( ! ( ( A->rc_type == 'c' && self->obj->A->xtype == CHOLMOD_COMPLEX)
	       || ( A->rc_type == 'r' && self->obj->A->xtype == CHOLMOD_REAL)))
	{
	  Scierror("Error: argument should be %s \n",
		   (self->obj->A->xtype = CHOLMOD_COMPLEX) ? "complex": "real");
	  return RET_BUG;
 	}

      nsp_matrix_to_cholmod_dense(A,&Bmatrix,&dummy) ;
      X = SuiteSparseQR_C_backslash(self->obj->ordering,self->obj->tol,
				    self->obj->A,&Bmatrix,&(self->obj->Common));
      Res = nsp_cholmod_dense_to_matrix (&X, &(self->obj->Common)) ;
      if ( Res == NULL) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(Res));
      return 1;
    }
  else if (IsSpColMatObj(stack,1))
    {
      cholmod_sparse *Xs ;
      cholmod_sparse *Bspmatrix;
      NspSpColMatrix *A,*Res;
      if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
      if ( A->m != self->obj->A->nrow )
	{
	  Scierror("Error: argument should have %d rows\n",self->obj->A->nrow);
	  return RET_BUG;
	}

      if ( ! ( ( A->rc_type == 'c' && self->obj->A->xtype == CHOLMOD_COMPLEX)
	       || ( A->rc_type == 'r' && self->obj->A->xtype == CHOLMOD_REAL)))
	{
	  Scierror("Error: argument should be %s \n",
		   (self->obj->A->xtype = CHOLMOD_COMPLEX) ? "complex": "real");
	  return RET_BUG;
 	}
      
      /* get sparse matrix B (unsymmetric) */
      Bspmatrix = cholmod_sparse_from_object(NSP_OBJECT(A), &dummy, 0,FALSE,TRUE, TRUE, &(self->obj->Common));
      if ( Bspmatrix == NULL) return RET_BUG;
      Xs = SuiteSparseQR_C_backslash_sparse(self->obj->ordering,self->obj->tol,
					   self->obj->A,Bspmatrix,&(self->obj->Common));
      /* Create a NspSpColMatrix and free Xs */
      cholmod_l_free_sparse (&Bspmatrix, &(self->obj->Common));
      if ((Res = nsp_cholmod_to_spcol_sparse(&Xs, &(self->obj->Common)))== NULL)
	return RET_BUG;
      if ( Res == NULL) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(Res));
      return 1;
    }
  return 0;
}

static int int_spqr_meth_get_ordering(NspSpqr *self,Stack stack, int rhs, int opt, int lhs)
{
  char *ordering_s=NULL;
  CheckStdRhs(0,0);
  CheckLhs(1,1);
  
  if ( self->obj == NULL || self->obj->A  == NULL ) 
    {
      Scierror("Error: cholmod object is not properly built\n");
      return RET_BUG;
    }
  
  switch ( self->obj->ordering )
    {
    case SPQR_ORDERING_FIXED:   ordering_s="fixed" ;   break ;
    case SPQR_ORDERING_NATURAL: ordering_s="natural" ; break ;
    case SPQR_ORDERING_COLAMD:  ordering_s="colamd" ;  break ;
    case SPQR_ORDERING_GIVEN:   ordering_s="given" ;   break ;
    case SPQR_ORDERING_CHOLMOD: ordering_s="best" ;    break ;
    case SPQR_ORDERING_AMD:     ordering_s="amd" ;     break ;
    case SPQR_ORDERING_METIS:   ordering_s="metis" ;   break ;
    case SPQR_ORDERING_DEFAULT: ordering_s="default" ; break ;
    }
  if ( nsp_move_string(stack,1,ordering_s,-1) == FAIL)
    return RET_BUG; 
  return Max(lhs,1);
}

static NspMethods cholmod_methods[] = {
  {"rsolve",(nsp_method *) int_spqr_meth_rsolve},
  {"backslash",(nsp_method *) int_spqr_meth_backslash},
  {"qmult",(nsp_method *) int_spqr_meth_qmult},
  {"isreal",(nsp_method *) int_spqr_meth_isreal},
  {"get_qr",(nsp_method *) int_spqr_meth_get_qr},
  {"get_ordering",(nsp_method *) int_spqr_meth_get_ordering},
  { NULL, NULL}
};

static NspMethods *spqr_get_methods(void) { return cholmod_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab spqr_attrs[] = {
  { NULL,NULL,NULL,NULL, NULL  },
};


/*-------------------------------------------
 * functions
 *-------------------------------------------*/

/*----------------------------------------------------
 * Interface
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

/* qr interface for sparse matrices
 */

static OpTab spqr_func[]={
  { NULL, NULL}
};

/* call ith function in the spqr interface */

int spqr_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(spqr_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table
    (for adding or removing functions) */

void spqr_Interf_Info(int i, char **fname, function (**f))
{
  *fname = spqr_func[i].name;
  *f = spqr_func[i].fonc;
}

#endif /* WITH_SPQR */
