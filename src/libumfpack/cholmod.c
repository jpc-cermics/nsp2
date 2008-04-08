/* Nsp
 * Copyright (C) 2006-2007 Jean-Philippe Chancelier Enpc/Cermics
 * 
 * Some routines at the end are copied from CHOLMOD/MATLAB Module. 
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
 * Interface with the cholmod library using a NspCholmod Object. 
 * We use ZOMPLEX for sparse Matrice since passing sparse matrices 
 * from Nsp to cholmod uses Matlab sparse triplet (Sptriplet.c) 
 * which are ZOMPLEX. 
 * For full matrices COMPLEX is preferred since it is the default 
 * in Nsp.
 *
 * 
 */

#include "nsp/machine.h"
#ifdef WITH_CHOLMOD 

#include <cholmod.h>
#define  Cholmod_Private 
#include "nsp/object.h"
#include "nsp/cholmod.h"
#include "nsp/interf.h"
#include "mex/mex.h"

#ifndef SPUMONI
#define SPUMONI 0
#endif

static void nsp_sputil_config (int spumoni, cholmod_common *cm, int in_mex );
static void nsp_matrix_to_cholmod_dense(NspMatrix *A, cholmod_dense *B,double *dummy) ;
static NspMatrix *nsp_cholmod_dense_to_matrix(cholmod_dense **Ahandle,cholmod_common *cm);
static void nsp_cholmod_sparse_free(cholmod_sparse *B);
static NspSpColMatrix * nsp_cholmod_to_spcol_sparse(cholmod_sparse **Ahandle, cholmod_common *cm);
static NspMatrix *nsp_matrix_from_int(const int *P,int n, int one_based);
static int nsp_cholmod_from_spcol(NspCholmod *Ch,NspSpColMatrix *Sp,  double *beta,int stype,int transpose,
				  int ll, int quick_return_if_not_posdef,int ordering,NspMatrix *Perm);
static int nsp_cholmod_set_ordering(int ordering,NspMatrix *Perm, cholmod_common *cm);
static int nsp_spcol_to_cholmod_sparse(NspSpColMatrix *A, cholmod_sparse *B,double *dummy,int stype, int transpose);
static cholmod_sparse *cholmod_pattern_from_object(NspObject *Obj,cholmod_sparse *Ashallow,double *dummy,int transpose, cholmod_common *cm );
/* 
 * NspCholmod inherits from NspObject 
 */

int nsp_type_cholmod_id=0;
NspTypeCholmod *nsp_type_cholmod=NULL;

/*
 * Type object for Cholmod 
 * all the instance of NspTypeCholmod share the same id. 
 * nsp_type_cholmod: is an instance of NspTypeCholmod 
 *    used for objects of NspCholmod type (i.e built with new_cholmod) 
 * other instances are used for derived classes 
 */
NspTypeCholmod *new_type_cholmod(type_mode mode)
{
  NspTypeCholmod *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_cholmod != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_cholmod;
    }
  if ((type =  malloc(sizeof(NspTypeCholmod))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = cholmod_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = cholmod_get_methods; 
  type->new = (new_func *) new_cholmod;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for cholmod */ 

  top->pr = (print_func *) nsp_cholmod_print;                  
  top->dealloc = (dealloc_func *) nsp_cholmod_destroy;
  top->copy  =  (copy_func *) nsp_cholmod_copy;                 
  top->size  = (size_func *) nsp_cholmod_size;                
  top->s_type =  (s_type_func *) nsp_cholmod_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_cholmod_type_short_string;
  top->info = (info_func *) nsp_cholmod_info ;                  
  /* top->is_true = (is_true_func  *) nsp_cholmod_is_true; */
  /* top->loop =(loop_func *) nsp_cholmod_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_cholmod_object;
  top->eq  = (eq_func *) nsp_cholmod_eq;
  top->neq  = (eq_func *) nsp_cholmod_neq;
  top->save  = (save_func *) nsp_cholmod_xdr_save;
  top->load  = (load_func *) nsp_cholmod_xdr_load;
  top->create = (create_func*) int_cholmod_create;
  
  /* specific methods for cholmod */
      
  type->init = (init_func *) init_cholmod;

/* 
 * Cholmod interfaces can be added here 
 * type->interface = (NspTypeBase *) new_type_b();
 * type->interface->interface = (NspTypeBase *) new_type_C()
 * ....
 */
  if ( nsp_type_cholmod_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeCholmod called nsp_type_cholmod
       */
      type->id =  nsp_type_cholmod_id = nsp_new_type_id();
      nsp_type_cholmod = type;
      if ( nsp_register_type(nsp_type_cholmod) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_cholmod(mode);
    }
  else 
    {
       type->id = nsp_type_cholmod_id;
       return type;
    }
}

/*
 * initialize Cholmod instances 
 * locally and by calling initializer on parent class 
 */

static int init_cholmod(NspCholmod *o,NspTypeCholmod *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Cholmod 
 */

NspCholmod *new_cholmod() 
{
  NspCholmod *loc; 
  /* type must exists */
  nsp_type_cholmod = new_type_cholmod(T_BASE);
  if ( (loc = malloc(sizeof(NspCholmod)))== NULLCHOLMOD) return loc;
  /* initialize object */
  if ( init_cholmod(loc,nsp_type_cholmod) == FAIL) return NULLCHOLMOD;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Cholmod 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_cholmod_size(NspCholmod *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char cholmod_type_name[]="Cholmod";
static char cholmod_short_type_name[]="cholmod";

static char *nsp_cholmod_type_as_string(void)
{
  return(cholmod_type_name);
}

static char *nsp_cholmod_type_short_string(NspObject *v)
{
  return(cholmod_short_type_name);
}

/*
 * A == B 
 */

static int nsp_cholmod_eq(NspCholmod *A, NspObject *B)
{
  NspCholmod *loc = (NspCholmod *) B;
  if ( check_cast(B,nsp_type_cholmod_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  return FALSE;
}

/*
 * A != B 
 */

static int nsp_cholmod_neq(NspCholmod *A, NspObject *B)
{
  return ( nsp_cholmod_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

static int nsp_cholmod_xdr_save(XDR *xdrs, NspCholmod *M)
{
  Sciprintf("Warning: cannot save Cholmod objects (cowardly not saving this object)\n");
  return OK;
}

/*
 * load 
 */

static NspCholmod  *nsp_cholmod_xdr_load(XDR *xdrs)
{
  NspCholmod *M = NULL;
  /* should never get there since cholmod object are not saved */
  return M;
}

/*
 * delete 
 */

void nsp_cholmod_destroy(NspCholmod *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
     cholmod_free_factor (&(H->obj->L),&(H->obj->Common));
     cholmod_finish (&(H->obj->Common)) ;
     cholmod_print_common (" ",&(H->obj->Common)) ;
     FREE(H->obj);
   }
  FREE(H);
}

/*
 * info 
 */

int nsp_cholmod_info(NspCholmod *M, int indent,const char *name, int rec_level)
{
  int i;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if ( M == NULLCHOLMOD || M->obj == NULL ) 
    {
      Sciprintf("Null Pointer Cholmod \n");
      return TRUE;
    }
  Sciprintf("%s\t= [...]\t%s %c (%dx%d)\n",pname,nsp_cholmod_type_short_string(NSP_OBJECT(M)),
	    (M->obj->L->xtype ==  CHOLMOD_ZOMPLEX) ? 'c' : 'r', 
	    M->obj->m,M->obj->n); 
  return TRUE;
}

/*
 * print 
 */

int nsp_cholmod_print(NspCholmod *Mat, int indent,char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  if (user_pref.pr_as_read_syntax)
    {
      Sciprintf("// Cannot print an cholmod object using as_read=%%t option \n");
    }
  else 
    {
      nsp_cholmod_info(Mat,indent,pname,rec_level);
    }
  return TRUE;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Cholmod objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspCholmod   *nsp_cholmod_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_cholmod_id) == TRUE ) return ((NspCholmod *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_cholmod));
  return NULL;
}

int IsCholmodObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_cholmod_id);
}

int IsCholmod(NspObject *O)
{
  return nsp_object_type(O,nsp_type_cholmod_id);
}

NspCholmod  *GetCholmodCopy(Stack stack, int i)
{
  if (  GetCholmod(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspCholmod  *GetCholmod(Stack stack, int i)
{
  NspCholmod *M;
  if (( M = nsp_cholmod_object(NthObj(i))) == NULLCHOLMOD)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspCholmod *cholmod_create_void(char *name,NspTypeBase *type)
{
 NspCholmod *H  = (type == NULL) ? new_cholmod() : type->new();
 if ( H ==  NULLCHOLMOD)
  {
   Sciprintf("No more memory\n");
   return NULLCHOLMOD;
  }
  /* shared by all objects */
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULL)
   return NULLCHOLMOD;
 NSP_OBJECT(H)->ret_pos = -1 ;
 H->obj = NULL;
 return H;
}

NspCholmod *cholmod_create(char *name,NspTypeBase *type)
{
 NspCholmod *H  = cholmod_create_void(name,type);
 if ( H ==  NULLCHOLMOD) return NULLCHOLMOD;
 if ((H->obj = calloc(1,sizeof(nsp_cholmod))) == NULL) return NULL;
  H->obj->ref_count=1;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspCholmod *nsp_cholmod_copy(NspCholmod *self)
{
  NspCholmod *H  =cholmod_create_void(NVOID,(NspTypeBase *) nsp_type_cholmod);
  if ( H ==  NULLCHOLMOD) return NULLCHOLMOD;
  H->obj = self->obj;
  self->obj->ref_count++;
 return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Cholmod
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_cholmod_create(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Perm= NULL;
  char *sstype=NULL, *ctype=NULL;
  int stype=1;	    /* use upper part of A */
  int transpose= FALSE;
  int ordering=-1; /* use default ordering */
  int ll = FALSE ; /* LDL' used by default */		  
  double beta[2]={0,0} ;
  NspSpColMatrix *A;
  NspCholmod *H=NULL;
  nsp_option opts[] ={{"type",string,NULLOBJ,-1},
		      {"mode",string,NULLOBJ,-1},
		      {"beta",s_double,NULLOBJ,-1},
		      {"ordering",s_int,NULLOBJ,-1},
		      {"perm", matcopy,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  /* Get a sparse matrix */
  CheckStdRhs(1,1);
  CheckLhs(0,2);
  /* now we can store the Numeric part */
  /* want to be sure that type cholmod is initialized */
  nsp_type_cholmod = new_type_cholmod(T_BASE);
  if(( H = cholmod_create(NVOID,(NspTypeBase *) nsp_type_cholmod)) == NULLCHOLMOD) 
    goto err;
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) goto err;

  /* optional arguments */
  if ( get_optional_args(stack,rhs,opt,opts,&sstype,&ctype,&beta,&ordering,&Perm) == FAIL) 
    return RET_BUG;
  /* checks the optional type argument */
  if ( sstype != NULL) 
    {
      char *types[]={ "row", "col", "sym", "lo", "up",  NULL };
      int rep = is_string_in_array(sstype, types,1);
      if ( rep < 0 ) 
	{
	  string_not_in_array(stack,sstype,types,"optional argument");
	  return RET_BUG;
	}
      if ( rep == 0 || rep == 1 ) stype=0; /* use all of A */
      if ( rep == 1) transpose=TRUE;
      if ( rep == 3 ) stype = -1; /* use lower part */
    }
  /* checks the optional mode argument */
  if ( ctype != NULL) 
    {
      char *types[]={ "ll'", "ldl'",  NULL };
      int rep = is_string_in_array(ctype, types,1);
      if ( rep < 0 ) 
	{
	  string_not_in_array(stack,ctype,types,"optional argument");
	  return RET_BUG;
	}
      ll = (rep == 0) ? TRUE : FALSE;
    }
  /* checks the optional perm argument */
  if ( Perm != NULL) 
    {
      int k;
      for (k = 0 ; k < Perm->mn ; k++)
	{
	  Perm->I[k] = Perm->R[k]-1 ;
	}
      ordering = Perm->I[0];
    }  
  if ( nsp_cholmod_from_spcol(H,A,beta,stype,transpose,ll , (lhs < 2),ordering,Perm ) == FAIL)
    goto err; 

  if ( lhs < 2 && H->obj->Common.status != CHOLMOD_OK)
    {
      Scierror("Error: matrix is not positive definite\n");
      return RET_BUG;
    }
  /* we return the minor and H 
   */
  MoveObj(stack,1,NSP_OBJECT(H));
  if ( lhs >= 2) 
    {
      if ( nsp_move_double(stack,2,((H->obj->L->minor == H->obj->n) ? 0 : (H->obj->L->minor+1)))==FAIL)
	return RET_BUG;
    }
  return Max(lhs,1);
 err: 
  /* XXXX free */
  return RET_BUG;
} 


static int int_cholmod_meth_isreal(NspCholmod *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;
  int ret;
  CheckRhs(0,0); 
  CheckLhs(1,1);
  if ( self->obj == NULL || self->obj->L  == NULL) 
    {
      Scierror("Error: cholmod object is not properly built\n");
      return RET_BUG;
    }
  ret =  (self->obj->L->xtype ==  CHOLMOD_ZOMPLEX) ?  FALSE :  TRUE;
  if ((Obj = nsp_new_boolean_obj(ret))==NULLOBJ) return RET_BUG;
  MoveObj(stack,1,Obj);
  return 1;
}



static int int_cholmod_meth_solve(NspCholmod *self, Stack stack, int rhs, int opt, int lhs)
{
  double dummy = 0;
  cholmod_sparse Bspmatrix, *Xs ;
  cholmod_dense Bmatrix, *X ;
  
  CheckRhs(1,1); 
  CheckLhs(1,1);

  if ( self->obj == NULL || self->obj->L  == NULL) 
    {
      Scierror("Error: cholmod object is not properly built\n");
      return RET_BUG;
    }
  if (IsMatObj(stack,1))
    {
      NspMatrix *A,*Res;
      if ( (A = GetMat(stack, 1)) == NULLMAT ) return RET_BUG;
      if ( A->m != self->obj->m )
	{
	  Scierror("Error: argument should have %d rows\n",self->obj->m);
	  return RET_BUG;
	}
      nsp_matrix_to_cholmod_dense(A,&Bmatrix,&dummy) ;
      X = cholmod_solve (CHOLMOD_LDLt, self->obj->L, &Bmatrix, &(self->obj->Common)) ;
      /* use X to fill a NspMatrix then destroy X */
      /* XXXX: the complex case is to be done Xmust be complex not zomplex */
      Res = nsp_cholmod_dense_to_matrix (&X, &(self->obj->Common)) ;
      if ( Res == NULL) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(Res));
      return 1;
    }
  else if (IsSpColMatObj(stack,1))
    { 
      NspSpColMatrix *A,*Res;
      if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
      if ( A->m != self->obj->m )
	{
	  Scierror("Error: argument should have %d rows\n",self->obj->m);
	  return RET_BUG;
	}
      /* get sparse matrix B (unsymmetric) */
      if ( nsp_spcol_to_cholmod_sparse(A,&Bspmatrix, &dummy, 0, FALSE)== FAIL) 
	return RET_BUG;
      Xs = cholmod_spsolve (CHOLMOD_LDLt, self->obj->L, &Bspmatrix, &(self->obj->Common)) ;
      nsp_cholmod_sparse_free( &Bspmatrix);
      /* Create a NspSpColMatrix and free Xs */
      if ((Res = nsp_cholmod_to_spcol_sparse(&Xs, &(self->obj->Common)))== NULL) 
	return RET_BUG;
      if ( Res == NULL) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(Res));
      return 1;
    }
  return 0;
}


static int int_cholmod_meth_get_ld(NspCholmod *self, Stack stack, int rhs, int opt, int lhs)
{
  cholmod_factor *Lc;
  cholmod_sparse *Lsparse;
  NspSpColMatrix *Res;

  CheckRhs(0,0); 
  CheckLhs(1,3);

  if ( self->obj == NULL || self->obj->L  == NULL) 
    {
      Scierror("Error: cholmod object is not properly built\n");
      return RET_BUG;
    }
  /* we need a copy here since we do not want L to be destroyed */
  Lc = cholmod_copy_factor(self->obj->L, &(self->obj->Common));
  Lsparse = cholmod_factor_to_sparse (Lc, &(self->obj->Common)) ;
  cholmod_free_factor (&Lc,&(self->obj->Common));
  if (Lsparse->xtype == CHOLMOD_COMPLEX)
    {
      /* convert Lsparse from complex to zomplex */
      cholmod_sparse_xtype (CHOLMOD_ZOMPLEX, Lsparse, &(self->obj->Common)) ;
    }
  /* return L as a sparse matrix (it may contain numerically zero entries) */
  /* after calling this function Lsparse is freed */
  if ((Res = nsp_cholmod_to_spcol_sparse(&Lsparse, &(self->obj->Common))) == NULL)
    return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(Res));
  /* return minor (translate to MATLAB convention) */
  if ( lhs  > 1)
    {
      int minor = self->obj->L->minor;
      nsp_move_double(stack,2,(minor == self->obj->n) ? 0 : (minor+1));
    }
  if ( lhs > 2 ) 
    {
      NspMatrix *Res2;
      if ((Res2 = nsp_matrix_from_int(self->obj->L->Perm,self->obj->n, 1))== NULLMAT) 
	return RET_BUG;
      MoveObj(stack,3,NSP_OBJECT(Res2));
    }
  return Max(lhs,1);
}


/* Multiple-rank update or downdate of a sparse LDL' factorization.
 * Usage:
 *
 *	LD.update[C]		update an LDL' factorization
 */

static int int_cholmod_meth_updown(NspCholmod *self, Stack stack, int rhs, int opt, int lhs,int direction)
{
  cholmod_sparse Cspmatrix;
  NspSpColMatrix *C;
  double dummy = 0;
  CheckRhs(1,1); 
  CheckLhs(0,1);

  if ( self->obj == NULL || self->obj->L  == NULL) 
    {
      Scierror("Error: cholmod object is not properly built\n");
      return RET_BUG;
    }
  if ((C=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( C->m != self->obj->m || C->n != 1 ) 
    {
      Scierror("Error: argument should be a  %dx1 column vector\n",self->obj->m);
      return RET_BUG;
    }
  /* get sparse matrix B (unsymmetric) */
  if ( nsp_spcol_to_cholmod_sparse(C,&Cspmatrix, &dummy, 0, FALSE)== FAIL) 
	return RET_BUG;
  if (!cholmod_updown (direction, &Cspmatrix, self->obj->L,  &(self->obj->Common)))
    {
      nsp_cholmod_sparse_free( &Cspmatrix);
      Scierror("Error: ldlupdate failed\n") ;
      return RET_BUG;
    }
  nsp_cholmod_sparse_free( &Cspmatrix);
  return 0;
}

static int int_cholmod_meth_resymbol(NspCholmod *self, Stack stack, int rhs, int opt, int lhs,int direction)
{
  cholmod_sparse Aspmatrix;
  NspSpColMatrix *A;
  double dummy = 0;
  CheckRhs(1,1); 
  CheckLhs(0,1);

  if ( self->obj == NULL || self->obj->L  == NULL) 
    {
      Scierror("Error: cholmod object is not properly built\n");
      return RET_BUG;
    }

  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( A->m != self->obj->m || A->n != self->obj->m ) 
    {
      Scierror("Error: argument should be a  %dx%d matrix\n",self->obj->m,self->obj->n);
      return RET_BUG;
    }

  /* get sparse matrix A */
  /* XXXX here we only need a pattern matrix cholmod_sparse *sputil_get_sparse_pattern 
   *      A->xtype = CHOLMOD_PATTERN ;
   */

  if ( nsp_spcol_to_cholmod_sparse(A,&Aspmatrix, &dummy, 0, FALSE)== FAIL) 
	return RET_BUG;
  Aspmatrix.xtype =  CHOLMOD_PATTERN;
  cholmod_resymbol (&Aspmatrix, NULL, 0, TRUE, self->obj->L,  &(self->obj->Common));
  nsp_cholmod_sparse_free( &Aspmatrix);
  return 0;
}

static int int_cholmod_meth_update(NspCholmod *self, Stack stack, int rhs, int opt, int lhs)
{
  return int_cholmod_meth_updown(self,stack,rhs,opt,lhs,TRUE);
}

/* downdate 
 *
 */

static int int_cholmod_meth_downdate(NspCholmod *self, Stack stack, int rhs, int opt, int lhs)
{
  return int_cholmod_meth_updown(self,stack,rhs,opt,lhs,FALSE);
}


static int int_cholmod_meth_getperm(NspCholmod *self, Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Res;
  CheckRhs(0,0); 
  CheckLhs(0,1);

  if ( self->obj == NULL || self->obj->L  == NULL) 
    {
      Scierror("Error: cholmod object is not properly built\n");
      return RET_BUG;
    }
  if ((Res = nsp_matrix_from_int(self->obj->L->Perm,self->obj->n, 1))== NULLMAT) 
    return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}

static int int_cholmod_meth_get_minor(NspCholmod *self, Stack stack, int rhs, int opt, int lhs)
{
  int minor;
  CheckRhs(0,0); 
  CheckLhs(0,1);

  if ( self->obj == NULL || self->obj->L  == NULL) 
    {
      Scierror("Error: cholmod object is not properly built\n");
      return RET_BUG;
    }
  minor = self->obj->L->minor;

  if ( nsp_move_double(stack,1, (minor == self->obj->n) ? 0 : (minor+1)) == FAIL) 
    return RET_BUG;
  return 1;
}

#define CHOLMOD_METH(name, val)						\
  static int int_cholmod_meth_get_##name(NspCholmod *self, Stack stack, int rhs, int opt, int lhs) \
  {									\
    CheckRhs(0,0);							\
    CheckLhs(0,1);							\
    if ( self->obj == NULL || self->obj->L  == NULL)			\
      {									\
	Scierror("Error: cholmod object is not properly built\n");	\
	return RET_BUG;							\
      }									\
    if ( nsp_move_double(stack,1,val) == FAIL)  return RET_BUG;		\
    return 1;								\
  }									\

CHOLMOD_METH(rcond, cholmod_rcond( self->obj->L, &(self->obj->Common)));
CHOLMOD_METH(ordering, self->obj->L->ordering);
CHOLMOD_METH(lnz, self->obj->Common.lnz);
CHOLMOD_METH(fl, self->obj->Common.fl);
CHOLMOD_METH(memory, self->obj->Common.memory_usage / 1048576);


static NspMethods cholmod_methods[] = {
  {"solve",(nsp_method *) int_cholmod_meth_solve},
  {"isreal",(nsp_method *) int_cholmod_meth_isreal},
  {"get_ld",(nsp_method *) int_cholmod_meth_get_ld},
  {"get_minor",(nsp_method *) int_cholmod_meth_get_minor},
  {"get_perm",(nsp_method *) int_cholmod_meth_getperm},
  {"update", (nsp_method *) int_cholmod_meth_update},
  {"downdate", (nsp_method *) int_cholmod_meth_downdate},
  {"resymbol", (nsp_method *) int_cholmod_meth_resymbol},
  {"get_rcond",(nsp_method *) int_cholmod_meth_get_rcond},
  {"get_ordering",(nsp_method *) int_cholmod_meth_get_ordering},
  {"get_lnz",(nsp_method *) int_cholmod_meth_get_lnz},
  {"get_fl",(nsp_method *) int_cholmod_meth_get_fl},
  {"get_memory", (nsp_method *) int_cholmod_meth_get_memory},
  { NULL, NULL}
};

static NspMethods *cholmod_get_methods(void) { return cholmod_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab cholmod_attrs[] = {
  { NULL,NULL,NULL,NULL, NULL  },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

/* chol interface for sparse matrices 
 * [R,p,q]=chol(Sp, type=, mode=,beta=,ordering=,perm=)
 * 
 * This interface is given for compatibility but 
 * it is faster to use cholmod_create objects which use 
 * LL' and LDL' decompositions which are faster than R'R
 * and use less memory.  

 * R = chol2 (A)		same as R = chol (A), just faster
 * [R,p] = chol2 (A)		save as [R,p] = chol(A), just faster
 * [R,p,q] = chol2 (A)		factorizes A(q,q) into R'*R
 *
 * A must be sparse.  It can be complex or real.
 *
 * R is returned with no explicit zero entries. 
 * spones (R) will be equal to the R returned
 * by symbfact2 if no numerically zero entries are dropped, or a subset
 * otherwise.
 */

int int_cholmod_chol(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Perm= NULL;
  cholmod_sparse *Lsparse,*Rsparse;
  cholmod_factor *Lc;
  char *sstype=NULL, *ctype=NULL;
  int stype=1;	    /* use upper part of A */
  int transpose= FALSE;
  int minor;
  int ordering=-1; /* use default ordering */
  int ll = FALSE ; /* LDL' used by default */		  
  double beta[2]={0,0} ;
  NspSpColMatrix *A,*Res;
  NspCholmod *H=NULL;
  nsp_option opts[] ={{"type",string,NULLOBJ,-1},
		      {"mode",string,NULLOBJ,-1},
		      {"beta",s_double,NULLOBJ,-1},
		      {"ordering",s_int,NULLOBJ,-1},
		      {"perm", matcopy,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  /* Get a sparse matrix */
  CheckStdRhs(1,1);
  CheckLhs(0,3);
  /* now we can store the Numeric part */
  /* want to be sure that type cholmod is initialized */
  nsp_type_cholmod = new_type_cholmod(T_BASE);
  if(( H = cholmod_create(NVOID,(NspTypeBase *) nsp_type_cholmod)) == NULLCHOLMOD) 
    goto err;
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) goto err;

  /* optional arguments */
  if ( get_optional_args(stack,rhs,opt,opts,&sstype,&ctype,&beta,&ordering,&Perm) == FAIL) 
    return RET_BUG;
  /* checks the optional type argument */
  /* checks the optional type argument */
  if ( sstype != NULL) 
    {
      char *types[]={ "row", "col", "sym", "lo", "up",  NULL };
      int rep = is_string_in_array(sstype, types,1);
      if ( rep < 0 ) 
	{
	  string_not_in_array(stack,sstype,types,"optional argument");
	  return RET_BUG;
	}
      if ( rep == 0 || rep == 1 ) stype=0; /* use all of A */
      if ( rep == 1) transpose=TRUE;
      if ( rep == 3 ) stype = -1; /* use lower part */
    }
  /* checks the optional mode argument */
  if ( ctype != NULL) 
    {
      char *types[]={ "ll'", "ldl'",  NULL };
      int rep = is_string_in_array(ctype, types,1);
      if ( rep < 0 ) 
	{
	  string_not_in_array(stack,ctype,types,"optional argument");
	  return RET_BUG;
	}
      ll = (rep == 0) ? TRUE : FALSE;
    }
  /* checks the optional perm argument */
  if ( Perm != NULL) 
    {
      int k;
      for (k = 0 ; k < Perm->mn ; k++)
	{
	  Perm->I[k] = Perm->R[k]-1 ;
	}
      ordering = Perm->I[0];
    }  
  if ( nsp_cholmod_from_spcol(H,A,beta,stype,transpose,ll , (lhs < 2),ordering,Perm ) == FAIL)
    goto err; 
    
  if ( lhs < 2 && H->obj->Common.status != CHOLMOD_OK)
    {
      Scierror("Error: matrix is not positive definite\n");
      return RET_BUG;
    }

  /* get the matrix L 
   */
  Lc = cholmod_copy_factor(H->obj->L, &(H->obj->Common));
  Lsparse = cholmod_factor_to_sparse (Lc, &(H->obj->Common)) ;
  cholmod_free_factor (&Lc,&(H->obj->Common));
  if (Lsparse->xtype == CHOLMOD_COMPLEX)
    {
      /* convert Lsparse from complex to zomplex */
      cholmod_sparse_xtype (CHOLMOD_ZOMPLEX, Lsparse, &(H->obj->Common)) ;
    }
  /* return L as a sparse matrix (it may contain numerically zero entries) */
  /* after calling this function Lsparse is freed */
  minor = H->obj->L->minor ;
  
#if 0 
  if (minor < n)
    {
      /* remove columns minor to n-1 from Lsparse */
      sputil_trim (Lsparse, minor, cm) ;
    }
  
  /* drop zeros from Lsparse 
   * Not useful since default above is cm->final_resymbol = TRUE;
   */
  /* sputil_drop_zeros (Lsparse) ; */
  
#endif 

  /* Lsparse is lower triangular; conjugate transpose to get R */
  Rsparse = cholmod_transpose (Lsparse, 2, &(H->obj->Common)) ;
  cholmod_free_sparse (&Lsparse, &(H->obj->Common)) ;
  /* Lsparse is cleaned by this operation */
  if ((Res = nsp_cholmod_to_spcol_sparse(&Rsparse, &(H->obj->Common))) == NULL)
    return RET_BUG;

  MoveObj(stack,1,NSP_OBJECT(Res));
  
  /* return minor (translate to MATLAB convention) */

  if ( lhs  > 1)
    {
      nsp_move_double(stack,2,(minor == H->obj->n) ? 0 : (minor+1));
    }
  if ( lhs > 2 ) 
    {
      NspMatrix *Res2;
      if ((Res2 = nsp_matrix_from_int(H->obj->L->Perm,H->obj->n, 1))== NULLMAT) 
	return RET_BUG;
      MoveObj(stack,3,NSP_OBJECT(Res2));
    }
  /* clean  cholmod object */
  nsp_cholmod_destroy(H);
  return Max(lhs,1);
  
 err: 
  /* XXXX free */
  return RET_BUG;
} 


#if 0 
static int int_cholmod_norm(Stack stack, int rhs, int opt, int lhs)
{
  int n=0;
  double dummy = 0,anorm;
  cholmod_sparse Amatrix;
  NspSpColMatrix *A;
  cholmod_common Common, *cm ;

  CheckStdRhs(1,2);
  CheckLhs(0,1);
  cm = &Common ;
  cholmod_start (cm) ;
  nsp_sputil_config(SPUMONI, cm,FALSE) ;
  /* Get a sparse matrix */
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ( A->m != A->n )
    {
      Scierror("Error: should be used for square matrix\n");
      return RET_BUG;
    }
  if ( rhs >= 2) 
    {
      if (GetScalarInt(stack,2,&n) == FAIL) return RET_BUG;
      if ( n != 1 && n != 0) 
	{
	  Scierror("Error: only 1 or 0 \n");
	  return RET_BUG;
	}
    }
  if ( nsp_spcol_to_cholmod_sparse(A,&Amatrix, &dummy,-1,FALSE)== FAIL) 
    return RET_BUG;
  anorm = cholmod_norm_sparse (&Amatrix, n , cm) ;
  nsp_cholmod_sparse_free(&Amatrix);
  nsp_move_double(stack,1,anorm);
  return 1;
} 
#endif 


/* Order a matrix and then analyze it, using CHOLMOD's best-effort ordering.
 * Returns the count of the number of nonzeros in each column of L for the
 * permuted matrix A.
 * analyze(A,type=,mode=,beta=,ordering=,perm=)
 *
 */

int int_cholmod_analyze(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;
  cholmod_factor *L ;
  cholmod_common cm;
  double dummy = 0;
  cholmod_sparse Amatrix,*Ac;
  NspMatrix *Res;
  NspMatrix *Perm= NULL;
  char *sstype=NULL, *ctype=NULL;
  int stype=1;	    /* use upper part of A */
  int transpose= FALSE;
  int ordering=-1; /* use default ordering */
  double beta[2]={0,0} ;
  nsp_option opts[] ={{"type",string,NULLOBJ,-1},
		      {"mode",string,NULLOBJ,-1},
		      {"beta",s_double,NULLOBJ,-1},
		      {"ordering",s_int,NULLOBJ,-1},
		      {"perm", matcopy,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  /* Get a sparse matrix */
  CheckStdRhs(1,1);
  CheckLhs(0,2);
  
  if ((Obj = nsp_get_object(stack,1)) == NULL)   return RET_BUG;
  
  /* optional arguments */
  if ( get_optional_args(stack,rhs,opt,opts,&sstype,&ctype,&beta,&ordering,&Perm) == FAIL) 
    return RET_BUG;
  /* checks the optional type argument */
  if ( sstype != NULL) 
    {
      char *types[]={ "row", "col", "sym", "lo", "up",  NULL };
      int rep = is_string_in_array(sstype, types,1);
      if ( rep < 0 ) 
	{
	  string_not_in_array(stack,sstype,types,"optional argument");
	  return RET_BUG;
	}
      if ( rep == 0 || rep == 1 ) stype=0; /* use all of A */
      if ( rep == 1) transpose=TRUE;
      if ( rep == 3 ) stype = -1; /* use lower part */
    }
  /* checks the optional perm argument */
  if ( Perm != NULL) 
    {
      int k;
      for (k = 0 ; k < Perm->mn ; k++)
	{
	  Perm->I[k] = Perm->R[k]-1 ;
	}
      ordering = Perm->I[0];
    }  

  /*
   * start CHOLMOD and set parameters
   */
  
  cholmod_start (&(cm)) ;
  nsp_sputil_config (SPUMONI, &(cm),FALSE) ;
  
  /* only do the simplicial analysis (L->Perm and L->ColCount) */
  cm.supernodal = CHOLMOD_SIMPLICIAL ;

  if (  nsp_cholmod_set_ordering(ordering,Perm,&cm) == FAIL) 
    return RET_BUG;

  Ac = cholmod_pattern_from_object(Obj, &Amatrix,&dummy,transpose,&cm);
  
  if ( Ac == NULL) 
    return RET_BUG;


  if (stype != 0 &&  Ac->nrow != Ac->ncol ) 
    {
      /* matrix must be square except when factorization of A*A' or A'*A is done */
      Scierror("Error: matrix must be square\n");
      return RET_BUG;
    }

  /*
   * analyze
   */
  L = cholmod_analyze_p (Ac,(Perm==NULL) ? NULL: Perm->I,NULL,0,&cm ) ;

  if ((Res = nsp_matrix_from_int(L->Perm,Ac->nrow, 1))== NULLMAT) 
    return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(Res));
  if ( lhs  > 1)
    {
      if ((Res = nsp_matrix_from_int(L->ColCount,Ac->nrow, 1))== NULLMAT)
	return RET_BUG;
      MoveObj(stack,2,NSP_OBJECT(Res));
    }

  cholmod_free_factor (&L, &cm) ;
  if ( Ac != &Amatrix ) 
    cholmod_free_sparse (&Ac, &cm) ;
  else 
    nsp_cholmod_sparse_free( &Amatrix);
  cholmod_finish (&cm) ;
  return Max(lhs,1);

} 

static OpTab cholmod_func[]={
  { "analyze", int_cholmod_analyze},
  { "chol_sp", int_cholmod_chol},
  { "cholmod_create", int_cholmod_create},
  { NULL, NULL}
};

/* call ith function in the cholmod interface */

int cholmod_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(cholmod_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void cholmod_Interf_Info(int i, char **fname, function (**f))
{
  *fname = cholmod_func[i].name;
  *f = cholmod_func[i].fonc;
}

/* A set of routines to convert from/to Nsp and Cholmod 
 * Some functions were coded with the help of cholmod_matlab 
 * file form CHOLMOD/MATLAB Module Version 1.2.  Copyright (C) 2005-2006,
 * Timothy A. Davis
 */ 

/**
 * nsp_cholmod_from_spcol:
 * @Ch: 
 * @Sp: 
 * @beta: 
 * @stype: 
 * @transpose: 
 * @ll: 
 * @quick_return_if_not_posdef: 
 * @cholmod_natural: 
 * @perm: 
 * 
 * 
 * Initialize a #NspCholmod object from a sparse matrix @Sp
 * note that  Ch->obj->Common.status can be on return not equal to 
 * CHOLMOD_OK if matrix is not positive definite 
 * 
 * Returns: %OK or %FAIL
 **/

static int nsp_cholmod_from_spcol(NspCholmod *Ch,NspSpColMatrix *Sp,  double *beta,int stype,int transpose,
				  int ll, int quick_return_if_not_posdef,int ordering,NspMatrix *Perm)

{
  double dummy = 0;
  cholmod_sparse Amatrix;

  /*
   * start CHOLMOD and set parameters
   */

  cholmod_start (&(Ch->obj->Common)) ;
  nsp_sputil_config (SPUMONI, &(Ch->obj->Common),FALSE) ;
  /* In nsp solve should return complex */
  Ch->obj->Common.prefer_zomplex = FALSE ;
  
  if (stype != 0 && Sp->m != Sp->n ) 
    {
      /* matrix must be square except when factorization of A*A' or A'*A is done */
      Scierror("Error: matrix must be square\n");
      return FAIL;
    }
  
  Ch->obj->m =  Sp->m;
  Ch->obj->n =  Sp->n;

  /* convert to packed LDL' when done */
  Ch->obj->Common.final_asis = FALSE ;
  Ch->obj->Common.final_super = FALSE ;
  /* Choose here between ll or ldl' */
  Ch->obj->Common.final_ll = ll ;
  Ch->obj->Common.final_pack = TRUE ;
  Ch->obj->Common.final_monotonic = TRUE ;

  if (  nsp_cholmod_set_ordering(ordering,Perm,&Ch->obj->Common) == FAIL) 
    return FAIL;
  /* since numerically zero entries are NOT dropped from the symbolic
   * pattern, we DO need to drop entries that result from supernodal
   * amalgamation. 
   */
  Ch->obj->Common.final_resymbol = TRUE ;
  Ch->obj->Common.quick_return_if_not_posdef = quick_return_if_not_posdef;
  
  /* This will disable the supernodal LL', which will be slow. */
  /* Ch->objd->Common.supernodal = CHOLMOD_SIMPLICIAL ; */

  if ( nsp_spcol_to_cholmod_sparse(Sp,&Amatrix, &dummy,stype,transpose)== FAIL) 
    return FAIL;
  /*
   * analyze and factorize 
   */
  /* Ch->obj->L = cholmod_analyze (&Amatrix, &Ch->obj->Common);*/
  Ch->obj->L = cholmod_analyze_p (&Amatrix,(Perm==NULL) ? NULL: Perm->I,NULL,0,&Ch->obj->Common ) ;

  cholmod_factorize_p (&Amatrix, beta, NULL, 0, Ch->obj->L, &Ch->obj->Common);
  /* 
   */
  nsp_cholmod_sparse_free( &Amatrix);
  return OK;
}

/**
 * nsp_matrix_to_cholmod_dense:
 * @A: a #NspMatrix 
 * @B: a chomod_dense pointer 
 * @dummy: a pointer to a valid scalar double 
 * 
 * fills the chomod_dense matrix @B with data from the #NspMatrix @A without copy.
 * Note that after the call @B and @A share common data. Thus @B should 
 * not be modified or freed. 
 *
 **/

static void nsp_matrix_to_cholmod_dense(NspMatrix *A, cholmod_dense *B,double *dummy) 
{
  B->nrow = A->m;
  B->ncol = A->n;
  B->d = A->m;
  B->nzmax = A->mn;
  B->dtype = CHOLMOD_DOUBLE ;
  if ( A->m == 0 || A->n == 0)
    {
      B->x = dummy ;
      B->z = dummy ;
      return;
    }
  /* A->R points to complex or real */
  B->x = A->R;
  B->xtype = ( A->rc_type == 'c') ? CHOLMOD_COMPLEX : CHOLMOD_REAL;
}


/**
 * nsp_cholmod_dense_to_matrix:
 * @Ahandle: a handler to a cholmod_dense matrix 
 * @cm: a cholmod_common
 * 
 * returns a #NspMatrix from a cholmod_dense. The array of values 
 * is nor copied but moved from one struture to the other and then 
 * the cholmod_dense object is freed. 
 * XXXX to be tested with complex 
 * 
 * Returns: a new #NspMatrix 
 **/

static NspMatrix *nsp_cholmod_dense_to_matrix(cholmod_dense **Ahandle,cholmod_common *cm)
{
  NspMatrix *A;
  cholmod_dense *Ac =*Ahandle;
  if ( Ac->xtype == CHOLMOD_ZOMPLEX ) 
    {
      Scierror("Error: nsp_cholmod_dense_to_matrix encountered a ZOMPLEX Matrix\n");
      return NULL;
    }
  if ((A = nsp_matrix_create(NVOID,(Ac->xtype == CHOLMOD_REAL) ? 'r' : 'c', 0,0)) == NULLMAT )
    return A;
  /* we use the allocated Ahandle to fill A */
  A->m=Ac->nrow;
  A->n=Ac->ncol;
  A->mn = A->m*A->n;
  A->R = Ac->x;
  /* free the cholmod matrix protecting copied data */
  Ac->x = NULL ;
  Ac->z = NULL ; 
  cholmod_free_dense (Ahandle, cm) ;
  return (A) ;
}

/**
 * nsp_spcol_to_cholmod_sparse:
 * @A: a #NspSpColMatrix (unmodified)
 * @B: a pointer to a cholmod_sparse
 * @dummy: a pointer to a valid double 
 * @stype: an integer -1: lower, 0: unsymmetric, 1: upper 
 * @transpose: %TRUE or %FALSE
 * 
 * fills a cholmod_sparse structure given in @B with data from #NspSpColMatrix @A.
 * If @A do not contain a Matlab triplet description the Matlab triplet is first created.
 * if @transpose is %TRUE the Matlab triplet description of the transpose of @A is first created.
 * (Thus note that transpose is only active if @A do not contain a Matlab triplet description).
 * Then the triplet pointers are moved to @B. 
 * The resulting matrix @B can be modified and should free the allocated triplet 
 * since it is not referenced in @A after the call.
 * Note that after the call @A is unchanged. 
 * 
 * 
 * Returns: %OK or %FAIL
 **/

static int nsp_spcol_to_cholmod_sparse(NspSpColMatrix *A, cholmod_sparse *B,double *dummy,int stype, int transpose)
{
  int *Ap ;
  /* be sure that A contains a Matlab triplet */
  if (A->convert != 't' ) 
    {
      if ( transpose == FALSE )
	{
	  if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) 
	    return FAIL;
	  B->nrow = A->m;
	  B->ncol = A->n;
	}
      else 
	{
	  if ( nsp_sprow_set_triplet_from_m((NspSpRowMatrix *)A,TRUE)==FAIL) 
	    return FAIL;
	  B->nrow = A->n;
	  B->ncol = A->m;
	}
    }
  B->p = A->triplet.Jc;
  B->i = A->triplet.Ir;
  Ap = B->p ;
  B->nzmax = Ap [B->ncol] ;
  B->packed = TRUE ;
  B->sorted = TRUE ;
  B->nz = NULL ;
  B->itype = CHOLMOD_INT ;
  B->dtype = CHOLMOD_DOUBLE ;
  B->stype = stype ;
  B->xtype = ( A->rc_type == 'c') ? CHOLMOD_ZOMPLEX : CHOLMOD_REAL ;
  if ( A->m == 0 || A->n == 0 ) 
    {
      /* this is not dereferenced, but the existence (non-NULL) of these
       * pointers is checked in CHOLMOD */
      B->x = dummy ;
      B->z = dummy ;
    }
  else 
    {
      B->x = A->triplet.Pr;
      B->z = A->triplet.Pi;
    }
  /* Be sure now that A won't free its triplet */
  A->convert = 'v';
  A->triplet.Jc=NULL;
  A->triplet.Ir=NULL;
  A->triplet.Pr=NULL;
  A->triplet.Pi=NULL;
  return OK;
}

/**
 * nsp_cholmod_sparse_free:
 * @B: a cholmod_sparse
 * 
 * free data stored in a cholmod_sparse object.
 * the cholmod_sparse object itself is not freed. 
 * This is usefull to free a cholmod_sparse allocated 
 * by #nsp_spcol_to_cholmod_sparse
 **/

static void nsp_cholmod_sparse_free(cholmod_sparse *B)
{
  FREE(B->p);
  FREE(B->i);
  if ( B->nrow != 0 && B->ncol != 0 ) 
    {
      FREE(B->x);
      FREE(B->z);
    }
}


/**
 * nsp_cholmod_to_spcol_sparse:
 * @Ahandle: handle to a cholmod_sparse object 
 * @cm: a cholmod_common 
 * 
 * creates a new #NspSpColMatrix using data from a cholmod_sparse 
 * object which is freed in this routine.
 * 
 * Returns: a new #NspSpColMatrix or %NULL
 **/
 
static NspSpColMatrix * nsp_cholmod_to_spcol_sparse(cholmod_sparse **Ahandle, cholmod_common *cm)
{
  NspSpColMatrix *An;
  cholmod_sparse *A= *Ahandle;
  char rc = (A->xtype != CHOLMOD_REAL) ? 'c' : 'r' ;
  if ((An = nsp_spcolmatrix_create(NVOID,rc,0,0) ) == NULLSPCOL) 
    return NULLSPCOL;
  An->convert = 't' ;
  An->triplet.m = A->nrow;
  An->triplet.n = A->ncol;
  An->triplet.Aisize =  A->nzmax; 
  An->triplet.Jc =  A->p;
  An->triplet.Ir =  A->i;
  An->triplet.Pr =  A->x;
  An->triplet.Pi =  NULL;
  if (A->xtype != CHOLMOD_REAL)
    {
      An->triplet.Pi =  A->z;
    }
  /* now we can free A protecting moved array */
  A->p = NULL ;
  A->i = NULL ;
  A->x = NULL ;
  A->z = NULL ;
  cholmod_free_sparse (Ahandle, cm) ;
  /* be sure that A is nsp back converted */
  if ( nsp_spcol_update_from_triplet(An) == FAIL) 
    return NULL;
  return (An) ;
}
  

/**
 * nsp_matrix_from_int:
 * @P: a pointer to an int array
 * @n: size of the array
 * @one_based: offset added to each element of the array @P
 * 
 * Convert an int vector into a #NspMatrix
 * 
 * Returns: a new #NspMatrix or %NULL
 **/

static NspMatrix *nsp_matrix_from_int(const int *P,int n, int one_based)
{
  NspMatrix *A;
  int i ;
  if (( A= nsp_matrix_create(NVOID,'r',1,n)) == NULLMAT )
    return NULLMAT;
  for (i = 0 ; i < n ; i++) A->R[i]= (P[i] + one_based) ;
  return A;
}


/* from cholmod_matlab 
 * CHOLMOD/MATLAB Module.  Version 1.2.  Copyright (C) 2005-2006,
 * Timothy A. Davis
 */ 

static int nsp_cholmod_set_ordering(int ordering,NspMatrix *Perm, cholmod_common *cm)
{
  if (ordering == 0)
    {
      /* natural ordering */
      cm->nmethods = 1 ;
      cm->method [0].ordering = CHOLMOD_NATURAL ;
      cm->postorder = FALSE ;
    }
  else if (ordering == -1)
    {
      /* default strategy ... nothing to change */
      cm->nmethods = 0;
    }
  else if (ordering == -2)
    {
      /* default strategy, but with NESDIS in place of METIS */
      cm->default_nesdis = TRUE ;
    }
  else if (ordering == -3)
    {
      /* use AMD only */
      cm->nmethods = 1 ;
      cm->method [0].ordering = CHOLMOD_AMD ;
      cm->postorder = TRUE ;
    }
  else if (ordering == -4)
    {
      /* use METIS only */
      cm->nmethods = 1 ;
      cm->method [0].ordering = CHOLMOD_METIS ;
      cm->postorder = TRUE ;
    }
  else if (ordering == -5)
    {
      /* use NESDIS only */
      cm->nmethods = 1 ;
      cm->method [0].ordering = CHOLMOD_NESDIS ;
      cm->postorder = TRUE ;
    }
  else if (ordering == -6)
    {
      /* natural ordering, but with etree postordering */
      cm->nmethods = 1 ;
      cm->method [0].ordering = CHOLMOD_NATURAL ;
      cm->postorder = TRUE ;
    }
  else if ( Perm != NULL)
    {
      if (!cholmod_check_perm (Perm->I, Perm->mn,Perm->mn, cm))
	{
	  Scierror("Error: invalid input permutation\n") ;
	  return FAIL;
	}
      /* use only the given permutation */
      cm->nmethods = 1 ;
      cm->method [0].ordering = CHOLMOD_GIVEN ;
      cm->postorder = FALSE ;
    }
  else
    {
      Scierror ("Error: invalid ordering option\n") ;
      return FAIL;
    }
  return OK;
}



static cholmod_sparse *get_cholmod_pattern_from_matrix(NspMatrix *A,int transpose, cholmod_common *cm)
{
  cholmod_sparse *Ac;
  double dummy;
  cholmod_dense Bmatrix;
  /* Bmatrix should not be freed */
  nsp_matrix_to_cholmod_dense(A,&Bmatrix,&dummy) ;
  /* use FALSE to get a pattern matrix i.e spones (sparse(B))*/
  Ac = cholmod_dense_to_sparse (&Bmatrix, FALSE, cm) ;
  if ( transpose) 
    {
      cholmod_sparse *At;
      At = cholmod_transpose (Ac, 0, cm) ;
      cholmod_free_sparse (&Ac,cm);
      Ac = At;
    }
  return Ac;
} 

static cholmod_sparse *get_cholmod_pattern_from_bmatrix(NspBMatrix *B,int transpose, cholmod_common *cm)
{
  cholmod_sparse *Ac;
  NspMatrix *A=nsp_bmatrix_to_matrix(B);
  if ( A == NULL) return NULL;
  Ac=get_cholmod_pattern_from_matrix(A, transpose,cm);
  nsp_matrix_destroy(A);
  return Ac;
}

static int get_cholmod_pattern_from_spcol(NspSpColMatrix *A, cholmod_sparse *B,double *dummy,int stype,int transpose)
{
  if ( nsp_spcol_to_cholmod_sparse(A,B,dummy,stype,transpose) == FAIL) return FAIL;
  if ( B->nrow != 0 && B->ncol != 0 ) 
    {
      FREE(B->x);
      FREE(B->z);
    }
  B->xtype = CHOLMOD_PATTERN ;
  return OK;
}

/* Create a CHOLMOD_PATTERN sparse matrix for a nsp matrix 
 * NspMatrix or NspBMatrix or NspSpColMatrix 
 *
 * A shallow copy or duplicate is returned; the shallow copy must not be freed.
 * For a shallow copy, the return value A is the same as Ashallow.  For a
 * complete duplicate, A and Ashallow will differ.
 * A shallow copy is returned for NspSpColMatrix argument 
 */

static cholmod_sparse *cholmod_pattern_from_object(NspObject *Obj,cholmod_sparse *Ashallow,double *dummy,int transpose, cholmod_common *cm )
{
  if ( IsMat(Obj)) 
    return get_cholmod_pattern_from_matrix((NspMatrix *) Obj,transpose,cm);
  else if ( IsBMat(Obj)) 
    return get_cholmod_pattern_from_bmatrix((NspBMatrix *) Obj,transpose,cm);
  else if ( IsSpColMat(Obj))
    {
      if ( get_cholmod_pattern_from_spcol((NspSpColMatrix *) Obj,Ashallow,dummy,0, transpose)==FAIL) return NULL;
      return Ashallow;
    }
  else
    {
      Scierror("Error: argument has wrong type matrix, bmatrix or sparse expected\n");
    }
  return NULL;
}


/* from cholmod_matlab 
 * CHOLMOD/MATLAB Module.  Version 1.2.  Copyright (C) 2005-2006,
 * Timothy A. Davis
 */ 

static void nsp_sputil_error_handler_mex (int status, char *file, int line, char *message);
static void nsp_sputil_error_handler (int status, char *file, int line, char *message);

static void nsp_sputil_config (int spumoni, cholmod_common *cm, int in_mex )
{
  /* cholmod_solve must return a real or zomplex X for MATLAB */
  cm->prefer_zomplex = TRUE ;

  /* use mxMalloc and related memory management routines */
  cm->malloc_memory  = mxMalloc ;
  cm->free_memory    = mxFree ;
  cm->realloc_memory = mxRealloc ;
  cm->calloc_memory  = mxCalloc ;

  /* printing and error handling */
  if (spumoni == 0)
    {
      /* do not print anything from within CHOLMOD */
      cm->print = -1 ;
      cm->print_function = NULL ;
    }
  else
    {
      /* spumoni = 1: print warning and error messages.  cholmod_print_*
       *	routines will print a one-line summary of each object printed.
       * spumoni = 2: also print a short summary of each object.
       */
      cm->print = spumoni + 2 ;
      cm->print_function = mexPrintf ;
    }

  /* error handler */
  if ( in_mex == TRUE ) 
    cm->error_handler  = nsp_sputil_error_handler_mex ;
  else
    cm->error_handler  = nsp_sputil_error_handler ;

  /* complex arithmetic */
  cm->complex_divide = cholmod_divcomplex ;
  cm->hypotenuse     = cholmod_hypot ;

#ifndef NPARTITION
#if defined(METIS_VERSION)
#if (METIS_VERSION >= METIS_VER(4,0,2))
  /* METIS 4.0.2 uses function pointers for malloc and free */
  METIS_malloc = cm->malloc_memory ;
  METIS_free   = cm->free_memory ;
#endif
#endif
#endif

  /* Turn off METIS memory guard.  It is not needed, because mxMalloc will
   * safely terminate the mexFunction and free any workspace without killing
   * all of MATLAB.  This assumes cholmod_make was used to compile CHOLMOD
   * for MATLAB. */
  cm->metis_memory = 0.0 ;
}


static void nsp_sputil_error_handler_mex (int status, char *file, int line, char *message)
{
  if (status < CHOLMOD_OK)
    {
      /* "ERROR: file %s line %d, status %d\n", file, line, status) ;
       */
      mexErrMsgTxt (message) ;
    }
  else
    {
      /* mexPrintf ("Warning: file %s line %d, status %d\n", file, line, status); */
    }
}

static void nsp_sputil_error_handler (int status, char *file, int line, char *message)
{
  if (status < CHOLMOD_OK)
    {
      /* "ERROR: file %s line %d, status %d\n", file, line, status) ;
       */
      mexErrMsgTxt (message) ;
    }
  else
    {
      /* mexPrintf ("Warning: file %s line %d, status %d\n", file, line, status); */
    }
}

#endif /* WITH_CHOLMOD */
