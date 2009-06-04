/* Nsp
 * Copyright (C) 2006-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 * Interface with the umfpack library using a NspUmfpack Object. 
 *
 */

#include <gtk/gtk.h>
#define  Umfpack_Private 
#include "nsp/object.h"
#include "nsp/umfpack.h"
#include "nsp/interf.h"
#include <umfpack.h>
#include "nsp/lapack-c.h"
#include "nsp/spmf.h"


/* 
 * NspUmfpack inherits from NspObject 
 */

int nsp_type_umfpack_id=0;
NspTypeUmfpack *nsp_type_umfpack=NULL;

/*
 * Type object for Umfpack 
 * all the instance of NspTypeUmfpack share the same id. 
 * nsp_type_umfpack: is an instance of NspTypeUmfpack 
 *    used for objects of NspUmfpack type (i.e built with new_umfpack) 
 * other instances are used for derived classes 
 */
NspTypeUmfpack *new_type_umfpack(type_mode mode)
{
  NspTypeUmfpack *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_umfpack != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_umfpack;
    }
  if ((type =  malloc(sizeof(NspTypeUmfpack))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = umfpack_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = umfpack_get_methods; 
  type->new = (new_func *) new_umfpack;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for umfpack */ 

  top->pr = (print_func *) nsp_umfpack_print;                  
  top->dealloc = (dealloc_func *) nsp_umfpack_destroy;
  top->copy  =  (copy_func *) nsp_umfpack_copy;                 
  top->size  = (size_func *) nsp_umfpack_size;                
  top->s_type =  (s_type_func *) nsp_umfpack_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_umfpack_type_short_string;
  top->info = (info_func *) nsp_umfpack_info ;                  
  /* top->is_true = (is_true_func  *) nsp_umfpack_is_true; */
  /* top->loop =(loop_func *) nsp_umfpack_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_umfpack_object;
  top->eq  = (eq_func *) nsp_umfpack_eq;
  top->neq  = (eq_func *) nsp_umfpack_neq;
  top->save  = (save_func *) nsp_umfpack_xdr_save;
  top->load  = (load_func *) nsp_umfpack_xdr_load;
  top->create = (create_func*) int_umfpack_create;
  
  /* specific methods for umfpack */
      
  type->init = (init_func *) init_umfpack;

/* 
 * Umfpack interfaces can be added here 
 * type->interface = (NspTypeBase *) new_type_b();
 * type->interface->interface = (NspTypeBase *) new_type_C()
 * ....
 */
  if ( nsp_type_umfpack_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeUmfpack called nsp_type_umfpack
       */
      type->id =  nsp_type_umfpack_id = nsp_new_type_id();
      nsp_type_umfpack = type;
      if ( nsp_register_type(nsp_type_umfpack) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_umfpack(mode);
    }
  else 
    {
       type->id = nsp_type_umfpack_id;
       return type;
    }
}

/*
 * initialize Umfpack instances 
 * locally and by calling initializer on parent class 
 */

static int init_umfpack(NspUmfpack *o,NspTypeUmfpack *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Umfpack 
 */

NspUmfpack *new_umfpack() 
{
  NspUmfpack *loc; 
  /* type must exists */
  nsp_type_umfpack = new_type_umfpack(T_BASE);
  if ( (loc = malloc(sizeof(NspUmfpack)))== NULLUMFPACK) return loc;
  /* initialize object */
  if ( init_umfpack(loc,nsp_type_umfpack) == FAIL) return NULLUMFPACK;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Umfpack 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_umfpack_size(NspUmfpack *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char umfpack_type_name[]="Umfpack";
static char umfpack_short_type_name[]="umfpack";

static char *nsp_umfpack_type_as_string(void)
{
  return(umfpack_type_name);
}

static char *nsp_umfpack_type_short_string(NspObject *v)
{
  return(umfpack_short_type_name);
}

/*
 * A == B 
 */

static int nsp_umfpack_eq(NspUmfpack *A, NspObject *B)
{
  NspUmfpack *loc = (NspUmfpack *) B;
  if ( check_cast(B,nsp_type_umfpack_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  return FALSE;
}

/*
 * A != B 
 */

static int nsp_umfpack_neq(NspUmfpack *A, NspObject *B)
{
  return ( nsp_umfpack_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

static int nsp_umfpack_xdr_save(XDR *xdrs, NspUmfpack *M)
{
  Sciprintf("Warning: cannot save Umfpack objects (cowardly not saving this object)\n");
  return OK;
}

/*
 * load 
 */

static NspUmfpack  *nsp_umfpack_xdr_load(XDR *xdrs)
{
  NspUmfpack *M = NULL;
  /* should never get there since umfpack object are not saved */
  return M;
}

/*
 * delete 
 */

void nsp_umfpack_destroy(NspUmfpack *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
     void *Numeric = H->obj->data;
     if ( Numeric != NULL) umfpack_di_free_numeric(&Numeric);
     FREE(H->obj->mtlb_T.Jc);
     FREE(H->obj->mtlb_T.Ir);
     FREE(H->obj->mtlb_T.Pr);
     FREE(H->obj->mtlb_T.Pi);
     FREE(H->obj);
   }
  FREE(H);
}

/*
 * info 
 */

int nsp_umfpack_info(NspUmfpack *M, int indent,const char *name, int rec_level)
{
  int i;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if ( M == NULLUMFPACK || M->obj == NULL ) 
    {
      Sciprintf("Null Pointer Umfpack \n");
      return TRUE;
    }
  Sciprintf("%s\t= [...]\t%s %c (%dx%d)\n",pname,nsp_umfpack_type_short_string(NSP_OBJECT(M)),
	    M->obj->rc_type,M->obj->mtlb_T.m,M->obj->mtlb_T.n);
  return TRUE;
}

/*
 * print 
 */

int nsp_umfpack_print(NspUmfpack *Mat, int indent,char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(Mat)->name;
  if (user_pref.pr_as_read_syntax)
    {
      Sciprintf("// Cannot print an umfpack object using as_read=%%t option \n");
    }
  else 
    {
      nsp_umfpack_info(Mat,indent,pname,rec_level);
    }
  return TRUE;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Umfpack objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspUmfpack   *nsp_umfpack_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_umfpack_id) == TRUE ) return ((NspUmfpack *) O);
  else 
    Scierror("Error: Argument should be a %s\n",type_get_name(nsp_type_umfpack));
  return NULL;
}

int IsUmfpackObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_umfpack_id);
}

int IsUmfpack(NspObject *O)
{
  return nsp_object_type(O,nsp_type_umfpack_id);
}

NspUmfpack  *GetUmfpackCopy(Stack stack, int i)
{
  if (  GetUmfpack(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspUmfpack  *GetUmfpack(Stack stack, int i)
{
  NspUmfpack *M;
  if (( M = nsp_umfpack_object(NthObj(i))) == NULLUMFPACK)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspUmfpack *umfpack_create_void(char *name,NspTypeBase *type)
{
 NspUmfpack *H  = (type == NULL) ? new_umfpack() : type->new();
 if ( H ==  NULLUMFPACK)
  {
   Sciprintf("No more memory\n");
   return NULLUMFPACK;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULL)
   return NULLUMFPACK;

 NSP_OBJECT(H)->ret_pos = -1 ;
  H->obj = NULL;
 return H;
}

NspUmfpack *umfpack_create(char *name,char rc_type,char* data,NspTypeBase *type)
{
 NspUmfpack *H  = umfpack_create_void(name,type);
 if ( H ==  NULLUMFPACK) return NULLUMFPACK;
  if ((H->obj = malloc(sizeof(nsp_umfpack))) == NULL) return NULL;
  H->obj->ref_count=1;
  /* XXXXX write_copy not implemented for CharArg  if ((H->obj->data = nsp_string_copy(data)) == NULL) return NULL; */
 return H;
}

/*
 * copy for gobject derived class  
 */

NspUmfpack *nsp_umfpack_copy(NspUmfpack *self)
{
  NspUmfpack *H  =umfpack_create_void(NVOID,(NspTypeBase *) nsp_type_umfpack);
  if ( H ==  NULLUMFPACK) return NULLUMFPACK;
  H->obj = self->obj;
  self->obj->ref_count++;
 return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Umfpack
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static const char *nsp_umfpack_error(int num_error)
{
  switch (num_error)
    {
    case UMFPACK_WARNING_singular_matrix: return "singular matrix";
    case UMFPACK_ERROR_out_of_memory:   return "out of memory";
    case UMFPACK_ERROR_internal_error:  return "internal error";
    case UMFPACK_ERROR_invalid_matrix:  return "invalid matrix";
    default:                            return "unknown error";
    }
}

/**
 * nsp_umfpack_create:
 * @A: a #NspSpCoLMatrix
 * 
 * creates a #NspUmfpack from a sparse matrix. 
 * @A is unchanged. 
 * 
 * Returns: a new #NspUmfpack or %NULLUMFPACK
 **/

static NspUmfpack *nsp_umfpack_create(NspSpColMatrix *A,int flag,int *singular)
{
  double *Control = NULL, *Info = NULL;
  void *Symbolic=NULL, *Numeric=NULL;
  int stat1,stat;
  NspUmfpack *H=NULL;
  *singular =FALSE;
  if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) goto err;
  if ( A->rc_type == 'r' ) 
    {
      stat1 = umfpack_di_symbolic(A->m, A->n, A->triplet.Jc, A->triplet.Ir, A->triplet.Pr, 
				 &Symbolic, Control, Info);
      if ( stat1  != UMFPACK_OK && stat1 != UMFPACK_WARNING_singular_matrix ) goto symb_fact_error;
      stat = umfpack_di_numeric(A->triplet.Jc, A->triplet.Ir, A->triplet.Pr, 
				Symbolic, &Numeric, Control, Info);
      if ( stat  != UMFPACK_OK && stat != UMFPACK_WARNING_singular_matrix ) goto num_fact_error;
    }
  else 
    {
      stat1 = umfpack_zi_symbolic(A->m, A->n, A->triplet.Jc, A->triplet.Ir, A->triplet.Pr,
				 A->triplet.Pi, &Symbolic, Control, Info);
      if ( stat1  != UMFPACK_OK && stat1 != UMFPACK_WARNING_singular_matrix ) goto symb_fact_error;
      stat = umfpack_zi_numeric(A->triplet.Jc, A->triplet.Ir, A->triplet.Pr,
				A->triplet.Pi,Symbolic, &Numeric, Control, Info);
      if ( stat  != UMFPACK_OK && stat != UMFPACK_WARNING_singular_matrix ) goto num_fact_error;
    }

  if ( stat ==  UMFPACK_WARNING_singular_matrix ||  stat1 ==  UMFPACK_WARNING_singular_matrix) 
    {
      if ( flag == TRUE ) 
	*singular =TRUE;
      else 
	Sciprintf("Warning: Matrix is singular\n");
    }
  umfpack_di_free_symbolic(&Symbolic);
  /* now we can store the Numeric part */
  /* want to be sure that type umfpack is initialized */
  nsp_type_umfpack = new_type_umfpack(T_BASE);
  if(( H = umfpack_create_void(NVOID,(NspTypeBase *) nsp_type_umfpack)) == NULLUMFPACK) 
    goto err;
  /* then we use optional arguments to fill attributes */
  if((H->obj = calloc(1,sizeof(nsp_umfpack)))== NULL ) 
    goto err;
  H->obj->ref_count = 1;
  H->obj->data = Numeric;
  H->obj->rc_type = A->rc_type;
  /* update triplet from A and release triplet conversion in A */
  H->obj->mtlb_T = A->triplet;
  A->convert = 'v';
  A->triplet.Jc=NULL;
  A->triplet.Ir=NULL;
  A->triplet.Pr=NULL;
  A->triplet.Pi=NULL;
  return H;
 symb_fact_error: 
  Scierror("Error: symbolic factorization failed in umfpack (%s)\n",
	   nsp_umfpack_error(stat1));
  return NULLUMFPACK;
 num_fact_error:
  Scierror("Error: numeric factorization failed in umfpack (%s)\n",
	   nsp_umfpack_error(stat));
  return NULLUMFPACK;
 err: 
  /* clean stuff here XXXX ? */
  if ( Numeric != NULL) umfpack_di_free_numeric(&Numeric);
  return NULLUMFPACK;
} 


static int int_umfpack_create(Stack stack, int rhs, int opt, int lhs)
{
  int singular=FALSE;
  NspSpColMatrix *A;
  NspUmfpack *H=NULL;
  /* Get a sparse matrix */
  CheckStdRhs(1,1);
  CheckLhs(1,2);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) 
    return RET_BUG;
  if ((H = nsp_umfpack_create(A,(lhs==2),&singular)) == NULLUMFPACK)
    return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(H));
  if ( lhs == 2)
    {
      NspObject *bool;
      if ((bool = nsp_create_boolean_object(NVOID, singular==TRUE))== NULLOBJ) 
	return RET_BUG;
      MoveObj(stack,2,NSP_OBJECT(bool));
    }
  return Max(1,lhs);
} 


/* extract L,U,P,R,Q from a umfpack object */

static int int_umfpack_meth_luget(NspUmfpack *self, Stack stack, int rhs, int opt, int lhs)
{
  void *Numeric;
  NspSpColMatrix *L,*U,*Lt;
  NspMatrix *p,*q,*Rs;
  int lnz, unz, n_row, n_col, n, nz_udiag, i, stat, do_recip;
  char rc_flag;
  int error_flag = 0 ;

  CheckRhs(0,0); 
  CheckLhs(0,5); 
  if ( self->obj == NULL || self->obj->data  == NULL) 
    {
      Scierror("Error: umfpack object is not properly built\n");
      return RET_BUG;
    }
  Numeric= self->obj->data; 
  rc_flag = self->obj->rc_type; 
  if (rc_flag == 'r' )
    umfpack_di_get_lunz(&lnz, &unz, &n_row, &n_col, &nz_udiag, Numeric);
  else
    umfpack_zi_get_lunz(&lnz, &unz, &n_row, &n_col, &nz_udiag, Numeric);
  
  n = Min(n_row,n_col);
  /* prepare space for a n_row x n L matrix
   * take care that we are using Lt i.e nxn_row 
   */
  if ((Lt= nsp_spcolmatrix_create(NVOID,rc_flag,n,n_row))== NULLSPCOL) 
    return RET_BUG;
  /* allocate matlab triplet */
  if ( nsp_spcol_alloc_col_triplet(Lt,lnz)== FAIL) 
    return RET_BUG;
  /* prepare space for a n x n_col R matrix */
  if ((U= nsp_spcolmatrix_create(NVOID,rc_flag,n,n_col))== NULLSPCOL) 
    return RET_BUG;
  /* allocate matlab triplet */
  if ( nsp_spcol_alloc_col_triplet(U,unz)== FAIL) 
    return RET_BUG;

  if ((p =nsp_matrix_create(NVOID,'r',n_row,1))== NULLMAT) return RET_BUG;
  if ((q =nsp_matrix_create(NVOID,'r',n_col,1))== NULLMAT) return RET_BUG;
  if ((Rs =nsp_matrix_create(NVOID,'r',n_row,1))== NULLMAT) return RET_BUG;

  if ( rc_flag == 'r' )
    stat = umfpack_di_get_numeric(Lt->triplet.Jc, Lt->triplet.Ir, Lt->triplet.Pr,
				  U->triplet.Jc, U->triplet.Ir, U->triplet.Pr,
				  p->I, q->I, (double *)NULL, &do_recip, Rs->R, Numeric);
  else
    stat = umfpack_zi_get_numeric(Lt->triplet.Jc, Lt->triplet.Ir, Lt->triplet.Pr,Lt->triplet.Pi,
				  U->triplet.Jc, U->triplet.Ir, U->triplet.Pr, U->triplet.Pi,
				  p->I, q->I, (double *)NULL, (double *)NULL, &do_recip, Rs->R, Numeric);

  if ( stat != UMFPACK_OK ) 
    { 
      error_flag = 2; goto the_end; 
    };
  
  /* If do_recip is TRUE (one), then the scale factors Rs [i] are to be used
   * by multiplying row i by Rs [i].  Otherwise, the entries in row i are to
   * be divided by Rs [i].
   */

  if ( do_recip )
    for ( i = 0 ; i < n_row ; i++ ) Rs->R[i] = 1.0 / Rs->R[i];

  if ( nsp_spcol_update_from_triplet(Lt) == FAIL) 
    return RET_BUG;
  if ( nsp_spcol_update_from_triplet(U) == FAIL) 
    return RET_BUG;
  
  /* Note that Lt is returned in row compressed form 
   * thus we transpose Lt in L to get it in column compressed form.
   * and we conjugate L 
   */ 
  
  if ((L =nsp_spcolmatrix_transpose(Lt)) == NULLSPCOL)  return RET_BUG;
  nsp_spcolmatrix_conj(L);

  nsp_spcolmatrix_destroy(Lt);
  
  /* nsp indices start at 1 */
  for ( i = 0 ; i < n_row ; i++ ) p->I[i]++; 
  for ( i = 0 ; i < n_col ; i++ ) q->I[i]++; 
  /* p and q are in integer mode */
  p->convert = 'i';
  q->convert = 'i';

the_end:
  switch (error_flag)
    {
    case 0:   /* no error */
      MoveObj(stack,1,NSP_OBJECT(L));
      if ( lhs >= 2) MoveObj(stack,2,NSP_OBJECT(U));
      if ( lhs >= 3) MoveObj(stack,3,NSP_OBJECT(p));
      if ( lhs >= 4) MoveObj(stack,4,NSP_OBJECT(q));
      if ( lhs >= 5) MoveObj(stack,5,NSP_OBJECT(Rs));
      return Max(lhs,1);
      break;
    case 1:   /* enought memory (with malloc) */
      Scierror("%s: not enough memory",nsp_umfpack_error(stat));
      break;
    case 2:   /* a problem with one umfpack routine */
      Scierror("%s: %s",NspFname(stack), nsp_umfpack_error(stat));
      break;
    }
  return RET_BUG;
}


/* solve self*x=B
 * B is unchanged 
 */

static NspMatrix * nsp_umfpack_solve(NspUmfpack *self,NspMatrix *B, int mode, int irstep, int flag)
{
  int status=0;
  double Control[UMFPACK_CONTROL], *dControl=NULL;
  double *Info = NULL;
  NspMatrix *Bc=NULL, *X=NULL,*Wi=NULL,*W=NULL,*rep=NULLMAT;
  nsp_sparse_triplet T;
  char rc_type, rc_x;
  void *Numeric=NULL;
  int i;
  
  Numeric= self->obj->data; 
  rc_type = self->obj->rc_type; 
  T = self->obj->mtlb_T;

  if ( T.m != T.n )
    {
      Scierror("Error: the given linear system is not square (%d,%d)\n",T.m,T.n);
      return rep;
    };

  if ( B->m != T.m || B->n < 1 )
    {
      if ( flag == TRUE ) 
	Scierror("Error: first argument of method solve has wrong dimensions (%d,%d)\n", B->m,B->n);
      else 
	Scierror("Error: incompatible arguments, B has wrong dimensions (%d,%d)\n", B->m,B->n);
      return rep;
    };
  if ( B->rc_type  == 'c')
    {
      if ( (B=Bc =nsp_matrix_copy(B)) == NULLMAT) return rep;
      Mat2mtlb_cplx (B);
    }
  /* allocate memory for the solution */
  rc_x = ( rc_type == 'c'   ||  B->rc_type == 'c' ) ? 'c' : 'r' ;
  if ((X =nsp_matrix_create(NVOID,rc_x,T.n,B->n)) == NULLMAT ) goto err;
  /* allocate memory for working arrays */
  if ((Wi =nsp_matrix_create(NVOID,'r',Max(T.m,T.n),1)) == NULLMAT ) goto err;
  if ((W  =nsp_matrix_create(NVOID,'r',((rc_type == 'c') ? 10: 5)*Max(T.m,T.n),1)) == NULLMAT ) 
    goto err;
  if ( rc_type == 'c' &&  B->rc_type  == 'r' )
    {
      /* A is complex -> B is complexified and matlab converted */
      if (nsp_mat_complexify(B,0.00) == FAIL ) goto err;
      Mat2mtlb_cplx (B);
    }
  if (self->obj->rc_type == 'r' )
    {
      if ( irstep != -1 ) 
	{
	  umfpack_di_defaults(Control);
	  Control[UMFPACK_IRSTEP] = irstep;
	  dControl = Control;
	}
      /* Axr = Br */
      for ( i = 0 ; i < B->n ; i++ )
	{
	  status = umfpack_di_wsolve(mode, T.Jc, T.Ir, T.Pr, X->R+i*T.n,B->R+i*B->m,
				     Numeric, dControl, Info, Wi->I, W->R);
	}
      if ( B->rc_type == 'c' )
	{
	  /* Axi= Bi */
	  for ( i = 0 ; i < B->n ; i++ )
	    {
	      status = umfpack_di_wsolve(mode, T.Jc, T.Ir, T.Pr,X->R+i*T.n+T.m*T.n,B->R+i*B->m+B->mn,
					 Numeric, dControl, Info, Wi->I, W->R);
	    }
	  /* X is mtlb converted we have to back convert */
	  X->convert = 'c';
	  Mat2double (X);
	}
    }
  else /* A is complex and B too  */
    {
      if ( irstep != -1 ) 
	{
	  umfpack_zi_defaults(Control);
	  Control[UMFPACK_IRSTEP] = irstep;
	  dControl = Control;
	}
      for ( i = 0 ; i < B->n ; i++ )
	{
	  status = umfpack_zi_wsolve(mode, T.Jc, T.Ir, T.Pr,  T.Pi,
				     X->R+i*T.n, X->R+i*T.n+T.m*T.n, 
				     B->R+i*B->m, B->R+i*B->m+B->mn, Numeric, dControl, Info, Wi->I, W->R);
	}
      /* X is mtlb converted we have to back convert */
      X->convert = 'c';
      Mat2double (X);
    }
  switch (status) 
    {
    case UMFPACK_OK: break;
    case UMFPACK_WARNING_singular_matrix: 
      Sciprintf("Warning: A divide-by-zero occurred. Your solution will contain Inf or Nans\n" 
		"\tSome parts of the solution may be valid.\n");
      break;
    case UMFPACK_ERROR_invalid_system: 
      Scierror("Error: the given linear system is not square (%d,%d)\n",T.m,T.n);
      goto err;
    default: 
      Sciprintf(" Error: umfpack error %s\n", nsp_umfpack_error(Info [UMFPACK_STATUS]));
      goto err;
    }
  rep = X;
 err:
  if ( W != NULL) nsp_matrix_destroy(W);
  if ( Wi != NULL) nsp_matrix_destroy(Wi);
  if ( Bc != NULL) nsp_matrix_destroy(Bc);
  return rep;
}


static int nsp_umfpack_solve_mode( Stack stack,char *mode, int *imode) 
{
  char *types[]={ "A", "At",  "Aat", "Pt_L" ,
		  "L", "Lt_P", "Lat_P", "Lt",
		  "U_Qt" , "U", "Q_Ut" , "Q_Uat",
		  "Ut", "Uat", NULL};
  int modes[]={ UMFPACK_A, UMFPACK_At,  UMFPACK_Aat, UMFPACK_Pt_L ,
		UMFPACK_L, UMFPACK_Lt_P, UMFPACK_Lat_P, UMFPACK_Lt,
		UMFPACK_U_Qt , UMFPACK_U, UMFPACK_Q_Ut , UMFPACK_Q_Uat,
		UMFPACK_Ut, UMFPACK_Uat};
  int rep = is_string_in_array(mode, types,1);
  if ( rep < 0 ) 
    {
      string_not_in_array(stack,mode,types,"optional argument");
      return FAIL;
    }
  *imode = modes[rep];
  return OK ;
}



int int_umfpack_meth_solve(NspUmfpack *self, Stack stack, int rhs, int opt, int lhs)
{
  char *mode = NULL;
  int irstep=-1, imode = UMFPACK_A ;
  NspMatrix *B,*X=NULL;
  nsp_option opts[] ={{"mode",string,NULLOBJ,-1},
		      {"irstep",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckStdRhs(1,1); 
  CheckLhs(1,1);

  if ( get_optional_args(stack,rhs,opt,opts,&mode,&irstep) == FAIL) 
    return RET_BUG;
  /* checks the optional type argument */
  if ( mode != NULL) 
    {
      if ( nsp_umfpack_solve_mode(stack,mode,&imode) == FAIL) return RET_BUG;
    }

  if ( self->obj == NULL || self->obj->data  == NULL) 
    {
      Scierror("Error: umfpack object is not properly built\n");
      return RET_BUG;
    }
  if ((B = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if ((X = nsp_umfpack_solve(self,B, imode , irstep, TRUE))== NULLMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(X));
  return 1;
}

/* Unfinished : in complex cases the triplet must be in nsp-complex mode 
 * if we want X and B to be also in that mode.
 */

int int_umfpack_meth_solve_new(NspUmfpack *self, Stack stack, int rhs, int opt, int lhs)
{
  double *Control = NULL, *Info = NULL;
  NspMatrix *B,*X=NULL,*Wi=NULL,*W=NULL;
  nsp_sparse_triplet T;
  char rc_type, rc_x;
  void *Numeric=NULL;
  int i,rep=RET_BUG;
  
  CheckRhs(1,1); 
  CheckLhs(1,1);

  if ( self->obj == NULL || self->obj->data  == NULL) 
    {
      Scierror("Error: umfpack object is not properly built\n");
      return RET_BUG;
    }
  Numeric= self->obj->data; 
  rc_type = self->obj->rc_type; 
  T = self->obj->mtlb_T;
  if ((B = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  
  if ( B->m != T.m || B->n < 1 )
    {
      Scierror("Error: first argument to method has wrong dimensions (%d,%d)\n",
	       B->m,B->n);
      return RET_BUG;
    };
  /* allocate memory for the solution */
  rc_x = ( rc_type == 'c'   ||  B->rc_type == 'c' ) ? 'c' : 'r' ;
  if ((X =nsp_matrix_create(NVOID,rc_x,B->m,B->n)) == NULLMAT ) return RET_BUG;
  /* allocate memory for working arrays */
  if ((Wi =nsp_matrix_create(NVOID,'r',T.m,1)) == NULLMAT ) goto err;
  if ((W  =nsp_matrix_create(NVOID,'r',((rc_type == 'c') ? 10: 5)*T.m,1)) == NULLMAT ) 
    goto err;

  if ( rc_x == 'c' &&  B->rc_type  == 'r' )
    {
      /* A is complex -> B is complexified */
      if ((B = GetMatCopy(stack,1)) == NULLMAT) return RET_BUG;
      if (nsp_mat_complexify(B,0.00) == FAIL ) return RET_BUG;
    }
  if (self->obj->rc_type == 'r' )
    {
      if ( B->rc_type == 'c' ) Mat2mtlb_cplx (B);
      /* Axr = Br */
      for ( i = 0 ; i < B->n ; i++ )
	umfpack_di_wsolve(UMFPACK_A, T.Jc, T.Ir, T.Pr, X->R+i*B->m,B->R+i*B->m,
			  Numeric, Control, Info, Wi->I, W->R);
      if ( B->rc_type == 'c' )
	{
	  /* Axi= Bi */
	  for ( i = 0 ; i < B->n ; i++ )
	    umfpack_di_wsolve(UMFPACK_A, T.Jc, T.Ir, T.Pr,X->R+i*B->m+B->mn,B->R+i*B->m+B->mn,
			      Numeric, Control, Info, Wi->I, W->R);
	  /* X is mtlb converted we have to back convert */
	  X->convert = 'c';
	  Mat2double (X);
	}
    }
  else 
    {
      /* A is complex and B too (X and B are nsp-complex coded) 
       * The complex matrix should be in nsp-complex coded
       */
      for ( i = 0 ; i < B->n ; i++ )
	umfpack_zi_wsolve(UMFPACK_A, T.Jc, T.Ir, T.Pr,  T.Pi,
			  X->R+i*B->m, NULL,
			  B->R+i*B->m, NULL, Numeric, Control, Info, Wi->I, W->R);
    }
  rep=1;
  MoveObj(stack,1,NSP_OBJECT(X));
 err:
  if ( W != NULL) nsp_matrix_destroy(W);
  if ( Wi != NULL) nsp_matrix_destroy(Wi);
  return rep;
}



static int int_umfpack_meth_isreal(NspUmfpack *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;
  int ret;
  CheckRhs(0,0); 
  CheckLhs(1,1);
  if ( self->obj == NULL || self->obj->data  == NULL) 
    {
      Scierror("Error: umfpack object is not properly built\n");
      return RET_BUG;
    }
  ret = self->obj->rc_type == 'r' ? TRUE : FALSE;
  if ((Obj = nsp_new_boolean_obj(ret))==NULLOBJ) return RET_BUG;
  MoveObj(stack,1,Obj);
  return 1;
}


static int int_umfpack_meth_det(NspUmfpack *self, Stack stack, int rhs, int opt, int lhs)
{
  double *Info = NULL,dx,dz=0.0,*p=NULL,dexp;
  void *Numeric;
  NspObject *Ret1,*Ret2=NULL;
  int status;
  CheckRhs(0,0); 
  CheckLhs(1,2);
  if ( self->obj == NULL || self->obj->data  == NULL) 
    {
      Scierror("Error: umfpack object is not properly built\n");
      return RET_BUG;
    }
  Numeric= self->obj->data; 
  if ( lhs  == 2)
    {
      /* return determinant in the form det * 10^dexp */
      p = &dexp ;
    }
  if ( self->obj->rc_type == 'c' )
    {
      status = umfpack_zi_get_determinant (&dx, &dz, p, Numeric, Info) ;
    }
  else 
    {
      status = umfpack_di_get_determinant (&dx, p, Numeric, Info) ;
    }
  if (status < 0)
    {
      Scierror("Error: method det failed\n");
    }
  if ( self->obj->rc_type == 'c' )
    {
      Ret1 =(NspObject *)  nsp_matrix_create_from_array(NVOID,1,1,&dx,&dz);
    }
  else
    {
      Ret1 = nsp_new_double_obj(dx);
    }
  if ( Ret1 == NULLOBJ ) return RET_BUG;
  if ( lhs  == 2)
    {
      if ((Ret2 = nsp_new_double_obj(dexp))==NULLOBJ) 
	{
	  nsp_object_destroy(&Ret1);
	  return RET_BUG;
	}
    }
  MoveObj(stack,1,Ret1);
  if ( lhs == 2) MoveObj(stack,2,Ret2);
  return Max(lhs,1);
}


/* 
 *  an utility for nsp_umfpack_dlacon and nsp_umfpack_zlacon 
 *  added by Bruno Pincon.
 *  To change if ever we will use the usual complex format for 
 *  nsp_sparse_triplet T (in this case replace nsp_hypot with
 *  nsp_abs_c)
 */
static double nsp_norm1_sptriplet(nsp_sparse_triplet T, char rc_type)
{
  double norm1 = 0.0;
  int j, k;

  if ( rc_type == 'r' )   /* real case */
    for ( j = 0 ; j < T.n ; j++ )
      {
	double norm1j = 0.0;
	for ( k = T.Jc[j] ; k < T.Jc[j+1] ; k++ )
	  norm1j += fabs(T.Pr[k]);
	norm1 = Max(norm1, norm1j);
      }
  else                  /* complex case */
    for ( j = 0 ; j < T.n ; j++ )
      {
	double norm1j = 0.0;
	for ( k = T.Jc[j] ; k < T.Jc[j+1] ; k++ )
	  norm1j += nsp_hypot(T.Pr[k],T.Pi[k]);
	norm1 = Max(norm1, norm1j);
      }
  return norm1;
}


/* 
 *  estimate rcond1 using lapack estimator (real case) 
 *  added by Bruno Pincon
 */
static int nsp_umfpack_dlacon(void *Numeric, nsp_sparse_triplet T, double *Rcond)
{
  double Control[UMFPACK_CONTROL];
  int *iwork=NULL, *isign=NULL, n=T.n, kase, rep;
  double *X=NULL, *Y=NULL, *V=NULL, *rwork=NULL, Anorm1, est;

  /* compute ||A||_1  */
  Anorm1 = nsp_norm1_sptriplet(T, 'r');
  if ( Anorm1 == 0.0 )
    {
      *Rcond = 0.0; return OK;
    }

  /* compute an estimation of ||inv(A)||_1 using lapack dlacon  */
  X = nsp_alloc_work_doubles(n);
  Y = nsp_alloc_work_doubles(n);
  V = nsp_alloc_work_doubles(n);
  isign = nsp_alloc_work_int(n);
  iwork = nsp_alloc_work_int(n);
  rwork = nsp_alloc_work_doubles(n);
  if ( !X || !Y || !V || !isign || !iwork || !rwork ) 
    goto err;
  
  umfpack_di_defaults(Control);
  Control[UMFPACK_IRSTEP] = 0;
  kase = 0;
  do
    {
      C2F(dlacon)(&n, V, X, isign, &est, &kase);

      switch (kase)
	{
	case 1:
	  /* solve Y = A^(-1) X <=> A Y = X */
	  rep = umfpack_di_wsolve(UMFPACK_A, NULL, NULL, NULL, Y, X,
				  Numeric, Control, NULL, iwork, rwork);
	  if ( rep != UMFPACK_OK ) goto errbis;
	  /* X <- Y */
	  memcpy(X, Y, n*sizeof(double));	  
	  break;

	case 2:
	  /* solve Y = A'^(-1) X <=> A' Y = X */
	  rep = umfpack_di_wsolve(UMFPACK_At, NULL, NULL, NULL, Y, X,
				  Numeric, Control, NULL, iwork, rwork);
	  if ( rep != UMFPACK_OK ) goto errbis;
	  /* X <- Y */
	  memcpy(X, Y, n*sizeof(double));	  
	  break;
	}
    }
  while ( kase != 0 );

  FREE(X); FREE(Y); FREE(V); FREE(isign); FREE(iwork); FREE(rwork);
  *Rcond = 1.0/(Anorm1*est);
  return OK;

 errbis:
  FREE(X); FREE(Y); FREE(V); FREE(isign); FREE(iwork); FREE(rwork);
  if ( rep == UMFPACK_WARNING_singular_matrix )
    {
      *Rcond = 0.0; return OK;
    }
  else    /* this should not arise */
    {
      Sciprintf(" Error: unexpected output from umfpack_di_solve: %s\n", nsp_umfpack_error(rep));
      return FAIL;
    }
 err:
  FREE(X); FREE(Y); FREE(V); FREE(isign); FREE(iwork); FREE(rwork);
  return FAIL;
}



/* 
 *  estimate rcond1 using lapack estimator (complex case) 
 *  added by Bruno Pincon
 */
static int nsp_umfpack_zlacon(void *Numeric, nsp_sparse_triplet T, double *Rcond)
{
  double Control[UMFPACK_CONTROL];
  int *iwork=NULL, n=T.n, kase, rep;
  doubleC *X=NULL, *Y=NULL, *V=NULL;
  double *rwork=NULL, Anorm1, est;

  /* compute ||A||_1  */
  Anorm1 = nsp_norm1_sptriplet(T, 'c');
  if ( Anorm1 == 0.0 )
    {
      *Rcond = 0.0; return OK;
    }

  /* compute an estimation of ||inv(A)||_1 using lapack zlacon  */
  X = nsp_alloc_work_doubleC(n);
  Y = nsp_alloc_work_doubleC(n);
  V = nsp_alloc_work_doubleC(n);
  iwork = nsp_alloc_work_int(n);
  rwork = nsp_alloc_work_doubles(4*n);
  if ( !X || !Y || !V || !iwork || !rwork ) 
    goto err;
  
  umfpack_zi_defaults(Control);
  Control[UMFPACK_IRSTEP] = 0;
  kase = 0;
  do
    {
      C2F(zlacon)(&n, V, X, &est, &kase);

      switch (kase)
	{
	case 1:
	  /* solve Y = A^(-1) X <=> A Y = X */
	  rep = umfpack_zi_wsolve(UMFPACK_A, NULL, NULL, NULL, NULL, (double *)Y, NULL,
				  (double *)X, NULL, Numeric, Control, NULL, iwork, rwork);
	  if ( rep != UMFPACK_OK ) goto errbis;
	  /* X <- Y */
	  memcpy(X, Y, n*sizeof(doubleC));	  
	  break;

	case 2:
	  /* solve Y = A'^(-1) X <=> A' Y = X */
	  rep = umfpack_zi_wsolve(UMFPACK_At, NULL, NULL, NULL, NULL, (double *)Y, NULL,
				  (double *)X, NULL, Numeric, Control, NULL, iwork, rwork);
	  if ( rep != UMFPACK_OK ) goto errbis;
	  /* X <- Y */
	  memcpy(X, Y, n*sizeof(doubleC));	  
	  break;
	}
    }
  while ( kase != 0 );

  FREE(X); FREE(Y); FREE(V); FREE(iwork); FREE(rwork);
  *Rcond = 1.0/(Anorm1*est);
  return OK;

 errbis:
  FREE(X); FREE(Y); FREE(V); FREE(iwork); FREE(rwork);
  if ( rep == UMFPACK_WARNING_singular_matrix )
    {
      *Rcond = 0.0; return OK;
    }
  else   /* this should not arise */
    {
      Sciprintf(" Error: unexpected output from umfpack_zi_solve: %s\n", nsp_umfpack_error(rep));
      return FAIL;
    }
 err:
  FREE(X); FREE(Y); FREE(V); FREE(iwork); FREE(rwork);
  return FAIL;
}


/* 
 *  interface for rcond method
 *  added by Bruno Pincon
 */
int int_umfpack_meth_rcond(NspUmfpack *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0); 
  CheckLhs(1,1);
  void *Numeric;
  nsp_sparse_triplet T;
  char rc_type;
  double rcond;

  if ( self->obj == NULL || self->obj->data  == NULL ) 
    {
      Scierror("Error: umfpack object is not properly built\n");
      return RET_BUG;
    }

  Numeric= self->obj->data; 
  rc_type = self->obj->rc_type; 
  T = self->obj->mtlb_T;

  if ( T.m != T.n )
    {
      Scierror("Error: matrix should be square (it is %d x %d)\n", T.m,T.n);
      return RET_BUG;
    };

  if ( rc_type == 'r' )
    {
      if ( nsp_umfpack_dlacon(Numeric, T, &rcond) == FAIL )
	return RET_BUG;
    }
  else
    {
      if ( nsp_umfpack_zlacon(Numeric, T, &rcond) == FAIL )
	return RET_BUG;
    }

  if ( nsp_move_double(stack,1,rcond) == FAIL ) 
    return RET_BUG;

  return 1;
}


static NspMethods umfpack_methods[] = {
  {"solve",(nsp_method *) int_umfpack_meth_solve},
  {"luget",(nsp_method *) int_umfpack_meth_luget},
  {"isreal",(nsp_method *) int_umfpack_meth_isreal},
  {"det",(nsp_method *) int_umfpack_meth_det},
  {"rcond",(nsp_method *) int_umfpack_meth_rcond},
  { NULL, NULL}
};

static NspMethods *umfpack_get_methods(void) { return umfpack_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab umfpack_attrs[] = {
  { NULL,NULL,NULL,NULL, NULL  },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static int int_umfpack_solve(Stack stack, int rhs, int opt, int lhs)
{
  int singular=FALSE;
  int rep = RET_BUG;
  NspSpColMatrix *A;
  NspMatrix *B ,*X=NULL;
  NspUmfpack *H=NULL;

  char *mode = NULL;
  int irstep=-1, imode = UMFPACK_A ;
  nsp_option opts[] ={{"mode",string,NULLOBJ,-1},
		      {"irstep",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  /* Get a sparse matrix */
  CheckStdRhs(2,2);
  CheckLhs(0,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) 
    return RET_BUG;
  if ((B = GetMat(stack,2)) == NULLMAT) 
    return RET_BUG;
  if ((H = nsp_umfpack_create(A,FALSE,&singular)) == NULLUMFPACK)
    return RET_BUG;
  if ( H->obj == NULL || H->obj->data  == NULL) 
    {
      Scierror("Error: umfpack object is not properly built\n");
      return RET_BUG;
    }
  if ( get_optional_args(stack,rhs,opt,opts,&mode,&irstep) == FAIL) 
    return RET_BUG;
  /* checks the optional type argument */
  if ( mode != NULL) 
    {
      if ( nsp_umfpack_solve_mode(stack,mode,&imode) == FAIL) return RET_BUG;
    }
  if ((X = nsp_umfpack_solve(H,B,imode,irstep,FALSE))== NULLMAT) goto err;
  MoveObj(stack,1,NSP_OBJECT(X));
  rep =1;
 err:
  if ( H != NULL) nsp_umfpack_destroy(H);
  return rep;
} 



extern function int_cholmod_analyze;
extern function int_cholmod_chol;
extern function int_cholmod_create;

static OpTab umfpack_func[]={
#ifdef WITH_CHOLMOD
  { "analyze", int_cholmod_analyze}, /* cholmod */
  { "chol_sp", int_cholmod_chol},/* cholmod */
  { "cholmod_create", int_cholmod_create},/* cholmod */
#endif 
  { "umfpack_create", int_umfpack_create},
  { "umfpack_solve",int_umfpack_solve},
  { NULL, NULL}
};

/* call ith function in the umfpack interface */

int umfpack_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(umfpack_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void umfpack_Interf_Info(int i, char **fname, function (**f))
{
  *fname = umfpack_func[i].name;
  *f = umfpack_func[i].fonc;
}


