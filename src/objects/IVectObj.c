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

#define IVect_Private 
#include "nsp/object.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"

/**
 * SECTION:ivect
 * @title: Implicit vectors 
 * @short_description: An object used to store implicit vectors 
 * @see_also: 
 *
 * <para>
 * #NspIVect objects are used to store implicit vectors of double 
 * in a non expanded way.
 * </para>
 **/

/*
 * NspIVect inherits from NspObject 
 */

int nsp_type_ivect_id=0;
NspTypeIVect *nsp_type_ivect=NULL;

NspTypeIVect *new_type_ivect(type_mode mode)
{
  NspTypeIVect *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_ivect != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_ivect;
    }
  if ((type =  malloc(sizeof(NspTypeIVect))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; /*ivect_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = NULL ; /*ivect_get_methods; */
  type->new = (new_func *) new_ivect;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for ivect */ 

  top->pr = (print_func *)nsp_ivect_print;             
  top->dealloc = (dealloc_func *)nsp_ivect_destroy;    
  top->copy  =  (copy_func *)nsp_ivect_copy;           
  top->size  = (size_func *)nsp_ivect_size;            
  top->s_type =  (s_type_func *)nsp_ivect_type_as_string;
  top->sh_type = (sh_type_func *)nsp_ivect_type_short_string;
  top->info = (info_func *)nsp_ivect_info;  
  /* top->is_true = (is_true_func  *)nsp_ivect_is_true; */
  /*top->loop =(loop_func *)nsp_ivect_loop;*/             
  top->path_extract =  NULL;        
  top->get_from_obj = (get_from_obj_func *)nsp_ivect_object; 
  top->eq  = (eq_func *)nsp_ivect_eq;                       
  top->neq  = (eq_func *)nsp_ivect_neq;

  top->save  = (save_func *)nsp_ivect_xdr_save;
  top->load  = (load_func *)nsp_ivect_xdr_load;

  /* specific methods for ivect */
  type->init = (init_func *) init_ivect;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_ivect_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_ivect
       */
      type->id =  nsp_type_ivect_id = nsp_new_type_id();
      nsp_type_ivect = type;
      if ( nsp_register_type(nsp_type_ivect) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_ivect(mode);
    }
  else 
    {
      type->id = nsp_type_ivect_id;
      return type;
    }
}

/*
 * initialize Ivect instances 
 * locally and by calling initializer on parent class 
 */

static int init_ivect(NspIVect *o,NspTypeIVect *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of IVect 
 */

NspIVect *new_ivect() 
{
  NspIVect *loc; 
  /* type must exists */
  nsp_type_ivect = new_type_ivect(T_BASE);
  if ( (loc = malloc(sizeof(NspIVect)))== NULLIVECT) return loc;
  /* initialize object */
  if ( init_ivect(loc,nsp_type_ivect) == FAIL) return NULLIVECT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined 
 *-----------------------------------------------*/

/*
 * size 
 */

static int nsp_ivect_size(NspIVect *Mat, int flag)
{
  return 0;
}

/*
 *type as string  
 */

static char ivect_type_name[]="IVect";
static char ivect_short_type_name[]="iv";

static char *nsp_ivect_type_as_string(void)
{
  return(ivect_type_name);
}

static char *nsp_ivect_type_short_string(NspIVect *V)
{
  return(ivect_short_type_name);
}

static int IVectFullComp(NspIVect * A,NspIVect * B,char *op,int *err)
{
  Scierror("IVectFullComp: to be implemented \n");
  return FALSE;
}

static int nsp_ivect_eq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_ivect_id) == FALSE) return FALSE ;
  rep = IVectFullComp((NspIVect *) A,(NspIVect *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

static int nsp_ivect_neq(NspObject *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_ivect_id) == FALSE) return TRUE;
  rep = IVectFullComp((NspIVect *) A,(NspIVect *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * Save an IVect in a file stream 
 */

static int nsp_ivect_xdr_save(XDR *xdrs, NspIVect *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs,M->first) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs,M->step) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs,M->last) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->flag) == FAIL) return FAIL;
  return OK;
}

/*
 * Load an IVectr from a file stream 
 */

static NspIVect *nsp_ivect_xdr_load(XDR *xdrs)
{
  int flag;
  double first,step,last;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLIVECT;
  if (nsp_xdr_load_d(xdrs,&first) == FAIL) return NULLIVECT;
  if (nsp_xdr_load_d(xdrs,&step) == FAIL) return NULLIVECT;
  if (nsp_xdr_load_d(xdrs,&last) == FAIL) return NULLIVECT;
  if (nsp_xdr_load_i(xdrs,&flag) == FAIL) return NULLIVECT;
  return nsp_ivect_create(name,first,step,last,flag); 
}


/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for BMatrix objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

/*
 * A =nsp_ivect_object(O);
 * checks that O is an object of NspIVect type. 
 * or a Hobj which points to an object of type IVect
 * if so, returns a pointer to that NspIVect and else returns NULL
 */

NspIVect   *nsp_ivect_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_ivect_id) == TRUE) return ((NspIVect *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_ivect));
  return(NULL);
}


/*
 * IsIVectObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  IVect 
 * or a Hobj which points to an object of type IVect
 */

int IsIVectObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_ivect_id);
}

/*
 * IsIVect(O)
 * only checks that object is an object of type  IVect 
 * or a Hobj which points to an object of type IVect
 */

int IsIVect(NspObject *O)
{
  return nsp_object_type(O , nsp_type_ivect_id);
}

/*-------------------------------------------------------------------
 * wrappers for the BMatrix 
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/


/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

/*
 * interface for operator : 
 */

static int int_column(Stack stack, int rhs, int opt, int lhs)
{
  double dfirst,step,last;
  NspIVect *IV;
  CheckRhs(2,3);
  CheckLhs(1,1);
  if (DoubleScalar(NthObj(1),&dfirst) == FAIL) return RET_BUG;
  if ( rhs == 3 ) 
    {
      if (DoubleScalar(NthObj(2),&step) == FAIL) return RET_BUG;
      if (DoubleScalar(NthObj(3),&last) == FAIL) return RET_BUG;
    }
  else
    {
      step = 1.0E0;
      if (DoubleScalar(NthObj(2),&last) == FAIL) return RET_BUG;
    }
  if ((IV =nsp_ivect_create(NVOID,dfirst,step,last,0) ) == NULLIVECT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) IV);
  return 1;
}

/*
 * From Ivect to Matrix 
 */

static int int_iviv2mat(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  NspIVect *HIV;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HIV =nsp_ivect_object(NthObj(1))) == NULLIVECT) return RET_BUG;
  if ((M=nsp_ivect_2_mat( HIV)) == NULLMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) M);
  return 1;
}

/*
 * IVect2LaTeXIVect: writes NspIVect Objet on fd in tex language
 */

static int int_iv_2latexmat(Stack stack, int rhs, int opt, int lhs)
{
  NspIVect *HIV;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HIV =nsp_ivect_object(NthObj(1))) == NULLIVECT) return RET_BUG;
  /* XXXXX A Faire IVect2LaTeXIVect(HIV); **/
  return 1;
}

/*
 *  IVect2LaTeXTab: writes NspIVect Objet on fd in TeX language
 */

static int int_iv_2latextab(Stack stack, int rhs, int opt, int lhs)
{
  NspIVect *HIV;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HIV =nsp_ivect_object(NthObj(1))) == NULLIVECT) return RET_BUG;    
  /* XXX A FAIRE IVect2LaTeXTab(HIV); **/
  return 1;
}

/*
 * The Interface for basic IVect operations 
 */

static OpTab IVect_func[]={
  {"latexmat_iv",int_iv_2latexmat},
  {"latextab_iv",int_iv_2latextab},
  {"column",int_column},
  {"iv2mat",int_iviv2mat},
  {(char *) 0, NULL}
};

int IVect_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(IVect_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) **/

void IVect_Interf_Info(int i, char **fname, function (**f))
{
  *fname = IVect_func[i].name;
  *f = IVect_func[i].fonc;
}




