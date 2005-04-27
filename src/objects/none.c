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

#define  None_Private 
#include "nsp/object.h"
#include "nsp/interf.h"

/* 
 * NspNone inherits from NspObject
 */

int nsp_type_none_id=0;
NspTypeNone *nsp_type_none=NULL;

/*
 * Type object for None 
 * all the instance of NspTypeNone share the same id. 
 * nsp_type_none: is a an instance of NspTypeNone 
 *    used for objects of NspNone type (i.e built with new_none) 
 * other instances are used for derived classes 
 */

NspTypeNone *new_type_none(type_mode mode)
{
  NspTypeNone *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_none != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_none;
    }
  
  if ((type =  malloc(sizeof(NspTypeNone))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs =  NULL; /*none_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = none_get_methods; 
  type->new = (new_func *) new_none;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for none */ 
  
  top->pr = (print_func *) none_print;                    
  top->dealloc = (dealloc_func *) none_destroy;
  top->copy  =  (copy_func *) none_copy;                   
  top->size  = (size_func *) none_size;                  
  top->s_type =  (s_type_func *) none_type_as_string;    
  top->sh_type = (sh_type_func *) none_type_short_string;
  top->info = (info_func *) none_info ;                    
  /* top->is_true = (is_true_func  *) NoneIsTrue; */
  /* top->loop =(loop_func *) none_loop;*/
  top->path_extract = (path_func *) none_path_extract ; 
  top->get_from_obj = (get_from_obj_func *) none_object;
  top->eq  = (eq_func *) none_eq;
  top->neq  = (eq_func *) none_neq;
  top->save  = (save_func *) none_xdr_save;
  top->load  = (load_func *) none_xdr_load;

  /* specific methods for none */
      
  type->init = (init_func *) init_none;
      
  /* 
   * None interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_none_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeNone called nsp_type_none
       */
      type->id =  nsp_type_none_id = nsp_new_type_id();
      nsp_type_none = type;
      if ( nsp_register_type(nsp_type_none) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_none(mode);
    }
  else 
    return type;
}

/*
 * initialize None instances 
 * locally and by calling initializer on parent class 
 */

static int init_none(NspNone *o,NspTypeNone *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of None 
 */

NspNone *new_none() 
{
  NspNone *loc; 
  /* type must exists */
  nsp_type_none = new_type_none(T_BASE);
  if ( (loc = malloc(sizeof(NspNone)))== NULLNONE) return loc;
  /* initialize object */
  if ( init_none(loc,nsp_type_none) == FAIL) return NULLNONE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for None 
 *-----------------------------------------------*/

/*
 * size 
 */

static int none_size(NspNone *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char none_type_name[]="None";
static char none_short_type_name[]="none";

static char *none_type_as_string(void)
{
  return(none_type_name);
}

static char *none_type_short_string(void)
{
  return(none_short_type_name);
}

static int none_full_comp(NspNone * A,NspNone * B,char *op,int *err)
{
  Scierror("none_full_comp: to be implemented \n");
  return FALSE;
}

/*
 * A == B 
 */

static int none_eq(NspNone *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_none_id) == FALSE) return FALSE ;
  rep = none_full_comp(A,(NspNone *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

/*
 * A != B 
 */

static int none_neq(NspNone *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_none_id) == FALSE) return TRUE;
  rep = none_full_comp(A,(NspNone *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/* used for evaluation of H(exp1) in exps like H(exp1)(exp2)....(expn)= val 
 * note that H(exp1)= val          -> setrowscols
 *       and H(exp1)(.....) = val  -> pathextract(H,exp1) and then 
 *       iterate on the result 
 */

static NspObject *none_path_extract(NspNone *a, NspObject *ob)
{
  char *str;
  if ((str=nsp_string_object(ob)) == NULL ) return NULLOBJ;
  return nsp_get_attribute_object((NspObject *) a,((NspObject *)a)->basetype,str) ;
}

/*
 * save 
 */

static int none_xdr_save(NspFile  *F, NspNone *M)
{
  if (nsp_xdr_save_i(F->xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(F->xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("none_xdr_save: to be implemented \n");
  return OK;
}

/*
 * load 
 */

static NspNone  *none_xdr_load(NspFile  *F)
{
  NspNone *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(F->xdrs,name,NAME_MAXL) == FAIL) return NULLNONE;
  Scierror("none_xdr_load: to be implemented \n");
  return M;
}

/*
 * delete 
 */

void none_destroy(NspNone *H)
{
  FREE(NSP_OBJECT(H)->name);
  FREE(H);
}

/*
 * info 
 */

void none_info(NspNone *H, int indent)
{
  int i;
  if ( H == NULLNONE) 
    {
      Sciprintf("Null Pointer None \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("[None %s]\n", NSP_OBJECT(H)->name);
}

/*
 * print 
 */

void none_print(NspNone *H, int indent)
{
  none_info(H,indent);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for None objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspNone   *none_object(NspObject *O)
{
  /* Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type **/
  if ( check_cast(O,nsp_type_none_id) == TRUE) return ((NspNone *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",
type_get_name(nsp_type_none));
  return(NULL);
}

int IsNoneObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_none_id);
}

int IsNone(NspObject *O)
{
  return nsp_object_type(O,nsp_type_none_id);
}

NspNone  *GetNoneCopy(Stack stack, int i)
{
  if (  GetNone(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspNone  *GetNone(Stack stack, int i)
{
  NspNone *M;
  if (( M = none_object(NthObj(i))) == NULLNONE)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassA instance 
 *-----------------------------------------------------*/

NspNone *none_create(char *name,NspTypeBase *type)
{
  NspNone *H  = (type == NULL) ? new_none() : type->new();
  if ( H ==  NULLNONE)
    {
      Sciprintf("No more memory\n");
      return NULLNONE;
    }
  if ( ( NSP_OBJECT(H)->name = NewString(name)) == NULLSTRING) return(NULLNONE);
  NSP_OBJECT(H)->ret_pos = -1 ;
  return H;
}

/*
 * copy 
 */

NspNone *none_copy(NspNone *H)
{
  return none_create(NVOID,NULL);
}

/*-------------------------------------------------------------------
 * wrappers for the None
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_none_create(Stack stack, int rhs, int opt, int lhs)
{
  NspNone *H;
  int color=-1,thickness=-1;

  nsp_option opts[] ={{ "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(0,2);
  if ( get_optional_args(stack,rhs,opt,opts,&color,&thickness) == FAIL) return RET_BUG;
  if(( H = none_create(NVOID,NULL)) == NULLNONE) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static NspMethods *none_get_methods(void) { return NULL;};

/*-------------------------------------------
 * function 
 *-------------------------------------------*/

int int_none_test(Stack stack, int rhs, int opt, int lhs)
{
  /* test */
  CheckRhs(1,1);
  CheckLhs(1,1);
  NspNone *a;
  if (( a= GetNone(stack,1))== NULLNONE) return RET_BUG;
 nsp_object_print((NspObject *) a,0);
  return 0;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab None_func[]={
  {"none_create",int_none_create}, 
  {"setrowscols_none",int_set_attribute},
  /* 
     {"$dot_none",int_get_attribute},
     {"$set_none",int_set_attributes},
  */
  {"test_none",int_none_test},
  {(char *) 0, NULL}
};

/* call ith function in the None interface */

int None_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(None_func[i].fonc))(stack,rhs,opt,lhs);
}

/*
 * used to walk through the interface table 
 * (for adding or removing functions) 
 */

void None_Interf_Info(int i, char **fname, function (**f))
{
  *fname = None_func[i].name;
  *f = None_func[i].fonc;
}

