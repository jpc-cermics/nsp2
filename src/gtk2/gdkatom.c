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

#include "nsp/object.h"
#include <gtk/gtk.h>
#define  GdkAtom_Private 
#include "nsp/gtk/gdkatom.h"
#include "nsp/interf.h"

/* 
 * NspGdkAtom inherits from NspObject
 */

int nsp_type_gdkatom_id=0;
NspTypeGdkAtom *nsp_type_gdkatom=NULL;

/*
 * Type object for GdkAtom 
 * all the instance of NspTypeGdkAtom share the same id. 
 * nsp_type_gdkatom: is a an instance of NspTypeGdkAtom 
 *    used for objects of NspGdkAtom type (i.e built with new_gdkatom) 
 * other instances are used for derived classes 
 */

NspTypeGdkAtom *new_type_gdkatom(type_mode mode)
{
  NspTypeGdkAtom *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkatom != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkatom;
    }
  
  if ((type =  malloc(sizeof(NspTypeGdkAtom))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; 
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = gdkatom_get_methods; 
  type->new = (new_func *) new_gdkatom;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkatom */ 
  
  top->pr = (print_func *) gdkatom_print;                    
  top->dealloc = (dealloc_func *) gdkatom_destroy;
  top->copy  =  (copy_func *) gdkatom_copy;                   
  top->size  = (size_func *) gdkatom_size;                  
  top->s_type =  (s_type_func *) gdkatom_type_as_string;    
  top->sh_type = (sh_type_func *) gdkatom_type_short_string;
  top->info = (info_func *) gdkatom_info ;                    
  /* top->is_true = (is_true_func  *) GdkAtomIsTrue; */
  /* top->loop =(loop_func *) gdkatom_loop;*/
  top->path_extract = (path_func *) gdkatom_path_extract ; 
  top->get_from_obj = (get_from_obj_func *) gdkatom_object;
  top->eq  = (eq_func *) gdkatom_eq;
  top->neq  = (eq_func *) gdkatom_neq;
  top->save  = (save_func *) gdkatom_xdr_save;
  top->load  = (load_func *) gdkatom_xdr_load;

  /* specific methods for gdkatom */
      
  type->init = (init_func *) init_gdkatom;
      
  /* 
   * GdkAtom interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_gdkatom_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkAtom called nsp_type_gdkatom
       */
      type->id =  nsp_type_gdkatom_id = nsp_new_type_id();
      nsp_type_gdkatom = type;
      if ( nsp_register_type(nsp_type_gdkatom) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gdkatom(mode);
    }
  else 
    return type;
}

/*
 * initialize GdkAtom instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkatom(NspGdkAtom *o,NspTypeGdkAtom *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  return OK;
}

/*
 * new instance of GdkAtom 
 */

NspGdkAtom *new_gdkatom() 
{
  NspGdkAtom *loc; 
  /* type must exists */
  nsp_type_gdkatom = new_type_gdkatom(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkAtom)))== NULLGDKATOM) return loc;
  /* initialize object */
  if ( init_gdkatom(loc,nsp_type_gdkatom) == FAIL) return NULLGDKATOM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GdkAtom 
 *-----------------------------------------------*/

/*
 * size 
 */

static int gdkatom_size(NspGdkAtom *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char gdkatom_type_name[]="GdkAtom";
static char gdkatom_short_type_name[]="gdkatom";

static char *gdkatom_type_as_string(void)
{
  return(gdkatom_type_name);
}

static char *gdkatom_type_short_string(void)
{
  return(gdkatom_short_type_name);
}

/*
 * A == B 
 */

static int gdkatom_eq(NspGdkAtom *A, NspObject *B)
{
  if ( check_cast(B,nsp_type_gdkatom_id) == FALSE) return FALSE ;
  if (A->atom == ((NspGdkAtom *) B)->atom) return TRUE; 
  return FALSE;
}

/*
 * A != B 
 */

static int gdkatom_neq(NspGdkAtom *A, NspObject *B)
{
  if ( check_cast(B,nsp_type_gdkatom_id) == FALSE) return TRUE;
  if (A->atom == ((NspGdkAtom *) B)->atom) return FALSE; 
  return TRUE;
}

/* used for evaluation of H(exp1) in exps like H(exp1)(exp2)....(expn)= val 
 * note that H(exp1)= val          -> setrowscols
 *       and H(exp1)(.....) = val  -> pathextract(H,exp1) and then 
 *       iterate on the result 
 */

static NspObject *gdkatom_path_extract(NspGdkAtom *a, NspObject *ob)
{
  char *str;
  if ((str=nsp_string_object(ob)) == NULL ) return NULLOBJ;
  return nsp_get_attribute_object((NspObject *) a,((NspObject *)a)->basetype,str) ;
}

/*
 * save 
 */

static int gdkatom_xdr_save(NspFile  *F, NspGdkAtom *M)
{
  if (nsp_xdr_save_i(F,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(F, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("gdkatom_xdr_save: to be implemented \n");
  return OK;
}

/*
 * load 
 */

static NspGdkAtom  *gdkatom_xdr_load(NspFile  *F)
{
  NspGdkAtom *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(F,name,NAME_MAXL) == FAIL) return NULLGDKATOM;
  Scierror("gdkatom_xdr_load: to be implemented \n");
  return M;
}

/*
 * delete 
 */

void gdkatom_destroy(NspGdkAtom *H)
{
  if ( H->name) g_free(H->name);
  FREE(NSP_OBJECT(H)->name);
  FREE(H);
}

/*
 * info 
 */

void gdkatom_info(NspGdkAtom *self, int indent)
{
  char *a_name;
  int i;
  if ( self == NULLGDKATOM) 
    {
      Sciprintf("Null Pointer GdkAtom \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  a_name = gdk_atom_name(self->atom);
  Sciprintf("%s\t= GdkAtom name=%s 0x%lx ]\n", NSP_OBJECT(self)->name,
	    a_name ? a_name : "",
	    (unsigned long)self->atom);
  g_free(a_name);
}

/*
 * print 
 */

void gdkatom_print(NspGdkAtom *H, int indent)
{
  gdkatom_info(H,indent);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkAtom objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkAtom   *gdkatom_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast(O,nsp_type_gdkatom_id) == TRUE) return ((NspGdkAtom *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_gdkatom));
  return(NULL);
}

int IsGdkAtomObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkatom_id);
}

int IsGdkAtom(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkatom_id);
}

NspGdkAtom  *GetGdkAtomCopy(Stack stack, int i)
{
  if (  GetGdkAtom(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkAtom  *GetGdkAtom(Stack stack, int i)
{
  NspGdkAtom *M;
  if (( M = gdkatom_object(NthObj(i))) == NULLGDKATOM)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassA instance 
 *-----------------------------------------------------*/

NspGdkAtom *gdkatom_create(char *name,char *aname,GdkAtom atom,NspTypeBase *type)
{
  NspGdkAtom *H  = (type == NULL) ? new_gdkatom() : type->new();
  if ( H ==  NULLGDKATOM)
    {
      Sciprintf("No more memory\n");
      return NULLGDKATOM;
    }
  if ( ( NSP_OBJECT(H)->name = NewString(name)) == NULLSTRING) return(NULLGDKATOM);
  NSP_OBJECT(H)->ret_pos = -1 ;
  H->name = aname;
  H->atom = atom; 
  return H;
}

/*
 * copy 
 */

NspGdkAtom *gdkatom_copy(NspGdkAtom *H)
{
  return gdkatom_create(NVOID,H->name,H->atom,NULL);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkAtom
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_gdkatom_create(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *ob = NULL;
  CheckRhs(0,1);
  GdkAtom atom;
  if ( rhs == 1) 
    {
      if (( ob =nsp_get_object(stack,1))== NULLOBJ) return RET_BUG; 
    }
  if ( nsp_gdk_atom_from_object(ob, &atom) == FAIL) return RET_BUG; 
  if ((ob= (NspObject *) gdkatom_create(NVOID,NULL,atom,NULL)) == NULL) return RET_BUG;
  MoveObj(stack,1,ob);
  return 1;
} 

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static int nsp_gdkatom_get_name(NspGdkAtom *self, Stack stack,int rhs,int opt,int lhs)
{
  char *a_name;
  a_name = gdk_atom_name(self->atom);
  if ( nsp_move_string(stack,1,a_name ? a_name : "",-1)== FAIL) return RET_BUG;
  g_free(a_name);
  return 1;
}

static NspMethods gdkatom_methods[] = {
  {"get_name",  (nsp_method *)  nsp_gdkatom_get_name},
  { NULL, NULL }
};



static NspMethods *gdkatom_get_methods(void) { return gdkatom_methods;}

/*-------------------------------------------
 * function 
 *-------------------------------------------*/

int int_gdkatom_test(Stack stack, int rhs, int opt, int lhs)
{
  /* test */
  CheckRhs(1,1);
  CheckLhs(1,1);
  NspGdkAtom *a;
  if (( a= GetGdkAtom(stack,1))== NULLGDKATOM) return RET_BUG;
  nsp_object_print((NspObject *) a,0);
  return 0;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab GdkAtom_func[]={
  /* #include "gdkatom-in.nam" */ 
  {"gdkatom_create",int_gdkatom_create}, 
  {"setrowscols_gdkatom",int_set_attribute},
  {"$dot_gdkatom",int_get_attribute},
  {"$set_gdkatom",int_set_attributes},
  {"test_gdkatom",int_gdkatom_test},
  {(char *) 0, NULL}
};

/** call ith function in the GdkAtom interface **/

int GdkAtom_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(GdkAtom_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void GdkAtom_Interf_Info(int i, char **fname, function (**f))
{
  *fname = GdkAtom_func[i].name;
  *f = GdkAtom_func[i].fonc;
}

/*-----------------------------------
 * Utilities 
 *-----------------------------------*/

GdkAtom nsp_gdkatom_get(NspObject *object)
{
  /** Follow pointer **/
  if ( check_cast(object,nsp_type_hobj_id) == TRUE)  object = ((NspHobj *) object)->O;
  return ((NspGdkAtom *) object)->atom;
}

int nsp_gdk_atom_from_object(NspObject *object, GdkAtom *atom)
{
  if (object == NULL) *atom = NULL;
  else if (IsString(object)) *atom =  gdk_atom_intern(nsp_string_object(object), FALSE);
  else if (IsGdkAtom(object)) *atom = nsp_gdkatom_get(object);
  else {
    Scierror("unable to convert argument to GdkAtom");
    return FAIL;
  }
  return OK;
}


