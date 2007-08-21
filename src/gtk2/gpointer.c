/* Nsp
 * Copyright (C) 1998-2007 Jean-Philippe Chancelier Enpc/Cermics
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
#define  GPointer_Private 
#include "nsp/gtk/gpointer.h"
#include "nsp/interf.h"

/* XXXXX : temporaire ici */ 

#include <gtk/gtk.h>

/* 
 * NspGPointer inherits from NspObject
 */

int nsp_type_gpointer_id=0;
NspTypeGPointer *nsp_type_gpointer=NULL;

/*
 * Type object for GPointer 
 * all the instance of NspTypeGPointer share the same id. 
 * nsp_type_gpointer: is a an instance of NspTypeGPointer 
 *    used for objects of NspGPointer type (i.e built with new_gpointer) 
 * other instances are used for derived classes 
 */

NspTypeGPointer *new_type_gpointer(type_mode mode)
{
  NspTypeGPointer *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gpointer != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gpointer;
    }
  
  if ((type =  malloc(sizeof(NspTypeGPointer))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; 
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = gpointer_get_methods; 
  type->new = (new_func *) new_gpointer;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gpointer */ 
  
  top->pr = (print_func *) gpointer_print;                    
  top->dealloc = (dealloc_func *) gpointer_destroy;
  /* top->size  = (size_func *) gpointer_size; */
  top->copy  =  (copy_func *) gpointer_copy;                   
  top->s_type =  (s_type_func *) gpointer_type_as_string;    
  top->sh_type = (sh_type_func *) gpointer_type_short_string;
  top->info = (info_func *) gpointer_info ;                    
  /* top->is_true = (is_true_func  *) GPointerIsTrue; */
  /* top->loop =(loop_func *) gpointer_loop;*/
  top->path_extract = (path_func *) object_path_extract ;
  top->get_from_obj = (get_from_obj_func *) gpointer_object;
  top->eq  = (eq_func *) gpointer_eq;
  top->neq  = (eq_func *) gpointer_neq;
  /* top->save  = (save_func *) gpointer_xdr_save;*/
  /* top->load  = (load_func *) gpointer_xdr_load;*/

  /* specific methods for gpointer */
      
  type->init = (init_func *) init_gpointer;
      
  /* 
   * GPointer interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_gpointer_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGPointer called nsp_type_gpointer
       */
      type->id =  nsp_type_gpointer_id = nsp_new_type_id();
      nsp_type_gpointer = type;
      if ( nsp_register_type(nsp_type_gpointer) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gpointer(mode);
    }
  else 
    {
      type->id = nsp_type_gpointer_id;
      return type;
    }
}

/*
 * initialize GPointer instances 
 * locally and by calling initializer on parent class 
 */

static int init_gpointer(NspGPointer *o,NspTypeGPointer *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  /* init fields */
  o->pointer =NULL;
  o->gtype = 0; /* filled elsewhere */
  return OK;
}

/*
 * new instance of GPointer 
 */

NspGPointer *new_gpointer() 
{
  NspGPointer *loc; 
  /* type must exists */
  nsp_type_gpointer = new_type_gpointer(T_BASE);
  if ( (loc = malloc(sizeof(NspGPointer)))== NULLGPOINTER) return loc;
  /* initialize object */
  if ( init_gpointer(loc,nsp_type_gpointer) == FAIL) return NULLGPOINTER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GPointer 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gpointer_type_name[]="GPointer";
static char gpointer_short_type_name[]="gp";

static char *gpointer_type_as_string(void)
{
  return(gpointer_type_name);
}

static char *gpointer_type_short_string(NspObject *v)
{
  return(gpointer_short_type_name);
}

static int gpointer_full_comp(NspGPointer * A,NspGPointer * B,char *op,int *err)
{
  Scierror("gpointer_full_comp: to be implemented \n");
  return FALSE;
}

/*
 * A == B 
 */

static int gpointer_eq(NspGPointer *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_gpointer_id) == FALSE) return FALSE ;
  rep = gpointer_full_comp(A,(NspGPointer *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

/*
 * A != B 
 */

static int gpointer_neq(NspGPointer *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_gpointer_id) == FALSE) return TRUE;
  rep = gpointer_full_comp(A,(NspGPointer *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * delete 
 */

void gpointer_destroy(NspGPointer *self)
{
  nsp_object_destroy_name(NSP_OBJECT(self));
  FREE(self);
}

/*
 * info 
 */

int gpointer_info(NspGPointer *self, int indent,char *name,int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(self)->name;
  if ( self == NULLGPOINTER) 
    {
      Sciprintf("Null Pointer GPointer \n");
      return TRUE;
    }
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(pname,NVOID) != 0) 
	{
	  Sciprintf1(indent,"%s=unavalaible",pname);
	}
      else 
	{
	  Sciprintf1(indent,"unavailable");
	}
    }
  else 
    {
      Sciprintf1(indent,"%s\t= %s GPointer at 0x%lx ]\n",pname,
		g_type_name(self->gtype),
		(long)self->pointer);
    }
  return TRUE;

}

/*
 * print 
 */

int gpointer_print(NspGPointer *self, int indent,char *name, int rec_level)
{
  gpointer_info(self,indent,name,rec_level);
  return TRUE;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GPointer objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGPointer   *gpointer_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast(O,nsp_type_gpointer_id) == TRUE) return ((NspGPointer *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_gpointer));
  return(NULL);
}

int IsGPointerObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gpointer_id);
}

int IsGPointer(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gpointer_id);
}

NspGPointer  *GetGPointerCopy(Stack stack, int i)
{
  if (  GetGPointer(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGPointer  *GetGPointer(Stack stack, int i)
{
  NspGPointer *M;
  if (( M = gpointer_object(NthObj(i))) == NULLGPOINTER)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 *-----------------------------------------------------*/

NspGPointer *gpointer_create(char *name,GType gtype, gpointer pointer,NspTypeBase *type)
{
  NspGPointer *self =  (type == NULL) ? new_gpointer() : type->new();
  
  if ( self ==  NULLGPOINTER)
    {
      Sciprintf("No more memory\n");
      return NULLGPOINTER;
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(self),name) == NULL)
    return(NULLGPOINTER);
  NSP_OBJECT(self)->ret_pos = -1 ;
  
  g_return_val_if_fail(gtype != 0, NULL);
  g_return_val_if_fail(pointer , NULL);

  self->pointer = pointer;
  self->gtype = gtype;
  return self;
}

/*
 * copy 
 */

NspGPointer *gpointer_copy(NspGPointer *self)
{
  return gpointer_create(NVOID,self->gtype, self->pointer,NULL);
}

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static NspMethods *gpointer_get_methods(void) { return NULL;};

/*-------------------------------------------
 * function 
 *-------------------------------------------*/

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab GPointer_func[]={
  /* #include "gpointer-in.nam" */ 
  {(char *) 0, NULL}
};

/** call ith function in the GPointer interface **/

int GPointer_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(GPointer_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void GPointer_Interf_Info(int i, char **fname, function (**f))
{
  *fname = GPointer_func[i].name;
  *f = GPointer_func[i].fonc;
}

