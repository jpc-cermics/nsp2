/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
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

/**
 * SECTION:gboxed 
 * @title: NspGBoxed
 * @short_description: An object used to store GBoxed objects. 
 * @see_also: 
 *
 * <para>
 * A #NspGBoxed is used to store a G_TYPE_BOXED object. Note 
 * however that a G_TYPE_BOXED object have a most specific type 
 * and for each specific type a Nsp type is defined and inherits from 
 * #NspGBoxed. Thus object of type #NspGBoxed are scarcely created at Nsp level. 
 * One exception is when a NspObject itself is stored in a GBoxed. 
 * 
 * </para>
 **/

#include "nsp/object.h"
#define  GBoxed_Private 
#include "nsp/gtk/gboxed.h"
#include "nsp/interf.h"

/* 
 * NspGBoxed inherits from NspObject
 */

int nsp_type_gboxed_id=0;
NspTypeGBoxed *nsp_type_gboxed=NULL;

/*
 * Type object for GBoxed 
 * all the instance of NspTypeGBoxed share the same id. 
 * nsp_type_gboxed: is a an instance of NspTypeGBoxed 
 *    used for objects of NspGBoxed type (i.e built with new_gboxed) 
 * other instances are used for derived classes 
 */

NspTypeGBoxed *new_type_gboxed(type_mode mode)
{
  NspTypeGBoxed *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gboxed != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gboxed;
    }
  
  if ((type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; 
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = gboxed_get_methods; 
  type->new = (new_func *) new_gboxed;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gboxed */ 
  
  top->pr = (print_func *) gboxed_print;                    
  top->dealloc = (dealloc_func *) gboxed_destroy;
  /* top->size  = (size_func *) gboxed_size; */
  top->copy  =  (copy_func *) gboxed_copy;                   
  top->s_type =  (s_type_func *) gboxed_type_as_string;    
  top->sh_type = (sh_type_func *) gboxed_type_short_string;
  top->info = (info_func *) gboxed_info ;                    
  /* top->is_true = (is_true_func  *) GBoxedIsTrue; */
  /* top->loop =(loop_func *) gboxed_loop;*/
  top->path_extract = (path_func *) object_path_extract ;
  top->get_from_obj = (get_from_obj_func *) gboxed_object;
  top->eq  = (eq_func *) gboxed_eq;
  top->neq  = (eq_func *) gboxed_neq;
  /* top->save  = (save_func *) gboxed_xdr_save;*/
  /* top->load  = (load_func *) gboxed_xdr_load;*/

  /* specific methods for gboxed */
      
  type->init = (init_func *) init_gboxed;
      
  /* 
   * GBoxed interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_gboxed_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGBoxed called nsp_type_gboxed
       */
      type->id =  nsp_type_gboxed_id = nsp_new_type_id();
      nsp_type_gboxed = type;
      if ( nsp_register_type(nsp_type_gboxed) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gboxed(mode);
    }
  else 
    {
      type->id = nsp_type_gboxed_id;
      return type;
    }
}

/*
 * initialize GBoxed instances 
 * locally and by calling initializer on parent class 
 */

static int init_gboxed(NspGBoxed *o,NspTypeGBoxed *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  o->boxed = NULL;
  o->gtype = 0;
  o->free_on_dealloc= TRUE;
  return OK;
}

/*
 * new instance of GBoxed 
 */

NspGBoxed *new_gboxed() 
{
  NspGBoxed *loc; 
  /* type must exists */
  nsp_type_gboxed = new_type_gboxed(T_BASE);
  if ( (loc = malloc(sizeof(NspGBoxed)))== NULLGBOXED) return loc;
  /* initialize object */
  if ( init_gboxed(loc,nsp_type_gboxed) == FAIL) return NULLGBOXED;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GBoxed 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gboxed_type_name[]="GBoxed";
static char gboxed_short_type_name[]="gb";

static char *gboxed_type_as_string(void)
{
  return(gboxed_type_name);
}

static char *gboxed_type_short_string(NspObject *v)
{
  return(gboxed_short_type_name);
}

/*
 * A == B 
 */

static int gboxed_eq(NspGBoxed *A, NspObject *B)
{
  if ( check_cast(B,nsp_type_gboxed_id) == FALSE) return FALSE ;
  if ( A->boxed == ((NspGBoxed *) B)->boxed ) return TRUE;
  return FALSE;
}

/*
 * A != B 
 */

static int gboxed_neq(NspGBoxed *A, NspObject *B)
{
  if ( check_cast(B,nsp_type_gboxed_id) == FALSE) return TRUE;
  if ( A->boxed == ((NspGBoxed *) B)->boxed ) return FALSE;
  return TRUE;
}

/*
 * delete 
 */

void gboxed_destroy(NspGBoxed *self)
{
  /* 
     Scierror("==>destroy boxed %s free=%d at Ox%lx\n",
     g_type_name(self->gtype),
     self->free_on_dealloc,self);
  */
  if (self->free_on_dealloc && self->boxed) {
    nspg_unblock_threads();
    g_boxed_free(self->gtype, self->boxed);
    self->boxed = NULL;
    nspg_block_threads();
  }
  nsp_object_destroy_name(NSP_OBJECT(self));
  FREE(self);
}

/*
 * info 
 */

int gboxed_info(NspGBoxed *self, int indent,char *name,int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(self)->name;
  if ( self == NULLGBOXED) 
    {
      Sciprintf("Null Pointer GBoxed \n");
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
      Sciprintf1(indent,"%s\t= %s GBoxed at 0x%lx\n",pname,
		 g_type_name(self->gtype),
		 (long)self->boxed);
    }
  return TRUE;
}

/*
 * print 
 */

int gboxed_print(NspGBoxed *self, int indent,char *name, int rec_level)
{
  gboxed_info(self,indent,name,rec_level);
  return TRUE;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GBoxed objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGBoxed   *gboxed_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_gboxed_id) == TRUE) return ((NspGBoxed *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_gboxed));
  return(NULL);
}

int IsGBoxedObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gboxed_id);
}

int IsGBoxed(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gboxed_id);
}

NspGBoxed  *GetGBoxedCopy(Stack stack, int i)
{
  if (  GetGBoxed(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGBoxed  *GetGBoxed(Stack stack, int i)
{
  NspGBoxed *M;
  if (( M = gboxed_object(NthObj(i))) == NULLGBOXED)
     ArgMessage(stack,i);
  return M;
}


/**
 * gboxed_create:
 * @name: name of object to be created 
 * @boxed_type: the GType of the boxed value.
 * @boxed: the boxed value.
 * @copy_boxed: whether the new boxed wrapper should hold a copy of the value.
 * @own_ref: whether the boxed wrapper should own the boxed value.
 * @tp: The nsp type of the object to be created. 
 * 
 * Creates a wrapper for a boxed value i.e a Nsp object which contains a GObject as data.
 * If @copy_boxed is set to True, then a copy of the transmited @boxed is done. 
 * If @own_ref is True, then the value held by the wrapper will be freed when the wrapper is deallocated.  If
 * @copy_boxed is True, then @own_ref must also be True. 
 * @tp gives the Nsp type of the object which is to be created. If @tp is %NULL then a #NspBoxed object 
 * is created else @tp can be set to a type which inherits from #NspBoxed. For example if 
 * @tp is et to nsp_type_gdkcolor then a NspGdkColor object will de created.
 * 
 * Returns: a new #NspGBoxed object 
 **/

NspGBoxed *gboxed_create(char *name,GType boxed_type, gpointer boxed, gboolean copy_boxed,
			 gboolean own_ref,void *tp)
{
  NspTypeBase *type = (NspTypeBase *) tp;
  NspGBoxed *self; 

  if ( boxed == NULL) return (NspGBoxed *) nsp_none_create(NVOID,NULL);
    
  self =  (type == NULL) ? new_gboxed() : type->new();
  
  if ( self ==  NULLGBOXED)
    {
      Sciprintf("No more memory\n");
      return NULLGBOXED;
    }
  
  if ( nsp_object_set_initial_name(NSP_OBJECT(self),name) == NULL)
    return(NULLGBOXED);
  NSP_OBJECT(self)->ret_pos = -1 ;
  
  g_return_val_if_fail(boxed_type != 0, NULL);
  g_return_val_if_fail(!copy_boxed || (copy_boxed && own_ref), NULL);
  g_return_val_if_fail(boxed , NULL);
  
  if (copy_boxed) 
    {
      boxed = g_boxed_copy(boxed_type, boxed);
      own_ref = TRUE;
    }
  /* 
  Scierror("==>Create a boxed %s copy=%d own ref=%d at Ox%lx\n",g_type_name(boxed_type), copy_boxed,own_ref,self);
  */
  self->boxed = boxed;
  self->gtype = boxed_type;
  self->free_on_dealloc = own_ref;
  return self;
}

/*
 * copy 
 */

NspGBoxed *gboxed_copy(NspGBoxed *self)
{
  NspTypeBase *type = nsp_type_from_gtype(self->gtype);
  return gboxed_create(NVOID,self->gtype, self->boxed, TRUE, TRUE,type);
}

/*-------------------------------------------------------------------
 * wrappers for the GBoxed
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static NspMethods *gboxed_get_methods(void) { return NULL;};

/*-------------------------------------------
 * function 
 *-------------------------------------------*/

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

/* we create a new boxed type to be able to store a 
 * NspObject in a Gtk typed object 
 */

static NspObject * g_nspobj_copy (NspObject *Obj)
{
  return nsp_object_copy_with_name(Obj);
}

static void g_nspobj_free (NspObject *Obj)
{
  nsp_object_destroy(&Obj);
}


GType gdk_nspobj_get_type (void)
{
  static GType our_type = 0;
  
  if (our_type == 0)
    our_type = g_boxed_type_register_static (g_intern_static_string ("Gnspobj"),
					     (GBoxedCopyFunc) g_nspobj_copy,
					     (GBoxedFreeFunc) g_nspobj_free);
  return our_type;
}

int int_obj2gboxed(Stack stack,int rhs,int opt,int lhs)
{
  NspObject *Obj;
  NspObject *nsp_ret;
  int_types T[] = {obj, t_end} ;
  GType type = gdk_nspobj_get_type ();
  CheckRhs(1,1);
  CheckLhs(0,1);
  if (GetArgs(stack,rhs,opt,T,&Obj)== FAIL)  return RET_BUG;
  nsp_ret = (NspObject *) gboxed_create(NVOID,type,Obj, TRUE, TRUE,NULL);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static OpTab GBoxed_func[]={
  /* #include "gboxed-in.nam" */ 
  {"setrowscols_gb",int_set_attribute},
  {"gboxed",int_obj2gboxed},
  {(char *) 0, NULL}
};

/* call ith function in the GBoxed interface **/

int GBoxed_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(GBoxed_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) **/

void GBoxed_Interf_Info(int i, char **fname, function (**f))
{
  *fname = GBoxed_func[i].name;
  *f = GBoxed_func[i].fonc;
}

/*------------------------------------------
 * Utilities 
 *------------------------------------------*/

int nspg_boxed_check(NspObject *self,GType boxed_type) 
{
  return ((NspGBoxed *) self)->gtype ==  boxed_type;
}
  
