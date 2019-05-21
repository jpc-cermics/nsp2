/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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

#define  NspGObject_Private
#include <nsp/object.h>
#include <nsp/smatrix.h>
#include <nsp/bmatrix.h>
#include <nsp/hash.h>
#include <nsp/list.h>
#include <nsp/cells.h>
#include <nsp/none.h>
#include <nsp/mpmatrix.h>
#include <nsp/matrix.h>
#include <nsp/file.h>
#include <nsp/type.h>
#include <nsp/hobj.h>
#include <glib.h>
#include <nsp/gvariant.h>
#include <nsp/gtk/gobject.h>
#include <nsp/gtk/gpointer.h>
#include <nsp/gtk/gboxed.h>
#include <nsp/gtk/gsource.h>
#include <nsp/gparamspec.h>

#include <nsp/pr-output.h>
#include <nsp/interf.h>
#include <nsp/matutil.h>
#include <nsp/seval.h>

/* class NspGObject
 * NspGObject inherits from NspObject
 */

static AttrTab gobjects_attrs[];

int nsp_type_gobject_id=0;
NspTypeGObject *nsp_type_gobject=NULL;

NspTypeGObject *new_type_gobject(type_mode mode)
{
  NspTypeGObject *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gobject != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gobject;
    }

  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gobjects_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gobject_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gobject;

  top= NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gobject */

  top->pr = (print_func *) gobject_print;
  top->dealloc = (dealloc_func *) gobject_destroy;
  top->copy  =  (copy_func *) gobject_copy;
  top->size  = (size_func *) gobject_size;
  top->s_type =  (s_type_func *) gobject_type_as_string;
  top->sh_type = (sh_type_func *) gobject_type_short_string;
  top->info = (info_func *) gobject_info ;
  /* top->is_true = (is_true_func  *) NspGObjectIsTrue; */
  /* top->loop =(loop_func *) gobject_loop;*/
  top->path_extract = (path_func *) gobject_path_extract ;
  top->get_from_obj = (get_from_obj_func *) gobject_object;
  top->eq  = (eq_func *) gobject_eq;
  top->neq  = (eq_func *) gobject_neq;
  top->save  = (save_func *) gobject_xdr_save;
  top->load  = (load_func *) gobject_xdr_load;

  /* specific methods for gobject */

  type->init = (init_func *) init_gobject;

  /*
   * interfaces can be added here
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */

  if ( nsp_type_gobject_id == 0 )
    {
      /*
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGObject called nsp_type_gobject
       */
      type->id =  nsp_type_gobject_id = nsp_new_type_id();
      nsp_type_gobject = type;
      if ( nsp_register_type(nsp_type_gobject) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gobject(mode);
    }
  else
    {
      type->id = nsp_type_gobject_id;
      return type;
    }
}

/*
 * initialize Gobject instances
 * locally and by calling initializer on parent class
 */

static int init_gobject(NspGObject *o,NspTypeGObject *type)
{
  /* to be done always */
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type;
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  o->obj = NULL;
  o->closures = NULL;
  return OK;
}

/*
 * new instance of GObject
 */

NspGObject *new_gobject()
{
  NspGObject *loc;
  /* type must exists */
  nsp_type_gobject = new_type_gobject(T_BASE);
  if ( (loc = malloc(sizeof(NspGObject)))== NULLGOBJECT) return loc;
  /* initialize object */
  if ( init_gobject(loc,nsp_type_gobject) == FAIL) return NULLGOBJECT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGObject
 *-----------------------------------------------*/

/*
 * size
 */

static int gobject_size(NspGObject *Mat, int flag)
{
  return 0;
}

/*
 * type as string
 */

static char gobject_type_name[]="GObject";
static char gobject_short_type_name[]="gobj";

static char *gobject_type_as_string(void)
{
  return(gobject_type_name);
}

static char *gobject_type_short_string(NspObject *v)
{
  return(gobject_short_type_name);
}

/*
 * A == B
 */

static int gobject_eq(NspGObject *A, NspObject *B)
{
  if ( check_cast(B,nsp_type_gobject_id) == FALSE) return FALSE ;
  if ( A->obj == ((NspGObject *)B)->obj ) return TRUE;
  return FALSE;
}

/*
 * A != B
 */

static int gobject_neq(NspGObject *A, NspObject *B)
{
  if ( check_cast(B,nsp_type_gobject_id) == FALSE) return TRUE;
  if ( A->obj == ((NspGObject *)B)->obj ) return FALSE;
  return TRUE;
}

/* used for evaluation of H(exp1) in exps like H(exp1)(exp2)....(expn)= val
 * note that H(exp1)= val          -> setrowscols
 *       and H(exp1)(.....) = val  -> pathextract(H,exp1) and then
 *       iterate on the result
 */

static NspObject *gobject_path_extract(NspGObject *a,int n, NspObject **ob, int *copy)
{
  char *str;
  *copy = FALSE;
  if ( n != 1) return NULLOBJ;
  if ((str=nsp_string_object(*ob)) == NULL ) return NULLOBJ;
  return nsp_get_attribute_object((NspObject *) a,((NspObject *)a)->basetype,str,copy) ;
}

/*
 * save
 */

static int gobject_xdr_save(XDR  *xdrs, NspGObject *M)
{
#if 1
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_gobject)) == FAIL) return FAIL;
#else
  if (nsp_xdr_save_i(xdrs, M->type->id) == FAIL)    return FAIL;
#endif
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("gobject_xdr_save: to be implemented \n");
  return OK;
}

/*
 * load
 */

static NspGObject  *gobject_xdr_load(XDR  *xdrs)
{
  NspGObject *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGOBJECT;
  Scierror("gobject_xdr_load: to be implemented \n");
  return M;
}

/*
 * delete
 */

void gobject_destroy(NspGObject *self)
{
  if (self->obj)
    {
      /*
	 fprintf(stderr,"==>gobject_destroy (call unref)\n");
	 nsp_object_print((NspObject *)self,0);
      */
      nspg_block_threads();
      g_object_unref(self->obj);
      nspg_unblock_threads();
    }
  nsp_object_destroy_name(NSP_OBJECT(self));
  FREE(self);
}

/*
 * info
 */

int gobject_info(NspGObject *self, int indent,char *name,int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(self)->name;
  if ( self == NULLGOBJECT)
    {
      Sciprintf("Null Pointer NspGObject \n");
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
      /* A changer XXXX pour que GObject soit remplac� par le type GTk */
      Sciprintf1(indent,"%s\t= %s at 0x%lx ref_count=%d, %s\n", pname,
		 self->obj ? G_OBJECT_TYPE_NAME(self->obj) : "uninitialized",
		 NSP_POINTER_CAST_TO_INT self->obj,
		 ((GObject *) self->obj)->ref_count,
		 nsp_object_type_as_string(NSP_OBJECT(self)));
    }
  return TRUE;
}

/*
 * print
 */

int gobject_print(NspGObject *H, int indent,char *name, int rec_level)
{
  gobject_info(H,indent,name,rec_level);
  return TRUE;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces
 * for NspGObject objects
 * Note that some of these functions could become MACROS XXXXX
 *-----------------------------------------------------*/

NspGObject   *gobject_object(NspObject *O)
{
  /*Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /*Check type **/
  if ( check_cast(O,nsp_type_gobject_id) == TRUE) return ((NspGObject *) O);
  else
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_gobject));
  return(NULL);
}
int IsGObjectObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gobject_id);
}


int IsGObject(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gobject_id);
}

NspGObject  *GetGObjectCopy(Stack stack, int i)
{
  if (  GetGObject(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGObject  *GetGObject(Stack stack, int i)
{
  NspGObject *M;
  if (( M = gobject_object(NthObj(i))) == NULLGOBJECT)
     ArgMessage(stack,i);
  return M;
}

/*
 * constructor for gobject or derived classes
 */

NspGObject *gobject_create(const char *name,  GObject *obj, NspTypeBase *type)
{
  NspGObject *H;
  /* when obj is NULL we return NULL
   * to simplify some automatic interfaces ?
   */
  if ( obj == NULL )
    {
      /* Sciprintf("Error: no gobject available in gobject_create, returning None\n"); */
      return (NspGObject *) nsp_none_create(NVOID,NULL);
    }
  H = (type == NULL) ? new_gobject() : type->new();
  if ( H ==  NULLGOBJECT)
    {
      Sciprintf("No more memory\n");
      return NULLGOBJECT;
    }
  g_object_ref(obj);
  if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULL)
    return(NULLGOBJECT);
  NSP_OBJECT(H)->ret_pos = -1 ;
  H->obj = obj ;
  return H;
}



/*
 * copy a gobject or a derived object (which is copied as a derived object!)
 */

NspGObject *gobject_copy(NspGObject *self)
{
  return gobject_create(NVOID,self->obj,(NspTypeBase *) self->type);
}

/*-------------------------------------------------------------------
 * wrappers for the NspGObject
 * i.e functions at Nsp level
 *-------------------------------------------------------------------*/

/**
 * int_gobj_create:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 * interface that can be used to create a NspGObject given
 * its type and its properties.
 * gobject_create(a-type,hash-table).
 *
 * Returns:
 **/

int int_gobj_create(Stack stack,int rhs,int opt,int lhs)
{
  int rep=RET_BUG;
  NspObject *type;
  NspHash *h =NULL;
  GType object_type;
  guint n_params = 0, i;
  GParameter *params = NULL;
  GObjectClass *class;
  GObject *gobj;
  NspObject *nsp_ret;
  int_types T[] = {obj_check,hash , t_end} ;
  if (GetArgs(stack,rhs,opt,T, &nsp_type_type,&type,&h)== FAIL)
    return RET_BUG;

  object_type  = g_type_from_name (type_get_name(((NspType *) type)->nsp_type));

  if (!object_type)
    {
      Scierror("%s: first argument is not a GType \n", NspFname(stack));
      return RET_BUG;
    }
  if (G_TYPE_IS_ABSTRACT(object_type)) {
    Scierror("%s: cannot create instance of abstract (non-instantiable) type `%s'\n",
	     NspFname(stack),
	     g_type_name(object_type));
    return RET_BUG;
  }
  if ((class = g_type_class_ref (object_type)) == NULL) {
    Scierror("%s: could not get a reference to type class\n",NspFname(stack));
    return RET_BUG;
  }

  if (h != NULL)
    {
      int i;
      params = g_new0(GParameter, h->hsize);
      for ( i =0 ; i < h->hsize ; i++)
	{
	  Hash_Entry *loc = ((Hash_Entry *) h->htable) + i;
	  if ( loc->used )
	    {
	      GParamSpec *pspec;
	      const char *key =nsp_object_get_name(loc->data);
	      pspec = g_object_class_find_property (class, key);
	      if (!pspec) {
		Scierror("gobject `%s' doesn't support property `%s'\n",
			 g_type_name(object_type), key);
		goto cleanup;
	      }
	      g_value_init(&params[n_params].value, G_PARAM_SPEC_VALUE_TYPE(pspec));
	      if (nspg_value_from_nspobject(&params[n_params].value,loc->data)) {
		Scierror("could not convert value for property `%s'\n", key);
		goto cleanup;
	      }
	      params[n_params].name = g_strdup(key);
	      n_params++;
	    }
	}
    }
  if ((gobj = g_object_newv(object_type, n_params, params))== NULL) goto cleanup;
  if ((nsp_ret =(NspObject *) nspgobject_new(NVOID,gobj))== NULL)  goto cleanup;
  MoveObj(stack,1,nsp_ret);
  rep=1;
 cleanup:
  for (i = 0; i < n_params; i++) {
    g_free((gchar *) params[i].name);
    g_value_unset(&params[i].value);
  }
  g_free(params);
  g_type_class_unref(class);
  return rep;
}

/*------------------------------------------------------
 * attributes  (set/get methods)
 *------------------------------------------------------*/

/* all the gobject have a user_data field in which
 * an object can be stored and changed without copy
 */

static NspObject * int_gobject_get_user_data(void *self,const char *attr)
{
  GQuark quark;
  NspObject *data;
  quark = g_quark_from_string(attr);
  data = g_object_get_qdata(((NspGObject *) self)->obj, quark);
  if (!data) {
    Scierror("Error: data %s does not exists\n",attr);
    return NULL;
  }
  return data;
}

static NspObject * int_gobject_get_user_data_obj(void *self,const char *attr,int *copy)
{
  *copy = FALSE;
  return int_gobject_get_user_data(self,attr);
}

static int int_gobject_set_user_data(void *self,const char *attr, NspObject *O)
{
  GQuark quark;
  NspObject *data;
  quark = g_quark_from_string(attr);
  HOBJ_GET_OBJECT(O,FAIL);
  if ((data = nsp_object_copy_and_name(attr,O)) == NULLOBJ)
    return RET_BUG;
  g_object_set_qdata_full(((NspGObject *) self)->obj, quark, data , nspg_destroy_notify);
  return OK ;
}

/* self.user_data = , self.user_data and sef.user_data(....) =
 * direct acces to user_data to mofify it without a need to get then set
 *
 */


static AttrTab gobjects_attrs[] = {
  { "user_data", int_gobject_get_user_data, int_gobject_set_user_data, int_gobject_get_user_data_obj,
    (attr_set_object_function *)int_set_object_failed    },
  { (char *) 0, NULL}
};




/*------------------------------------------------------
 * methods
 *------------------------------------------------------*/

static int nspgobject_ref(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  g_object_ref(self->obj);
  return 0;
}

static int nspgobject_unref(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  g_object_unref(self->obj);
  return 0;
}

GType  nspg_type_from_object(NspObject *obj)
{
  return g_type_from_name (NSP_OBJECT(obj)->type->s_type());
}

/* gets in a NspSMatrix the property names of a NspGObject.
 */

static NspSMatrix *nsp_gobject_get_properties (NspGObject *object,int type_only_flag)
{
  NspSMatrix *nsp_ret;
  GType type;
  GObjectClass *class;
  GParamSpec **specs;
  guint n_specs;
  int i,count=0;

  if ((type = nspg_type_from_object((NspObject *)object)) == 0) return NULL;

  class = G_OBJECT_CLASS (g_type_class_peek (type));
  specs = g_object_class_list_properties (class, &n_specs);

  if (n_specs == 0) {
    if ((nsp_ret = nsp_smatrix_create_with_length(NVOID,0,0,-1)) == NULL) return NULL;
    return nsp_ret;
  }

  /* first pass to count */

  for (i = 0 ; i < n_specs ; i++)
    {
      GParamSpec *spec = specs[i];
      /*
       *	 gboolean can_modify = ((spec->flags & G_PARAM_WRITABLE) != 0 &&
       *	 (spec->flags & G_PARAM_CONSTRUCT_ONLY) == 0);
       */

      if ((spec->flags & G_PARAM_READABLE) == 0)
        {
          /* can't display unreadable properties */
          continue;
        }

      if (type_only_flag == TRUE && spec->owner_type != type)
	{
	  /* we're only interested in params of type */
	  continue;
	}
      count++;
    }


  if (count == 0) {
    if ((nsp_ret = nsp_smatrix_create_with_length(NVOID,0,0,-1)) == NULL) return NULL;
    return nsp_ret;
  }

  if ((nsp_ret = nsp_smatrix_create_with_length(NVOID,count,1,-1)) == NULL) return NULL;
  count=0;
  for (i = 0 ; i < n_specs ; i++)
    {
      GParamSpec *spec = specs[i];
      /*
       * gboolean can_modify =  ((spec->flags & G_PARAM_WRITABLE) != 0 &&
       * (spec->flags & G_PARAM_CONSTRUCT_ONLY) == 0);
       */

      if ((spec->flags & G_PARAM_READABLE) == 0) continue ;
      if (type_only_flag == TRUE && spec->owner_type != type) continue ;
      /* g_param_spec_get_nick (spec) */
      if ((nsp_ret->S[count++] =new_nsp_string(spec->name)) == (nsp_string) 0 )  return NULLSMAT;
    }
  g_free (specs);
  return nsp_ret;
}



static int nspgobject_get_property_names(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  NspObject *ret;
  int flag = TRUE;

  CheckRhs(0,1);
  if (rhs==1) { if ( GetScalarBool(stack,1,&flag) == FAIL) return RET_BUG;}
  if (( ret=(NspObject *) nsp_gobject_get_properties(self ,flag))== NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}


static int nspgobject_get_property(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gchar *param_name;
  GParamSpec *pspec;
  GValue value = { 0, };
  NspObject *ret;

  int_types T[] = { string , t_end} ;
  if ( GetArgs(stack,rhs,opt,T, &param_name) == FAIL) return RET_BUG;

  pspec = g_object_class_find_property(G_OBJECT_GET_CLASS(self->obj),
				       param_name);
  if (!pspec) {
    Scierror("the object does not support the given parameter (%s)\n",param_name);
    return RET_BUG;
  }
  g_value_init(&value, G_PARAM_SPEC_VALUE_TYPE(pspec));
  g_object_get_property(self->obj, param_name, &value);
  ret = nspg_value_as_nspobject(&value, TRUE);
  g_value_unset(&value);
  MoveObj(stack,1,ret);
  return 1;
}

static int nspgobject_set_property(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gchar *param_name;
  GParamSpec *pspec;
  GValue value = { 0, };
  NspObject *pvalue;

  int_types T[] = { string ,obj, t_end} ;
  if ( GetArgs(stack,rhs,opt,T, &param_name,&pvalue) == FAIL) return RET_BUG;

  pspec = g_object_class_find_property(G_OBJECT_GET_CLASS(self->obj),
				       param_name);
  if (!pspec) {
    Scierror("the object does not support the given parameter (%s)\n",param_name);
    return RET_BUG;
  }
  g_value_init(&value, G_PARAM_SPEC_VALUE_TYPE(pspec));
  if (nspg_value_from_nspobject(&value, pvalue) < 0) {
    Scierror("could not convert argument to correct param type\n");
    return RET_BUG;
  }
  g_object_set_property(self->obj, param_name, &value);
  g_value_unset(&value);
  return 0;
}

static int nspgobject_freeze_notify(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(1,1);
  g_object_freeze_notify(self->obj);
  return 0;
}

static int nspgobject_notify(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  char *property_name;
  int_types T[] = { string , t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&property_name) == FAIL) return RET_BUG;
  g_object_notify(self->obj, property_name);
  return 0;
}

static int nspgobject_thaw_notify(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(1,1);
  g_object_thaw_notify(self->obj);
  return 0;
}

/* we can attach data to a widget using .set_data[name=value,...]
 * and get_data through .get_data[name]
 * XXX The problem is that in order to modify datas we have to make a copy
 * It should be good to implement extraction and insertion as in hash tables.
 * or check why for methods x.get_data[name](2)=values is not accepted.
 */

static int nspgobject_get_data(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  char *key;
  GQuark quark;
  NspObject *data;

  int_types T[] = { string , t_end} ;
  if ( GetArgs(stack,rhs,opt,T, &key) == FAIL) return RET_BUG;

  quark = g_quark_from_string(key);
  data = g_object_get_qdata(self->obj, quark);
  if (!data) {
    Scierror("Error: data %s does not exists\n",key);
    return RET_BUG;
  }
  MoveObj(stack,1,data);
  return 1;
}

/* .check_data['name'] */

static int nspgobject_check_data(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  char *key;
  GQuark quark;
  NspObject *data;

  int_types T[] = { string , t_end} ;
  if ( GetArgs(stack,rhs,opt,T, &key) == FAIL) return RET_BUG;
  quark = g_quark_from_string(key);
  data = g_object_get_qdata(self->obj, quark);
  ret = ( data == NULL) ? 0 : 1;
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

/* .setdata[name=val,....] */

static int nspgobject_set_data(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  GQuark quark;
  NspObject *O;
  int i;
  if ( rhs - opt != 0 )
    {
      Scierror("%s: arguments must be given as name=val\n",NspFname(stack));
      return RET_BUG;
    }
  for ( i = 1 ; i <= rhs ; i++)
    {
      const char *key;
      /*get a copy of object (GetObj takes care of Hobj pointers) **/
      if (( O =nsp_object_copy(nsp_get_object(stack,i))) == NULLOBJ ) return RET_BUG;
      key =nsp_object_get_name(NthObj(i));
      if (nsp_object_set_name(O,nsp_object_get_name(NthObj(i))) == FAIL) return RET_BUG;
      quark = g_quark_from_string(key);
      g_object_set_qdata_full(self->obj, quark, O , nspg_destroy_notify);
    }
  return 0;
}


/*
 * method connect  self.connect['signal-name',function,list-of-extra-args];
 *
 */

static int nspgobject_connect_general(NspGObject *self, Stack stack,int rhs,int opt,int lhs,int flag)
{
  NspPList  *callback;
  NspObject *extra_args = NULL;
  gchar *name;
  guint handlerid, sigid;
  GQuark detail = 0;
  GClosure *closure;

  CheckRhs(2,3);
  CheckLhs(1,1);
  if (( name=  GetString(stack,1)) == (char*)0) return RET_BUG;
  /* Need a GetFunction here XXXXXX */
  if (( callback = GetNspPListCopy(stack,2)) == NULLP_PLIST) return RET_BUG;
  if ((nsp_object_set_name((NspObject *) callback,name)== FAIL)) return RET_BUG;
  if ( rhs == 3 )
    {
      /* extra arguments: can be any object */
#if 1
      if (( extra_args = nsp_object_copy_and_name("m",nsp_get_object(stack,3)))== NULL) return RET_BUG;
#else
      if (( extra_args =(NspObject *) GetListCopy(stack,3)) == NULLLIST ) return RET_BUG;
      if ((nsp_object_set_name((NspObject *)extra_args,"m")== FAIL)) return RET_BUG;
#endif
    }

  if (!g_signal_parse_name(name, G_OBJECT_TYPE(self->obj), &sigid, &detail, TRUE)) {
    Scierror("connect: unknown signal name (%s)\n",name);
    return RET_BUG;
  }
  closure = nspg_closure_new(callback, (NspList *) extra_args, NULL);
  nspgobject_watch_closure((NspObject *)self, closure);
  handlerid = g_signal_connect_closure_by_id(self->obj, sigid, detail, closure, flag);
  if ( nsp_move_double(stack,1,(double) handlerid) == FAIL) return RET_BUG;
  return 1;
}

static int nspgobject_connect(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  return nspgobject_connect_general(self, stack, rhs,opt, lhs,FALSE);
}


static int nspgobject_connect_after(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  return nspgobject_connect_general(self, stack, rhs,opt, lhs,TRUE);
}

/*
 *
 */

static int
nspgobject_connect_object(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  return nspgobject_connect_general(self, stack, rhs,opt, lhs,FALSE);
}

static int
nspgobject_connect_object_after(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  return nspgobject_connect_general(self, stack, rhs,opt, lhs,TRUE);
}

static int
nspgobject_disconnect(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  guint handler_id;
  int_types T[] = { s_int , t_end} ;
  if ( GetArgs(stack,rhs,opt,T, &handler_id) == FAIL) return RET_BUG;
  g_signal_handler_disconnect(self->obj, handler_id);
  return 0;
}

static int
nspgobject_handler_block(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  guint handler_id;
  int_types T[] = {s_int , t_end} ;
  if ( GetArgs(stack,rhs,opt,T, &handler_id) == FAIL) return RET_BUG;
  g_signal_handler_block(self->obj, handler_id);
  return 0;
}

static int
nspgobject_handler_unblock(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  guint handler_id;
  int_types T[] = { s_int , t_end} ;
  if ( GetArgs(stack,rhs,opt,T, &handler_id) == FAIL) return RET_BUG;
  g_signal_handler_unblock(self->obj, handler_id);
  return 0;
}

static int
nspgobject_emit(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  guint signal_id, i;
  GQuark detail;
  NspObject *nsp_ret;
  gchar *name;
  GSignalQuery query;
  GValue *params, ret = { 0, };

  CheckRhs(1,100) ; /* XXXX */

  if ((name = GetString(stack,1)) == (char*)0) return RET_BUG;;

  if (!g_signal_parse_name(name, G_OBJECT_TYPE(self->obj), &signal_id, &detail, TRUE))
    {
      Scierror("unknown signal name %s\n",name);
      return RET_BUG;
    }
  g_signal_query(signal_id, &query);
  if ( rhs != query.n_params + 1  ) {
    Scierror("%d parameters needed for signal %s; %d given\n", query.n_params, name, rhs - 2);
    return RET_BUG;
  }
  params = g_new0(GValue, query.n_params + 1);
  g_value_init(&params[0], G_OBJECT_TYPE(self->obj));
  g_value_set_object(&params[0], G_OBJECT(self->obj));

  for (i = 0; i < query.n_params; i++)
    g_value_init(&params[i + 1],
		 query.param_types[i] & ~G_SIGNAL_TYPE_STATIC_SCOPE);
  for (i = 0; i < query.n_params; i++) {
    NspObject *item = NthObj(i+2);
    if (nspg_value_from_nspobject(&params[i+1], item) < 0) {
      Scierror("could not convert type %s to %s required for parameter %d\n",
	       NSP_OBJECT(item)->type->s_type(),
	       g_type_name(G_VALUE_TYPE(&params[i+1])), i);
      for (i = 0; i < query.n_params + 1; i++)
	g_value_unset(&params[i]);
      g_free(params);
      return RET_BUG;
    }
  }
  if (query.return_type != G_TYPE_NONE)
    g_value_init(&ret, query.return_type & ~G_SIGNAL_TYPE_STATIC_SCOPE);

  g_signal_emitv(params, signal_id, detail, &ret);
  for (i = 0; i < query.n_params + 1; i++)
    g_value_unset(&params[i]);
  g_free(params);
  if ((query.return_type & ~G_SIGNAL_TYPE_STATIC_SCOPE) != G_TYPE_NONE) {
    nsp_ret = nspg_value_as_nspobject(&ret, TRUE);
    g_value_unset(&ret);
    MoveObj(stack,1,nsp_ret);
    return 1;
  } else {
    return 0;
  }
}

static int
nspgobject_stop_emission(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gchar *signal;
  guint signal_id;
  GQuark detail;
  int_types T[] = { string ,t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&signal) == FAIL) return RET_BUG;

  if (!g_signal_parse_name(signal, G_OBJECT_TYPE(self->obj), &signal_id, &detail, TRUE)) {
    Scierror("unknown signal name %s\n",signal);
    return RET_BUG;
  }
  g_signal_stop_emission(self->obj, signal_id, detail);
  return 0;
}

static int
nspgobject_chain_from_overridden(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  GSignalInvocationHint *ihint;
  guint signal_id, i;
  NspObject *nsp_ret;
  const gchar *name;
  GSignalQuery query;
  GValue *params, ret = { 0, };

  ihint = g_signal_get_invocation_hint(self->obj);
  if (!ihint) {
    Scierror("could not find signal invocation information for this object.\n");
    return RET_BUG;
  }

  signal_id = ihint->signal_id;
  name = g_signal_name(signal_id);

  if (signal_id == 0) {
    Scierror("unknown signal name\n");
    return RET_BUG;
  }
  g_signal_query(signal_id, &query);
  if (rhs != query.n_params) {
    Scierror("%d parameters needed for signal %s; %d given\n",
	     query.n_params, name,rhs);
    return RET_BUG;
  }
  params = g_new0(GValue, query.n_params + 1);
  g_value_init(&params[0], G_OBJECT_TYPE(self->obj));
  g_value_set_object(&params[0], G_OBJECT(self->obj));

  for (i = 0; i < query.n_params; i++)
    g_value_init(&params[i + 1],
		 query.param_types[i] & ~G_SIGNAL_TYPE_STATIC_SCOPE);
  for (i = 0; i < query.n_params; i++) {
    NspObject *item = NthObj(i+1);

    if (nspg_value_from_nspobject(&params[i+1], item) < 0) {
      Scierror("could not convert type %s to %s required for parameter %d\n",
	       NSP_OBJECT(item)->type->s_type(),
	       g_type_name(G_VALUE_TYPE(&params[i+1])), i);
      for (i = 0; i < query.n_params + 1; i++)
	g_value_unset(&params[i]);
      g_free(params);
      return RET_BUG;
    }
  }
  if (query.return_type != G_TYPE_NONE)
    g_value_init(&ret, query.return_type & ~G_SIGNAL_TYPE_STATIC_SCOPE);
  g_signal_chain_from_overridden(params, &ret);
  for (i = 0; i < query.n_params + 1; i++)
    g_value_unset(&params[i]);
  g_free(params);
  if ((query.return_type & ~G_SIGNAL_TYPE_STATIC_SCOPE) != G_TYPE_NONE) {
    nsp_ret = nspg_value_as_nspobject(&ret, TRUE);
    g_value_unset(&ret);
    MoveObj(stack,1,nsp_ret);
    return 1;
  } else {
    return 0;
  }
}

/* 
 * cast a gtk_objet to parent 
 */

static int
nspgobject_gcast_up(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  GType gtype;
  NspObject *nsp_ret;
  GObject *gobj = self->obj;
  NspTypeBase *surtype;
  NspTypeBase *type = ((NspObject *) self)->basetype;
  CheckRhs (0, 0);
  CheckLhs (1, 1);
  if ( type->surtype == NULL) 
    {
      Scierror("Error: cannot go up from type %s\n",type_get_name(type));
      return RET_BUG;
    }
  /* we need the BASE representant for type_surtype */
  surtype = nsp_get_type_from_id(type->surtype->id);
  gtype= g_type_from_name (type_get_name(surtype));
  if ( gtype == G_TYPE_INVALID)
    {
      Scierror("Error: invalid gtype %s\n",type_get_name(surtype));
      return RET_BUG;
    }
  else
    {
      /* Sciprintf("surtype is %s\n",type_get_name(surtype)); */
    }
  nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)gobj, surtype);
  if (nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

/* cast to most specific type
 */

static int
nspgobject_gcast_bottom(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  if ((nsp_ret = (NspObject *) nspgobject_new(NVOID,(GObject *) self->obj))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}



static int
nspgobject_bind_property(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  GBindingFlags flags =G_BINDING_DEFAULT ;
  NspGObject *gobj;
  char *prop1=NULL, *prop2= NULL, *option=NULL;
  int_types T[] = {string, obj_check, string, string, t_end};
  if ( GetArgs(stack,rhs,opt,T,&prop1,&nsp_type_gobject, &gobj,&prop2,&option) == FAIL) return RET_BUG;
  if (strcmp(option,"DEFAULT")==0 ) flags= G_BINDING_DEFAULT;
  else if (strcmp(option,"BIDIRECTIONAL")==0 ) flags= G_BINDING_BIDIRECTIONAL;
  else if (strcmp(option,"SYNC_CREATE")==0 ) flags= G_BINDING_SYNC_CREATE;
  else if (strcmp(option,"INVERT_BOOLEAN")==0 ) flags= G_BINDING_INVERT_BOOLEAN;
  g_object_bind_property (self->obj,prop1, gobj->obj,prop2,flags);
  return 0;
}

static NspMethods gobject_methods[] = {
  { "ref",  (nsp_method *) nspgobject_ref},
  { "unref", (nsp_method *)  nspgobject_unref},
  { "get_property", (nsp_method *) nspgobject_get_property},
  { "get_property_names", (nsp_method *) nspgobject_get_property_names},
  { "set_property", (nsp_method *) nspgobject_set_property},
  { "freeze_notify", (nsp_method *) nspgobject_freeze_notify},
  { "notify", (nsp_method *) nspgobject_notify},
  { "thaw_notify", (nsp_method *) nspgobject_thaw_notify},
  { "get_data", (nsp_method *) nspgobject_get_data},
  { "set_data", (nsp_method *) nspgobject_set_data},
  { "check_data", (nsp_method *) nspgobject_check_data},
  { "connect", (nsp_method *) nspgobject_connect},
  { "connect_after", (nsp_method *) nspgobject_connect_after},
  { "connect_object", (nsp_method *) nspgobject_connect_object},
  { "connect_object_after", (nsp_method *) nspgobject_connect_object_after},
  { "disconnect", (nsp_method *) nspgobject_disconnect},
  { "handler_disconnect", (nsp_method *) nspgobject_disconnect},
  { "handler_block", (nsp_method *) nspgobject_handler_block},
  { "handler_unblock", (nsp_method *) nspgobject_handler_unblock},
  { "emit", (nsp_method *) nspgobject_emit},
  { "stop_emission", (nsp_method *) nspgobject_stop_emission},
  { "emit_stop_by_name", (nsp_method *) nspgobject_stop_emission},
  { "chain", (nsp_method *) nspgobject_chain_from_overridden},
  { "gcast_up",  (nsp_method *) nspgobject_gcast_up},
  { "gcast_bottom",  (nsp_method *) nspgobject_gcast_bottom},
  { "bind_property",  (nsp_method *) nspgobject_bind_property},
  { NULL, NULL }
};


static NspMethods *gobject_get_methods(void) { return gobject_methods;};

/*-------------------------------------------
 * function
 *-------------------------------------------*/

/* XXX
 * should be documented somewhere
 */
#if 0
extern function int_cellstopixbuf;
extern function int_pixbuftocells;
extern function int_pixbuf_set_from_cells;
extern function int_pixbuf_get_channel;
#endif

static int int_gtk_timeout_add(Stack stack,int rhs,int opt,int lhs);
static int int_g_source_remove(Stack stack,int rhs,int opt,int lhs);
static int int_g_source_destroy(Stack stack,int rhs,int opt,int lhs);
static int int_gtk_quit_add(Stack stack,int rhs,int opt,int lhs);
static int int_gtk_idle_add(Stack stack,int rhs,int opt,int lhs);

static NspSMatrix *nsp_cells_to_string(NspCells *ce);

static int int_cells_to_str(Stack stack,int rhs,int opt,int lhs)
{
  NspSMatrix *S;
  NspCells *C;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((C = GetCells(stack,1)) == NULLCELLS) return RET_BUG;
  if ((S = nsp_cells_to_string(C)) == NULLSMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(S));
  return 1;
}

extern GtkWidget*create_prop_editor (GObject   *object, GType      type);


static int int_create_prop_editor(Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GType widget_type = 0;
  NspGObject *gobj1,*gobj2;
  GtkWidget *wid;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ( rhs == 2 )
    {
      int_types T[] = {obj_check, obj_check,t_end};
      if ( GetArgs(stack,rhs,opt,T,&nsp_type_gobject, &gobj1,&nsp_type_gobject, &gobj2) == FAIL) return RET_BUG;
      if ((widget_type = nspg_type_from_object((NspObject *) gobj2)) == FAIL)
	return RET_BUG;
    }
  else
    {
      int_types T[] = {obj_check, t_end};
      if ( GetArgs(stack,rhs,opt,T,&nsp_type_gobject, &gobj1) == FAIL) return RET_BUG;
    }
  wid = create_prop_editor (G_OBJECT(gobj1->obj),widget_type);
  if (wid == NULL) return RET_BUG;
  if ((nsp_ret = (NspObject *) nspgobject_new(NVOID,(GObject *) wid))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

/*----------------------------------------------------
 * Interface
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

extern function int_gvalue_create;

static OpTab NspGObject_func[]={
  /* #include "gobject-in.nam" */
  {"gobject_create",int_gobj_create},
  {"setrowscols_gobj",int_set_attribute},
  {"gtk_timeout_add",int_gtk_timeout_add},
  {"g_timeout_add",int_gtk_timeout_add},
  {"g_source_destroy",int_g_source_destroy},
  {"g_source_remove",int_g_source_remove},
  {"gtk_quit_add",int_gtk_quit_add},
  {"gtk_idle_add",int_gtk_idle_add},
#if 0
  {"pixbuftocells",int_pixbuftocells},
  {"pixbuf_set_from_cells",int_pixbuf_set_from_cells},
  {"cellstopixbuf",int_cellstopixbuf},
  {"pixbuf_get_channel",int_pixbuf_get_channel},
#endif
  {"cellstostr",int_cells_to_str},
  {"create_prop_editor",int_create_prop_editor},
  {"gvalue_create",int_gvalue_create},
  {(char *) 0, NULL}
};

/*call ith function in the NspGObject interface **/

int GObject_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(NspGObject_func[i].fonc))(stack,rhs,opt,lhs);
}

/*used to walk through the interface table
    (for adding or removing functions) **/

void GObject_Interf_Info(int i, char **fname, function (**f))
{
  *fname = NspGObject_func[i].name;
  *f = NspGObject_func[i].fonc;
}


/*---------------------------------------------------
 * Utilities
 *---------------------------------------------------*/

/**
 * nspgobject_check:
 * @value: a void pointer that can be casted to a #NspObject
 * @type: a void pointer that can be casted to a #NspTypeBase
 *
 * checks that object @value can be casted to an object of type @type
 *
 * Returns: %TRUE or %FALSE.
 **/

int nspgobject_check(void *value, void *type)
{
  return check_cast((NspObject *) value, ((NspTypeBase *) type)->id);
}

/**
 * nspgobject_new:
 * @name: a string
 * @obj: a #GObject
 *
 * creates a new nsp object which belong to a class derived from #NspGObject.
 * The nsp_type to use is extracted from the GType of the given GObject,
 * this is only possible if the associated nsp type was registered using
 * register_nsp_type_in_gtype(). If the associated nsp type is not registered 
 * then parent type is tried. If the parent type is not registered then a 
 *  GObject is created
 *
 * Returns: a new #NspGObject
 **/

NspGObject *nspgobject_new(const char *name, GObject *obj)
{
  GType gtype, p_gtype;
  NspTypeBase *type;
  if ( obj == NULL)
    {
      /* Sciprintf("Error: no gobject available in gobject_create, returning None\n"); */
      return (NspGObject *) nsp_none_create(NVOID,NULL);
    }
  gtype = G_OBJECT_TYPE(G_OBJECT(obj));
  type = nsp_type_from_gtype(gtype);
  if ( type == NULL)
    {
      /* Sciprintf("Error: no nsp type associated to GType `%s'\n",g_type_name(gtype)); */
      p_gtype = g_type_parent (gtype);
      /* Sciprintf("  trying wth parent `%s'\n",g_type_name(gtype)) */
      type = nsp_type_from_gtype(p_gtype);
      if ( type == NULL)
	{
	  Sciprintf("Error: get type in gtype failed for gtype %s and parent %s\n",
		    g_type_name(gtype), g_type_name(p_gtype));
	  Sciprintf("\ta GObject is created\n");
	}
    }
  return gobject_create(name,obj,type);
}

/**
 * nspgobject_new_with_possible_type:
 * @name: a string
 * @obj: a #GObject
 *
 * creates a new nsp object which belong to a class derived from #NspGObject.
 * The nsp_type to use is extracted from the GType of the given GObject,
 * this is only possible if the associated nsp type was registered using
 * register_nsp_type_in_gtype(). If the associated nsp type is not registered 
 * then given type is used.
 *
 * Returns: a new #NspGObject
 **/

NspGObject *nspgobject_new_with_possible_type(const char *name, GObject *obj, NspTypeBase *possible_type)
{
  GType gtype; 
  NspTypeBase *type;
  if ( obj == NULL)
    {
      /* Sciprintf("Error: no gobject available in gobject_create, returning None\n"); */
      return (NspGObject *) nsp_none_create(NVOID,NULL);
    }
  gtype = G_OBJECT_TYPE(G_OBJECT(obj));
  type = nsp_type_from_gtype(gtype);
  if ( type == NULL) type = possible_type;
  return gobject_create(name,obj,type);
}

/**
 * nspg_destroy_notify:
 * @user_data: a NspObject pointer.
 *
 * A function that can be used as a GDestroyNotify callback that will
 * call ObjDestroy on the data.
 */

void nspg_destroy_notify(gpointer user_data)
{
  NspObject *obj = (NspObject *) user_data;
  nspg_block_threads();
  /*
     fprintf(stderr,"==>destroy_notify activated on \n");
     nsp_object_print(obj,0);
  */
  nsp_object_destroy(&obj);
  nspg_unblock_threads();
}

/**
 * nspgtk_custom_destroy_notify:
 * @custom: a NspGtkCustomNotify *
 *
 * A function used as GDestroyNotify callback in gtk.c
 */

void nspgtk_custom_destroy_notify(gpointer custom)
{
  NspGtkCustomNotify *ncustom= custom;
  nspg_block_threads();
  /*
     printf(stderr,"==>custom_destroy_notify activated on \n");
     nsp_object_print(ncustom->func,0);
  */
  nsp_object_destroy(&ncustom->func);
  if ( ncustom->data != NULL)
    {
      /*
       * nsp_object_print(ncustom->data,0);
       */
      nsp_object_destroy(&ncustom->data);
    }
  g_free(ncustom);
  nspg_unblock_threads();
}

/* a revoir XXXXX */

void nspg_unblock_threads(void) {};
void nspg_block_threads(void) {};

/* -------------- NspGClosure ----------------- */

/**
 * nspgobject_watch_closure:
 * @self: a GObject wrapper instance
 * @closure: a GClosure to watch
 *
 * Adds a closure to the list of watched closures for the wrapper.
 * The closure must be one returned by nspg_closure_new().  When the
 * cycle GC traverses the wrapper instance, it will enumerate the
 * references to Nsp objects stored in watched closures.  If the
 * cycle GC tells the wrapper to clear itself, the watched closures
 * will be invalidated.
 */

static GQuark	nsp_gobject_closures = 0;
static void nsp_gobject_closures_destroy (gpointer data);

int nsp_gobject_watch_closure(GObject *gobj, GClosure *closure)
{
  GSList *closures;
  if ( gobj == NULL) return FAIL;
  if ( closure == NULL) return FAIL;
  if ( nsp_gobject_closures == 0 )
    nsp_gobject_closures = g_quark_from_static_string ("nsp-gobject-closures");

  closures = g_object_steal_qdata ( gobj,nsp_gobject_closures);

  if (g_slist_find(closures,closure) != NULL) return FAIL;

  closures = g_slist_prepend(closures, g_closure_ref (closure));
  g_closure_sink (closure);
  g_object_set_qdata_full (gobj,nsp_gobject_closures,
			   closures, nsp_gobject_closures_destroy);
  return OK;
}

int nspgobject_watch_closure(NspObject *self, GClosure *closure)
{
  if (self == NULL) return FAIL;
  if (!IsGObject(self)) return FAIL;
  return nsp_gobject_watch_closure(((NspGObject *)self)->obj,closure);
}

static void
nsp_gobject_closures_destroy (gpointer data)
{
  GSList *slist, *closures = data;
  /*
    Sciprintf("==>Closures are being destroyed\n");
   */
  for (slist = closures; slist; slist = slist->next)
    {
      g_closure_invalidate (slist->data);
      g_closure_unref (slist->data);
    }
  g_slist_free (closures);
}

static void
nspg_closure_invalidate(gpointer data, GClosure *closure)
{
  NspGClosure *pc = (NspGClosure *)closure;
  NspObject *extra = (NspObject *) pc->extra_args;
  nspg_block_threads();
  /*
    Sciprintf("==>nspg_closure_invalidate\n");
  */
  NspPListDestroy(pc->callback);
  if (pc->extra_args != NULL)nsp_object_destroy(&extra);
  if (pc->swap_data != NULL)nsp_object_destroy(&pc->swap_data);
  pc->callback = NULL;
  pc->extra_args = NULL;
  pc->swap_data = NULL;
  nspg_unblock_threads();
}

/**
 * nspg_closure_new:
 * @callback: a Nsp callable object
 * @extra_args: a tuple of extra arguments, or None/NULL.
 * @swap_data: an alternative Nsp object to pass first.
 *
 * Creates a GClosure wrapping a Nsp callable and optionally a set
 * of additional function arguments.  This is needed to attach Nsp
 * handlers to signals, for instance.
 *
 * Returns: the new closure.
 */

GClosure *
nspg_closure_new(NspPList *callback, NspList *extra_args, NspObject *swap_data)
{
  GClosure *closure;
  closure = g_closure_new_simple(sizeof(NspGClosure), NULL);
  g_closure_add_invalidate_notifier(closure, NULL, nspg_closure_invalidate);
  g_closure_set_marshal(closure, nspg_closure_marshal);
  ((NspGClosure *)closure)->callback = callback;
  ((NspGClosure *)closure)->extra_args = extra_args;
  if (swap_data) {
    ((NspGClosure *)closure)->swap_data = swap_data;
    closure->derivative_flag = TRUE;
  }
  return closure;
}

/*
 * execute the closure
 */

static Stack Marshal_stack={0,NULL};
static int  stack_count=0; /* should be added in the stack */

void nsp_init_gtk_stack(void)
{
  nsp_init_stack(&Marshal_stack);
}


void
nspg_closure_marshal(GClosure *closure,
		     GValue *return_value,
		     guint n_param_values,
		     const GValue *param_values,
		     gpointer invocation_hint,
		     gpointer marshal_data)
{
  /* make a copy to be sure that some
   * values are preserved through recursion
   */
  Stack stack = Marshal_stack;
  NspGClosure *pc = (NspGClosure *)closure;
  int nargs = 0, i, n;
  stack_count++; /* count recursive calls */
  nspg_block_threads();
  if ( pc->callback  == NULLP_PLIST)
    {
      goto end;
    }
  /* nsp_init_stack(&Marshal_stack,Marshal_stack_S); */
  NspFname(stack) = "pipo";
  stack.first =0;

  /* fprintf(stderr,"FuncEval(%d) avec le Marshal_stack %s stack.val->S=<%lx>, first=%d\n",
   *   stack_count, "pipo" ,(long) stack.val->S,stack.first);
   */
  if ( stack_count != 1 )
    {
      /* XXX trying to preserve the already stored objects */
      NspObject **O= stack.val->S;
      int args =0;
      while ( *O != NULLOBJ) { args++; O++;}
      stack.first = args ;
      /* fprintf(stderr,"I preserve %d arguments \n",args); */
    }

  /* We put the params on the stack **/
  for (i = 0; i < n_param_values; i++)
    {
      /* swap in a different initial data for connect_object() */
      if (i == 0 && G_CCLOSURE_SWAP_DATA(closure)) {
	if (pc->swap_data == NULL) {
	  goto end;
	}
	stack.val->S[stack.first + nargs]= pc->swap_data;
	nargs++;
      } else {
	NspObject *item;
	/* XXXX: */
	if (( item = nspg_value_as_nspobject(&param_values[i], FALSE))== NULL)
	  {
	    goto end;
	  }
	stack.val->S[stack.first + nargs]= item;
	nargs++;
      }
    }
  /* check if we have extra arguments and put add it on the stack
   * extra arguments are given as a list of extra args
   */
  if ( pc->extra_args  != NULLLIST )
    {
      /*
	 Cell *C= pc->extra_args->first;
	 while ( C != NULLCELL)
	 {
	 if ( C->O != NULLOBJ )
	 {
	 stack.val->S[stack.first + nargs]= C->O;
	 nargs++;
	 }
	 C = C->next ;
	 }
      */
      stack.val->S[stack.first + nargs]= (NspObject *) pc->extra_args ;
      nargs++;
    }

  /* Calling a macro func is a macro coded in P_PList **/

  if ((n=nsp_eval_func((NspObject *)pc->callback,NspFname(stack),2,stack,stack.first,nargs,0,-1)) < 0 )
    {
      nsp_error_message_show();
      goto end;
    }

  /* fprintf(stderr,"Sortie de FuncEval avec %d arguments de retour et first=%d\n",n,stack.first); */


  /* FuncEval fait-il le menage tout seul ? XXXXX **/
  if ( n >= 1) {
    if (return_value) nspg_value_from_nspobject(return_value,stack.val->S[stack.first] );
  }
  /* clean the stack */
  for (i = 0 ; i < n ; i++)
    {
      nsp_void_object_destroy(&stack.val->S[stack.first+i]);
      stack.val->S[stack.first+i]= NULLOBJ;
    }
  goto end;
  end :
    {
      stack_count--;
      nspg_unblock_threads();
      return;
    }
}


/**
 * nsp_gtk_eval_function:
 * @func: code of function to be evaluated
 * @args: array containing the input arguments
 * @n_args: number of input arguments
 * @ret:  array to be filled with returned arguments
 * @nret: on entry, the number of expected returned arguments,
 *   on return the number of returned values (always less or equal to @nret)
 *   or a negative number (error code) in case of error
 *
 * evaluates the macro @func using the gtk stack.
 * this function should be replaced by #nsp_gtk_eval_function_catch which gives
 * more execution control.
 *
 * Return value: %OK or %FAIL
 **/

static int _nsp_gtk_eval_function(NspPList *func,const char *fname,NspObject *args[],int n_args,NspObject  *ret[],int *nret, int errcatch, int pausecatch);

int nsp_gtk_eval_function(NspPList *func,NspObject *args[],int n_args,NspObject  *ret[],int *nret)
{
  /* XXX replace "gtk_eval" by the f name */
  return _nsp_gtk_eval_function(func,"gtk_eval",args,n_args,ret,nret, FALSE, FALSE);
}

/* similar to the previous function but in a context
 * where  errcatch and  pausecatch can be used
 */

int nsp_gtk_eval_function_catch(NspPList *func,NspObject *args[],int n_args,NspObject  *ret[],int *nret,
				int errcatch, int pausecatch)
{
  /* XXX replace "gtk_eval" by the f name */
  return _nsp_gtk_eval_function(func,"gtk_eval",args,n_args,ret,nret, errcatch, pausecatch);
}





static int _nsp_gtk_eval_function(NspPList *func,const char *fname,NspObject *args[],int n_args,NspObject  *ret[],int *nret, int errcatch, int pausecatch)
{
  /* make a copy to be sure that some
   * values are preserved through recursion
   */
  Stack stack = Marshal_stack;
  int nargs = 0, i, n=0,rep =FAIL;
  /* save current values */
  int errcatch_cur = Marshal_stack.val->errcatch;
  int pause_cur = Marshal_stack.val->pause;
  /* must be reset at the end */
  stack.val->errcatch = errcatch;
  stack.val->pause = pausecatch;

  stack_count++;
  nspg_block_threads();
  if ( func  == NULLP_PLIST && fname == NULL)
    {
      goto end;
    }
  /* nsp_init_stack(&stack,stack_S); */
  NspFname(stack) = fname;
  stack.first =0;
  if ( stack_count != 1 )
    {
      /* XXX trying to preserve the already stored objects */
      NspObject **O= stack.val->S;
      int args =0;
      while ( *O != NULLOBJ) { args++; O++;}
      stack.first = args ;
      /* fprintf(stderr,"I preserve %d arguments \n",args); */
    }
  /*We put the params on the stack **/
  for (i = 0; i < n_args ; i++)
    stack.val->S[stack.first + nargs++]= args[i];
  /*Calling func is a macro coded in P_PList **/
  if ((n=nsp_eval_func((NspObject *) func,  NspFname(stack),2,stack,stack.first,nargs,0,*nret)) < 0 )
    {
      if ( errcatch == FALSE ) nsp_error_message_show();
      goto end;
    }
  /*FuncEval fait-il le menage tout seul ? XXXXX **/
  for ( i = 0 ; i < Min(n,*nret) ; i++)
    {
      ret[i]= stack.val->S[stack.first+i];
      stack.val->S[stack.first+i]= NULLOBJ;
    }
  *nret =Min(n,*nret);
  /* clean the stack */
  for ( i = *nret ; i < n  ; i++)
    {
      nsp_void_object_destroy(&stack.val->S[stack.first+i]);
      stack.val->S[stack.first+i]= NULLOBJ;
    }
  rep = OK;
  goto end;
  end :
    {
      stack_count--;
      Marshal_stack.val->errcatch= errcatch_cur;
      Marshal_stack.val->pause = pause_cur;
      nspg_unblock_threads();
      /* if FAIL returns the error code in *nret */
      if ( rep == FAIL) *nret = n;
      return rep;
    }
}

/**
 * nsp_gtk_eval_function_by_name:
 * @func:
 * @args:
 * @n_args:
 * @ret:
 * @nret:
 *
 * evaluates the macro @func using the gtk stack.
 *
 * Return value:
 **/


int nsp_gtk_eval_function_by_name(const char *name,NspObject *args[],int n_args,NspObject  *ret[],int *nret)
{
  /* we search here the function by name in the calling stacks
   * since it is used by scicos or mexcallmatlab.
   */
  NspObject *Obj= nsp_frames_search_object(name);
  return  _nsp_gtk_eval_function((NspPList *)Obj,name,args,n_args,ret,nret, FALSE,FALSE);
}


/**
 * nsp_gtk_eval_function_by_name_catch:
 * @func:
 * @args:
 * @n_args:
 * @ret:
 * @nret:
 *
 * evaluates the macro @func using the gtk stack.
 *
 * Return value:
 **/


int nsp_gtk_eval_function_by_name_catch(const char *name,NspObject *args[],int n_args,NspObject  *ret[],int *nret,
					int errcatch, int pausecatch)
{
  /* we search here the function by name in the calling stacks
   * since it is used by scicos or mexcallmatlab.
   */
  NspObject *Obj= nsp_frames_search_object(name);
  return  _nsp_gtk_eval_function((NspPList *)Obj,name,args,n_args,ret,nret,errcatch, pausecatch);
}


/**
 * nsp_constant_strip_prefix:
 * @name: the constant name.
 * @strip_prefix: the prefix to strip.
 *
 * Advances the pointer @name by strlen(@strip_prefix) characters.  If
 * the resulting name does not start with a letter or underscore, the
 * @name pointer will be rewound.  This is to ensure that the
 * resulting name is a valid identifier.  Hence the returned string is
 * a pointer into the string @name.
 *
 * Returns: the stripped constant name.
 */

static const char *
nsp_constant_strip_prefix(const gchar *name, const gchar *strip_prefix)
{
  gint prefix_len;
  guint j;

  prefix_len = strlen(strip_prefix);

  /* strip off prefix from value name, while keeping it a valid
   * identifier */
  for (j = prefix_len; j >= 0; j--) {
    if (g_ascii_isalpha(name[j]) || name[j] == '_') {
      return &name[j];
    }
  }
  return name;
}

/**
 * nsp_enum_add_constants:
 * @table: a hash table
 * @enum_type: the GType of the enumeration.
 * @strip_prefix: the prefix to strip from the constant names.
 *
 * Adds constants to the given Nsp Hash Table for each value name of
 * the enumeration. A prefix will be stripped from each enum name.
 */

int
nsp_enum_add_constants(NspHash *table, GType enum_type, const gchar *strip_prefix)
{

  NspObject *nsp_val,*O;
  GEnumClass *eclass;
  guint i;

  /* a more useful warning */
  if (!G_TYPE_IS_ENUM(enum_type)) {
    Scierror("Warning: `%s' is not of enum type\n", g_type_name(enum_type));
    return FAIL;
  }

  /* Sciprintf("XXXX: `%s' : enum type\n", g_type_name(enum_type)); */

  eclass = G_ENUM_CLASS(g_type_class_ref(enum_type));

  for (i = 0; i < eclass->n_values; i++) {
    const gchar *name = eclass->values[i].value_name;
    gint value = eclass->values[i].value;
    if (strip_prefix != NULL) name= nsp_constant_strip_prefix(name, strip_prefix);
    if (( nsp_val = (NspObject *) nsp_matrix_create_from_doubles(name,1,1,(double)value))== NULL) return FAIL;
    /* XXXXX */
    /* Sciprintf("YYY: `%s' derived from %s\n",name,g_type_name(enum_type)); */
    if (nsp_hash_find(table,name,&O)== OK)
      {
	Scierror("Warning: %s is already stored in table %s\n",name,nsp_object_get_name((NspObject *)table));
      }
    if (nsp_hash_enter(table,nsp_val) == FAIL) return FAIL;
  }
  g_type_class_unref(eclass);
  return OK;
}

/**
 * nsp_flags_add_constants:
 * @table: a hash table
 * @flags_type: the GType of the flags type.
 * @strip_prefix: the prefix to strip from the constant names.
 *
 * Adds constants to the given hash table for each value name of
 * the flags set.  A prefix will be stripped from each flag name.
 */

int
nsp_flags_add_constants(NspHash *table, GType flags_type,const gchar *strip_prefix)
{
  GFlagsClass *fclass;
  guint i;
  NspObject *nsp_val, *O;
  /* a more useful warning */
  if (!G_TYPE_IS_FLAGS(flags_type)) {
    Scierror("Warning: `%s' is not of flags type\n", g_type_name(flags_type));
    return FAIL;
  }

  fclass = G_FLAGS_CLASS(g_type_class_ref(flags_type));

  for (i = 0; i < fclass->n_values; i++) {
    const gchar *name = fclass->values[i].value_name;
    guint value = fclass->values[i].value;
    if (strip_prefix != NULL) name= nsp_constant_strip_prefix(name, strip_prefix);
    if (( nsp_val = (NspObject *) nsp_matrix_create_from_doubles(name,1,1,(double)value))== NULL) return FAIL;
    /* XXXXX */
    if (nsp_hash_find(table,name,&O)== OK)
      {
	Scierror("Warning: %s is already stored in table %s \n",name,nsp_object_get_name((NspObject *)table));
      }
    if (nsp_hash_enter(table,nsp_val) == FAIL) return FAIL;
  }
  g_type_class_unref(fclass);
  return OK;
}

/**
 * nspg_enum_get_value:
 * @enum_type: the GType of the flag.
 * @obj: a Nsp object representing the flag value (unchanged).
 * @val: a pointer to the location to store the integer representation of the flag.
 *
 * Converts a Nsp object to the integer equivalent.  The conversion
 * will depend on the type of the Nsp object.  If the object is an
 * integer, it is passed through directly. If it is a string, it will
 * be treated as a full or short enum name as defined in the GType.
 *
 * Returns: OK on success or FAIL on failure
 */

gint nspg_enum_get_value(GType enum_type, NspObject *obj, void *val)
{
  gint *gval=val;
  GEnumClass *eclass = NULL;
  const char *str;
  if ( val == NULL) return FAIL;

  if (!obj) { *gval = 0 ; return OK;}
  if (IsMat(obj))
    {
      return  IntScalar(obj, gval);
    }
  if ((str =nsp_string_object(obj)) != NULL)
    {
      GEnumValue *info;
      if (enum_type == G_TYPE_NONE)
	{
	  Scierror("could not convert string %s to enum because there is no GType associated to look up the value\n",str);
	  return  FAIL;
	}
      eclass = G_ENUM_CLASS(g_type_class_ref(enum_type));
      info = g_enum_get_value_by_name(eclass, str);
      g_type_class_unref(eclass);
      if (!info) info = g_enum_get_value_by_nick(eclass, str);
      if (!info) {
	Scierror("could not convert string %s\n",str);
	return FAIL;
      }
      *gval = info->value;
      return OK;
    }
  Scierror("enum values must be strings or ints\n");
  return FAIL;
}

/**
 * nspg_flags_get_value:
 * @flag_type: the GType of the flag.
 * @obj: a Nsp object representing the flag value
 * @val: a pointer to the location to store the integer representation of the flag.
 *
 * Converts a Nsp object to the integer equivalent.  The conversion
 * will depend on the type of the Nsp object.  If the object is an
 * integer, it is passed through directly.  If it is a string, it will
 * be treated as a full or short flag name as defined in the GType.
 * If it is a tuple, then the items are treated as strings and ORed
 * together.
 *
 * Returns: OK on success or FAIL on failure
 */

gint
nspg_flags_get_value(GType flag_type, NspObject *obj, void *val)
{
  gint *gval=val;
  GFlagsClass *fclass = NULL;

  if ( gval == NULL) return FAIL;

  if (!obj) { *gval = 0; return FAIL;}
  if ( IntScalar(obj, gval) == OK ) { return OK;}
  if ( IsSMat(obj))
    {
      int i;
      GFlagsValue *info;
      if (flag_type != G_TYPE_NONE)
	fclass = G_FLAGS_CLASS(g_type_class_ref(flag_type));
      else {
	Scierror("could not convert string to flag because there is no GType associated to look up the value\n");
	return FAIL;
      }
      *gval = 0;
      for ( i= 0 ; i < ((NspSMatrix *)obj)->mn ; i ++)
	{
	  char *str = ((NspSMatrix *)obj)->S[i];
	  info = g_flags_get_value_by_name(fclass, str);
	  g_type_class_unref(fclass);

	  if (!info)
	    info = g_flags_get_value_by_nick(fclass, str);
	  if (info) {
	    *gval |= info->value;
	  } else {
	    Scierror("could not convert string\n");
	    return FAIL;
	  }
	}
      g_type_class_unref(fclass);
      return OK;
    }
  Scierror("flag values must be strings, ints or matrix of strings\n");
  return FAIL;
}



/*
 * NspObjects <--> GValues
 */

/* -------------- GValue marshaling ------------------ */

/**
 * nspg_value_from_nspobject:
 * @value: the GValue object to store the converted value in.
 * @obj: the Nsp object to convert.
 *
 * This function converts a Nsp object and stores the result in a
 * GValue.  The GValue must be initialised in advance with
 * g_value_init().  If the Nsp object can't be converted to the
 * type of the GValue, then an error is returned.
 *
 * Returns: OK on success, FAIL on error.
 */

int
nspg_value_from_nspobject(GValue *value, NspObject *obj)
{
  Boolean bval;
  int val;
  char *str;
  double dval;
  switch (G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(value))) {
  case G_TYPE_INTERFACE:
    /* we only handle interface types that have a GObject prereq */
    if (g_type_is_a(G_VALUE_TYPE(value), G_TYPE_OBJECT)) {
      /*XXXX Attention c'est pas clair ne faudrait-il pas explorer ce qu'il y a ds GObject */
      if (! IsGObject(obj)) {
	return FAIL;
      }
      if (!G_TYPE_CHECK_INSTANCE_TYPE(NSP_GOBJECT_GET(obj),
				      G_VALUE_TYPE(value))) {
	return FAIL;
      }
      g_value_set_object(value, NSP_GOBJECT_GET(obj));
    } else {
      return FAIL;
    }
    break;
  case G_TYPE_CHAR:
  case G_TYPE_UCHAR:
    if ((str =nsp_string_object(obj)) != NULL)
#if GLIB_CHECK_VERSION(2,32,0)
      g_value_set_schar(value, str[0]);
#else
      g_value_set_char(value, str[0]);
#endif

    else {
      return FAIL;
    }
    break;
  case G_TYPE_BOOLEAN:
    if ( BoolScalar(obj, &bval) == OK)
      g_value_set_boolean(value, bval);
    else
      return FAIL;
    break;
  case G_TYPE_INT:
    if ( IntScalar(obj, &val) == OK)
      g_value_set_int(value, val);
    else
      return FAIL;
    break;
  case G_TYPE_UINT:
    {
      /* a reprendre XXXXXX */
      if ( IntScalar(obj, &val) == OK && val >= 0 && val <= G_MAXUINT)
	g_value_set_uint(value, (guint)val);
      else
	return FAIL;
    }
    break;
  case G_TYPE_LONG:
    /* a reprendre XXXXXX */
    if ( IntScalar(obj, &val) == OK )
      g_value_set_long(value, (long) val );
    else
      return FAIL;
    break;
  case G_TYPE_ULONG:
    {
      if ( IntScalar(obj, &val) == OK && val >= 0 )
	g_value_set_ulong(value, (long) val );
      else
	return FAIL;
    }
    break;
  case G_TYPE_INT64:
    return FAIL;
    break;
  case G_TYPE_UINT64:
    return FAIL;
    break;
  case G_TYPE_ENUM:
    {
      gint val = 0;
      if (nspg_enum_get_value(G_VALUE_TYPE(value), obj, &val) < 0)
	return FAIL;
      g_value_set_enum(value, val);
    }
    break;
  case G_TYPE_FLAGS:
    {
      gint val = 0;
      if (nspg_flags_get_value(G_VALUE_TYPE(value), obj, &val) < 0)
	return FAIL;
      g_value_set_flags(value, val);
    }
    break;
  case G_TYPE_FLOAT:
    if ( DoubleScalar(obj,&dval) == OK)
      g_value_set_float(value, (float) dval);
    else
      return FAIL;
    break;
  case G_TYPE_DOUBLE:
    if ( DoubleScalar(obj,&dval) == OK)
      g_value_set_double(value,dval);
    else
      return FAIL;
    break;
  case G_TYPE_STRING:
    if ((str =nsp_string_object(obj)) != NULL)
      g_value_set_string(value, str);
    else
      return FAIL;
    break;
  case G_TYPE_POINTER:
    return FAIL;
    break;
  case G_TYPE_BOXED:
    return FAIL;
    break;
  case G_TYPE_PARAM:
    return FAIL;
  case G_TYPE_OBJECT:
    if ( IsGObject(obj) && G_TYPE_CHECK_INSTANCE_TYPE( NSP_GOBJECT_GET(obj),
						       G_VALUE_TYPE(value)))
      {
	g_value_set_object(value, NSP_GOBJECT_GET(obj));
      }
    else
      return FAIL;
    break;
  default:
    break;
  }
  return 0;
}


/**
 * nsp_type_from_gtype:
 * @gtype: a GType
 *
 * utility function used to obtain a nsp type given a GType.
 * This is used when proper nsp objects are to be build from
 * GObjects.
 * It could be useful to also register basic Gtk types.
 *
 * Returns: a #NspTypeBase
 **/

static const gchar *nsp_gobject_class_id     = "NspGObject::class";
static GQuark       nsp_gobject_class_key    = 0;

NspTypeBase * nsp_type_from_gtype(GType gtype)
{
  NspTypeBase *type;
  type = (NspTypeBase *) g_type_get_qdata(gtype, nsp_gobject_class_key );
  return type;
}

/**
 * nsp_peek_type_from_gtype:
 * @gtype: a GType
 *
 * Obtain a nsp type given a GType.
 * If the nsp type is not registered the %NULL is returned
 *
 * Returns: a #NspTypeBase
 **/

NspTypeBase * nsp_peek_type_from_gtype(GType gtype)
{
  return g_type_get_qdata(gtype, nsp_gobject_class_key );
}

/**
 * register_nsp_type_in_gtype:
 * @type:
 * @gtype:
 *
 * utility function used to attach a nsp type to a GType.
 * This is used when proper nsp objects are to be build from
 * GObjects.
 * It could be useful to also register basic Gtk types.
 *
 **/

void register_nsp_type_in_gtype(NspTypeBase *type, GType gtype)
{
  if ( gtype == G_TYPE_INVALID ) return ;
  if (!nsp_gobject_class_key)
    nsp_gobject_class_key = g_quark_from_static_string(nsp_gobject_class_id);
  g_type_set_qdata(gtype, nsp_gobject_class_key, type);
}

/**
 * nspg_value_as_nspobject:
 * @value: the GValue object.
 * @copy_boxed: true if boxed values should be copied.
 *
 * This function creates/returns a Nsp wrapper object that
 * represents the GValue passed as an argument.
 *
 * Returns: a NspObject representing the value.
 */

#define NSP_MAT(val) \
	if (( M = nsp_matrix_create(NVOID,'r',1,1) ) == NULLMAT ) return NULL; \
	M->R[0] = val ;	return NSP_OBJECT(M); \

NspObject *
nspg_value_as_nspobject(const GValue *value, gboolean copy_boxed)
{
  gpointer *gobj;
  NspMatrix *M;

  switch (G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(value)))
    {
    case G_TYPE_INTERFACE:
      if (g_type_is_a(G_VALUE_TYPE(value), G_TYPE_OBJECT))
	return (NspObject *) gobject_create(NVOID,g_value_get_object(value),NULL);
      else
	break;
    case G_TYPE_CHAR: {
#if GLIB_CHECK_VERSION(2,32,0)
      gint8 val = g_value_get_schar(value);
#else
      gint8 val = g_value_get_char(value);
#endif
      return  nsp_new_string_obj(NVOID,(char *) &val,1);
    }
    case G_TYPE_UCHAR: {
      guint8 val = g_value_get_uchar(value);
      return  nsp_new_string_obj(NVOID,(char *) &val,1);
    }
    case G_TYPE_BOOLEAN: {
      NspObject *val = g_value_get_boolean(value) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
      return val;
    }
    case G_TYPE_INT: NSP_MAT(g_value_get_int(value));
    case G_TYPE_UINT: NSP_MAT(g_value_get_uint(value));
    case G_TYPE_LONG:NSP_MAT(g_value_get_long(value));
    case G_TYPE_ULONG:NSP_MAT(g_value_get_ulong(value));
    case G_TYPE_INT64:NSP_MAT(g_value_get_int64(value));
    case G_TYPE_UINT64:NSP_MAT(g_value_get_uint64(value));
    case G_TYPE_ENUM: NSP_MAT(g_value_get_enum(value));
    case G_TYPE_FLAGS: NSP_MAT(g_value_get_flags(value));
    case G_TYPE_FLOAT: NSP_MAT(g_value_get_float(value));
    case G_TYPE_DOUBLE: NSP_MAT(g_value_get_double(value));
    case G_TYPE_STRING: {
      const gchar *str = g_value_get_string(value);
      return  nsp_new_string_obj(NVOID,(str) ? str : "" ,-1);
    }
    case G_TYPE_POINTER:
      return (NspObject *) gpointer_create(NVOID,G_VALUE_TYPE(value), g_value_get_pointer(value),NULL);
    case G_TYPE_BOXED: {
      NspTypeBase *type= nsp_type_from_gtype(G_VALUE_TYPE(value));
      if ( type == NULL)
	{
	  Scierror("nspg_value_as_nspobject: G_TYPE_BOXED is to be done for %s\n",
		   g_type_name(G_VALUE_TYPE(value)));
	  return NULL;
	}
      if (copy_boxed)
	return (NspObject *)gboxed_create(NVOID,G_VALUE_TYPE(value), g_value_get_boxed(value),TRUE,TRUE,type);
      else
	return (NspObject *)gboxed_create(NVOID,G_VALUE_TYPE(value), g_value_get_boxed(value),FALSE,FALSE,type);
    }
    case G_TYPE_PARAM:
      {
	/*
	 * An example of GParamSpec is given in the demo_revealer where a callback 
	 * is called with a GParamSpec argument.
	 */
	NspObject *Obj;
	GParamSpec *spec = g_value_get_param (value);
	Obj = (NspObject *) nsp_gparamspec_create(NVOID,spec,NULL);
	if ( Obj == NULL)
	  Sciprintf("nspg_value_as_nspobject: G_TYPE_PARAM (of type %s) is to be done \n",
		    G_PARAM_SPEC_TYPE_NAME(spec));
	return Obj;
      }
      return NULL;
    case G_TYPE_OBJECT:
      {
	gobj = g_value_get_object(value);
	/* we use nspgobject_new to return the most specific NspObject which contains the GObject
	 * if it fails it will return a None
	 */
	return (NspObject *) nspgobject_new(NVOID,(GObject *) gobj);
      }
      
    case G_TYPE_VARIANT :
      {
	GVariant *gv= g_value_get_variant(value);
	if ( gv != NULL)
	  {
	    return (NspObject*) nsp_gvariant_create(NVOID,gv,(NspTypeBase *) nsp_type_gvariant);
	  }
	else
	  {
	    return (NspObject *) nsp_none_create(NVOID,NULL);
	  }
      }
    default:
      break;
    }
  Sciprintf("Warning: Do not know how to build an object from a gvalue of type %s\n",
	    g_type_name(G_VALUE_TYPE(value)));
  Scierror("Do not know how to build an object from a gvalue of type %s\n",
	   g_type_name(G_VALUE_TYPE(value)));
  return NULL;
}

/*
 * XXXX : to be stored elsewhere
 */

#include <gtk/gtk.h>
#include "nsp/gtk/gboxed.h"

int
nsp_gdk_rectangle_from_object(NspObject *object, GdkRectangle *rectangle)
{
  HOBJ_GET_OBJECT(object,FALSE);
  if (IsMat(object) && ((NspMatrix *) object)->rc_type == 'r'  )
    {
      Mat2double((NspMatrix *) object); /* be sure that we are in a canonic mode */
      rectangle->x= ((NspMatrix *) object)->R[0];
      rectangle->y= ((NspMatrix *) object)->R[1];
      rectangle->width= ((NspMatrix *) object)->R[2];
      rectangle->height= ((NspMatrix *) object)->R[3];
      return TRUE;
  }
  else if ( nspg_boxed_check(object, GDK_TYPE_RECTANGLE)) {
    *rectangle = *nspg_boxed_get(object, GdkRectangle);
    return TRUE;
  }
  Scierror("could not convert object to GdkRectangle\n");
  return FALSE;
}

/**
 * gtype_from_nsp_object:
 * @obj: an object
 *
 * find a GType corresponding to the given object.
 * Returns: the corresponding GType or G_TYPE_NONE.
 *
 * This is usefull for list and tree where column
 * types are to be given.
 */

GType gtype_from_nsp_object(NspObject *obj)
{
  if (obj == NULL ) {
    Scierror("can't get type from NULL object\n");
    return 0;
  }
  /* Hobj case */
  if ( IsHobj(obj))
    {
      if (((NspHobj *) obj)->htype != 'g') obj = ((NspHobj *) obj)->O;
      else
	{
	  if ((obj= nsp_global_frame_search_object(NSP_OBJECT(obj)->name)) == NULLOBJ)
	    {
	      return G_TYPE_INVALID;
	    }
	}
    }

  /* map some standard types to primitive GTypes ... */
  if (IsNone(obj))
    return G_TYPE_NONE;
  else if (IsType(obj))
    {
      GType gtype;
      NspTypeBase *type  = ((NspType *) obj)->nsp_type;
      /* Sciprintf("Gtype: %s\n",(type_get_name(type)));*/
      if ((gtype= g_type_from_name (type_get_name(type))) != G_TYPE_INVALID)
	return gtype;
    }
  else if (IsSMat(obj))
    return G_TYPE_STRING;
  else if (IsBMat(obj))
    return G_TYPE_BOOLEAN;
  else if (IsMat(obj) || IsMpMat(obj))
    return G_TYPE_DOUBLE;
  else if (IsGObject(obj))
    return G_OBJECT_TYPE( NSP_GOBJECT_GET(obj));
  else if (IsGBoxed(obj))
    return ((NspGBoxed *) obj)->gtype ;

  Scierror("Error: could not get a gtype code from object of type %s\n",
	   obj->type->s_type());
  return G_TYPE_INVALID;
}


/* -----------------------------------------------------------------
 * Utility function fill a row in a GtkListStore or GtkTreeStore
 * -----------------------------------------------------------------
 */

/**
 * nsp_gtk_gtypes_from_list:
 * @L: a #NspList
 * @len: an int pointer
 *
 * returns an array of GType from a #NspList object.
 * if a list element is a mxn matrix then n Gtypes are added
 * if a list element is itself a list(x1,....) then the Gtype associated to
 * the first element is added.
 * As an example if @L is equal to
 * list([1,2,3],["foo","bar";"zip","gz"],[%t],list(3,4),list(pixbuf....))
 * then the gtype array will contail
 * [double,double,double,string,string,boolean,double,pixbuf].
 * The caller will have to take care of freeing the returned value
 * and the size of the returned array is returned in len
 *
 * Returns: an array of #GType
 **/

GType *nsp_gtk_gtypes_from_list(NspList *L,int *len)
{
  int n_columns=0,count;
  GType *column_types;
  Cell *cloc = L->first ;

  /* first walk to count columns */
  while ( cloc != NULLCELL)
    {

      if ( cloc->O == NULLOBJ )
	{
	  Scierror("Warning: list element %d is undefined\n",n_columns+1);
	  return NULL;
	}
      else if ( IsList(cloc->O))
	{
	  /* column described by a list */
	  n_columns++;
	}
      else if ( IsMat(cloc->O) || IsMpMat(cloc->O) || IsSMat(cloc->O) || IsBMat(cloc->O) )
	{
	  /* n_columns coluns of object */
	  n_columns +=nsp_object_get_size(cloc->O,2);
	}
      else
	{
	  Scierror("list element %d has a wrong type %s to fill a GtkListStore\n",
		   n_columns+1,cloc->O->type->s_type());
	  return NULL;
	}
      cloc = cloc->next;
    }

  column_types = g_new(GType,n_columns);
  /* walk to fill column_types */
  count=0;
  cloc = L->first ;
  while ( cloc != NULLCELL)
    {
      GType gtype = G_TYPE_NONE;
      if ( IsList(cloc->O))
	{
	  Cell *cloc1 = ((NspList *) cloc->O)->first ;
	  if ( cloc1->O != NULLOBJ) gtype = gtype_from_nsp_object(cloc1->O);
	  if ( gtype == G_TYPE_INVALID) goto fail;
	  column_types[count++] = gtype;
	}
      else
	{
	  int n =nsp_object_get_size(cloc->O,2), n1 = count,i;
	  if ( n != 0 )
	    {
	      gtype = gtype_from_nsp_object(cloc->O);
	      if ( gtype == G_TYPE_INVALID) goto fail;
	      column_types[count++] = gtype;
	      for (i=1 ; i < n ; i++)
		column_types[count++] = column_types[n1];
	    }
	}
      cloc = cloc->next;
    }
  *len = n_columns ;
  return column_types;
 fail:
  g_free(column_types);
  return NULL;
}


/**
 * nsp_gtk_list_store_new_from_list:
 * @L: a #NspList.
 *
 * creates a GtkListStore from a list description.
 *
 * Returns: a new #GtkListStore.
 **/

GtkListStore *nsp_gtk_list_store_new_from_list(NspList *L)
{
  int ncols=0;
  GtkListStore *ls;
  GType *column_types;
  column_types = nsp_gtk_gtypes_from_list(L,&ncols);
  if (column_types == NULL) return NULL;
  /* create the list store */
  ls = gtk_list_store_newv(ncols, column_types);
  if (column_types != NULL) g_free(column_types);
  if (! ls) {
    Scierror("could not create GtkListStore object\n");
    return NULL;
  }
  return ls;
}

/**
 * nsp_gtk_tree_store_new_from_list:
 * @L:  a #NspList.
 *
 * creates a GtkTreeStore from a list description.
 *
 * Returns: a new #GtkTreeStore.
 **/

GtkTreeStore *nsp_gtk_tree_store_new_from_list(NspList *L)
{
  int ncols=0;
  GtkTreeStore *ts;
  GType *column_types;
  column_types = nsp_gtk_gtypes_from_list(L,&ncols);
  if (column_types == NULL) return NULL;
  /* create the list store */
  ts = gtk_tree_store_newv(ncols, column_types);
  g_free(column_types);
  if (! ts) {
    Scierror("could not create GtkListStore object\n");
    return NULL;
  }
  return ts;
}

/**
 * nsp_list_count_rows:
 * @L: a #NspList.
 * @n_rows: int pointer
 *
 * counts the number of  rows described in @L
 * and check that all the list elements have the same number of rows.
 * This numbre is returned in @n_rows.
 *
 *
 * Returns: %OK or %FAIL
 **/

int nsp_list_count_rows(NspList *L,int *n_rows)
{
  int count,nrows=-1;
  Cell *cloc = L->first ;
  count=0;
  while ( cloc != NULLCELL)
    {
      if ( cloc->O == NULLOBJ )
	{
	  Scierror("Warning: list element %d is undefined\n",count+1);
	  return FAIL;
	}
      else if ( IsNone(cloc->O))
	{
	  /* ignore */
	  count++;
	}
      else if ( IsList(cloc->O))
	{
	  int m =nsp_object_get_size(cloc->O,0);
	  if ( nrows != -1)
	    {
	      if ( m != nrows )
		{
		  Scierror("column %d has wrong size %d expecting %d\n",count+1,m,nrows);
		  return FAIL;
		}
	    }
	  else nrows = m;
	  count++;
	}
      else if ( IsMat(cloc->O) ||  IsMpMat(cloc->O) || IsSMat(cloc->O) || IsBMat(cloc->O) )
	{
	  int m =nsp_object_get_size(cloc->O,1);
	  if ( nrows != -1)
	    {
	      if ( m != nrows )
		{
		  Scierror("column %d has wrong size %d expecting %d\n",count+1,m,nrows);
		  return FAIL;
		}
	    }
	  else nrows = m;
	  count +=nsp_object_get_size(cloc->O,2);
	}
      else
	{
	  Scierror("list element %d has a wrong type %s to fill a GtkListStore\n",
		   count+1,cloc->O->type->s_type());
	  return FAIL;
	}
      cloc = cloc->next;
    }
  *n_rows = nrows ;
  return OK;
}

/**
 * nsp_gtk_list_or_tree_store_fill_from_list:
 * @model: a #GtkTreeModel
 * @iter: a #GtkTreeIter
 * @parent: a #GtkTreeIter
 * @L: a #NspList.
 *
 *
 * fills @model (a #GtkTreeModel) with data given in list @L.
 * if iterator is given data is inserted at iterator position
 * if iterator is null data is inserted at the end and rows are
 * created accordingly.
 *
 * Returns: %OK or %FAIL.
 **/

int nsp_gtk_list_or_tree_store_fill_from_list(GtkTreeModel *model,GtkTreeIter *iter,
					      GtkTreeIter *parent, NspList *L)
{
  int count,n_rows=-1;
  Cell *cloc = L->first ;
  /* counts the rows and check compatibility */
  GtkTreeIter iter1;


  if (!GTK_IS_LIST_STORE(model) && !GTK_IS_TREE_STORE(model)) {
    Scierror("can not set cells in this tree model\n");
    return FAIL;
  }

  if ( nsp_list_count_rows(L,&n_rows)== FAIL) return FAIL;
  if ( n_rows == -1 ) return OK ;

  if ( iter == NULL)
    {
      int i;
      GtkTreeIter iter2;
      iter = &iter1;
      if (GTK_IS_LIST_STORE(model))
	{
	  gtk_list_store_append((GtkListStore *) model,iter);
	  /* fill the list store with enough rows */
	  for (i = 1; i < n_rows ; i++)  gtk_list_store_append((GtkListStore *)model, &iter2);
	}
      else
	{
	  gtk_tree_store_append((GtkTreeStore *)model,iter,parent);
	  /* fill the list store with enough rows */
	  for (i = 1; i < n_rows ; i++)  gtk_tree_store_append((GtkTreeStore *)model, &iter2,parent);
	}
    }
  else
    {
      GtkTreeIter *h_iter;
      int i;
      /* check that we have enough rows */
      if ((h_iter = gtk_tree_iter_copy(iter))== NULL)
	{
	  Scierror("Unable to allocate iterator \n");
	  return FAIL;
	}
      for ( i= 0 ; i < n_rows -1 ; i++)
	{
	  int rep = gtk_tree_model_iter_next(model, h_iter);
	  if (rep == FALSE)
	    {
	      /* need to append rows */
	      GtkTreeIter iter2;
	      int j;
	      if (GTK_IS_LIST_STORE(model))
		{
		  for (j= i ; j < n_rows -1 ; j++)
		    gtk_list_store_append((GtkListStore *) model, &iter2);
		  break;
		}
	      else
		{
		  for (j= i ; j < n_rows -1 ; j++)
		    gtk_tree_store_append((GtkTreeStore *)model, &iter2,parent);
		  break;
		}
	    }
	}
      gtk_tree_iter_free(h_iter);
    }
  /* now we fill the list_store rows using the list again */
  cloc = L->first;
  count=0;
  while ( cloc != NULLCELL)
    {
      if ( cloc->O != NULLOBJ )
	{
	  if ( IsNone(cloc->O) )
	    {
	      /* ignore */
	      count++;
	    }
	  else if ( IsMat(cloc->O) || IsMpMat(cloc->O) )
	    {
	      Mat2double((NspMatrix *) cloc->O); /* be sure that we are in a canonic mode */
	      if ( nsp_gtk_tree_model_set_col_from_mat(model,iter,(NspMatrix *) cloc->O,count)== FAIL)
		return FAIL;
	      count +=nsp_object_get_size(cloc->O,2);
	    }
	  else if ( IsSMat(cloc->O) )
	    {
	      if ( nsp_gtk_tree_model_set_col_from_smat(model,iter,(NspSMatrix *) cloc->O,count)== FAIL)
		return FAIL;
	      count +=nsp_object_get_size(cloc->O,2);
	    }
	  else if (IsBMat(cloc->O) )
	    {
	      if ( nsp_gtk_tree_model_set_col_from_bmat(model,iter,(NspBMatrix *) cloc->O,count)== FAIL)
		return FAIL;
	      count +=nsp_object_get_size(cloc->O,2);
	    }
	  else if ( IsList(cloc->O) )
	    {
	      if ( nsp_gtk_tree_model_set_col_from_list(model,iter,(NspList *) cloc->O,count)== FAIL)
		return FAIL;
	      count++;
	    }
	  else
	    {
	      Scierror("Cannot set the column %d with Object of type %s\n",count+1,cloc->O->type->s_type());
	      return FAIL;
	    }
	}
      cloc = cloc->next;
    }
  return OK;
}


/**
 * nsp_gtk_list_store_from_list:
 * @L: a #NspList.
 * @flag: an integer
 *
 * creates a #GtkListStore using #NspList @L to set the column types.
 * If @flag is %TRUE then the list is also used to fill the #GtkListStore
 *
 * Returns: a new #GtkListStore
 **/

GtkListStore *nsp_gtk_list_store_from_list(NspList *L,int flag)
{
  GtkListStore *ls;
  if (( ls = nsp_gtk_list_store_new_from_list(L)) == NULL) return NULL;
  if ( flag == TRUE )
    if ( nsp_gtk_list_or_tree_store_fill_from_list((GtkTreeModel *) ls,NULL,NULL,L)== FAIL) return NULL;
  return ls;
}

/**
 * nsp_gtk_tree_store_from_list:
 * @L:
 * @flag:
 *
 * creates a #GtkTreeStore using #NspList @L to set the column types.
 * If @flag is %TRUE then the list is also used to fill the #GtkTreeStore
 *
 * Returns: a new #GtkTreeStore
 **/

GtkTreeStore *nsp_gtk_tree_store_from_list(NspList *L,int flag )
{
  GtkTreeStore *ls;
  if (( ls = nsp_gtk_tree_store_new_from_list(L)) == NULL) return NULL;
  if ( flag == TRUE)
    if ( nsp_gtk_list_or_tree_store_fill_from_list((GtkTreeModel *) ls,NULL,NULL,L)== FAIL) return NULL;
  return ls;
}

/**
 * nsp_gtk_tree_model_set_col_from_list:
 * @model: a #GtkTreeModel
 * @iter1: a #GtkTreeIter
 * @L: a #NspList
 * @column: an integer
 *
 * fills a column in a GtkTreeModel from a #NspList.
 *
 * Returns: %OK or %FAIL.
 **/

int nsp_gtk_tree_model_set_col_from_list(GtkTreeModel *model,GtkTreeIter *iter1,NspList *L,int column)
{
  GtkTreeIter iter;
  gint n_columns, count=0;
  GValue value = { 0, };
  GType model_type =  gtk_tree_model_get_column_type(model,column);
  Cell *cloc = L->first ;

  if (!GTK_IS_LIST_STORE(model) && !GTK_IS_TREE_STORE(model)) {
    Scierror("can not set cells in this tree model\n");
    return FAIL;
  }
  /* items are given in a list */

  n_columns = gtk_tree_model_get_n_columns(model);
  if ( column > n_columns )
    {
      Scierror("column %d does not exists in given tree model (n_columns=%d)\n",column,n_columns);
      return FAIL;
    }

  if ( iter1 == NULL)
    {
      /* start from the begining */
      if (!gtk_tree_model_get_iter_first(model, &iter)) return FAIL;
    }
  else
    {
      iter = *iter1;
    }

  g_value_init(&value, model_type);
  while ( cloc != NULLCELL)
    {
      if ( cloc->O != NULLOBJ )
	{
	  if ( nspg_value_from_nspobject(&value, cloc->O)) {
	    Scierror("value of list element %d is of wrong type for column %d\n",count+1,column);
	    return FAIL;
	  }
	  if (GTK_IS_LIST_STORE(model))
	    gtk_list_store_set_value(GTK_LIST_STORE(model), &iter, column, &value);
	  else if (GTK_IS_TREE_STORE(model))
	    gtk_tree_store_set_value(GTK_TREE_STORE(model), &iter, column, &value);
	}
      cloc = cloc->next;
      count++;
      if ( gtk_tree_model_iter_next(model, &iter) == 0 && cloc != NULLCELL ) return FAIL;

    }
  g_value_unset(&value);
  return OK;
}

/*
 * Utility function fill a row (or a set of rows) in a GtkListStore or GtkTreeStore
 * given an iterator
 */

/**
 * nsp_gtk_tree_model_set_row:
 * @model: a #GtkTreeModel
 * @iter1: a #GtkTreeIter
 * @parent: a #GtkTreeIter
 * @items: a #NspList
 *
 * Utility function fill a row (or a set of rows) in a GtkListStore or GtkTreeStore
 * given an iterator
 *
 * Returns: %OK or %FAIL.
 **/

int nsp_gtk_tree_model_set_row(GtkTreeModel *model, GtkTreeIter *iter,GtkTreeIter *parent,NspList *items)
{
  return  nsp_gtk_list_or_tree_store_fill_from_list(model,iter,parent,items);
}


/*
 * Utility function: a list store from a generic Matrix
 * the list_store is created and filled row by row
 */

typedef void (*set_val)(GValue *value,NspObject *M,int row,int col) ;

static int nsp_gtk_tree_model_set_row_from_generic_matrix(GtkTreeModel *model, GtkTreeIter *iter,set_val F,NspObject *M,int row);

static GtkListStore *nsp_gtk_list_store_from_generic_matrix(NspObject *M,set_val F)
{
  int i;
  GtkListStore *ls;
  GtkTreeIter iter;
  GType type = gtype_from_nsp_object((NspObject *) M);
  GType *column_types;
  int columns=nsp_object_get_size(M,2);
  column_types = g_new(GType,columns);
  for (i = 0; i < columns ; i++)  column_types[i] = type;
  /* now we store the matrix in the list store */
  ls = gtk_list_store_newv(columns, column_types);
  g_free(column_types);
  if (! ls) {
    Scierror("could not create GtkListStore object\n");
    return NULL;
  }
  for (i = 0; i < nsp_object_get_size(M,1) ; i++)
    {
      gtk_list_store_append(ls, &iter);
      if ( nsp_gtk_tree_model_set_row_from_generic_matrix(GTK_TREE_MODEL(ls), &iter,F,M,i) == FAIL)
	return NULL;
    }
  return ls;
}

static GtkTreeStore *nsp_gtk_tree_store_from_generic_matrix(NspObject *M,set_val F)
{
  int i;
  GtkTreeStore *ts;
  GtkTreeIter iter;
  GType type = gtype_from_nsp_object((NspObject *) M);
  GType *column_types;
  int columns=nsp_object_get_size(M,2);
  column_types = g_new(GType,columns);
  for (i = 0; i < columns ; i++)  column_types[i] = type;
  /* now we store the matrix in the list store */
  ts = gtk_tree_store_newv(columns, column_types);
  g_free(column_types);
  if (! ts) {
    Scierror("could not create GtkListStore object\n");
    return NULL;
  }
  for (i = 0; i < nsp_object_get_size(M,1) ; i++)
    {
      gtk_tree_store_append(ts, &iter,NULL);
      if ( nsp_gtk_tree_model_set_row_from_generic_matrix(GTK_TREE_MODEL(ts), &iter,F,M,i) == FAIL)
	return NULL;
    }
  return ts;
}

/*
 * Utility function fill a row in a GtkListStore or GtkTreeStore from a scalar Matrix
 */

static int nsp_gtk_tree_model_set_row_from_generic_matrix(GtkTreeModel *model, GtkTreeIter *iter,set_val F,NspObject *M,int row)
{
  gint n_columns, i=0;
  GValue value = { 0, };
  GType type = gtype_from_nsp_object(M);
  if (!GTK_IS_LIST_STORE(model) && !GTK_IS_TREE_STORE(model)) {
    Scierror("can not set cells in this tree model\n");
    return FAIL;
  }
  /* items are given in a matrix  */

  n_columns = gtk_tree_model_get_n_columns(model);
  if ( n_columns > nsp_object_get_size(M,2) )
    {
      Scierror("Tree model and given object (%s) have incompatible columns length (%d > %d)\n",
	       M->type->s_type(), n_columns,nsp_object_get_size(M,2));
      return FAIL;
    }

  for ( i= 0 ; i < n_columns ; i++)
    {
      g_value_init(&value, gtk_tree_model_get_column_type(model,i));
      if ( G_VALUE_TYPE(&value) != type)
	{
	  Scierror("column %d in Tree model cannot accept values of type %s \n", M->type->s_type());
	  return FAIL;
	}
      (*F)(&value,M,row,i);
      if (GTK_IS_LIST_STORE(model))
	gtk_list_store_set_value(GTK_LIST_STORE(model), iter, i, &value);
      else if (GTK_IS_TREE_STORE(model))
	gtk_tree_store_set_value(GTK_TREE_STORE(model), iter, i, &value);
      g_value_unset(&value);
    }
  return OK;
}

/*
 * Utility function fill a set of columns in a GtkListStore or GtkTreeStore from a scalar Matrix
 * the columns of Matrix are used to fill the liststore column starting at column column
 * the rows are filled starting at iter1 position if iter1 != NULL
 */

static int nsp_gtk_tree_model_set_col_from_generic_matrix(GtkTreeModel *model,GtkTreeIter *iter1,set_val F,NspObject *M,int column)
{
  GtkTreeIter iter;
  gint n_columns, i=0,j;
  GValue value = { 0, };
  GType type = gtype_from_nsp_object((NspObject *) M);
  GType model_type;

  if (!GTK_IS_LIST_STORE(model) && !GTK_IS_TREE_STORE(model)) {
    Scierror("can not set cells in this tree model\n");
    return FAIL;
  }
  /* items are given in a matrix  */

  n_columns = gtk_tree_model_get_n_columns(model);


  for ( j = 0 ; j < nsp_object_get_size(M,2) ; j++)
    {
      if ( column >= n_columns )
	{
	  Scierror("column %d does not exists in given tree model (n_columns=%d)\n",column,n_columns);
	  return FAIL;
	}
      model_type =  gtk_tree_model_get_column_type(model,column);
      if ( model_type != type)
	{
	  Scierror("column %d in TreeModel cannot accept values of type %s \n",column, M->type->s_type());
	  return FAIL;
	}
      g_value_init(&value, model_type);
      if ( iter1 == NULL)
	{
	  /* start from the begining */
	  if (!gtk_tree_model_get_iter_first(model, &iter)) return FAIL;
	}
      else
	{
	  iter = *iter1;
	}
      for ( i= 0 ; i < nsp_object_get_size(M,1) ; i++)
	{
	  (*F)(&value,M,i,j);
	  if (GTK_IS_LIST_STORE(model))
	    gtk_list_store_set_value(GTK_LIST_STORE(model), &iter, column, &value);
	  else if (GTK_IS_TREE_STORE(model))
	    gtk_tree_store_set_value(GTK_TREE_STORE(model), &iter, column, &value);
	  if ( i < nsp_object_get_size(M,1)-1) { if (!gtk_tree_model_iter_next(model, &iter)) return FAIL; }
	}
      g_value_unset(&value);
      column++;
    }
  return OK;
}

/*
 * Utility function: a list store from a Scalar Real Matrix
 */

static void set_value_from_mat(GValue *value,NspObject *M,int row,int col)
{
  g_value_set_double(value,((NspMatrix *) M)->R[row+col*((NspMatrix *) M)->m]);
}

GtkListStore *nsp_gtk_list_store_from_mat(NspMatrix *M)
{
  return nsp_gtk_list_store_from_generic_matrix((NspObject *) M,set_value_from_mat);
}

GtkTreeStore *nsp_gtk_tree_store_from_mat(NspMatrix *M)
{
  return nsp_gtk_tree_store_from_generic_matrix((NspObject *) M,set_value_from_mat);
}

int nsp_gtk_tree_model_set_row_from_mat(GtkTreeModel *model, GtkTreeIter *iter,NspMatrix *M,int row)
{
  return nsp_gtk_tree_model_set_row_from_generic_matrix(model,iter,set_value_from_mat,(NspObject *) M,row);
}

int nsp_gtk_tree_model_set_col_from_mat(GtkTreeModel *model,GtkTreeIter *iter1,NspMatrix *M,int col)
{
  return nsp_gtk_tree_model_set_col_from_generic_matrix(model,iter1,set_value_from_mat,(NspObject *) M,col);
}

/*
 * Utility function: a list store from a String Matrix
 */

static void set_value_from_smat(GValue *value,NspObject *M,int row,int col)
{
  g_value_set_string(value,((NspSMatrix *) M)->S[row+col*((NspSMatrix *) M)->m]);
}

GtkListStore *nsp_gtk_list_store_from_smat(NspSMatrix *M)
{
  return nsp_gtk_list_store_from_generic_matrix((NspObject *) M,set_value_from_smat);
}

GtkTreeStore *nsp_gtk_tree_store_from_smat(NspSMatrix *M)
{
  return nsp_gtk_tree_store_from_generic_matrix((NspObject *) M,set_value_from_smat);
}

int nsp_gtk_tree_model_set_row_from_smat(GtkTreeModel *model, GtkTreeIter *iter,NspSMatrix *M,int row)
{
  return nsp_gtk_tree_model_set_row_from_generic_matrix(model,iter,set_value_from_smat,(NspObject *)M,row);
}

int nsp_gtk_tree_model_set_col_from_smat(GtkTreeModel *model,GtkTreeIter *iter1,NspSMatrix *M,int col)
{
  return nsp_gtk_tree_model_set_col_from_generic_matrix(model,iter1,set_value_from_smat,(NspObject *)M,col);
}

/*
 * Utility function: a list store from a Boolean Matrix
 */

static void set_value_from_bmat(GValue *value,NspObject *M,int row,int col)
{
  g_value_set_boolean(value,((NspBMatrix *) M)->B[row+col*((NspBMatrix *) M)->m]);
}

GtkListStore *nsp_gtk_list_store_from_bmat(NspBMatrix *M)
{
  return nsp_gtk_list_store_from_generic_matrix((NspObject *) M,set_value_from_bmat);
}

GtkTreeStore *nsp_gtk_tree_store_from_bmat(NspBMatrix *M)
{
  return nsp_gtk_tree_store_from_generic_matrix((NspObject *) M,set_value_from_bmat);
}

int nsp_gtk_tree_model_set_row_from_bmat(GtkTreeModel *model, GtkTreeIter *iter,NspBMatrix *M,int row)
{
  return nsp_gtk_tree_model_set_row_from_generic_matrix(model,iter,set_value_from_bmat,(NspObject *)M,row);
}

int nsp_gtk_tree_model_set_col_from_bmat(GtkTreeModel *model,GtkTreeIter *iter1, NspBMatrix *M,int col)
{
  return nsp_gtk_tree_model_set_col_from_generic_matrix(model,iter1,set_value_from_bmat,(NspObject *)M,col);
}

/*
 *--------------------------------------------------------------------------
 * Idle, timeout etc......
 *--------------------------------------------------------------------------
 */

static gboolean nsp_gtk_invoke_idle_timeout  (gpointer data);
static void nsp_gtk_destroy_closure (gpointer data);

guint
nsp_gtk_timeout_add_full (guint32  interval,
			  NspPList *callback,
			  NspList *extra_args,
			  NspObject *swap_data)
{
  GClosure *closure;
  closure = nspg_closure_new(callback, extra_args, swap_data );
  if ( closure == NULL) return 0;
  return g_timeout_add_full (0,interval,
			     nsp_gtk_invoke_idle_timeout,
			     closure,
			     nsp_gtk_destroy_closure);
}

static gboolean
nsp_gtk_invoke_idle_timeout (gpointer data)
{
  GClosure *closure = data;
  gint ret_val = TRUE;
  GValue ret = {0,};
  g_value_init(&ret, G_TYPE_BOOLEAN);
  /* invoke nspg_closure_marshal */
  closure->marshal(closure,&ret,0,NULL,NULL,NULL);
  if (G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(&ret))== G_TYPE_BOOLEAN)
    ret_val =  g_value_get_boolean(&ret);
  return ret_val;
}

static void
nsp_gtk_destroy_closure (gpointer data)
{
  /* GClosure *closure = data; */
  nspg_closure_invalidate(NULL,data);
  /*g_free (closure);*/
}

static int int_gtk_timeout_add(Stack stack,int rhs,int opt,int lhs)
{
  NspPList  *callback;
  NspList *extra_args = NULL;
  guint handlerid;
  int interval;

  CheckRhs(2,3);
  CheckLhs(1,1);

  if ( GetScalarInt(stack,1,&interval) == FAIL) return RET_BUG;
  /* Need a GetFunction here XXXXXX **/
  if (( callback = GetNspPListCopy(stack,2)) == NULLP_PLIST) return RET_BUG;
  if ((nsp_object_set_name((NspObject *) callback,"timoeout")== FAIL)) return RET_BUG;
  /* extra arguments **/
  if ( rhs == 3 )
    {
      if (( extra_args = GetListCopy(stack,3)) == NULLLIST ) return RET_BUG;
      if ((nsp_object_set_name((NspObject *)extra_args,"m")== FAIL)) return RET_BUG;
    }
  handlerid = nsp_gtk_timeout_add_full(interval,callback,extra_args,NULL);
  if ( nsp_move_double(stack,1,(double) handlerid) == FAIL) return RET_BUG;
  return 1;
}

/* this is in fact a g_source_destroy */

static int int_g_source_destroy(Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int id;
  if ( GetArgs(stack,rhs,opt,T,&id) == FAIL) return RET_BUG;
  g_source_destroy(g_main_context_find_source_by_id(NULL,id));
  return 0;
}

static int int_g_source_remove(Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int id;
  if ( GetArgs(stack,rhs,opt,T,&id) == FAIL) return RET_BUG;
  g_source_remove(id);
  return 0;
}



/*
 * idle
 */

guint
nsp_gtk_idle_add_full (guint priority,NspPList *callback, NspList *extra_args,NspObject *swap_data)
{
  GClosure *closure;
  closure = nspg_closure_new(callback, extra_args, swap_data );
  if ( closure == NULL) return 0;
  return g_idle_add_full (priority,nsp_gtk_invoke_idle_timeout,
			  closure, nsp_gtk_destroy_closure);
}

static int int_gtk_idle_add(Stack stack,int rhs,int opt,int lhs)
{
  NspPList  *callback;
  NspList *extra_args = NULL;
  guint handlerid;
  int priority;

  CheckRhs(2,3);
  CheckLhs(1,1);

  if ( GetScalarInt(stack,1,&priority) == FAIL) return RET_BUG;
  if ( priority > G_PRIORITY_DEFAULT_IDLE || priority < G_PRIORITY_HIGH_IDLE )
    {
      Scierror("Errpr: priority must be in the range [%d,%d] ([high,default])\n",
	       G_PRIORITY_HIGH_IDLE, G_PRIORITY_DEFAULT_IDLE);
      return RET_BUG;
    }
  /* Need a GetFunction here XXXXXX **/
  if (( callback = GetNspPListCopy(stack,2)) == NULLP_PLIST) return RET_BUG;
  if ((nsp_object_set_name((NspObject *) callback,"timoeout")== FAIL)) return RET_BUG;
  /* extra arguments **/
  if ( rhs == 3 )
    {
      if (( extra_args = GetListCopy(stack,3)) == NULLLIST ) return RET_BUG;
      if ((nsp_object_set_name((NspObject *)extra_args,"m")== FAIL)) return RET_BUG;
    }
  handlerid = nsp_gtk_idle_add_full(priority,callback,extra_args,NULL);
  if ( nsp_move_double(stack,1,(double) handlerid) == FAIL) return RET_BUG;
  return 1;
}

/* 
 * g_source_set_callback : associate a callback to a GSource 
 * this is tricky since the pattern to use for the callback depends on the source type 
 */

/* generic callback transmited to g_source_set_callback */

static gboolean nsp_gtk_invoke_g_source_callback (gpointer data)
{
  GClosure *closure = data;
  gint ret_val = TRUE;
  GValue ret = {0,};
  g_value_init(&ret, G_TYPE_BOOLEAN);
  /* invoke nspg_closure_marshal */
  closure->marshal(closure,&ret,0,NULL,NULL,NULL);
  if (G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(&ret))== G_TYPE_BOOLEAN)
    ret_val =  g_value_get_boolean(&ret);
  return ret_val;
}

/**
 * GSocketSourceFunc:
 * return %FALSE if the source should be removed.
 */

static gboolean nsp_gtk_invoke_g_source_callback_for_GSocket (GSocket *socket,GIOCondition condition, gpointer user_data)
{
  return nsp_gtk_invoke_g_source_callback (user_data);
}

/**
 * GDatagramBasedSourceFunc:
 * Returns: %G_SOURCE_REMOVE (false) if the source should be removed,
 *   %G_SOURCE_CONTINUE (true) otherwise
 */

static gboolean nsp_gtk_invoke_g_source_callback_for_GDatagramBased (GDatagramBased *datagram_based,
								     GIOCondition    condition,
								     gpointer        user_data)
{
  return nsp_gtk_invoke_g_source_callback (user_data);
}

/**
 * GCancellableSourceFunc:
 * Returns: it should return %FALSE if the source should be removed.
 */
static gboolean nsp_gtk_invoke_g_source_callback_for_GCancellable (GCancellable *cancellable,
								   gpointer      user_data)
{
  return nsp_gtk_invoke_g_source_callback (user_data);
}

/**
 * GPollableSourceFunc:
 * Returns: it should return %FALSE if the source should be removed.
 */

static gboolean nsp_gtk_invoke_g_source_callback_for_GPollable(GObject  *pollable_stream,
							       gpointer  user_data)
{
  return nsp_gtk_invoke_g_source_callback (user_data);
}

typedef enum { in_gsocket,  in_gdatagrambased , in_gcancellable, in_gpollable} _fun_tag;

extern int _wrap_g_source_set_callback(NspGSource *self,Stack stack,int rhs,int opt,int lhs)
{
  static const char *fun_list[] = {"GSocket","GDatagramBased", "GCancellable", "GPollable", NULL};
  NspObject *extra_args = NULL;
  NspPList  *callback;
  GClosure *closure;
  GSourceFunc fun = NULL;
  int rep;
  CheckRhs(2,3);
  CheckLhs(1,1);
  if ((rep= GetStringInArray(stack,1, fun_list,1)) == -1) return RET_BUG;
  /* Need a GetFunction here XXXXXX **/
  if (( callback = GetNspPListCopy(stack,2)) == NULLP_PLIST) return RET_BUG;
  if ((nsp_object_set_name((NspObject *) callback,"timoeout")== FAIL)) return RET_BUG;
  /* extra arguments **/
  switch (rep)
    {
    case in_gsocket: fun = (GSourceFunc) nsp_gtk_invoke_g_source_callback_for_GSocket;break;
    case in_gdatagrambased: fun = (GSourceFunc) nsp_gtk_invoke_g_source_callback_for_GDatagramBased;break;
    case in_gcancellable: fun = (GSourceFunc) nsp_gtk_invoke_g_source_callback_for_GCancellable ;break;
    case in_gpollable: fun = (GSourceFunc) nsp_gtk_invoke_g_source_callback_for_GPollable;break;
    }
  
  if ( rhs == 2 )
    {
      if (( extra_args = nsp_object_copy_and_name("m",nsp_get_object(stack,3)))== NULL) return RET_BUG;
    }
  closure = nspg_closure_new(callback, (NspList*) extra_args, NULL );
  if ( closure == NULL) return RET_BUG;
  g_source_set_callback(NSP_GBOXED_GET(self, GSource),(GSourceFunc) fun, closure, nsp_gtk_destroy_closure);
  return 0;
}

/*
 * quit add
 */

#if 0
static gboolean nsp_gtk_invoke_quit  (gpointer data);
#endif

guint
nsp_gtk_quit_add_full (guint main_level,  NspPList *callback,   NspList *extra_args,   NspObject *swap_data)
{
  GClosure *closure;
  closure = nspg_closure_new(callback, extra_args, swap_data );
  if ( closure == NULL) return 0;
  /* return gtk_quit_add_full(main_level,nsp_gtk_invoke_quit,NULL,closure, nspg_destroy_notify); */
  return 0;
}

#if 0
static gboolean
nsp_gtk_invoke_quit (gpointer data)
{
  GClosure *closure = data;
  gint ret_val = TRUE;
  GValue ret = {0,};
  /* invoke nspg_closure_marshal */
  closure->marshal(closure,&ret,0,NULL,NULL,NULL);
  if (G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(&ret))== G_TYPE_BOOLEAN)
    ret_val =  g_value_get_boolean(&ret);
  return ret_val;
}
#endif


static int int_gtk_quit_add(Stack stack,int rhs,int opt,int lhs)
{
  NspPList  *callback;
  NspList *extra_args = NULL;
  guint handlerid;
  int main_level;

  CheckRhs(2,3);
  CheckLhs(1,1);

  if ( GetScalarInt(stack,1,&main_level) == FAIL) return RET_BUG;
  /* Need a GetFunction here XXXXXX **/
  if (( callback = GetNspPListCopy(stack,2)) == NULLP_PLIST) return RET_BUG;
  if ((nsp_object_set_name((NspObject *) callback,"quitadd")== FAIL)) return RET_BUG;
  /* extra arguments **/
  if ( rhs == 3 )
    {
      if (( extra_args = GetListCopy(stack,3)) == NULLLIST ) return RET_BUG;
      if ((nsp_object_set_name((NspObject *)extra_args,"m")== FAIL)) return RET_BUG;
    }
  handlerid = nsp_gtk_quit_add_full(main_level, callback,extra_args,NULL);
  if ( nsp_move_double(stack,1,(double) handlerid) == FAIL) return RET_BUG;
  return 1;
}

/*-------------------------------------------------
 * gslist, glist and nsplist utilities
 *------------------------------------------------*/

GList *glist_from_typed_nsp_list(Stack stack,NspList *L,NspTypeBase *type)
{
  GList *items = NULL;
  Cell *cloc = L->first ;
  while ( cloc != NULLCELL)
    {
      if ( cloc->O != NULLOBJ )
	{
	  if (! nspgobject_check(cloc->O, type))
	    {
	      Scierror("%s:list item is not a %s",NspFname(stack),type_get_name(type));
	      g_list_free(items);
	      return NULL;
	    }
	  else
	    {
	      items = g_list_append(items, nspgobject_get(cloc->O));
	    }
	}
      cloc = cloc->next;
    }
  return items ;
}

GList *nsp_glist_from_nsplist(Stack stack,NspList *L)
{
  GList *items = NULL;
  Cell *cloc = L->first ;
  while ( cloc != NULLCELL)
    {
      if ( cloc->O != NULLOBJ )
	{
	  items = g_list_append(items, nspgobject_get(cloc->O));
	}
      cloc = cloc->next;
    }
  return items ;
}

GSList *nsp_gslist_from_nsplist(Stack stack,NspList *L)
{
  GSList *items = NULL;
  Cell *cloc = L->first ;
  while ( cloc != NULLCELL)
    {
      if ( cloc->O != NULLOBJ )
	{
	  items = g_slist_append(items, nspgobject_get(cloc->O));
	}
      cloc = cloc->next;
    }
  return items ;
}


/**
 * nsp_get_matrix_from_list_or_tree_store:
 * @model: a #GtkTreeModel
 *
 * extract a matrix with data from a model. This can only be done if the model
 * is homogeneous, i.e all the columns share the same type which can be
 * G_TYPE_STRING, G_TYPE_BOOLEAN or G_TYPE_DOUBLE.
 * This could be extended to non homogeneous models and could return a list
 * in that general case.
 *
 * Returns: a new #NspObject
 **/

static NspObject *nsp_get_matrix_from_list_or_tree_store(GtkTreeModel *model)
{
  GType mtype;
  GValue value = { 0, };
  int ncols,count=0,col;
  GtkTreeIter iter;
  NspObject *ret = NULLOBJ;
  if (!GTK_IS_LIST_STORE(model) && !GTK_IS_TREE_STORE(model)) {
    Scierror("Error: expecting a list or tree store model\n");
    return NULLOBJ;
  }
  /* get first element to detect the type to give to the matrix
   */
  if (!gtk_tree_model_get_iter_first(model, &iter)) return NULL;
  gtk_tree_model_get_value(model,&iter ,0, &value);
  mtype = G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(&value));
  if ( mtype != G_TYPE_STRING
       && mtype != G_TYPE_BOOLEAN
       && mtype != G_TYPE_DOUBLE )
    {
      Scierror("Do not know how to build an object from a gvalue of type %s\n",
	       g_type_name(G_VALUE_TYPE(&value)));
      return NULL;
    }
  g_value_unset(&value);
  /* count the rows and columns */
  count++;
  ncols= gtk_tree_model_get_n_columns(GTK_TREE_MODEL(model));
  while ( gtk_tree_model_iter_next(model,&iter) ) count++;
  /* Check that all the columns have the same type */
  if (!gtk_tree_model_get_iter_first(model, &iter)) return NULL;
  for ( col = 1 ; col < ncols  ; col++)
    {
      GType ctype;
      gtk_tree_model_get_value(model,&iter ,col, &value);
      ctype = G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(&value));
      g_value_unset(&value);
      if ( ctype != mtype )
	{
	  Scierror("Error: all the colmuns should be of the same type, column %d is wrong\n",col+1);
	  return NULL;
	}
    }

  /* initialize matrix */
  switch ( mtype )
    {
    case G_TYPE_STRING:
      if ((ret = (NspObject *) nsp_smatrix_create_with_length(NVOID,count,ncols,-1))== NULLOBJ)
	return NULL;
      break;
    case G_TYPE_BOOLEAN:
      if ((ret = (NspObject *) nsp_bmatrix_create(NVOID,count,ncols))== NULLOBJ)
	return NULL;
      break;
      break;
    case G_TYPE_DOUBLE :
      if ((ret = (NspObject *) nsp_matrix_create(NVOID,'r',count,ncols))== NULLOBJ)
	return NULL;
      break;
     }
  for ( col = 0 ; col < ncols  ; col++)
    {
      int i;
      if (!gtk_tree_model_get_iter_first(model, &iter)) return NULL;
      for ( i = 0 ; i < count ; i++)
	{
	  const gchar *str;
	  gtk_tree_model_get_value(model,&iter ,col, &value);
	  switch ( mtype )
	    {
	    case G_TYPE_STRING:
	      str = g_value_get_string(&value);
	      ((NspSMatrix *) ret)->S[i+ count*col]=nsp_string_copy(str);
	      if ( ((NspSMatrix *) ret)->S[i+ count*col] == NULL)
		return NULL;
	      break;
	    case G_TYPE_BOOLEAN:
	      ((NspBMatrix *) ret)->B[i+ count*col]= g_value_get_boolean(&value);
	      break;
	    case G_TYPE_DOUBLE:
	      ((NspMatrix *) ret)->R[i+ count*col]= g_value_get_double(&value);
	      break;
	    }
	  g_value_unset(&value);
	  gtk_tree_model_iter_next(model,&iter);
	}
    }
  if ( ret == NULLOBJ )
    {
      Scierror("Error: get_value method return a NULL Object \n");
      return NULL;
    }
  return ret;
}


NspObject *nsp_get_matrix_from_list_store(GtkListStore *model)
{
  return nsp_get_matrix_from_list_or_tree_store(GTK_TREE_MODEL(model));
}

NspObject *nsp_get_matrix_from_tree_store(GtkTreeStore *model)
{
  return nsp_get_matrix_from_list_or_tree_store(GTK_TREE_MODEL(model));
}

/**
 * nsp_get_list_from_list_or_tree_store:
 * @model: a #GtkTreeModel
 *
 * extract data from a model as a list with one list element for each column.
 *
 * Returns: a new #NspObject
 **/

static NspObject *nsp_get_list_from_list_or_tree_store(GtkTreeModel *model)
{
  GType mtype;
  GValue value = { 0, };
  int ncols,count=0,col,i;
  GtkTreeIter iter;
  NspList *L= NULLLIST;

  if (!GTK_IS_LIST_STORE(model) && !GTK_IS_TREE_STORE(model)) {
    Scierror("Error: expecting a list or tree store model\n");
    return NULLOBJ;
  }

  /* get first element to detect the type to give to the matrix
   */
  /* count the rows and columns */
  ncols= gtk_tree_model_get_n_columns(GTK_TREE_MODEL(model));
  if (!gtk_tree_model_get_iter_first(model, &iter)) return NULL;
  count++;
  while ( gtk_tree_model_iter_next(model,&iter) ) count++;
  /* count gives the number of rows */
  if ( (L=nsp_list_create(NVOID)) == NULLLIST ) return NULLOBJ;

  for (col = ncols-1 ; col >=0  ; col-- )
    {
      NspObject *Col=NULLOBJ;
      if (!gtk_tree_model_get_iter_first(model, &iter)) return NULL;
      gtk_tree_model_get_value(model,&iter ,col, &value);
      mtype = G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(&value));
      if ( mtype != G_TYPE_STRING
	   && mtype != G_TYPE_BOOLEAN
	   && mtype != G_TYPE_DOUBLE )
	{
	  Scierror("Do not know how to build an object from a gvalue of type %s\n",
		   g_type_name(G_VALUE_TYPE(&value)));
	  return NULL;
	}
      g_value_unset(&value);
      /* initialize proper object for the column  */
      switch ( mtype )
	{
	case G_TYPE_STRING:
	  if ((Col = (NspObject *) nsp_smatrix_create_with_length("le",count,1,-1))== NULLOBJ)
	    return NULL;
	  break;
	case G_TYPE_BOOLEAN:
	  if ((Col = (NspObject *) nsp_bmatrix_create("le",count,1))== NULLOBJ)
	    return NULL;
	  break;
	  break;
	case G_TYPE_DOUBLE :
	  if ((Col = (NspObject *) nsp_matrix_create("le",'r',count,1))== NULLOBJ)
	    return NULL;
	  break;
	}
      if (!gtk_tree_model_get_iter_first(model, &iter)) return NULL;
      for ( i = 0 ; i < count ; i++)
	{
	  const gchar *str;
	  gtk_tree_model_get_value(model,&iter ,col, &value);
	  switch ( mtype )
	    {
	    case G_TYPE_STRING:
	      str = g_value_get_string(&value);
	      ((NspSMatrix *) Col)->S[i]=nsp_string_copy(str);
	      if ( ((NspSMatrix *) Col)->S[i] == NULL)
		return NULL;
	      break;
	    case G_TYPE_BOOLEAN:
	      ((NspBMatrix *) Col)->B[i]= g_value_get_boolean(&value);
	      break;
	    case G_TYPE_DOUBLE:
	      ((NspMatrix *) Col)->R[i]= g_value_get_double(&value);
	      break;
	    }
	  g_value_unset(&value);
	  gtk_tree_model_iter_next(model,&iter);
	}
      /* now store the column in the returned list */
      if ( nsp_list_begin_insert(L,Col) == FAIL) return NULLOBJ;
    }
  if (L == NULLLIST )
    {
      Scierror("Error: get_value method return a NULL Object \n");
      return NULL;
    }
  return NSP_OBJECT(L);
}


NspObject *nsp_get_list_from_list_store(GtkListStore *model)
{
  return nsp_get_list_from_list_or_tree_store(GTK_TREE_MODEL(model));
}


NspObject *nsp_get_list_from_tree_store(GtkTreeStore *model)
{
  return nsp_get_list_from_list_or_tree_store(GTK_TREE_MODEL(model));
}

/**
 * nsp_cells_to_string:
 * @ce: a #NspCells
 *
 * A utility function for editvar, which converts the cells elements
 * to string. This is mainly used in editvar to fill the value field
 * of list or hash elements.
 *
 * Returns: a new #NspSMatrix
 **/

NspSMatrix * nsp_cells_to_string(NspCells *ce)
{
  int i;
  NspSMatrix *Loc;
  if ( ( Loc =nsp_smatrix_create_with_length(NVOID,ce->m,ce->n,-1)) == NULLSMAT)
    return(NULLSMAT);
  for ( i = 0 ; i < ce->mn ; i++)
    {
      if ( ce->objs[i] == NULLOBJ)
	{
	  if ((Loc->S[i] =nsp_string_copy("*")) == (nsp_string) 0) return(NULLSMAT);
	}
      else
	{
	  if (( IsMat(ce->objs[i]) ||  IsMpMat(ce->objs[i])) && ((NspMatrix *) ce->objs[i])->mn == 1 && ((NspMatrix *) ce->objs[i])->rc_type== 'r')
	    {
	      nsp_const_string format;
	      char buf[1024];
	      nsp_num_formats fmt;
	      nsp_init_pr_format (&fmt);
	      nsp_matrix_set_format(&fmt,((NspMatrix *) ce->objs[i])) ;
	      format = fmt.curr_real_fmt;
	      sprintf(buf,format,((NspMatrix *) ce->objs[i])->R[0]);
	      if ((Loc->S[i] =nsp_basic_to_string(buf)) == (nsp_string) 0)  return(NULLSMAT);
	    }

	  else if ( IsSMat(ce->objs[i]) && ((NspSMatrix *) ce->objs[i])->mn == 1)
	    {
	      if ((Loc->S[i] =nsp_string_copy(((NspSMatrix *) ce->objs[i])->S[0])) == (nsp_string) 0)
		return(NULLSMAT);
	    }
	  else if ( IsBMat(ce->objs[i]) && ((NspBMatrix *) ce->objs[i])->mn == 1)
	    {
	      const char *str = ((NspBMatrix *) ce->objs[i])->B[0] ? "T": "F";
	      if ((Loc->S[i] =nsp_string_copy(str)) == (nsp_string) 0)
		return(NULLSMAT);
	    }
	  else
	    {
	      if ((Loc->S[i] =nsp_string_copy("*")) == (nsp_string) 0) return(NULLSMAT);
	    }
	}
    }
  return(Loc);
}
