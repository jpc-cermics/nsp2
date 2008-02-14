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
 *
 *--------------------------------------------------------------------------*/

#include <nsp/object.h>
#include <gtk/gtk.h>

#define  GMarkupNode_Private 
#include "nsp/object.h"
#include "gmarkup.h"
#include "nsp/interf.h"
#include "nsp/plistc.h"

/* 
 * NspGMarkupNode inherits from NspObject 
 */

int nsp_type_gmarkup_node_id=0;
NspTypeGMarkupNode *nsp_type_gmarkup_node=NULL;

/*
 * Type object for GMarkupNode 
 * all the instance of NspTypeGMarkupNode share the same id. 
 * nsp_type_gmarkup_node: is an instance of NspTypeGMarkupNode 
 *    used for objects of NspGMarkupNode type (i.e built with new_gmarkup_node) 
 * other instances are used for derived classes 
 */
NspTypeGMarkupNode *new_type_gmarkup_node(type_mode mode)
{
  NspTypeGMarkupNode *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gmarkup_node != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gmarkup_node;
    }
  if ((type =  malloc(sizeof(NspTypeGMarkupNode))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gmarkup_node_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gmarkup_node_get_methods; 
  type->new = (new_func *) new_gmarkup_node;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gmarkup_node */ 

  top->pr = (print_func *) nsp_gmarkup_node_print;                  
  top->dealloc = (dealloc_func *) nsp_gmarkup_node_destroy;
  top->copy  =  (copy_func *) nsp_gmarkup_node_copy;                 
  top->size  = (size_func *) nsp_gmarkup_node_size;                
  top->s_type =  (s_type_func *) nsp_gmarkup_node_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_gmarkup_node_type_short_string;
  top->info = (info_func *) nsp_gmarkup_node_info ;                  
  /* top->is_true = (is_true_func  *) nsp_gmarkup_node_is_true; */
  /* top->loop =(loop_func *) nsp_gmarkup_node_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_gmarkup_node_object;
  top->eq  = (eq_func *) nsp_gmarkup_node_eq;
  top->neq  = (eq_func *) nsp_gmarkup_node_neq;
  top->save  = (save_func *) nsp_gmarkup_node_xdr_save;
  top->load  = (load_func *) nsp_gmarkup_node_xdr_load;
  top->create = (create_func*) int_gmarkup_node_create;
  
  /* specific methods for gmarkup_node */
      
  type->init = (init_func *) init_gmarkup_node;

  /* 
   * GMarkupNode interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gmarkup_node_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGMarkupNode called nsp_type_gmarkup_node
       */
      type->id =  nsp_type_gmarkup_node_id = nsp_new_type_id();
      nsp_type_gmarkup_node = type;
      if ( nsp_register_type(nsp_type_gmarkup_node) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gmarkup_node(mode);
    }
  else 
    {
      type->id = nsp_type_gmarkup_node_id;
      return type;
    }
}

/*
 * initialize GMarkupNode instances 
 * locally and by calling initializer on parent class 
 */

static int init_gmarkup_node(NspGMarkupNode *o,NspTypeGMarkupNode *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GMarkupNode 
 */

NspGMarkupNode *new_gmarkup_node() 
{
  NspGMarkupNode *loc; 
  /* type must exists */
  nsp_type_gmarkup_node = new_type_gmarkup_node(T_BASE);
  if ( (loc = malloc(sizeof(NspGMarkupNode)))== NULLMARKUPNODE) return loc;
  /* initialize object */
  if ( init_gmarkup_node(loc,nsp_type_gmarkup_node) == FAIL) return NULLMARKUPNODE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GMarkupNode 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_gmarkup_node_size(NspGMarkupNode *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char gmarkup_node_type_name[]="GMarkupNode";
static char gmarkup_node_short_type_name[]="gmn";

static char *nsp_gmarkup_node_type_as_string(void)
{
  return(gmarkup_node_type_name);
}

static char *nsp_gmarkup_node_type_short_string(NspObject *v)
{
  return(gmarkup_node_short_type_name);
}

/*
 * A == B 
 */

static int nsp_gmarkup_node_eq(NspGMarkupNode *A, NspObject *B)
{
  /* XXXX */
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_gmarkup_node_neq(NspGMarkupNode *A, NspObject *B)
{
  return ( nsp_gmarkup_node_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

static int nsp_gmarkup_node_xdr_save(XDR *xdrs, NspGMarkupNode *M)
{
  /* XXXX */
  return OK;
}

/*
 * load 
 */

static NspGMarkupNode  *nsp_gmarkup_node_xdr_load(XDR *xdrs)
{
  return NULL;
}

/*
 * delete 
 */

void nsp_gmarkup_node_destroy(NspGMarkupNode *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  /* XXXX */
  FREE(H);
}

/*
 * info 
 */

void nsp_gmarkup_node_info(NspGMarkupNode *M, int indent,const char *name, int rec_level)
{
  int i;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("%s\t= \t\t%s <%s>\n", pname, nsp_gmarkup_node_type_short_string(NSP_OBJECT(M)),M->name);
}

/*
 * print 
 */

int nsp_gmarkup_node_print(NspGMarkupNode *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(pname,NVOID) != 0) 
	{
	  Sciprintf1(indent,"%s=gmarkup_node_create();XXXXXXXXX",pname);
	}
      else 
	{
	  Sciprintf1(indent,"gmarkup_node_create();XXXXXXX");
	}
    }
  else 
    {
      int i;
      for ( i=0 ; i < indent ; i++) Sciprintf(" ");
      Sciprintf("%s\t= \t\t%s <%s>\n", pname, nsp_gmarkup_node_type_short_string(NSP_OBJECT(M)),M->name);
      nsp_object_print((NspObject *)M->children,indent+1,"",rec_level+1);
    }
  return TRUE;
}




/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GMarkupNode objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGMarkupNode   *nsp_gmarkup_node_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_gmarkup_node_id) == TRUE ) return ((NspGMarkupNode *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gmarkup_node));
  return NULL;
}

int IsGMarkupNodeObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gmarkup_node_id);
}

int IsGMarkupNode(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gmarkup_node_id);
}

NspGMarkupNode  *GetGMarkupNodeCopy(Stack stack, int i)
{
  if (  GetGMarkupNode(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGMarkupNode  *GetGMarkupNode(Stack stack, int i)
{
  NspGMarkupNode *M;
  if (( M = nsp_gmarkup_node_object(NthObj(i))) == NULLMARKUPNODE)
    ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspGMarkupNode *gmarkup_node_create_void(const char *name,NspTypeBase *type)
{
  NspGMarkupNode *H  = (type == NULL) ? new_gmarkup_node() : type->new();
  if ( H ==  NULLMARKUPNODE)
    {
      Sciprintf("No more memory\n");
      return NULLMARKUPNODE;
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULL)
    return NULLMARKUPNODE;
  NSP_OBJECT(H)->ret_pos = -1 ;
  H->name = NULL;
  H->children = NULL;
  H->attributes = NULL;
  return H;
}

NspGMarkupNode *gmarkup_node_create(const char *name,NspTypeBase *type)
{
  NspGMarkupNode *H  = gmarkup_node_create_void(name,type);
  if ( H ==  NULLMARKUPNODE) return NULLMARKUPNODE;
  return H;
}

/*
 * copy for gobject derived class  
 */

NspGMarkupNode *nsp_gmarkup_node_copy(NspGMarkupNode *self)
{
  NspGMarkupNode *H  =gmarkup_node_create_void(NVOID,(NspTypeBase *) nsp_type_gmarkup_node);
  if ( H ==  NULLMARKUPNODE) return NULLMARKUPNODE;
  if ((H->name =nsp_string_copy(self->name)) == (nsp_string) 0) return NULLMARKUPNODE;
  if ((H->children =(NspList *) nsp_object_copy_with_name(NSP_OBJECT(self->children)))== NULL) return NULLMARKUPNODE;
  if ( self->attributes != NULL ) 
    if ((H->attributes =(NspHash *) nsp_object_copy_with_name(NSP_OBJECT(self->attributes)))== NULL) return NULLMARKUPNODE;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the GMarkupNode
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_gmarkup_node_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGMarkupNode *H;
  CheckStdRhs(0,0);
  /* want to be sure that type gmarkup_node is initialized */
  nsp_type_gmarkup_node = new_type_gmarkup_node(T_BASE);
  if(( H = gmarkup_node_create_void(NVOID,(NspTypeBase *) nsp_type_gmarkup_node)) == NULLMARKUPNODE) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/* methods 
 *
 */


static int int_gmarkup_node_meth_get_name(NspGMarkupNode *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Ret;
  CheckRhs(0,0); 
  CheckLhs(1,1);
  if ((Ret = nsp_new_string_obj(NVOID,self->name,-1))== NULLOBJ) return RET_BUG;
  MoveObj(stack,1,Ret);
  return 1;
}

/* 
static int int_gmarkup_node_meth_get_op(NspGMarkupNode *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Ret;
  int ret ;
  CheckRhs(0,0);
  CheckLhs(1,1); 
  ret = ((int) self->obj->op);
  if ((Ret=nsp_new_double_obj((double) ret))== NULLOBJ) return RET_BUG;
  MoveObj(stack,1,Ret);
  return Max(lhs,1);
}

static int int_gmarkup_node_meth_get_opname(NspGMarkupNode *self, Stack stack, int rhs, int opt, int lhs)
{
  const char *str;
  NspObject *Ret;
  CheckRhs(0,0);
  CheckLhs(1,1); 
  switch ( ((int) self->obj->op)) 
    {
    case STRING: if ((Ret = nsp_new_string_obj(NVOID,"STRING",-1))== NULLOBJ) return RET_BUG;break;
    case COMMENT: if ((Ret = nsp_new_string_obj(NVOID,"COMMENT",-1))== NULLOBJ) return RET_BUG;break;
    case NUMBER: if ((Ret = nsp_new_string_obj(NVOID,"NUMBER",-1))== NULLOBJ) return RET_BUG;break;
    case NAME : if ((Ret = nsp_new_string_obj(NVOID,"NAME",-1))== NULLOBJ) return RET_BUG;break;
    case OPNAME : if ((Ret = nsp_new_string_obj(NVOID,"OPNAME",-1))== NULLOBJ) return RET_BUG;break;
    case OBJECT :  if ((Ret = nsp_new_string_obj(NVOID,"OBJECT",-1))== NULLOBJ) return RET_BUG;break;
    default:
      str=nsp_astcode_to_name(self->obj->op);
      if ( str != (char *) 0 )
	{
	  if ((Ret = nsp_new_string_obj(NVOID,str,-1))== NULLOBJ) return RET_BUG;
	}
      else 
	{
	  if ((Ret = (NspObject *) nsp_smatrix_create(NVOID,0,0,"v",(int)0)) == NULLOBJ) 
	    return RET_BUG;
	}
    }
  MoveObj(stack,1,Ret);
  return Max(lhs,1);
}

*/

static NspMethods gmarkup_node_methods[] = {
  {"get_node_name",(nsp_method *) int_gmarkup_node_meth_get_name},
  { NULL, NULL}
};

static NspMethods *gmarkup_node_get_methods(void) { return gmarkup_node_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_gmarkup_node_get_op(void *self,const char *attr)
{
  int ret = 0;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_gmarkup_node_set_op(void *self, char *attr, NspObject *O)
{
  int op;
  if ( IntScalar(O,&op) == FAIL) return FAIL;
  return OK;
}

static NspObject *_wrap_gmarkup_node_get_arity(void *self,const char *attr)
{
  int ret=0;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_gmarkup_node_set_arity(void *self, char *attr, NspObject *O)
{
  int arity;
  if ( IntScalar(O,&arity) == FAIL) return FAIL;
  return OK;
}

static NspObject *_wrap_gmarkup_node_get_attrs(void *Hv,const char *attr)
{
  return (NspObject *) ((NspGMarkupNode *) Hv)->attributes;
}

static NspObject *_wrap_gmarkup_node_get_object_attrs(void *Hv,const char *str,int *copy)
{
  *copy=FALSE;
  return (NspObject *) ((NspGMarkupNode *) Hv)->attributes;
}

static int _wrap_gmarkup_node_set_attrs(void *Hv,const char *attr, NspObject *O)
{
  NspHash *m;
  if ((m = (NspHash *) nsp_object_copy_and_name(attr,O)) == NULLHASH) return RET_BUG;
  /* here we must clear the old value and check that the elements are correct */
  ((NspGMarkupNode *) Hv)->attributes = m;
  return OK ;
}

static NspObject *_wrap_gmarkup_node_get_children(void *Hv,const char *attr)
{
  return (NspObject *) ((NspGMarkupNode *) Hv)->children;
}

static NspObject *_wrap_gmarkup_node_get_object_children(void *Hv,const char *str,int *copy)
{
  *copy=FALSE;
  return (NspObject *) ((NspGMarkupNode *) Hv)->children;
}

static int _wrap_gmarkup_node_set_children(void *Hv,const char *attr, NspObject *O)
{
  NspList *m;
  if ((m = (NspList *) nsp_object_copy_and_name(attr,O)) == NULLLIST) return RET_BUG;
  /* here we must clear the old value and check that the elements are correct */
  ((NspGMarkupNode *) Hv)->children = m;
  return OK ;
}


static AttrTab gmarkup_node_attrs[] = {
  { "op", (attr_get_function *)_wrap_gmarkup_node_get_op, (attr_set_function *)_wrap_gmarkup_node_set_op,
    (attr_get_object_function *)int_get_object_failed,(attr_set_object_function *)int_set_object_failed },
  { "arity", (attr_get_function *)_wrap_gmarkup_node_get_arity, (attr_set_function *)_wrap_gmarkup_node_set_arity,
    (attr_get_object_function *)int_get_object_failed,(attr_set_object_function *)int_set_object_failed },
  { "attributes", _wrap_gmarkup_node_get_attrs,  _wrap_gmarkup_node_set_attrs,
    _wrap_gmarkup_node_get_object_attrs, (attr_set_object_function *)int_set_object_failed},
  { "children", _wrap_gmarkup_node_get_children, _wrap_gmarkup_node_set_children, 
    _wrap_gmarkup_node_get_object_children,  (attr_set_object_function *)int_set_object_failed},

  { NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/

#include <ctype.h>
#include <glib/gfileutils.h>
#include <glib/gmarkup.h>
#include <glib/gmem.h>
#include <glib/gmessages.h>
#include <glib/gstrfuncs.h>

typedef struct _GMarkupDomContext GMarkupDomContext;

struct _GMarkupDomContext
{
  const gchar *node_name;
  NspGMarkupNode *root;
  NspGMarkupNode *current_root;
  NspGMarkupNode *current;
};

static void xml_start_element (GMarkupParseContext *context, const gchar *element_name,
			       const gchar **attribute_names,
			       const gchar **attribute_values, gpointer user_data,
			       GError **error)
{
  int count,i;
  const gchar **atr;
  GMarkupDomContext *dom_context = user_data;
  NspGMarkupNode *node = NULL;
  const char *name= ( dom_context->root == NULL) ? NVOID: "node";

  if ((node = gmarkup_node_create(name,NULL)) == NULL) return ; 
  if ((node->name =nsp_string_copy(element_name)) == (nsp_string) 0) return ;
  node->gm_father = dom_context->current_root;
  node->children= nsp_list_create("ch");
  dom_context->current = node;

  if ( dom_context->root == NULL) dom_context->root = node;
  
  if ( dom_context->current_root != NULL)
    {
      if (  nsp_list_end_insert(dom_context->current_root->children,(NspObject *)node) == FAIL) return;
    }
  dom_context->current_root = node;

  atr = attribute_names;  count=0;
  while ( *atr++ != NULL) count++;
  if(( node->attributes = nsp_hash_create("attributes",count)) == NULLHASH) return;

  for ( i=0; i < count ; i++)
    {
      NspObject *Obj; 
      if (( Obj = nsp_new_string_obj(attribute_names[i],attribute_values[i],-1)) == NULLOBJ )
	return;
      if (nsp_hash_enter( node->attributes,Obj) == FAIL) return;
    }
}


static void xml_end_element (GMarkupParseContext *context,
                             const gchar *element_name, gpointer user_data,
                             GError **error)
{
  GMarkupDomContext *dom_context = user_data;
  g_return_if_fail (dom_context != NULL);
  g_return_if_fail (dom_context->current != NULL);
  dom_context->current_root =dom_context->current= dom_context->current->gm_father;
}

static void xml_text (GMarkupParseContext *context, const gchar *text,
                      gsize text_len, gpointer user_data, GError **error)
{
  NspObject *Obj;
  GMarkupDomContext *dom_context = user_data;
  if ( text_len == 0) return;
  if ( text_len != strlen(text) )
    {
      Sciprintf("Warning text_len != strlen(text)_n");
    }
  if ((Obj = nsp_new_string_obj("text",text,-1)) == NULLOBJ ) return;
  if ( dom_context->current_root != NULL)
    {
      if (  nsp_list_end_insert(dom_context->current_root->children,Obj) == FAIL) 
	return;
    }
}

static void xml_passthrough (GMarkupParseContext *context, const gchar *text,
                      gsize text_len, gpointer user_data, GError **error)
{
  NspObject *Obj;
  GMarkupDomContext *dom_context = user_data;
  if ((Obj = nsp_new_string_obj("text",text,-1)) == NULLOBJ ) return;
  if ( dom_context->current_root != NULL)
    {
      if (  nsp_list_end_insert(dom_context->current_root->children,Obj) == FAIL) 
	return;
    }
}

/**
 * g_markup_dom_new:
 * @file_name: name of a file to parse contents from.
 * @node_name: name of node to be searched, or %NULL.
 * @error: return location for a #GError, or %NULL.
 *
 * Create a dom tree of @filename content. If @node_name is 
 * givent then a node is returned with all ocurrences of 
 * @node_name sub-trees as children. 
 *
 * Return value: a new #NspGMarkupNode
 **/

NspGMarkupNode *g_markup_dom_new (const gchar *filename,const gchar *node_name, GError **error)
{
  GMarkupParseContext *markup_parse_context = NULL;
  GMarkupDomContext context = {node_name,NULL, NULL, NULL};
  
  g_return_val_if_fail (filename != NULL, context.root);

  {
    GMarkupParser markup_parser;

    markup_parser.start_element = xml_start_element;
    markup_parser.end_element = xml_end_element;
    markup_parser.text = xml_text;
    markup_parser.passthrough = xml_passthrough ;
    markup_parser.error = NULL;
    /* G_MARKUP_TREAT_CDATA_AS_TEXT, */
    markup_parse_context = g_markup_parse_context_new (&markup_parser,0,&context, NULL);
  }
  
  if ( node_name != NULL)
    {
      NspGMarkupNode *node = NULL;
      if ((node = gmarkup_node_create(NVOID,NULL)) == NULL) return NULL; 
      if ((node->name =nsp_string_copy("request")) == (nsp_string) 0) return NULL ;
      node->children= nsp_list_create("ch");
      context.root = node;
    }
  
  {
    gchar *text = NULL;
    gsize length = -1;

    g_file_get_contents (filename, &text, &length, error);
    if (text != NULL)
    {
      g_markup_parse_context_parse (markup_parse_context, text, length, error);
      g_free (text), text = NULL;
    }
    g_free (markup_parse_context), markup_parse_context = NULL;
  }
  return context.root;
}

int int_gmarkup(Stack stack, int rhs, int opt, int lhs)
{
  char *str,*node_name=NULL;
  NspGMarkupNode *node;
  CheckStdRhs(1,2);
  if ((str=GetString(stack,1))== NULL) return RET_BUG;
  if ( rhs == 2 )
    if ((node_name=GetString(stack,2))== NULL) return RET_BUG;
  if ((node = g_markup_dom_new (str,node_name, NULL))== NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(node));
  return 1;
} 

int int_gmarkup_escape_text(Stack stack, int rhs, int opt, int lhs)
{
  char *str;
  gchar *res;
  CheckStdRhs(1,1);
  if ((str=GetString(stack,1))== NULL) return RET_BUG;
  if ((res=g_markup_escape_text(str,strlen(str))) == NULL) return RET_BUG;
  if ( nsp_move_string(stack,1,res,-1) == FAIL) return RET_BUG;
  return 1;
} 

gchar*      g_markup_escape_text            (const gchar *text,
                                             gssize length);
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab gmarkup_node_func[]={
  { "gmarkup", int_gmarkup},
  { "gmarkup_escape_text", int_gmarkup_escape_text},
  { NULL, NULL}
};

/* call ith function in the gmarkup_node interface */

int gmarkup_node_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(gmarkup_node_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) */

void gmarkup_node_Interf_Info(int i, char **fname, function (**f))
{
  *fname = gmarkup_node_func[i].name;
  *f = gmarkup_node_func[i].fonc;
}

