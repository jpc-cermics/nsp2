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
 *
 *--------------------------------------------------------------------------*/

#include <ctype.h>
#include <gtk/gtk.h>
#define  GMarkupNode_Private 
#include <nsp/nsp.h>
#include <nsp/object.h>
#include <nsp/smatrix.h>
#include <nsp/bmatrix.h>
#include <nsp/list.h>
#include <nsp/hash.h>
#include <nsp/hobj.h>
#include <nsp/file.h>
#include <nsp/type.h>
#include <nsp/interf.h>
#include <nsp/plistc.h>
#include "gmarkup.h"

static int nsp_g_markup_collect(NspList *L,const char *needle,NspGMarkupNode *node, int recursive);

/* 
 * #include <glib/gfileutils.h>
 * #include <glib/gmarkup.h>
 * #include <glib/gmem.h>
 * #include <glib/gmessages.h>
 * #include <glib/gstrfuncs.h>
 */

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
  type->gtk_methods = FALSE;
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
 * XXXX unfinished 
 */

static int nsp_gmarkup_node_eq(NspGMarkupNode *A, NspObject *B)
{
  NspGMarkupNode *nB = (NspGMarkupNode *)B;
  if (check_cast (B,nsp_type_gmarkup_node_id) == FALSE)
    return FALSE;
  if (strcmp( A->name, nB->name) != 0 ) return FALSE; 
  if ( NSP_OBJECT(A->children)->type->eq(A->children,nB->children) == FALSE )
    return FALSE;
  if ( NSP_OBJECT(A->attributes)->type->eq(A->attributes,nB->attributes) == FALSE )
    return FALSE;
  /* we do not compare the gm_father */
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
  nsp_string_destroy(&H->name);
  nsp_list_destroy(H->children);
  nsp_hash_destroy(H->attributes);
  nsp_object_destroy_name(NSP_OBJECT(H));
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
  Sciprintf("%s\t= <%s>\t%s\n", pname, M->name, nsp_gmarkup_node_type_short_string(NSP_OBJECT(M)));
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
      Sciprintf("%s\t= <%s>\t%s\n", pname,M->name,
		nsp_gmarkup_node_type_short_string(NSP_OBJECT(M)));
      if ( M->attributes != NULL)
	{
	  nsp_object_print((NspObject *)M->attributes,indent+1,NULL,rec_level+1);
	}
      nsp_object_print((NspObject *)M->children,indent+1,NULL,rec_level+1);
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

  if ( H->attributes == NULL )
    {
      if((H->attributes = nsp_hash_create("attributes",4)) == NULLHASH) return RET_BUG;
    }
  if ( H->children == NULL)
    {
      if ((H->children= nsp_list_create("children"))== NULL) return RET_BUG;
    }
  if ( H->name == NULL )
    {
      if ((H->name =nsp_string_copy("void")) == (nsp_string) 0) return RET_BUG;
    }
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

/* WIP: must be sure that characters are escaped and that 
 * uf8 is used for output
 */

#define BUF_SIZE 512

static NspSMatrix *smatrix_from_gmarkup_node(int indent, NspGMarkupNode *self)
{
  Cell *C;
  NspHash *H = self->attributes;
  NspList *L = self->children;
  char buf[BUF_SIZE], *str;
  NspObject *Res,*Str;
  int n=0, i1;
  str = buf;
  for ( i1=0 ; i1 < indent; i1++) { str[i1]=' ';}
  n = indent;
  str +=n;
  n= snprintf(str,(&buf[BUF_SIZE-1] -str),"<%s",self->name);
  str += n;
  for ( i1 =0 ; i1 <= H->hsize ; i1++) 
    {
      Hash_Entry *loc = ((Hash_Entry *) H->htable) + i1;
      if ( loc->used && loc->data != NULLOBJ && IsSMat(loc->data) ) 
	{
	  char *txt =g_markup_escape_text(((NspSMatrix *) loc->data)->S[0],-1);
	  n= snprintf(str,(size_t) (&buf[BUF_SIZE-1] -str)," %s=\"%s\" ",
		      nsp_object_get_name(loc->data),
		      txt);
	  g_free(txt);
	  if ( n >= (size_t) (&buf[BUF_SIZE-1] -str) )
	    {
	      Sciprintf("Error: output was truncated attributes are too long \n");
	      buf[BUF_SIZE-1] = '\0';break;
	    }
	  str += n;
	}
    }
  n=snprintf(str,(&buf[BUF_SIZE-1] -str),">");
  if ((Res = nsp_create_object_from_str(NVOID,buf)) == NULL) return NULL;
  C= L->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  if ( IsGMarkupNode(C->O) )
	    {
	      NspSMatrix *Loc;
	      Loc = smatrix_from_gmarkup_node(indent+2,(NspGMarkupNode *) C->O);
	      if (Loc == NULL ) return NULL;
	      if (nsp_smatrix_concat_down1((NspSMatrix *) Res,(NspSMatrix *) Loc,TRUE) != OK)
		return NULL;
	    }
	  else if ( IsSMat(C->O) )
	    {
	      NspSMatrix *S= (NspSMatrix *) C->O;
	      for (i1=0 ; i1 < S->mn; i1++)
		{
		  int k, ok;
		  char *txt,*start,*stop;
		  ok = FALSE;
		  for ( k=0 ; k < strlen(S->S[i1]) ; k++)
		    {
		      if ( !( S->S[i1][k] == ' ' || S->S[i1][k] == '\t' || S->S[i1][k] == '\n')) {ok=TRUE;break;}
		    }
		  if (! ok) continue;
		  /* cut S->S[i1] into pieces */
		  start = S->S[i1];
		  while (1)
		    {
		      str = buf;
		      stop = strstr(start,"\n");
		      if ( stop != NULL) *stop='\0';
		      for ( k=0 ; k < indent; k++) { str[k]=' ';}
		      n = indent;
		      str +=n;
		      txt =g_markup_escape_text( start,-1);
		      snprintf(str,(&buf[BUF_SIZE-1] -str),"%s",txt);
		      g_free(txt);
		      if ((Str = nsp_create_object_from_str(NVOID,buf)) == NULL) return NULL;
		      if (nsp_smatrix_concat_down1((NspSMatrix *) Res,(NspSMatrix *) Str,TRUE) != OK)
			return NULL;
		      if ( stop == NULL) break;
		      *stop = '\n';
		      start = stop +1;
		    }
		}
	    }
	}
      C = C->next;
    }
  str = buf;
  for ( i1=0 ; i1 < indent; i1++) { str[i1]=' ';}
  n= indent;
  str += n;
  n= snprintf(str,BUF_SIZE-1-n,"</%s>",self->name);
  if ((Str = nsp_create_object_from_str(NVOID,buf)) == NULL) return NULL;
  if (nsp_smatrix_concat_down1((NspSMatrix *) Res,(NspSMatrix *)Str,TRUE) != OK) return NULL;
  return (NspSMatrix *) Res;
}

static int int_gmarkup_node_sprintf(NspGMarkupNode *self, Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *Res;
  CheckRhs(0,0); 
  CheckLhs(1,1);
  if (( Res = smatrix_from_gmarkup_node(0,self))== NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}

static int int_gmarkup_collect(NspGMarkupNode *self, Stack stack, int rhs, int opt, int lhs)
{
  int recursive=FALSE;
  NspList *L;
  char *name=NULL;
  nsp_option opts[] ={{ "recursive",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  int_types T[] = {string, new_opts, t_end} ;
  CheckStdRhs(1,1);
  CheckLhs(1,1);
  if ( GetArgs(stack,rhs,opt,T,&name,opts,&recursive) == FAIL) return RET_BUG;
  if ( (L= nsp_list_create(NVOID))== NULL) return RET_BUG;
  if ( nsp_g_markup_collect (L,name,self,recursive) == FAIL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(L));
  return Max(lhs,1);
}

static NspMethods gmarkup_node_methods[] = {
  {"get_node_name",(nsp_method *) int_gmarkup_node_meth_get_name},
  {"sprintf",(nsp_method *) int_gmarkup_node_sprintf },
  {"collect", (nsp_method *) int_gmarkup_collect },
  { NULL, NULL}
};
  
    
static NspMethods *gmarkup_node_get_methods(void) { return gmarkup_node_methods;};

/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_gmarkup_node_get_name(void *self,const char *attr)
{
  return nsp_new_string_obj(NVOID,((NspGMarkupNode *) self)->name,-1);
}

static int _wrap_gmarkup_node_set_name(void *self,const char *attr, NspObject *Obj)
{
  NspSMatrix *S= (NspSMatrix *) Obj;
  char *str;
  if ( ! IsSMat(Obj) ) return FAIL;
  if ( S->mn != 1  ) return FAIL;
  if ((str = nsp_string_copy(S->S[0])) == (nsp_string) 0) return FAIL;
  nsp_string_destroy(&((NspGMarkupNode *) self)->name);
  ((NspGMarkupNode *) self)->name=str;
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
  { "name", (attr_get_function *)_wrap_gmarkup_node_get_name, (attr_set_function *) 
    _wrap_gmarkup_node_set_name,
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

typedef struct _GMarkupDomContext GMarkupDomContext;

struct _GMarkupDomContext
{
  const gchar *node_name;
  NspGMarkupNode *root;
  NspGMarkupNode *current_root;
  NspGMarkupNode *current;
  int ignore_white;
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
  node->children= nsp_list_create("children");
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

static int xml_text_is_spaces(const char *str)
{
  while (1)
    {
      switch (*str) 
	{
	case '\n':
	case '\t':
	case ' ':
	  str++;
	  break;
	case '\0': 
	  return TRUE;
	  break;
	default:
	  return FALSE;
	  break;
	}
    }
  return TRUE;
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
  if (dom_context->ignore_white == TRUE 
      && xml_text_is_spaces(text) == TRUE ) 
    return;
  if ((Obj = nsp_new_string_obj("text",text,-1)) == NULLOBJ ) return;
  /* printf("Entering a xml_text {%s}\n",text);  */
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
  /* printf("Entering a passthrough %s\n",text); */
  if ((Obj = nsp_new_string_obj("text",text,-1)) == NULLOBJ ) return;
  if ( dom_context->current_root != NULL)
    {
      if (  nsp_list_end_insert(dom_context->current_root->children,Obj) == FAIL) 
	return;
    }
}

/* remove empty strings 
 * in clildren 
 */

static int nsp_g_markup_node_is_empty_str(NspObject *Obj)
{
  int i;
  char *str = ((NspSMatrix *) Obj)->S[0];
  for ( i = 0 ; i < strlen(str) ; i++)
    if ( ! isspace(str[i])) return FALSE;
  return TRUE;
}

static void nsp_g_markup_clean(NspGMarkupNode *node)
{
  Cell *Loc = NULL, *Loc1 = NULL; 
  NspList *L;
  if ( node == NULL) return ;
  L= node->children;
  L->icurrent = 0;
  L->current = NULLCELL;
  Loc = L->first;
  while ( Loc != NULLCELL ) 
    {
      int delete = FALSE;
      Loc1 = Loc;
      if ( Loc->O != NULLOBJ )
	{
	  if  ( IsString(Loc->O) && nsp_g_markup_node_is_empty_str(Loc->O) )
	    {
	      delete = TRUE;
	      if ( Loc->prev != NULL) Loc->prev->next = Loc->next; 
	      if ( Loc->next != NULL) Loc->next->prev = Loc->prev;
	      if ( L->first == Loc ) L->first = Loc->next;
	      if ( L->last == Loc )  L->last = Loc->prev;
	    }
	  else
	    {
	      if ( IsGMarkupNode(Loc->O ))
		{
		  nsp_g_markup_clean((NspGMarkupNode *) Loc->O );
		}
	    }
	}
      Loc = Loc->next;
      if (delete == TRUE)
	{
	  nsp_cell_destroy(&Loc1);
	  L->nel--;
	}
    }
}

static int nsp_g_markup_collect(NspList *L,const char *needle,NspGMarkupNode *node, int recursive)
{
  NspObject *Obj;
  if ( strcmp(node->name,needle)==0)
    {
      if ((Obj =  nsp_object_copy_and_name("elt",(NspObject *)node)) == NULL)
	return FAIL;
      if ( nsp_list_end_insert(L, Obj) == FAIL) return FAIL;
    }
  if ( strcmp(node->name,needle)!= 0 || recursive == TRUE)
    {
      Cell *C = node->children->first;
      while ( C != NULLCELL) 
	{
	  if ( C->O != NULLOBJ && IsGMarkupNode(C->O ) )
	    {
	      int rep = nsp_g_markup_collect(L,needle, (NspGMarkupNode *) C->O,recursive);
	      if (rep == FAIL) return FAIL;
	    }
	  C = C->next;
	}
    }
  return OK;
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

NspGMarkupNode *g_markup_dom_new (const gchar *filename,const gchar *node_name,int *err)
{
  gchar *text = NULL;
  gsize length = -1;
  GMarkupParser markup_parser;
  GMarkupParseContext *markup_parse_context = NULL;
  GMarkupDomContext context = {node_name,NULL, NULL, NULL, FALSE};
  GError *lerror = NULL;
  g_return_val_if_fail (filename != NULL, context.root);

  markup_parser.start_element = xml_start_element;
  markup_parser.end_element = xml_end_element;
  markup_parser.text = xml_text;
  markup_parser.passthrough = xml_passthrough ;
  markup_parser.error = NULL;
  /* G_MARKUP_TREAT_CDATA_AS_TEXT, */
  markup_parse_context = g_markup_parse_context_new (&markup_parser,0,&context, NULL);
  
  if ( node_name != NULL)
    {
      NspGMarkupNode *node = NULL;
      if ((node = gmarkup_node_create(NVOID,NULL)) == NULL) return NULL; 
      if ((node->name =nsp_string_copy(node_name)) == (nsp_string) 0) return NULL ;
      node->children= nsp_list_create("children");
      context.root = node;
    }
  g_file_get_contents (filename, &text, &length, &lerror);
  if (text != NULL)
    {
      int tag=g_markup_parse_context_parse (markup_parse_context, text, length, &lerror);
      if ( tag == FALSE ) 
	{
	  Scierror("Error: failed to parse markup file %s\n",filename);
	  Scierror("\t%s\n",lerror->message);
	  g_error_free(lerror);
	  *err=TRUE;
	}
      else
	{
	  *err=FALSE;
	}
      g_free (text), text = NULL;
    }
  g_markup_parse_context_free (markup_parse_context);
  markup_parse_context = NULL;
  return context.root;
}

int int_gmarkup(Stack stack, int rhs, int opt, int lhs)
{
  int err;
  NspGMarkupNode *node;
  int clean = FALSE;
  char *fname=NULL,*node_name=NULL;
  int_types T[] = {string,new_opts, t_end} ;
  nsp_option opts[] ={{ "clean_strings",s_bool,NULLOBJ,-1},
		      { "node",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,1);
  CheckLhs(1,1);
  if ( GetArgs(stack,rhs,opt,T,&fname,&opts,&clean,&node_name) == FAIL)
    return RET_BUG;

  if ((node = g_markup_dom_new (fname,node_name,&err))== NULL) return RET_BUG;
  if ( err == TRUE ) 
    {
      nsp_gmarkup_node_destroy(node);
      return RET_BUG;
    }
  else
    {
      if ( clean == TRUE ) nsp_g_markup_clean(node);
      MoveObj(stack,1,NSP_OBJECT(node));
    }
  return Max(lhs,1);
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


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab gmarkup_node_func[]={
  { "gmarkup", int_gmarkup},
  { "gmarkup_node_create",int_gmarkup_node_create},
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

