/* -*- Mode: C -*- */
#ifndef INC_NSP_NspGObject
#define INC_NSP_NspGObject

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/
  
/* class NspGObject : wrapper for Gtk GObjects  */

#include <stdio.h>   /** for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"

#include <gtk/gtk.h> 
#include <glib.h>
#include <glib-object.h>

/*
 * NspGObject inherits from NspObject 
 */

typedef struct _nsp_gobject NspGObject;

typedef int (*gobject_save) (NspFile  *F, NspGObject *M);

typedef struct _nsp_type_NspGObject { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
} NspTypeGObject;

struct _nsp_gobject {
  NspObject father; 
  NspTypeGObject *type;  
  GObject *obj;
  GSList *closures;
  NspTypeBase *obj_type; /* XXXXXX */
};

extern int nsp_type_gobject_id;
extern NspTypeGObject *nsp_type_gobject;

NspTypeGObject *new_type_gobject(type_mode mode);

NspGObject *new_gobject();

/*
 * Object methods redefined for gobject 
 */

#ifdef NspGObject_Private 
static int init_gobject(NspGObject *o,NspTypeGObject *type);
static int gobject_size(NspGObject *Mat, int flag);
static char *gobject_type_as_string(void);
static char *gobject_type_short_string(NspObject *v);
static int gobject_eq(NspGObject *A, NspObject *B);
static int gobject_neq(NspGObject *A, NspObject *B);
static int gobject_xdr_save(XDR  *F, NspGObject *M);
static NspGObject  *gobject_xdr_load(XDR  *F);
static NspMethods *gobject_get_methods(void); 
static NspObject *gobject_path_extract(NspGObject *A,int n, NspObject **Objs, int *copy);
#endif /* NspGObject_Private */

#define NULLGOBJECT (NspGObject*) 0

/* utilities */

#define NSP_GOBJECT_GET(ob) ((NspGObject *) ob)->obj 

extern NspGObject *gobject_create(const char *name,  GObject *obj, NspTypeBase *type);
extern NspGObject *gobject_gettype_and_create(char *name,  GObject *obj);
extern NspGObject *gobject_copy(NspGObject *H);
extern void gobject_destroy(NspGObject *H);
extern int gobject_info(NspGObject *H, int indent,char *name, int rec_level);
extern int gobject_print(NspGObject *H, int indent,char *name, int rec_level);

/* from NspGObjectObj.c */

extern NspGObject *gobject_object (NspObject *O); 
extern int IsGObjectObj (Stack stack, int i); 
extern int IsGObject(NspObject *O);
extern NspGObject *GetGObjectCopy (Stack stack, int i); 
extern NspGObject *GetGObject (Stack stack, int i); 

extern void nspg_destroy_notify(gpointer user_data);

extern NspGObject *nspgobject_new(const char *,GObject *go);
extern int nspgobject_check(void *value, void *type); 
#define nspgobject_get(x) ((NspGObject *) x)->obj

extern void register_nsp_type_in_gtype(NspTypeBase *type, GType gtype) ;

/* GType and NspTypes 
 */

void register_nsp_type_in_gtype(NspTypeBase *type, GType gtype) ;
NspTypeBase * nsp_type_from_gtype(GType gtype);

/* closures 
 */

typedef struct _NspGClosure NspGClosure;
struct _NspGClosure {
    GClosure closure;
    NspPList *callback;
    NspList *extra_args; /* tuple of extra args to pass to callback */
    NspObject *swap_data; /* other object for gtk_signal_connect_object */
};

extern GClosure *nspg_closure_new(NspPList *callback, NspList *extra_args, NspObject *swap_data);
extern GClosure *nspg_signal_class_closure_get(void);
extern int nspgobject_watch_closure(NspObject *self, GClosure *closure);

extern void nspg_closure_marshal(GClosure *closure,
				 GValue *return_value,
				 guint n_param_values,
				 const GValue *param_values,
				 gpointer invocation_hint,
				 gpointer marshal_data);

extern int nsp_gtk_eval_function(NspPList *func,NspObject *args[],int n_args,NspObject  *ret[],int *nret);
extern int nsp_gtk_eval_function_by_name(const char *name,NspObject *args[],int n_args,NspObject  *ret[],int *nret);

extern void nspg_unblock_threads(void);
extern void nspg_block_threads(void);

/* marshaling */

gint nspg_enum_get_value(GType enum_type, NspObject *obj, void *val);
gint nspg_flags_get_value(GType flag_type, NspObject *obj, void *val);
int nspg_value_from_nspobject(GValue *value, NspObject *obj);
NspObject * nspg_value_as_nspobject(const GValue *value, gboolean copy_boxed);

extern int nsp_flags_add_constants(NspHash *table, GType flags_type,const gchar *strip_prefix);
extern int nsp_enum_add_constants(NspHash *table, GType enum_type, const gchar *strip_prefix);


/* CustomNotify */

typedef struct {
  NspObject *func, *data;
} NspGtkCustomNotify; 

void nspgtk_custom_destroy_notify(gpointer user_data);

/* for ListStore and TreeStore */

extern GType gtype_from_nsp_object(NspObject *obj);

/* tree_path */


extern NspObject *nsp_gtk_tree_path_to_nspobject(GtkTreePath *path);
extern GtkTreePath *nsp_gtk_tree_path_from_nspobject(NspObject *object);

extern int nsp_gtk_tree_model_set_row(GtkTreeModel *model, GtkTreeIter *iter,GtkTreeIter *parent,NspList *items);
extern GtkListStore *nsp_gtk_list_store_from_list(NspList *M, int flag );
extern GtkTreeStore *nsp_gtk_tree_store_from_list(NspList *L, int flag );
extern int nsp_gtk_tree_model_set_col_from_list(GtkTreeModel *model,GtkTreeIter *iter1,NspList *L,int column);

extern GtkListStore *nsp_gtk_list_store_from_mat(NspMatrix *M);
extern GtkTreeStore *nsp_gtk_tree_store_from_mat(NspMatrix *M);
extern int nsp_gtk_tree_model_set_row_from_mat(GtkTreeModel *model, GtkTreeIter *iter,NspMatrix *M,int row);
extern int nsp_gtk_tree_model_set_col_from_mat(GtkTreeModel *model,GtkTreeIter *iter1, NspMatrix *M,int col);

extern GtkListStore *nsp_gtk_list_store_from_smat(NspSMatrix *M);
extern GtkTreeStore *nsp_gtk_tree_store_from_smat(NspSMatrix *M);
extern int nsp_gtk_tree_model_set_row_from_smat(GtkTreeModel *model, GtkTreeIter *iter,NspSMatrix *M,int row);
extern int nsp_gtk_tree_model_set_col_from_smat(GtkTreeModel *model,GtkTreeIter *iter1, NspSMatrix *M,int col);

extern GtkListStore *nsp_gtk_list_store_from_bmat(NspBMatrix *M);
extern GtkTreeStore *nsp_gtk_tree_store_from_bmat(NspBMatrix *M);
extern int nsp_gtk_tree_model_set_row_from_bmat(GtkTreeModel *model, GtkTreeIter *iter,NspBMatrix *M,int row);
extern int nsp_gtk_tree_model_set_col_from_bmat(GtkTreeModel *model,GtkTreeIter *iter1, NspBMatrix *M,int col);

extern int nsp_gdk_rectangle_from_object(NspObject *object, GdkRectangle *rectangle);
extern GType  nspg_type_from_object(NspObject *obj) ;

/* adding constants */

extern void gtk_add_constants(NspObject *module, const gchar *strip_prefix);
extern void gdk_add_constants(NspObject *module, const gchar *strip_prefix);
extern void atk_add_constants(NspObject *module, const gchar *strip_prefix);
extern void pango_add_constants(NspObject *module, const gchar *strip_prefix);

/* to be done */

/* extern void Nsp_BuildValue() ; */
/* extern void nspg_type_wrapper_new ()*/
extern NspObject * NspTuple_New(int n);
extern void NspTuple_SetItem(NspObject *,int n, void *);
extern int nspg_type_wrapper_new(GType ret);


extern GList *glist_from_typed_nsp_list(Stack stack,NspList *L,NspTypeBase *type);
extern GList *nsp_glist_from_nsplist(Stack stack,NspList *L);
extern GSList *nsp_gslist_from_nsplist(Stack stack,NspList *L);


#endif 

