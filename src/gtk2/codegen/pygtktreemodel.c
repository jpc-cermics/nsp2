#include "pygtktreemodel.h"
#include "nsp/object.h"

/* define this to print out debug messages */
#undef DEBUG_TREE_MODEL

#ifndef _
#  define _(s) (s)
#endif

enum {
    PROP_LEAK_REFERENCES = 1
};

static void nspgtk_generic_tree_model_class_init(NspGtkGenericTreeModelClass *klass);
static void nspgtk_generic_tree_model_init(NspGtkGenericTreeModel *self);
static void nspgtk_generic_tree_model_iface_init(GtkTreeModelIface *iface);
static void nspgtk_generic_tree_model_set_property (GObject *object,
						   guint property_id,
						   const GValue *value,
						   GParamSpec *pspec);
static void nspgtk_generic_tree_model_get_property (GObject *object,
						   guint property_id,
						   GValue *value,
						   GParamSpec *pspec);


GType
nspgtk_generic_tree_model_get_type(void)
{
    static GType object_type = 0;

    if (!object_type) {
	static const GTypeInfo object_info = {
	    sizeof(NspGtkGenericTreeModelClass),
	    (GBaseInitFunc) NULL,
	    (GBaseFinalizeFunc) NULL,
	    (GClassInitFunc) nspgtk_generic_tree_model_class_init,
	    NULL, /* class_finalize */
	    NULL, /* class_data */
	    sizeof(NspGtkGenericTreeModel),
	    0, /* n_preallocs */
	    (GInstanceInitFunc) nspgtk_generic_tree_model_init,
	};
	static const GInterfaceInfo tree_model_info = {
	    (GInterfaceInitFunc) nspgtk_generic_tree_model_iface_init,
	    NULL,
	    NULL,
	};

	object_type = g_type_register_static(G_TYPE_OBJECT,
					     "NspGtkGenericTreeModel",
					     &object_info, 0);
	g_type_add_interface_static(object_type,
				    GTK_TYPE_TREE_MODEL,
				    &tree_model_info);
    }
    return object_type;
}

static void
nspgtk_generic_tree_model_class_init(NspGtkGenericTreeModelClass *klass)
{
    GObjectClass *object_class = (GObjectClass*) klass;

    object_class->get_property = nspgtk_generic_tree_model_get_property;
    object_class->set_property = nspgtk_generic_tree_model_set_property;
 
    g_object_class_install_property (object_class,
				     PROP_LEAK_REFERENCES,
				     g_param_spec_boolean ("leak_references",
							   _("Leak references"),
							   _("Enable referencing iterator "
							     "objects (this will cause a memory leak or at least a reference "
							     "counting leak). You might need it though, if you return newly "
							     "created objects."),
							   TRUE,
							   G_PARAM_READWRITE));
}

static guint nspgtk_generic_tree_model_get_flags(GtkTreeModel *tree_model);
static gint nspgtk_generic_tree_model_get_n_columns(GtkTreeModel *tree_model);
static GType nspgtk_generic_tree_model_get_column_type(GtkTreeModel *tree_model,
						      gint index);
static gboolean nspgtk_generic_tree_model_get_iter(GtkTreeModel *tree_model,
						  GtkTreeIter *iter,
						  GtkTreePath *path);
static GtkTreePath *nspgtk_generic_tree_model_get_path(GtkTreeModel *tree_model,
						      GtkTreeIter *iter);
static void nspgtk_generic_tree_model_get_value(GtkTreeModel*tree_model,
					       GtkTreeIter *iter,
					       gint column, GValue *value);
static gboolean nspgtk_generic_tree_model_iter_next(GtkTreeModel *tree_model,
						   GtkTreeIter *iter);
static gboolean nspgtk_generic_tree_model_iter_children(GtkTreeModel *tree_model,
						       GtkTreeIter *iter,
						       GtkTreeIter *parent);
static gboolean nspgtk_generic_tree_model_iter_has_child(GtkTreeModel *tree_model,
							GtkTreeIter *iter);
static gint nspgtk_generic_tree_model_iter_n_children(GtkTreeModel *tree_model,
						     GtkTreeIter *iter);
static gboolean nspgtk_generic_tree_model_iter_nth_child(GtkTreeModel *tree_model,
							GtkTreeIter  *iter,
							GtkTreeIter  *parent,
							gint n);
static gboolean nspgtk_generic_tree_model_iter_parent(GtkTreeModel *tree_model,
						     GtkTreeIter *iter,
						     GtkTreeIter *child);

static void
nspgtk_generic_tree_model_iface_init(GtkTreeModelIface *iface)
{
    iface->get_flags = nspgtk_generic_tree_model_get_flags;
    iface->get_n_columns = nspgtk_generic_tree_model_get_n_columns;
    iface->get_column_type = nspgtk_generic_tree_model_get_column_type;
    iface->get_iter = nspgtk_generic_tree_model_get_iter;
    iface->get_path = nspgtk_generic_tree_model_get_path;
    iface->get_value = nspgtk_generic_tree_model_get_value;
    iface->iter_next = nspgtk_generic_tree_model_iter_next;
    iface->iter_children = nspgtk_generic_tree_model_iter_children;
    iface->iter_has_child = nspgtk_generic_tree_model_iter_has_child;
    iface->iter_n_children = nspgtk_generic_tree_model_iter_n_children;
    iface->iter_nth_child = nspgtk_generic_tree_model_iter_nth_child;
    iface->iter_parent = nspgtk_generic_tree_model_iter_parent;
}

static void
nspgtk_generic_tree_model_init(NspGtkGenericTreeModel *self)
{
    self->leak_references = TRUE;
}

static void
nspgtk_generic_tree_model_set_property (GObject *object, guint property_id,
				       const GValue *value, GParamSpec *pspec)
{
    switch (property_id) {
    case PROP_LEAK_REFERENCES:
	NSPGTK_GENERIC_TREE_MODEL (object)->leak_references = g_value_get_boolean (value);
	break;
    default:
	G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
	break;
    }
}
 
static void
nspgtk_generic_tree_model_get_property (GObject *object, guint property_id,
				       GValue *value, GParamSpec *pspec)
{
    switch (property_id) {
    case PROP_LEAK_REFERENCES:
	g_value_set_boolean (value,
			     NSPGTK_GENERIC_TREE_MODEL (object)->leak_references);
	break;
    default:
	G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
	break;
    }
}


NspGtkGenericTreeModel *
nspgtk_generic_tree_model_new(void)
{
    return NSPGTK_GENERIC_TREE_MODEL(
				    g_object_new(NSPGTK_TYPE_GENERIC_TREE_MODEL, NULL));
}


/* format of GtkTreeIter's for NspGtkGenericTreeModel:
 *  user_data == nspthon object
 *  user_data2 == floating reference?
 *
 * I haven't worked out how everything should work.  For now I will
 * leak references.
 */

#define METHOD_PREFIX "on_"

static guint
nspgtk_generic_tree_model_get_flags(GtkTreeModel *tree_model)
{
    NspObject *self, *nsp_ret;

    g_return_val_if_fail(NSPGTK_IS_GENERIC_TREE_MODEL(tree_model), 0);

    nspg_block_threads();

    /* this call finds the wrapper for this GObject */
    self = nspgobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("get_flags()");
#endif
    nsp_ret = NspObject_CallMethod(self, METHOD_PREFIX "get_flags", "");
    if (nsp_ret) {
	guint ret = NspInt_AsLong(nsp_ret);

	Nsp_DECREF(nsp_ret);
	nspg_unblock_threads();
	return ret;
    } else {
	NspErr_Print();
	NspErr_Clear();
	nspg_unblock_threads();
	return 0;
    }
}

static gint
nspgtk_generic_tree_model_get_n_columns(GtkTreeModel *tree_model)
{
    NspObject *self, *nsp_ret;

    g_return_val_if_fail(tree_model != NULL, 0);
    g_return_val_if_fail(NSPGTK_IS_GENERIC_TREE_MODEL(tree_model), 0);

    nspg_block_threads();

    /* this call finds the wrapper for this GObject */
    self = nspgobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("get_n_columns()");
#endif
    nsp_ret = NspObject_CallMethod(self, METHOD_PREFIX "get_n_columns", "");
    if (nsp_ret) {
	gint ret = NspInt_AsLong(nsp_ret);

	Nsp_DECREF(nsp_ret);
	nspg_unblock_threads();
	return ret;
    } else {
	NspErr_Print();
	NspErr_Clear();
	nspg_unblock_threads();
	return 0;
    }
}

static GType
nspgtk_generic_tree_model_get_column_type(GtkTreeModel *tree_model, gint index)
{
    NspObject *self, *nsp_ret;

    g_return_val_if_fail(tree_model != NULL, G_TYPE_INVALID);
    g_return_val_if_fail(NSPGTK_IS_GENERIC_TREE_MODEL(tree_model), G_TYPE_INVALID);
    nspg_block_threads();

    /* this call finds the wrapper for this GObject */
    self = nspgobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("get_column_type(%d)", index);
#endif
    nsp_ret = NspObject_CallMethod(self, METHOD_PREFIX "get_column_type",
				 "(i)", index);
    if (nsp_ret) {
	GType ret = nspg_type_from_object(nsp_ret);

	Nsp_DECREF(nsp_ret);
	nspg_unblock_threads();
	return ret;
    } else {
	NspErr_Print();
	NspErr_Clear();
	nspg_unblock_threads();
	return G_TYPE_INVALID;
    }
}

static gboolean
nspgtk_generic_tree_model_get_iter(GtkTreeModel *tree_model,
				  GtkTreeIter *iter, GtkTreePath *path)
{
    NspObject *self, *nsp_path, *nsp_ret;

    g_return_val_if_fail(tree_model != NULL, FALSE);
    g_return_val_if_fail(NSPGTK_IS_GENERIC_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail(iter != NULL, FALSE);
    g_return_val_if_fail(path != NULL, FALSE);

    nspg_block_threads();

    /* this call finds the wrapper for this GObject */
    self = nspgobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("get_iter(%p)", path);
#endif
    nsp_path = nspgtk_tree_path_to_nspobject(path);
    nsp_ret = NspObject_CallMethod(self, METHOD_PREFIX "get_iter",
				 "(O)", nsp_path);
    Nsp_DECREF(nsp_path);

    if (nsp_ret) {
	if (nsp_ret != Nsp_None) {
	    iter->user_data = nsp_ret;
	    if (!NSPGTK_GENERIC_TREE_MODEL(tree_model)->leak_references) {
		Nsp_DECREF((NspObject *)iter->user_data);
	    }
	    nspg_unblock_threads();
	    return TRUE;
	} else {
	    iter->user_data = NULL;
	    Nsp_DECREF(nsp_ret);
	    nspg_unblock_threads();
	    return FALSE;
	}
    } else {
	NspErr_Print();
	NspErr_Clear();
	iter->user_data = NULL;
	nspg_unblock_threads();
	return FALSE;
    }
}

static GtkTreePath *
nspgtk_generic_tree_model_get_path(GtkTreeModel *tree_model, GtkTreeIter *iter)
{
    NspObject *self, *nsp_ret;

    g_return_val_if_fail(tree_model != NULL, NULL);
    g_return_val_if_fail(NSPGTK_IS_GENERIC_TREE_MODEL(tree_model), NULL);
    g_return_val_if_fail(iter != NULL, NULL);
    /* this call finds the wrapper for this GObject */
    self = nspgobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("get_path(%p)", iter);
#endif
    nspg_block_threads();
    nsp_ret = NspObject_CallMethod(self, METHOD_PREFIX "get_path",
				 "(O)", (NspObject *)iter->user_data);
    if (nsp_ret) {
	GtkTreePath *path = nspgtk_tree_path_from_nspobject(nsp_ret);

	if (!path)
	    g_warning("could not convert return value of get_path() to "
		      "a GtkTreePath");
	Nsp_DECREF(nsp_ret);
	nspg_unblock_threads();
	return path;
    } else {
	NspErr_Print();
	NspErr_Clear();
	nspg_unblock_threads();
	return NULL;
    }
}

static void
nspgtk_generic_tree_model_get_value(GtkTreeModel*tree_model, GtkTreeIter *iter,
				   gint column, GValue *value)
{
    NspObject *self, *nsp_value;

    g_return_if_fail(tree_model != NULL);
    g_return_if_fail(NSPGTK_IS_GENERIC_TREE_MODEL(tree_model));
    g_return_if_fail(iter != NULL);

    nspg_block_threads();

    /* this call finds the wrapper for this GObject */
    self = nspgobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("get_value(%p, %d)", iter, column);
    _NspObject_Dump (iter->user_data);
#endif
    /* init value to column type */
    g_value_init(value, nspgtk_generic_tree_model_get_column_type(tree_model, column));

    nsp_value = NspObject_CallMethod(self, METHOD_PREFIX "get_value",
				   "(Oi)", (NspObject *)iter->user_data,column);

    if (nsp_value) {
	nspg_value_from_nspobject(value, nsp_value);
	Nsp_DECREF(nsp_value);
    } else {
	NspErr_Print();
	NspErr_Clear();
    }
    nspg_unblock_threads();
}

static gboolean
nspgtk_generic_tree_model_iter_next(GtkTreeModel *tree_model, GtkTreeIter *iter)
{
    NspObject *self, *nsp_ret;

    g_return_val_if_fail(tree_model != NULL, FALSE);
    g_return_val_if_fail(NSPGTK_IS_GENERIC_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail(iter != NULL, FALSE);

    nspg_block_threads();

    /* this call finds the wrapper for this GObject */
    self = nspgobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("iter_next(%p)", iter);
#endif
    nsp_ret = NspObject_CallMethod(self, METHOD_PREFIX "iter_next",
				 "(O)", (NspObject *)iter->user_data);
    if (nsp_ret) {
	if (nsp_ret != Nsp_None) {
	    /* XXXX handle reference counting here */
	    iter->user_data = nsp_ret;
	    if (!NSPGTK_GENERIC_TREE_MODEL(tree_model)->leak_references) {
		Nsp_DECREF((NspObject *)iter->user_data);
	    }
	    nspg_unblock_threads();
	    return TRUE;
	} else {
	    iter->user_data = NULL;
	    Nsp_DECREF(nsp_ret);
	    nspg_unblock_threads();
	    return FALSE;
	}
    } else {
	iter->user_data = NULL;
	NspErr_Print();
	NspErr_Clear();
	nspg_unblock_threads();
	return FALSE;
    }
}

static gboolean
nspgtk_generic_tree_model_iter_children(GtkTreeModel *tree_model, GtkTreeIter *iter,
				       GtkTreeIter *parent)
{
    NspObject *self, *nsp_ret, *nsp_parent = Nsp_None;

    g_return_val_if_fail(tree_model != NULL, FALSE);
    g_return_val_if_fail(NSPGTK_IS_GENERIC_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail(iter != NULL, FALSE);

    nspg_block_threads();

    /* this call finds the wrapper for this GObject */
    self = nspgobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("iter_children(%p, %p)", iter, parent);
#endif
    if (parent && parent->user_data != NULL)
	nsp_parent = (NspObject *)parent->user_data;
    nsp_ret = NspObject_CallMethod(self, METHOD_PREFIX "iter_children",
				 "(O)", nsp_parent);
    if (nsp_ret) {
	if (nsp_ret != Nsp_None) {
	    /* XXXX handle reference counting here */
	    iter->user_data = nsp_ret;
	    if (!NSPGTK_GENERIC_TREE_MODEL(tree_model)->leak_references) {
		Nsp_DECREF((NspObject *)iter->user_data);
	    }
	    nspg_unblock_threads();
	    return TRUE;
	} else {
	    iter->user_data = NULL;
	    Nsp_DECREF(nsp_ret);
	    nspg_unblock_threads();
	    return FALSE;
	}
    } else {
	iter->user_data = NULL;
	NspErr_Print();
	NspErr_Clear();
	nspg_unblock_threads();
	return FALSE;
    }
}

static gboolean
nspgtk_generic_tree_model_iter_has_child(GtkTreeModel *tree_model, GtkTreeIter *iter)
{
    NspObject *self, *nsp_ret;

    g_return_val_if_fail(tree_model != NULL, FALSE);
    g_return_val_if_fail(NSPGTK_IS_GENERIC_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail(iter != NULL, FALSE);

    nspg_block_threads();

    /* this call finds the wrapper for this GObject */
    self = nspgobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("iter_has_child(%p)", iter);
#endif
    nsp_ret = NspObject_CallMethod(self, METHOD_PREFIX "iter_has_child",
				 "(O)", (NspObject *)iter->user_data);
    if (nsp_ret) {
	gboolean ret = NspObject_IsTrue(nsp_ret);

	Nsp_DECREF(nsp_ret);
	nspg_unblock_threads();
	return ret;
    } else {
	NspErr_Print();
	NspErr_Clear();
	nspg_unblock_threads();
	return FALSE;
    }
}

static gint
nspgtk_generic_tree_model_iter_n_children(GtkTreeModel *tree_model, GtkTreeIter *iter)
{
    NspObject *self, *nsp_ret;

    g_return_val_if_fail(tree_model != NULL, FALSE);
    g_return_val_if_fail(NSPGTK_IS_GENERIC_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail(iter != NULL, FALSE);

    nspg_block_threads();

    /* this call finds the wrapper for this GObject */
    self = nspgobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("iter_n_children(%p)", iter);
#endif
    nsp_ret = NspObject_CallMethod(self, METHOD_PREFIX "iter_n_children",
				 "(O)", (NspObject *)iter->user_data);
    if (nsp_ret) {
	gint ret = NspInt_AsLong(nsp_ret);

	Nsp_DECREF(nsp_ret);
	nspg_unblock_threads();
	return ret;
    } else {
	NspErr_Print();
	NspErr_Clear();
	nspg_unblock_threads();
	return 0;
    }
}

static gboolean
nspgtk_generic_tree_model_iter_nth_child(GtkTreeModel *tree_model, GtkTreeIter  *iter,
					GtkTreeIter  *parent, gint n)
{
    NspObject *self, *nsp_ret, *nsp_parent = Nsp_None;

    g_return_val_if_fail(tree_model != NULL, FALSE);
    g_return_val_if_fail(NSPGTK_IS_GENERIC_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail(iter != NULL, FALSE);

    nspg_block_threads();

    /* this call finds the wrapper for this GObject */
    self = nspgobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("iter_nth_child(%p, %p, %d)", iter, parent, n);
#endif
    if (parent && parent->user_data != NULL)
	nsp_parent = (NspObject *)parent->user_data;
    nsp_ret = NspObject_CallMethod(self, METHOD_PREFIX "iter_nth_child",
				 "(Oi)", nsp_parent, n);
    if (nsp_ret) {
	if (nsp_ret != Nsp_None) {
	    /* XXXX handle reference counting here */
	    iter->user_data = nsp_ret;
	    if (!NSPGTK_GENERIC_TREE_MODEL(tree_model)->leak_references) {
		Nsp_DECREF((NspObject *)iter->user_data);
	    }
	    nspg_unblock_threads();
	    return TRUE;
	} else {
	    iter->user_data = NULL;
	    Nsp_DECREF(nsp_ret);
	    nspg_unblock_threads();
	    return FALSE;
	}
    } else {
	iter->user_data = NULL;
	NspErr_Print();
	NspErr_Clear();
	nspg_unblock_threads();
	return FALSE;
    }
}

static gboolean
nspgtk_generic_tree_model_iter_parent(GtkTreeModel *tree_model, GtkTreeIter *iter,
				     GtkTreeIter *child)
{
    NspObject *self, *nsp_ret, *nsp_child = Nsp_None;

    g_return_val_if_fail(tree_model != NULL, FALSE);
    g_return_val_if_fail(NSPGTK_IS_GENERIC_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail(iter != NULL, FALSE);

    nspg_block_threads();

    /* this call finds the wrapper for this GObject */
    self = nspgobject_new((GObject *)tree_model);

#ifdef DEBUG_TREE_MODEL
    g_message("iter_parent(%p, %p)", iter, child);
#endif
    if (child && child->user_data != NULL)
	nsp_child = (NspObject *)child->user_data;
    nsp_ret = NspObject_CallMethod(self, METHOD_PREFIX "iter_parent",
				 "(O)", nsp_child);
    if (nsp_ret) {
	if (nsp_ret != Nsp_None) {
	    /* XXXX handle reference counting here */
	    iter->user_data = nsp_ret;
	    if (!NSPGTK_GENERIC_TREE_MODEL(tree_model)->leak_references) {
		Nsp_DECREF((NspObject *)iter->user_data);
	    }
	    nspg_unblock_threads();
	    return TRUE;
	} else {
	    iter->user_data = NULL;
	    Nsp_DECREF(nsp_ret);
	    nspg_unblock_threads();
	    return FALSE;
	}
    } else {
	iter->user_data = NULL;
	NspErr_Print();
	NspErr_Clear();
	nspg_unblock_threads();
	return FALSE;
    }
}

