#ifndef _PYGTK_PRIVATE_H_
#define _PYGTK_PRIVATE_H_

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef _PYGTK_H_
#error "don't include pygtk.h and pygtk-private.h together"
#endif

#define _INSIDE_PYGTK_
#include "pygtk.h"
#undef WITH_THREAD

/* type objects */
extern PyTypeObject PyGdkAtom_Type;

/* check the type of a PyObject */
#define PyGdkAtom_Check(v) ((v)->ob_type == &PyGdkAtom_Type)

/* constructors for PyObject wrappers ... */
PyObject *PyGdkAtom_New(GdkAtom atom);

void pygtk_handler_marshal(gpointer a, PyObject *func, int nargs,GtkArg *args);
void pygtk_input_marshal(gpointer a, PyObject *func, int nargs, GtkArg *args);

/* private */
PyObject    *pygtk_tree_path_to_pyobject(GtkTreePath *path);
GtkTreePath *pygtk_tree_path_from_pyobject(PyObject *object);
gboolean     pygdk_rectangle_from_pyobject(PyObject *object,
					   GdkRectangle *rectangle);

GdkAtom pygdk_atom_from_pyobject(PyObject *object);

typedef struct {
    PyObject *func, *data;
} PyGtkCustomNotify;

void pygtk_custom_destroy_notify(gpointer user_data);

/* helper object for the style helper */
typedef struct {
    PyObject_HEAD
    GtkStyle *style; /* parent style */
    enum {STYLE_COLOUR_ARRAY, STYLE_GC_ARRAY, STYLE_PIXMAP_ARRAY} type;
    gpointer array;
} PyGtkStyleHelper_Object;

PyObject *_pygtk_style_helper_new(GtkStyle *style, int type, gpointer array);

PyObject *_pygtk_tree_model_row_new(GtkTreeModel *model, GtkTreeIter *iter);
PyObject *_pygtk_tree_model_row_iter_new(GtkTreeModel *model,
					 GtkTreeIter *parent_iter);
int       _pygtk_tree_model_set_row(GtkTreeModel *model, GtkTreeIter *iter,
				    PyObject *items);

#endif
