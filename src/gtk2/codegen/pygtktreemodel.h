#include <gtk/gtk.h>

#define NSPGTK_TYPE_GENERIC_TREE_MODEL (nspgtk_generic_tree_model_get_type())
#define NSPGTK_GENERIC_TREE_MODEL(object) (G_TYPE_CHECK_INSTANCE_CAST((object), NSPGTK_TYPE_GENERIC_TREE_MODEL, NspGtkGenericTreeModel))
#define NSPGTK_GENERIC_TREE_MODEL_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST((klass), NSPGTK_TYPE_GENERIC_TREE_MODEL, NspGtkGenericTreeModelClass))
#define NSPGTK_IS_GENERIC_TREE_MODEL(object) (G_TYPE_CHECK_INSTANCE_TYPE((object), NSPGTK_TYPE_GENERIC_TREE_MODEL))
#define NSPGTK_IS_GENERIC_TREE_MODEL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass), NSPGTK_TYPE_GENERIC_TREE_MODEL))
#define NSPGTK_GENERIC_TREE_MODEL_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS((obj), NSPGTK_TYPE_GENERIC_TREE_MODEL, NspGtkGenericTreeModelClass))

typedef struct _NspGtkGenericTreeModel NspGtkGenericTreeModel;
typedef struct _NspGtkGenericTreeModelClass NspGtkGenericTreeModelClass;

struct _NspGtkGenericTreeModel {
    GObject parent_instance;

    gboolean leak_references;
};

struct _NspGtkGenericTreeModelClass {
    GObjectClass parent_class;

};

GType nspgtk_generic_tree_model_get_type(void);

NspGtkGenericTreeModel *nspgtk_generic_tree_model_new(void);
