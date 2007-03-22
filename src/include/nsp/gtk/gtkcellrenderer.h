/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCellRenderer
#define INC_NSP_GtkCellRenderer

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkobject.h"

/*
* NspGtkCellRenderer inherits from NspGtkObject
* just change some type attributes 
*/

typedef NspGtkObject NspGtkCellRenderer ;
typedef NspTypeGtkObject NspTypeGtkCellRenderer ;

extern int nsp_type_gtkcellrenderer_id;
extern NspTypeGtkCellRenderer *nsp_type_gtkcellrenderer;

/* type instances for gtkobject */

NspTypeGtkCellRenderer *new_type_gtkcellrenderer(type_mode mode);

/* instance for GtkCellRenderer */

NspGtkCellRenderer *new_gtkcellrenderer();

/*
* Object methods redefined for gtkcellrenderer 
*/

#define NULLGTKCELLRENDERER (NspGtkCellRenderer*) 0

NspGtkCellRenderer *gtkcellrenderer_create(char *name,NspTypeBase *type);

/* from GtkCellRendererObj.c */

extern NspGtkCellRenderer *gtkcellrenderer_object (NspObject *O); 
extern int IsGtkCellRendererObj (Stack stack, int i); 
extern int IsGtkCellRenderer(NspObject *O);
extern NspGtkCellRenderer *GetGtkCellRendererCopy (Stack stack, int i); 
extern NspGtkCellRenderer *GetGtkCellRenderer (Stack stack, int i); 

#endif 

#ifdef GtkCellRenderer_Private 
static int init_gtkcellrenderer(NspGtkCellRenderer *o,NspTypeGtkCellRenderer *type);
static char *gtkcellrenderer_type_as_string(void);
static char *gtkcellrenderer_type_short_string(NspObject *v);
static AttrTab gtkcellrenderer_attrs[];
/* static int int_gtkcellrenderer_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcellrenderer_get_methods(void); 
#endif /* GtkCellRenderer_Private */
