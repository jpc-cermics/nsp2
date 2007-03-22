/* -*- Mode: C -*- */
#ifndef INC_NSP_PyGtkGenericCellRenderer
#define INC_NSP_PyGtkGenericCellRenderer

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcellrenderer.h"

/*
* NspPyGtkGenericCellRenderer inherits from NspGtkCellRenderer
* just change some type attributes 
*/

typedef NspGtkCellRenderer NspPyGtkGenericCellRenderer ;
typedef NspTypeGtkCellRenderer NspTypePyGtkGenericCellRenderer ;

extern int nsp_type_pygtkgenericcellrenderer_id;
extern NspTypePyGtkGenericCellRenderer *nsp_type_pygtkgenericcellrenderer;

/* type instances for gtkcellrenderer */

NspTypePyGtkGenericCellRenderer *new_type_pygtkgenericcellrenderer(type_mode mode);

/* instance for PyGtkGenericCellRenderer */

NspPyGtkGenericCellRenderer *new_pygtkgenericcellrenderer();

/*
* Object methods redefined for pygtkgenericcellrenderer 
*/

#ifdef PyGtkGenericCellRenderer_Private 
static int init_pygtkgenericcellrenderer(NspPyGtkGenericCellRenderer *o,NspTypePyGtkGenericCellRenderer *type);
static char *pygtkgenericcellrenderer_type_as_string(void);
static char *pygtkgenericcellrenderer_type_short_string(NspObject *v);
static AttrTab pygtkgenericcellrenderer_attrs[];
/* static int int_pygtkgenericcellrenderer_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pygtkgenericcellrenderer_get_methods(void); 
#endif /* PyGtkGenericCellRenderer_Private */

#define NULLPYGTKGENERICCELLRENDERER (NspPyGtkGenericCellRenderer*) 0

NspPyGtkGenericCellRenderer *pygtkgenericcellrenderer_create(char *name,NspTypeBase *type);

/* from PyGtkGenericCellRendererObj.c */

extern NspPyGtkGenericCellRenderer *pygtkgenericcellrenderer_object (NspObject *O); 
extern int IsPyGtkGenericCellRendererObj (Stack stack, int i); 
extern int IsPyGtkGenericCellRenderer(NspObject *O);
extern NspPyGtkGenericCellRenderer *GetPyGtkGenericCellRendererCopy (Stack stack, int i); 
extern NspPyGtkGenericCellRenderer *GetPyGtkGenericCellRenderer (Stack stack, int i); 

#endif 
