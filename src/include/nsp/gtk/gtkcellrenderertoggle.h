/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCellRendererToggle
#define INC_NSP_GtkCellRendererToggle

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcellrenderer.h"

/*
* NspGtkCellRendererToggle inherits from NspGtkCellRenderer
* just change some type attributes 
*/

typedef NspGtkCellRenderer NspGtkCellRendererToggle ;
typedef NspTypeGtkCellRenderer NspTypeGtkCellRendererToggle ;

extern int nsp_type_gtkcellrenderertoggle_id;
extern NspTypeGtkCellRendererToggle *nsp_type_gtkcellrenderertoggle;

/* type instances for gtkcellrenderer */

NspTypeGtkCellRendererToggle *new_type_gtkcellrenderertoggle(type_mode mode);

/* instance for GtkCellRendererToggle */

NspGtkCellRendererToggle *new_gtkcellrenderertoggle();

/*
* Object methods redefined for gtkcellrenderertoggle 
*/

#ifdef GtkCellRendererToggle_Private 
static int init_gtkcellrenderertoggle(NspGtkCellRendererToggle *o,NspTypeGtkCellRendererToggle *type);
static char *gtkcellrenderertoggle_type_as_string(void);
static char *gtkcellrenderertoggle_type_short_string(void);
static AttrTab gtkcellrenderertoggle_attrs[];
/* static int int_gtkcellrenderertoggle_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcellrenderertoggle_get_methods(void); 
#endif /* GtkCellRendererToggle_Private */

#define NULLGTKCELLRENDERERTOGGLE (NspGtkCellRendererToggle*) 0

NspGtkCellRendererToggle *gtkcellrenderertoggle_create(char *name,NspTypeBase *type);

/* from GtkCellRendererToggleObj.c */

extern NspGtkCellRendererToggle *gtkcellrenderertoggle_object (NspObject *O); 
extern int IsGtkCellRendererToggleObj (Stack stack, int i); 
extern int IsGtkCellRendererToggle(NspObject *O);
extern NspGtkCellRendererToggle *GetGtkCellRendererToggleCopy (Stack stack, int i); 
extern NspGtkCellRendererToggle *GetGtkCellRendererToggle (Stack stack, int i); 

#endif 
