/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkHandleBox
#define INC_NSP_GtkHandleBox

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbin.h"

/*
* NspGtkHandleBox inherits from NspGtkBin
* just change some type attributes 
*/

typedef NspGtkBin NspGtkHandleBox ;
typedef NspTypeGtkBin NspTypeGtkHandleBox ;

extern int nsp_type_gtkhandlebox_id;
extern NspTypeGtkHandleBox *nsp_type_gtkhandlebox;

/* type instances for gtkbin */

NspTypeGtkHandleBox *new_type_gtkhandlebox(type_mode mode);

/* instance for GtkHandleBox */

NspGtkHandleBox *new_gtkhandlebox();

/*
* Object methods redefined for gtkhandlebox 
*/

#ifdef GtkHandleBox_Private 
static int init_gtkhandlebox(NspGtkHandleBox *o,NspTypeGtkHandleBox *type);
static char *gtkhandlebox_type_as_string(void);
static char *gtkhandlebox_type_short_string(void);
static AttrTab gtkhandlebox_attrs[];
/* static int int_gtkhandlebox_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkhandlebox_get_methods(void); 
#endif /* GtkHandleBox_Private */

#define NULLGTKHANDLEBOX (NspGtkHandleBox*) 0

NspGtkHandleBox *gtkhandlebox_create(char *name,NspTypeBase *type);

/* from GtkHandleBoxObj.c */

extern NspGtkHandleBox *gtkhandlebox_object (NspObject *O); 
extern int IsGtkHandleBoxObj (Stack stack, int i); 
extern int IsGtkHandleBox(NspObject *O);
extern NspGtkHandleBox *GetGtkHandleBoxCopy (Stack stack, int i); 
extern NspGtkHandleBox *GetGtkHandleBox (Stack stack, int i); 

#endif 
