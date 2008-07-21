/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkFileChooser
#define INC_NSP_GtkFileChooser

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkFileChooser inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkFileChooser ;
typedef NspTypeGObject NspTypeGtkFileChooser ;

extern int nsp_type_gtkfilechooser_id;
extern NspTypeGtkFileChooser *nsp_type_gtkfilechooser;

/* type instances for gobject */

NspTypeGtkFileChooser *new_type_gtkfilechooser(type_mode mode);

/* instance for GtkFileChooser */

NspGtkFileChooser *new_gtkfilechooser();

/*
* Object methods redefined for gtkfilechooser 
*/

#define NULLGTKFILECHOOSER (NspGtkFileChooser*) 0

NspGtkFileChooser *gtkfilechooser_create(char *name,NspTypeBase *type);

/* from GtkFileChooserObj.c */

extern NspGtkFileChooser *gtkfilechooser_object (NspObject *O); 
extern int IsGtkFileChooserObj (Stack stack, int i); 
extern int IsGtkFileChooser(NspObject *O);
extern NspGtkFileChooser *GetGtkFileChooserCopy (Stack stack, int i); 
extern NspGtkFileChooser *GetGtkFileChooser (Stack stack, int i); 

#endif 

#ifdef GtkFileChooser_Private 
static int init_gtkfilechooser(NspGtkFileChooser *o,NspTypeGtkFileChooser *type);
static char *gtkfilechooser_type_as_string(void);
static char *gtkfilechooser_type_short_string(NspObject *v);
static AttrTab gtkfilechooser_attrs[];
/* static int int_gtkfilechooser_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkfilechooser_get_methods(void); 
#endif /* GtkFileChooser_Private */
