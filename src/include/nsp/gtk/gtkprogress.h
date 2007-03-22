/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkProgress
#define INC_NSP_GtkProgress

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkwidget.h"

/*
* NspGtkProgress inherits from NspGtkWidget
* just change some type attributes 
*/

typedef NspGtkWidget NspGtkProgress ;
typedef NspTypeGtkWidget NspTypeGtkProgress ;

extern int nsp_type_gtkprogress_id;
extern NspTypeGtkProgress *nsp_type_gtkprogress;

/* type instances for gtkwidget */

NspTypeGtkProgress *new_type_gtkprogress(type_mode mode);

/* instance for GtkProgress */

NspGtkProgress *new_gtkprogress();

/*
* Object methods redefined for gtkprogress 
*/

#define NULLGTKPROGRESS (NspGtkProgress*) 0

NspGtkProgress *gtkprogress_create(char *name,NspTypeBase *type);

/* from GtkProgressObj.c */

extern NspGtkProgress *gtkprogress_object (NspObject *O); 
extern int IsGtkProgressObj (Stack stack, int i); 
extern int IsGtkProgress(NspObject *O);
extern NspGtkProgress *GetGtkProgressCopy (Stack stack, int i); 
extern NspGtkProgress *GetGtkProgress (Stack stack, int i); 

#endif 

#ifdef GtkProgress_Private 
static int init_gtkprogress(NspGtkProgress *o,NspTypeGtkProgress *type);
static char *gtkprogress_type_as_string(void);
static char *gtkprogress_type_short_string(NspObject *v);
static AttrTab gtkprogress_attrs[];
/* static int int_gtkprogress_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkprogress_get_methods(void); 
#endif /* GtkProgress_Private */
