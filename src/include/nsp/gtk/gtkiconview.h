/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkIconView
#define INC_NSP_GtkIconView

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspGtkIconView inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspGtkIconView ;
typedef NspTypeGtkContainer NspTypeGtkIconView ;

extern int nsp_type_gtkiconview_id;
extern NspTypeGtkIconView *nsp_type_gtkiconview;

/* type instances for gtkcontainer */

NspTypeGtkIconView *new_type_gtkiconview(type_mode mode);

/* instance for GtkIconView */

NspGtkIconView *new_gtkiconview();

/*
* Object methods redefined for gtkiconview 
*/

#define NULLGTKICONVIEW (NspGtkIconView*) 0

NspGtkIconView *gtkiconview_create(char *name,NspTypeBase *type);

/* from GtkIconViewObj.c */

extern NspGtkIconView *gtkiconview_object (NspObject *O); 
extern int IsGtkIconViewObj (Stack stack, int i); 
extern int IsGtkIconView(NspObject *O);
extern NspGtkIconView *GetGtkIconViewCopy (Stack stack, int i); 
extern NspGtkIconView *GetGtkIconView (Stack stack, int i); 

#endif 

#ifdef GtkIconView_Private 
static int init_gtkiconview(NspGtkIconView *o,NspTypeGtkIconView *type);
static char *gtkiconview_type_as_string(void);
static char *gtkiconview_type_short_string(NspObject *v);
static AttrTab gtkiconview_attrs[];
/* static int int_gtkiconview_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkiconview_get_methods(void); 
#endif /* GtkIconView_Private */
