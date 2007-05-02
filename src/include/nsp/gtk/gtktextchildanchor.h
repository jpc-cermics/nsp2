/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTextChildAnchor
#define INC_NSP_GtkTextChildAnchor

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkTextChildAnchor inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkTextChildAnchor ;
typedef NspTypeGObject NspTypeGtkTextChildAnchor ;

extern int nsp_type_gtktextchildanchor_id;
extern NspTypeGtkTextChildAnchor *nsp_type_gtktextchildanchor;

/* type instances for gobject */

NspTypeGtkTextChildAnchor *new_type_gtktextchildanchor(type_mode mode);

/* instance for GtkTextChildAnchor */

NspGtkTextChildAnchor *new_gtktextchildanchor();

/*
* Object methods redefined for gtktextchildanchor 
*/

#define NULLGTKTEXTCHILDANCHOR (NspGtkTextChildAnchor*) 0

NspGtkTextChildAnchor *gtktextchildanchor_create(char *name,NspTypeBase *type);

/* from GtkTextChildAnchorObj.c */

extern NspGtkTextChildAnchor *gtktextchildanchor_object (NspObject *O); 
extern int IsGtkTextChildAnchorObj (Stack stack, int i); 
extern int IsGtkTextChildAnchor(NspObject *O);
extern NspGtkTextChildAnchor *GetGtkTextChildAnchorCopy (Stack stack, int i); 
extern NspGtkTextChildAnchor *GetGtkTextChildAnchor (Stack stack, int i); 

#endif 

#ifdef GtkTextChildAnchor_Private 
static int init_gtktextchildanchor(NspGtkTextChildAnchor *o,NspTypeGtkTextChildAnchor *type);
static char *gtktextchildanchor_type_as_string(void);
static char *gtktextchildanchor_type_short_string(NspObject *v);
static AttrTab gtktextchildanchor_attrs[];
/* static int int_gtktextchildanchor_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktextchildanchor_get_methods(void); 
#endif /* GtkTextChildAnchor_Private */
