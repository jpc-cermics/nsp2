/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkBorder
#define INC_NSP_GtkBorder

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGtkBorder inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGtkBorder ;
typedef NspTypeGBoxed NspTypeGtkBorder ;

extern int nsp_type_gtkborder_id;
extern NspTypeGtkBorder *nsp_type_gtkborder;

/* type instances for gboxed */

NspTypeGtkBorder *new_type_gtkborder(type_mode mode);

/* instance for GtkBorder */

NspGtkBorder *new_gtkborder();

/*
* Object methods redefined for gtkborder 
*/

#define NULLGTKBORDER (NspGtkBorder*) 0

NspGtkBorder *gtkborder_create(char *name,NspTypeBase *type);

/* from GtkBorderObj.c */

extern NspGtkBorder *gtkborder_object (NspObject *O); 
extern int IsGtkBorderObj (Stack stack, int i); 
extern int IsGtkBorder(NspObject *O);
extern NspGtkBorder *GetGtkBorderCopy (Stack stack, int i); 
extern NspGtkBorder *GetGtkBorder (Stack stack, int i); 

#endif 

#ifdef GtkBorder_Private 
static int init_gtkborder(NspGtkBorder *o,NspTypeGtkBorder *type);
static char *gtkborder_type_as_string(void);
static char *gtkborder_type_short_string(NspObject *v);
static AttrTab gtkborder_attrs[];
/* static int int_gtkborder_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkborder_get_methods(void); 
#endif /* GtkBorder_Private */
