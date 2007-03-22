/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkRcStyle
#define INC_NSP_GtkRcStyle

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkRcStyle inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkRcStyle ;
typedef NspTypeGObject NspTypeGtkRcStyle ;

extern int nsp_type_gtkrcstyle_id;
extern NspTypeGtkRcStyle *nsp_type_gtkrcstyle;

/* type instances for gobject */

NspTypeGtkRcStyle *new_type_gtkrcstyle(type_mode mode);

/* instance for GtkRcStyle */

NspGtkRcStyle *new_gtkrcstyle();

/*
* Object methods redefined for gtkrcstyle 
*/

#define NULLGTKRCSTYLE (NspGtkRcStyle*) 0

NspGtkRcStyle *gtkrcstyle_create(char *name,NspTypeBase *type);

/* from GtkRcStyleObj.c */

extern NspGtkRcStyle *gtkrcstyle_object (NspObject *O); 
extern int IsGtkRcStyleObj (Stack stack, int i); 
extern int IsGtkRcStyle(NspObject *O);
extern NspGtkRcStyle *GetGtkRcStyleCopy (Stack stack, int i); 
extern NspGtkRcStyle *GetGtkRcStyle (Stack stack, int i); 

#endif 

#ifdef GtkRcStyle_Private 
static int init_gtkrcstyle(NspGtkRcStyle *o,NspTypeGtkRcStyle *type);
static char *gtkrcstyle_type_as_string(void);
static char *gtkrcstyle_type_short_string(NspObject *v);
static AttrTab gtkrcstyle_attrs[];
/* static int int_gtkrcstyle_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkrcstyle_get_methods(void); 
#endif /* GtkRcStyle_Private */
