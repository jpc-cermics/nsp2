/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkEditable
#define INC_NSP_GtkEditable

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkEditable inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkEditable ;
typedef NspTypeGObject NspTypeGtkEditable ;

extern int nsp_type_gtkeditable_id;
extern NspTypeGtkEditable *nsp_type_gtkeditable;

/* type instances for gobject */

NspTypeGtkEditable *new_type_gtkeditable(type_mode mode);

/* instance for GtkEditable */

NspGtkEditable *new_gtkeditable();

/*
* Object methods redefined for gtkeditable 
*/

#define NULLGTKEDITABLE (NspGtkEditable*) 0

NspGtkEditable *gtkeditable_create(char *name,NspTypeBase *type);

/* from GtkEditableObj.c */

extern NspGtkEditable *gtkeditable_object (NspObject *O); 
extern int IsGtkEditableObj (Stack stack, int i); 
extern int IsGtkEditable(NspObject *O);
extern NspGtkEditable *GetGtkEditableCopy (Stack stack, int i); 
extern NspGtkEditable *GetGtkEditable (Stack stack, int i); 

#endif 

#ifdef GtkEditable_Private 
static int init_gtkeditable(NspGtkEditable *o,NspTypeGtkEditable *type);
static char *gtkeditable_type_as_string(void);
static char *gtkeditable_type_short_string(void);
static AttrTab gtkeditable_attrs[];
/* static int int_gtkeditable_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkeditable_get_methods(void); 
#endif /* GtkEditable_Private */
