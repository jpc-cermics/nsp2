/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkFixed
#define INC_NSP_GtkFixed

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspGtkFixed inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspGtkFixed ;
typedef NspTypeGtkContainer NspTypeGtkFixed ;

extern int nsp_type_gtkfixed_id;
extern NspTypeGtkFixed *nsp_type_gtkfixed;

/* type instances for gtkcontainer */

NspTypeGtkFixed *new_type_gtkfixed(type_mode mode);

/* instance for GtkFixed */

NspGtkFixed *new_gtkfixed();

/*
* Object methods redefined for gtkfixed 
*/

#define NULLGTKFIXED (NspGtkFixed*) 0

NspGtkFixed *gtkfixed_create(char *name,NspTypeBase *type);

/* from GtkFixedObj.c */

extern NspGtkFixed *gtkfixed_object (NspObject *O); 
extern int IsGtkFixedObj (Stack stack, int i); 
extern int IsGtkFixed(NspObject *O);
extern NspGtkFixed *GetGtkFixedCopy (Stack stack, int i); 
extern NspGtkFixed *GetGtkFixed (Stack stack, int i); 

#endif 

#ifdef GtkFixed_Private 
static int init_gtkfixed(NspGtkFixed *o,NspTypeGtkFixed *type);
static char *gtkfixed_type_as_string(void);
static char *gtkfixed_type_short_string(void);
static AttrTab gtkfixed_attrs[];
/* static int int_gtkfixed_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkfixed_get_methods(void); 
#endif /* GtkFixed_Private */
