/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTextAttributes
#define INC_NSP_GtkTextAttributes

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGtkTextAttributes inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGtkTextAttributes ;
typedef NspTypeGBoxed NspTypeGtkTextAttributes ;

extern int nsp_type_gtktextattributes_id;
extern NspTypeGtkTextAttributes *nsp_type_gtktextattributes;

/* type instances for gboxed */

NspTypeGtkTextAttributes *new_type_gtktextattributes(type_mode mode);

/* instance for GtkTextAttributes */

NspGtkTextAttributes *new_gtktextattributes();

/*
* Object methods redefined for gtktextattributes 
*/

#define NULLGTKTEXTATTRIBUTES (NspGtkTextAttributes*) 0

NspGtkTextAttributes *gtktextattributes_create(char *name,NspTypeBase *type);

/* from GtkTextAttributesObj.c */

extern NspGtkTextAttributes *gtktextattributes_object (NspObject *O); 
extern int IsGtkTextAttributesObj (Stack stack, int i); 
extern int IsGtkTextAttributes(NspObject *O);
extern NspGtkTextAttributes *GetGtkTextAttributesCopy (Stack stack, int i); 
extern NspGtkTextAttributes *GetGtkTextAttributes (Stack stack, int i); 

#endif 

#ifdef GtkTextAttributes_Private 
static int init_gtktextattributes(NspGtkTextAttributes *o,NspTypeGtkTextAttributes *type);
static char *gtktextattributes_type_as_string(void);
static char *gtktextattributes_type_short_string(NspObject *v);
static AttrTab gtktextattributes_attrs[];
/* static int int_gtktextattributes_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktextattributes_get_methods(void); 
#endif /* GtkTextAttributes_Private */
