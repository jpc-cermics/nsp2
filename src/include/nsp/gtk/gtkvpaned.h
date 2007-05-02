/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkVPaned
#define INC_NSP_GtkVPaned

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkpaned.h"

/*
* NspGtkVPaned inherits from NspGtkPaned
* just change some type attributes 
*/

typedef NspGtkPaned NspGtkVPaned ;
typedef NspTypeGtkPaned NspTypeGtkVPaned ;

extern int nsp_type_gtkvpaned_id;
extern NspTypeGtkVPaned *nsp_type_gtkvpaned;

/* type instances for gtkpaned */

NspTypeGtkVPaned *new_type_gtkvpaned(type_mode mode);

/* instance for GtkVPaned */

NspGtkVPaned *new_gtkvpaned();

/*
* Object methods redefined for gtkvpaned 
*/

#define NULLGTKVPANED (NspGtkVPaned*) 0

NspGtkVPaned *gtkvpaned_create(char *name,NspTypeBase *type);

/* from GtkVPanedObj.c */

extern NspGtkVPaned *gtkvpaned_object (NspObject *O); 
extern int IsGtkVPanedObj (Stack stack, int i); 
extern int IsGtkVPaned(NspObject *O);
extern NspGtkVPaned *GetGtkVPanedCopy (Stack stack, int i); 
extern NspGtkVPaned *GetGtkVPaned (Stack stack, int i); 

#endif 

#ifdef GtkVPaned_Private 
static int init_gtkvpaned(NspGtkVPaned *o,NspTypeGtkVPaned *type);
static char *gtkvpaned_type_as_string(void);
static char *gtkvpaned_type_short_string(NspObject *v);
static AttrTab gtkvpaned_attrs[];
/* static int int_gtkvpaned_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkvpaned_get_methods(void); 
#endif /* GtkVPaned_Private */
