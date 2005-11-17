/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkHPaned
#define INC_NSP_GtkHPaned

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkpaned.h"

/*
* NspGtkHPaned inherits from NspGtkPaned
* just change some type attributes 
*/

typedef NspGtkPaned NspGtkHPaned ;
typedef NspTypeGtkPaned NspTypeGtkHPaned ;

extern int nsp_type_gtkhpaned_id;
extern NspTypeGtkHPaned *nsp_type_gtkhpaned;

/* type instances for gtkpaned */

NspTypeGtkHPaned *new_type_gtkhpaned(type_mode mode);

/* instance for GtkHPaned */

NspGtkHPaned *new_gtkhpaned();

/*
* Object methods redefined for gtkhpaned 
*/

#define NULLGTKHPANED (NspGtkHPaned*) 0

NspGtkHPaned *gtkhpaned_create(char *name,NspTypeBase *type);

/* from GtkHPanedObj.c */

extern NspGtkHPaned *gtkhpaned_object (NspObject *O); 
extern int IsGtkHPanedObj (Stack stack, int i); 
extern int IsGtkHPaned(NspObject *O);
extern NspGtkHPaned *GetGtkHPanedCopy (Stack stack, int i); 
extern NspGtkHPaned *GetGtkHPaned (Stack stack, int i); 

#endif 

#ifdef GtkHPaned_Private 
static int init_gtkhpaned(NspGtkHPaned *o,NspTypeGtkHPaned *type);
static char *gtkhpaned_type_as_string(void);
static char *gtkhpaned_type_short_string(void);
static AttrTab gtkhpaned_attrs[];
/* static int int_gtkhpaned_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkhpaned_get_methods(void); 
#endif /* GtkHPaned_Private */
