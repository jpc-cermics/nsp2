/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkAccessible
#define INC_NSP_GtkAccessible

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/atkobject.h"

/*
* NspGtkAccessible inherits from NspAtkObject
* just change some type attributes 
*/

typedef NspAtkObject NspGtkAccessible ;
typedef NspTypeAtkObject NspTypeGtkAccessible ;

extern int nsp_type_gtkaccessible_id;
extern NspTypeGtkAccessible *nsp_type_gtkaccessible;

/* type instances for atkobject */

NspTypeGtkAccessible *new_type_gtkaccessible(type_mode mode);

/* instance for GtkAccessible */

NspGtkAccessible *new_gtkaccessible();

/*
* Object methods redefined for gtkaccessible 
*/

#ifdef GtkAccessible_Private 
static int init_gtkaccessible(NspGtkAccessible *o,NspTypeGtkAccessible *type);
static char *gtkaccessible_type_as_string(void);
static char *gtkaccessible_type_short_string(void);
static AttrTab gtkaccessible_attrs[];
/* static int int_gtkaccessible_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkaccessible_get_methods(void); 
#endif /* GtkAccessible_Private */

#define NULLGTKACCESSIBLE (NspGtkAccessible*) 0

NspGtkAccessible *gtkaccessible_create(char *name,NspTypeBase *type);

/* from GtkAccessibleObj.c */

extern NspGtkAccessible *gtkaccessible_object (NspObject *O); 
extern int IsGtkAccessibleObj (Stack stack, int i); 
extern int IsGtkAccessible(NspObject *O);
extern NspGtkAccessible *GetGtkAccessibleCopy (Stack stack, int i); 
extern NspGtkAccessible *GetGtkAccessible (Stack stack, int i); 

#endif 
