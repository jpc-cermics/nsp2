/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkKeymap
#define INC_NSP_GdkKeymap

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGdkKeymap inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGdkKeymap ;
typedef NspTypeGObject NspTypeGdkKeymap ;

extern int nsp_type_gdkkeymap_id;
extern NspTypeGdkKeymap *nsp_type_gdkkeymap;

/* type instances for gobject */

NspTypeGdkKeymap *new_type_gdkkeymap(type_mode mode);

/* instance for GdkKeymap */

NspGdkKeymap *new_gdkkeymap();

/*
* Object methods redefined for gdkkeymap 
*/

#define NULLGDKKEYMAP (NspGdkKeymap*) 0

NspGdkKeymap *gdkkeymap_create(char *name,NspTypeBase *type);

/* from GdkKeymapObj.c */

extern NspGdkKeymap *gdkkeymap_object (NspObject *O); 
extern int IsGdkKeymapObj (Stack stack, int i); 
extern int IsGdkKeymap(NspObject *O);
extern NspGdkKeymap *GetGdkKeymapCopy (Stack stack, int i); 
extern NspGdkKeymap *GetGdkKeymap (Stack stack, int i); 

#endif 

#ifdef GdkKeymap_Private 
static int init_gdkkeymap(NspGdkKeymap *o,NspTypeGdkKeymap *type);
static char *gdkkeymap_type_as_string(void);
static char *gdkkeymap_type_short_string(NspObject *v);
static AttrTab gdkkeymap_attrs[];
/* static int int_gdkkeymap_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkkeymap_get_methods(void); 
#endif /* GdkKeymap_Private */
