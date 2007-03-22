/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkGC
#define INC_NSP_GdkGC

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGdkGC inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGdkGC ;
typedef NspTypeGObject NspTypeGdkGC ;

extern int nsp_type_gdkgc_id;
extern NspTypeGdkGC *nsp_type_gdkgc;

/* type instances for gobject */

NspTypeGdkGC *new_type_gdkgc(type_mode mode);

/* instance for GdkGC */

NspGdkGC *new_gdkgc();

/*
* Object methods redefined for gdkgc 
*/

#define NULLGDKGC (NspGdkGC*) 0

NspGdkGC *gdkgc_create(char *name,NspTypeBase *type);

/* from GdkGCObj.c */

extern NspGdkGC *gdkgc_object (NspObject *O); 
extern int IsGdkGCObj (Stack stack, int i); 
extern int IsGdkGC(NspObject *O);
extern NspGdkGC *GetGdkGCCopy (Stack stack, int i); 
extern NspGdkGC *GetGdkGC (Stack stack, int i); 

#endif 

#ifdef GdkGC_Private 
static int init_gdkgc(NspGdkGC *o,NspTypeGdkGC *type);
static char *gdkgc_type_as_string(void);
static char *gdkgc_type_short_string(NspObject *v);
static AttrTab gdkgc_attrs[];
/* static int int_gdkgc_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkgc_get_methods(void); 
#endif /* GdkGC_Private */
