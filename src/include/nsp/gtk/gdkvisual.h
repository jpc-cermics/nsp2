/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkVisual
#define INC_NSP_GdkVisual

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGdkVisual inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGdkVisual ;
typedef NspTypeGObject NspTypeGdkVisual ;

extern int nsp_type_gdkvisual_id;
extern NspTypeGdkVisual *nsp_type_gdkvisual;

/* type instances for gobject */

NspTypeGdkVisual *new_type_gdkvisual(type_mode mode);

/* instance for GdkVisual */

NspGdkVisual *new_gdkvisual();

/*
* Object methods redefined for gdkvisual 
*/

#define NULLGDKVISUAL (NspGdkVisual*) 0

NspGdkVisual *gdkvisual_create(char *name,NspTypeBase *type);

/* from GdkVisualObj.c */

extern NspGdkVisual *gdkvisual_object (NspObject *O); 
extern int IsGdkVisualObj (Stack stack, int i); 
extern int IsGdkVisual(NspObject *O);
extern NspGdkVisual *GetGdkVisualCopy (Stack stack, int i); 
extern NspGdkVisual *GetGdkVisual (Stack stack, int i); 

#endif 

#ifdef GdkVisual_Private 
static int init_gdkvisual(NspGdkVisual *o,NspTypeGdkVisual *type);
static char *gdkvisual_type_as_string(void);
static char *gdkvisual_type_short_string(void);
static AttrTab gdkvisual_attrs[];
/* static int int_gdkvisual_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkvisual_get_methods(void); 
#endif /* GdkVisual_Private */
