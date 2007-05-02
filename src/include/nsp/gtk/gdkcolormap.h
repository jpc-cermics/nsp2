/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkColormap
#define INC_NSP_GdkColormap

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGdkColormap inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGdkColormap ;
typedef NspTypeGObject NspTypeGdkColormap ;

extern int nsp_type_gdkcolormap_id;
extern NspTypeGdkColormap *nsp_type_gdkcolormap;

/* type instances for gobject */

NspTypeGdkColormap *new_type_gdkcolormap(type_mode mode);

/* instance for GdkColormap */

NspGdkColormap *new_gdkcolormap();

/*
* Object methods redefined for gdkcolormap 
*/

#define NULLGDKCOLORMAP (NspGdkColormap*) 0

NspGdkColormap *gdkcolormap_create(char *name,NspTypeBase *type);

/* from GdkColormapObj.c */

extern NspGdkColormap *gdkcolormap_object (NspObject *O); 
extern int IsGdkColormapObj (Stack stack, int i); 
extern int IsGdkColormap(NspObject *O);
extern NspGdkColormap *GetGdkColormapCopy (Stack stack, int i); 
extern NspGdkColormap *GetGdkColormap (Stack stack, int i); 

#endif 

#ifdef GdkColormap_Private 
static int init_gdkcolormap(NspGdkColormap *o,NspTypeGdkColormap *type);
static char *gdkcolormap_type_as_string(void);
static char *gdkcolormap_type_short_string(NspObject *v);
static AttrTab gdkcolormap_attrs[];
/* static int int_gdkcolormap_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkcolormap_get_methods(void); 
#endif /* GdkColormap_Private */
