/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkPixbufLoader
#define INC_NSP_GdkPixbufLoader

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGdkPixbufLoader inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGdkPixbufLoader ;
typedef NspTypeGObject NspTypeGdkPixbufLoader ;

extern int nsp_type_gdkpixbufloader_id;
extern NspTypeGdkPixbufLoader *nsp_type_gdkpixbufloader;

/* type instances for gobject */

NspTypeGdkPixbufLoader *new_type_gdkpixbufloader(type_mode mode);

/* instance for GdkPixbufLoader */

NspGdkPixbufLoader *new_gdkpixbufloader();

/*
* Object methods redefined for gdkpixbufloader 
*/

#define NULLGDKPIXBUFLOADER (NspGdkPixbufLoader*) 0

NspGdkPixbufLoader *gdkpixbufloader_create(char *name,NspTypeBase *type);

/* from GdkPixbufLoaderObj.c */

extern NspGdkPixbufLoader *gdkpixbufloader_object (NspObject *O); 
extern int IsGdkPixbufLoaderObj (Stack stack, int i); 
extern int IsGdkPixbufLoader(NspObject *O);
extern NspGdkPixbufLoader *GetGdkPixbufLoaderCopy (Stack stack, int i); 
extern NspGdkPixbufLoader *GetGdkPixbufLoader (Stack stack, int i); 

#endif 

#ifdef GdkPixbufLoader_Private 
static int init_gdkpixbufloader(NspGdkPixbufLoader *o,NspTypeGdkPixbufLoader *type);
static char *gdkpixbufloader_type_as_string(void);
static char *gdkpixbufloader_type_short_string(NspObject *v);
static AttrTab gdkpixbufloader_attrs[];
/* static int int_gdkpixbufloader_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkpixbufloader_get_methods(void); 
#endif /* GdkPixbufLoader_Private */
