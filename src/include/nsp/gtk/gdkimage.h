/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkImage
#define INC_NSP_GdkImage

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGdkImage inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGdkImage ;
typedef NspTypeGObject NspTypeGdkImage ;

extern int nsp_type_gdkimage_id;
extern NspTypeGdkImage *nsp_type_gdkimage;

/* type instances for gobject */

NspTypeGdkImage *new_type_gdkimage(type_mode mode);

/* instance for GdkImage */

NspGdkImage *new_gdkimage();

/*
* Object methods redefined for gdkimage 
*/

#ifdef GdkImage_Private 
static int init_gdkimage(NspGdkImage *o,NspTypeGdkImage *type);
static char *gdkimage_type_as_string(void);
static char *gdkimage_type_short_string(void);
static AttrTab gdkimage_attrs[];
/* static int int_gdkimage_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkimage_get_methods(void); 
#endif /* GdkImage_Private */

#define NULLGDKIMAGE (NspGdkImage*) 0

NspGdkImage *gdkimage_create(char *name,NspTypeBase *type);

/* from GdkImageObj.c */

extern NspGdkImage *gdkimage_object (NspObject *O); 
extern int IsGdkImageObj (Stack stack, int i); 
extern int IsGdkImage(NspObject *O);
extern NspGdkImage *GetGdkImageCopy (Stack stack, int i); 
extern NspGdkImage *GetGdkImage (Stack stack, int i); 

#endif 
