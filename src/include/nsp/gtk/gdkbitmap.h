/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkBitmap
#define INC_NSP_GdkBitmap

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gdkdrawable.h"

/*
* NspGdkBitmap inherits from NspGdkDrawable
* just change some type attributes 
*/

typedef NspGdkDrawable NspGdkBitmap ;
typedef NspTypeGdkDrawable NspTypeGdkBitmap ;

extern int nsp_type_gdkbitmap_id;
extern NspTypeGdkBitmap *nsp_type_gdkbitmap;

/* type instances for gdkdrawable */

NspTypeGdkBitmap *new_type_gdkbitmap(type_mode mode);

/* instance for GdkBitmap */

NspGdkBitmap *new_gdkbitmap();

/*
* Object methods redefined for gdkbitmap 
*/

#define NULLGDKBITMAP (NspGdkBitmap*) 0

NspGdkBitmap *gdkbitmap_create(char *name,NspTypeBase *type);

/* from GdkBitmapObj.c */

extern NspGdkBitmap *gdkbitmap_object (NspObject *O); 
extern int IsGdkBitmapObj (Stack stack, int i); 
extern int IsGdkBitmap(NspObject *O);
extern NspGdkBitmap *GetGdkBitmapCopy (Stack stack, int i); 
extern NspGdkBitmap *GetGdkBitmap (Stack stack, int i); 

#endif 

#ifdef GdkBitmap_Private 
static int init_gdkbitmap(NspGdkBitmap *o,NspTypeGdkBitmap *type);
static char *gdkbitmap_type_as_string(void);
static char *gdkbitmap_type_short_string(void);
static AttrTab gdkbitmap_attrs[];
/* static int int_gdkbitmap_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkbitmap_get_methods(void); 
#endif /* GdkBitmap_Private */
