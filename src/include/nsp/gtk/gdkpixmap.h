/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkPixmap
#define INC_NSP_GdkPixmap

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gdkdrawable.h"

/*
* NspGdkPixmap inherits from NspGdkDrawable
* just change some type attributes 
*/

typedef NspGdkDrawable NspGdkPixmap ;
typedef NspTypeGdkDrawable NspTypeGdkPixmap ;

extern int nsp_type_gdkpixmap_id;
extern NspTypeGdkPixmap *nsp_type_gdkpixmap;

/* type instances for gdkdrawable */

NspTypeGdkPixmap *new_type_gdkpixmap(type_mode mode);

/* instance for GdkPixmap */

NspGdkPixmap *new_gdkpixmap();

/*
* Object methods redefined for gdkpixmap 
*/

#define NULLGDKPIXMAP (NspGdkPixmap*) 0

NspGdkPixmap *gdkpixmap_create(char *name,NspTypeBase *type);

/* from GdkPixmapObj.c */

extern NspGdkPixmap *gdkpixmap_object (NspObject *O); 
extern int IsGdkPixmapObj (Stack stack, int i); 
extern int IsGdkPixmap(NspObject *O);
extern NspGdkPixmap *GetGdkPixmapCopy (Stack stack, int i); 
extern NspGdkPixmap *GetGdkPixmap (Stack stack, int i); 

#endif 

#ifdef GdkPixmap_Private 
static int init_gdkpixmap(NspGdkPixmap *o,NspTypeGdkPixmap *type);
static char *gdkpixmap_type_as_string(void);
static char *gdkpixmap_type_short_string(NspObject *v);
static AttrTab gdkpixmap_attrs[];
/* static int int_gdkpixmap_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkpixmap_get_methods(void); 
#endif /* GdkPixmap_Private */
