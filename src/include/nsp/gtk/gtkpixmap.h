/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkPixmap
#define INC_NSP_GtkPixmap

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkmisc.h"

/*
* NspGtkPixmap inherits from NspGtkMisc
* just change some type attributes 
*/

typedef NspGtkMisc NspGtkPixmap ;
typedef NspTypeGtkMisc NspTypeGtkPixmap ;

extern int nsp_type_gtkpixmap_id;
extern NspTypeGtkPixmap *nsp_type_gtkpixmap;

/* type instances for gtkmisc */

NspTypeGtkPixmap *new_type_gtkpixmap(type_mode mode);

/* instance for GtkPixmap */

NspGtkPixmap *new_gtkpixmap();

/*
* Object methods redefined for gtkpixmap 
*/

#ifdef GtkPixmap_Private 
static int init_gtkpixmap(NspGtkPixmap *o,NspTypeGtkPixmap *type);
static char *gtkpixmap_type_as_string(void);
static char *gtkpixmap_type_short_string(void);
static AttrTab gtkpixmap_attrs[];
/* static int int_gtkpixmap_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkpixmap_get_methods(void); 
#endif /* GtkPixmap_Private */

#define NULLGTKPIXMAP (NspGtkPixmap*) 0

NspGtkPixmap *gtkpixmap_create(char *name,NspTypeBase *type);

/* from GtkPixmapObj.c */

extern NspGtkPixmap *gtkpixmap_object (NspObject *O); 
extern int IsGtkPixmapObj (Stack stack, int i); 
extern int IsGtkPixmap(NspObject *O);
extern NspGtkPixmap *GetGtkPixmapCopy (Stack stack, int i); 
extern NspGtkPixmap *GetGtkPixmap (Stack stack, int i); 

#endif 
