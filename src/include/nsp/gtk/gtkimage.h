/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkImage
#define INC_NSP_GtkImage

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkmisc.h"

/*
* NspGtkImage inherits from NspGtkMisc
* just change some type attributes 
*/

typedef NspGtkMisc NspGtkImage ;
typedef NspTypeGtkMisc NspTypeGtkImage ;

extern int nsp_type_gtkimage_id;
extern NspTypeGtkImage *nsp_type_gtkimage;

/* type instances for gtkmisc */

NspTypeGtkImage *new_type_gtkimage(type_mode mode);

/* instance for GtkImage */

NspGtkImage *new_gtkimage();

/*
* Object methods redefined for gtkimage 
*/

#ifdef GtkImage_Private 
static int init_gtkimage(NspGtkImage *o,NspTypeGtkImage *type);
static char *gtkimage_type_as_string(void);
static char *gtkimage_type_short_string(void);
static AttrTab gtkimage_attrs[];
/* static int int_gtkimage_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkimage_get_methods(void); 
#endif /* GtkImage_Private */

#define NULLGTKIMAGE (NspGtkImage*) 0

NspGtkImage *gtkimage_create(char *name,NspTypeBase *type);

/* from GtkImageObj.c */

extern NspGtkImage *gtkimage_object (NspObject *O); 
extern int IsGtkImageObj (Stack stack, int i); 
extern int IsGtkImage(NspObject *O);
extern NspGtkImage *GetGtkImageCopy (Stack stack, int i); 
extern NspGtkImage *GetGtkImage (Stack stack, int i); 

#endif 
