/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkPixbuf
#define INC_NSP_GdkPixbuf

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGdkPixbuf inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGdkPixbuf ;
typedef NspTypeGObject NspTypeGdkPixbuf ;

extern int nsp_type_gdkpixbuf_id;
extern NspTypeGdkPixbuf *nsp_type_gdkpixbuf;

/* type instances for gobject */

NspTypeGdkPixbuf *new_type_gdkpixbuf(type_mode mode);

/* instance for GdkPixbuf */

NspGdkPixbuf *new_gdkpixbuf();

/*
* Object methods redefined for gdkpixbuf 
*/

#define NULLGDKPIXBUF (NspGdkPixbuf*) 0

NspGdkPixbuf *gdkpixbuf_create(char *name,NspTypeBase *type);

/* from GdkPixbufObj.c */

extern NspGdkPixbuf *gdkpixbuf_object (NspObject *O); 
extern int IsGdkPixbufObj (Stack stack, int i); 
extern int IsGdkPixbuf(NspObject *O);
extern NspGdkPixbuf *GetGdkPixbufCopy (Stack stack, int i); 
extern NspGdkPixbuf *GetGdkPixbuf (Stack stack, int i); 

#endif 

#ifdef GdkPixbuf_Private 
static int init_gdkpixbuf(NspGdkPixbuf *o,NspTypeGdkPixbuf *type);
static char *gdkpixbuf_type_as_string(void);
static char *gdkpixbuf_type_short_string(NspObject *v);
static AttrTab gdkpixbuf_attrs[];
/* static int int_gdkpixbuf_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkpixbuf_get_methods(void); 
#endif /* GdkPixbuf_Private */
