/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkPixbufAnimationIter
#define INC_NSP_GdkPixbufAnimationIter

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGdkPixbufAnimationIter inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGdkPixbufAnimationIter ;
typedef NspTypeGObject NspTypeGdkPixbufAnimationIter ;

extern int nsp_type_gdkpixbufanimationiter_id;
extern NspTypeGdkPixbufAnimationIter *nsp_type_gdkpixbufanimationiter;

/* type instances for gobject */

NspTypeGdkPixbufAnimationIter *new_type_gdkpixbufanimationiter(type_mode mode);

/* instance for GdkPixbufAnimationIter */

NspGdkPixbufAnimationIter *new_gdkpixbufanimationiter();

/*
* Object methods redefined for gdkpixbufanimationiter 
*/

#define NULLGDKPIXBUFANIMATIONITER (NspGdkPixbufAnimationIter*) 0

NspGdkPixbufAnimationIter *gdkpixbufanimationiter_create(char *name,NspTypeBase *type);

/* from GdkPixbufAnimationIterObj.c */

extern NspGdkPixbufAnimationIter *gdkpixbufanimationiter_object (NspObject *O); 
extern int IsGdkPixbufAnimationIterObj (Stack stack, int i); 
extern int IsGdkPixbufAnimationIter(NspObject *O);
extern NspGdkPixbufAnimationIter *GetGdkPixbufAnimationIterCopy (Stack stack, int i); 
extern NspGdkPixbufAnimationIter *GetGdkPixbufAnimationIter (Stack stack, int i); 

#endif 

#ifdef GdkPixbufAnimationIter_Private 
static int init_gdkpixbufanimationiter(NspGdkPixbufAnimationIter *o,NspTypeGdkPixbufAnimationIter *type);
static char *gdkpixbufanimationiter_type_as_string(void);
static char *gdkpixbufanimationiter_type_short_string(NspObject *v);
static AttrTab gdkpixbufanimationiter_attrs[];
/* static int int_gdkpixbufanimationiter_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkpixbufanimationiter_get_methods(void); 
#endif /* GdkPixbufAnimationIter_Private */
