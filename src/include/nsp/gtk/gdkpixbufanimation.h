/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkPixbufAnimation
#define INC_NSP_GdkPixbufAnimation

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGdkPixbufAnimation inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGdkPixbufAnimation ;
typedef NspTypeGObject NspTypeGdkPixbufAnimation ;

extern int nsp_type_gdkpixbufanimation_id;
extern NspTypeGdkPixbufAnimation *nsp_type_gdkpixbufanimation;

/* type instances for gobject */

NspTypeGdkPixbufAnimation *new_type_gdkpixbufanimation(type_mode mode);

/* instance for GdkPixbufAnimation */

NspGdkPixbufAnimation *new_gdkpixbufanimation();

/*
* Object methods redefined for gdkpixbufanimation 
*/

#ifdef GdkPixbufAnimation_Private 
static int init_gdkpixbufanimation(NspGdkPixbufAnimation *o,NspTypeGdkPixbufAnimation *type);
static char *gdkpixbufanimation_type_as_string(void);
static char *gdkpixbufanimation_type_short_string(void);
static AttrTab gdkpixbufanimation_attrs[];
/* static int int_gdkpixbufanimation_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkpixbufanimation_get_methods(void); 
#endif /* GdkPixbufAnimation_Private */

#define NULLGDKPIXBUFANIMATION (NspGdkPixbufAnimation*) 0

NspGdkPixbufAnimation *gdkpixbufanimation_create(char *name,NspTypeBase *type);

/* from GdkPixbufAnimationObj.c */

extern NspGdkPixbufAnimation *gdkpixbufanimation_object (NspObject *O); 
extern int IsGdkPixbufAnimationObj (Stack stack, int i); 
extern int IsGdkPixbufAnimation(NspObject *O);
extern NspGdkPixbufAnimation *GetGdkPixbufAnimationCopy (Stack stack, int i); 
extern NspGdkPixbufAnimation *GetGdkPixbufAnimation (Stack stack, int i); 

#endif 
