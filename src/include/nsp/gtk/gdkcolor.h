/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkColor
#define INC_NSP_GdkColor

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGdkColor inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGdkColor ;
typedef NspTypeGBoxed NspTypeGdkColor ;

extern int nsp_type_gdkcolor_id;
extern NspTypeGdkColor *nsp_type_gdkcolor;

/* type instances for gboxed */

NspTypeGdkColor *new_type_gdkcolor(type_mode mode);

/* instance for GdkColor */

NspGdkColor *new_gdkcolor();

/*
* Object methods redefined for gdkcolor 
*/

#define NULLGDKCOLOR (NspGdkColor*) 0

NspGdkColor *gdkcolor_create(char *name,NspTypeBase *type);

/* from GdkColorObj.c */

extern NspGdkColor *gdkcolor_object (NspObject *O); 
extern int IsGdkColorObj (Stack stack, int i); 
extern int IsGdkColor(NspObject *O);
extern NspGdkColor *GetGdkColorCopy (Stack stack, int i); 
extern NspGdkColor *GetGdkColor (Stack stack, int i); 

#endif 

#ifdef GdkColor_Private 
static int init_gdkcolor(NspGdkColor *o,NspTypeGdkColor *type);
static char *gdkcolor_type_as_string(void);
static char *gdkcolor_type_short_string(void);
static AttrTab gdkcolor_attrs[];
/* static int int_gdkcolor_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkcolor_get_methods(void); 
#endif /* GdkColor_Private */
