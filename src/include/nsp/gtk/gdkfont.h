/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkFont
#define INC_NSP_GdkFont

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGdkFont inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGdkFont ;
typedef NspTypeGBoxed NspTypeGdkFont ;

extern int nsp_type_gdkfont_id;
extern NspTypeGdkFont *nsp_type_gdkfont;

/* type instances for gboxed */

NspTypeGdkFont *new_type_gdkfont(type_mode mode);

/* instance for GdkFont */

NspGdkFont *new_gdkfont();

/*
* Object methods redefined for gdkfont 
*/

#ifdef GdkFont_Private 
static int init_gdkfont(NspGdkFont *o,NspTypeGdkFont *type);
static char *gdkfont_type_as_string(void);
static char *gdkfont_type_short_string(void);
static AttrTab gdkfont_attrs[];
/* static int int_gdkfont_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkfont_get_methods(void); 
#endif /* GdkFont_Private */

#define NULLGDKFONT (NspGdkFont*) 0

NspGdkFont *gdkfont_create(char *name,NspTypeBase *type);

/* from GdkFontObj.c */

extern NspGdkFont *gdkfont_object (NspObject *O); 
extern int IsGdkFontObj (Stack stack, int i); 
extern int IsGdkFont(NspObject *O);
extern NspGdkFont *GetGdkFontCopy (Stack stack, int i); 
extern NspGdkFont *GetGdkFont (Stack stack, int i); 

#endif 
