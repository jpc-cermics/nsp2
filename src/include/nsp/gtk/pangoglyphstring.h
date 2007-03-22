/* -*- Mode: C -*- */
#ifndef INC_NSP_PangoGlyphString
#define INC_NSP_PangoGlyphString

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspPangoGlyphString inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspPangoGlyphString ;
typedef NspTypeGBoxed NspTypePangoGlyphString ;

extern int nsp_type_pangoglyphstring_id;
extern NspTypePangoGlyphString *nsp_type_pangoglyphstring;

/* type instances for gboxed */

NspTypePangoGlyphString *new_type_pangoglyphstring(type_mode mode);

/* instance for PangoGlyphString */

NspPangoGlyphString *new_pangoglyphstring();

/*
* Object methods redefined for pangoglyphstring 
*/

#define NULLPANGOGLYPHSTRING (NspPangoGlyphString*) 0

NspPangoGlyphString *pangoglyphstring_create(char *name,NspTypeBase *type);

/* from PangoGlyphStringObj.c */

extern NspPangoGlyphString *pangoglyphstring_object (NspObject *O); 
extern int IsPangoGlyphStringObj (Stack stack, int i); 
extern int IsPangoGlyphString(NspObject *O);
extern NspPangoGlyphString *GetPangoGlyphStringCopy (Stack stack, int i); 
extern NspPangoGlyphString *GetPangoGlyphString (Stack stack, int i); 

#endif 

#ifdef PangoGlyphString_Private 
static int init_pangoglyphstring(NspPangoGlyphString *o,NspTypePangoGlyphString *type);
static char *pangoglyphstring_type_as_string(void);
static char *pangoglyphstring_type_short_string(NspObject *v);
static AttrTab pangoglyphstring_attrs[];
/* static int int_pangoglyphstring_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pangoglyphstring_get_methods(void); 
#endif /* PangoGlyphString_Private */
