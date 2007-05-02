/* -*- Mode: C -*- */
#ifndef INC_NSP_PangoFont
#define INC_NSP_PangoFont

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspPangoFont inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspPangoFont ;
typedef NspTypeGObject NspTypePangoFont ;

extern int nsp_type_pangofont_id;
extern NspTypePangoFont *nsp_type_pangofont;

/* type instances for gobject */

NspTypePangoFont *new_type_pangofont(type_mode mode);

/* instance for PangoFont */

NspPangoFont *new_pangofont();

/*
* Object methods redefined for pangofont 
*/

#define NULLPANGOFONT (NspPangoFont*) 0

NspPangoFont *pangofont_create(char *name,NspTypeBase *type);

/* from PangoFontObj.c */

extern NspPangoFont *pangofont_object (NspObject *O); 
extern int IsPangoFontObj (Stack stack, int i); 
extern int IsPangoFont(NspObject *O);
extern NspPangoFont *GetPangoFontCopy (Stack stack, int i); 
extern NspPangoFont *GetPangoFont (Stack stack, int i); 

#endif 

#ifdef PangoFont_Private 
static int init_pangofont(NspPangoFont *o,NspTypePangoFont *type);
static char *pangofont_type_as_string(void);
static char *pangofont_type_short_string(NspObject *v);
static AttrTab pangofont_attrs[];
/* static int int_pangofont_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pangofont_get_methods(void); 
#endif /* PangoFont_Private */
