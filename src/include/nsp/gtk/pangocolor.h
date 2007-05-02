/* -*- Mode: C -*- */
#ifndef INC_NSP_PangoColor
#define INC_NSP_PangoColor

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspPangoColor inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspPangoColor ;
typedef NspTypeGBoxed NspTypePangoColor ;

extern int nsp_type_pangocolor_id;
extern NspTypePangoColor *nsp_type_pangocolor;

/* type instances for gboxed */

NspTypePangoColor *new_type_pangocolor(type_mode mode);

/* instance for PangoColor */

NspPangoColor *new_pangocolor();

/*
* Object methods redefined for pangocolor 
*/

#define NULLPANGOCOLOR (NspPangoColor*) 0

NspPangoColor *pangocolor_create(char *name,NspTypeBase *type);

/* from PangoColorObj.c */

extern NspPangoColor *pangocolor_object (NspObject *O); 
extern int IsPangoColorObj (Stack stack, int i); 
extern int IsPangoColor(NspObject *O);
extern NspPangoColor *GetPangoColorCopy (Stack stack, int i); 
extern NspPangoColor *GetPangoColor (Stack stack, int i); 

#endif 

#ifdef PangoColor_Private 
static int init_pangocolor(NspPangoColor *o,NspTypePangoColor *type);
static char *pangocolor_type_as_string(void);
static char *pangocolor_type_short_string(NspObject *v);
static AttrTab pangocolor_attrs[];
/* static int int_pangocolor_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pangocolor_get_methods(void); 
#endif /* PangoColor_Private */
