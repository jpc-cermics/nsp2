/* -*- Mode: C -*- */
#ifndef INC_NSP_PangoFontFace
#define INC_NSP_PangoFontFace

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspPangoFontFace inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspPangoFontFace ;
typedef NspTypeGObject NspTypePangoFontFace ;

extern int nsp_type_pangofontface_id;
extern NspTypePangoFontFace *nsp_type_pangofontface;

/* type instances for gobject */

NspTypePangoFontFace *new_type_pangofontface(type_mode mode);

/* instance for PangoFontFace */

NspPangoFontFace *new_pangofontface();

/*
* Object methods redefined for pangofontface 
*/

#define NULLPANGOFONTFACE (NspPangoFontFace*) 0

NspPangoFontFace *pangofontface_create(char *name,NspTypeBase *type);

/* from PangoFontFaceObj.c */

extern NspPangoFontFace *pangofontface_object (NspObject *O); 
extern int IsPangoFontFaceObj (Stack stack, int i); 
extern int IsPangoFontFace(NspObject *O);
extern NspPangoFontFace *GetPangoFontFaceCopy (Stack stack, int i); 
extern NspPangoFontFace *GetPangoFontFace (Stack stack, int i); 

#endif 

#ifdef PangoFontFace_Private 
static int init_pangofontface(NspPangoFontFace *o,NspTypePangoFontFace *type);
static char *pangofontface_type_as_string(void);
static char *pangofontface_type_short_string(NspObject *v);
static AttrTab pangofontface_attrs[];
/* static int int_pangofontface_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pangofontface_get_methods(void); 
#endif /* PangoFontFace_Private */
