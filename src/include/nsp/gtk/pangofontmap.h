/* -*- Mode: C -*- */
#ifndef INC_NSP_PangoFontMap
#define INC_NSP_PangoFontMap

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspPangoFontMap inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspPangoFontMap ;
typedef NspTypeGObject NspTypePangoFontMap ;

extern int nsp_type_pangofontmap_id;
extern NspTypePangoFontMap *nsp_type_pangofontmap;

/* type instances for gobject */

NspTypePangoFontMap *new_type_pangofontmap(type_mode mode);

/* instance for PangoFontMap */

NspPangoFontMap *new_pangofontmap();

/*
* Object methods redefined for pangofontmap 
*/

#define NULLPANGOFONTMAP (NspPangoFontMap*) 0

NspPangoFontMap *pangofontmap_create(char *name,NspTypeBase *type);

/* from PangoFontMapObj.c */

extern NspPangoFontMap *pangofontmap_object (NspObject *O); 
extern int IsPangoFontMapObj (Stack stack, int i); 
extern int IsPangoFontMap(NspObject *O);
extern NspPangoFontMap *GetPangoFontMapCopy (Stack stack, int i); 
extern NspPangoFontMap *GetPangoFontMap (Stack stack, int i); 

#endif 

#ifdef PangoFontMap_Private 
static int init_pangofontmap(NspPangoFontMap *o,NspTypePangoFontMap *type);
static char *pangofontmap_type_as_string(void);
static char *pangofontmap_type_short_string(NspObject *v);
static AttrTab pangofontmap_attrs[];
/* static int int_pangofontmap_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pangofontmap_get_methods(void); 
#endif /* PangoFontMap_Private */
