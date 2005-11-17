/* -*- Mode: C -*- */
#ifndef INC_NSP_PangoFontset
#define INC_NSP_PangoFontset

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspPangoFontset inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspPangoFontset ;
typedef NspTypeGObject NspTypePangoFontset ;

extern int nsp_type_pangofontset_id;
extern NspTypePangoFontset *nsp_type_pangofontset;

/* type instances for gobject */

NspTypePangoFontset *new_type_pangofontset(type_mode mode);

/* instance for PangoFontset */

NspPangoFontset *new_pangofontset();

/*
* Object methods redefined for pangofontset 
*/

#define NULLPANGOFONTSET (NspPangoFontset*) 0

NspPangoFontset *pangofontset_create(char *name,NspTypeBase *type);

/* from PangoFontsetObj.c */

extern NspPangoFontset *pangofontset_object (NspObject *O); 
extern int IsPangoFontsetObj (Stack stack, int i); 
extern int IsPangoFontset(NspObject *O);
extern NspPangoFontset *GetPangoFontsetCopy (Stack stack, int i); 
extern NspPangoFontset *GetPangoFontset (Stack stack, int i); 

#endif 

#ifdef PangoFontset_Private 
static int init_pangofontset(NspPangoFontset *o,NspTypePangoFontset *type);
static char *pangofontset_type_as_string(void);
static char *pangofontset_type_short_string(void);
static AttrTab pangofontset_attrs[];
/* static int int_pangofontset_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pangofontset_get_methods(void); 
#endif /* PangoFontset_Private */
