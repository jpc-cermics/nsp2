/* -*- Mode: C -*- */
#ifndef INC_NSP_PangoFontFamily
#define INC_NSP_PangoFontFamily

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspPangoFontFamily inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspPangoFontFamily ;
typedef NspTypeGObject NspTypePangoFontFamily ;

extern int nsp_type_pangofontfamily_id;
extern NspTypePangoFontFamily *nsp_type_pangofontfamily;

/* type instances for gobject */

NspTypePangoFontFamily *new_type_pangofontfamily(type_mode mode);

/* instance for PangoFontFamily */

NspPangoFontFamily *new_pangofontfamily();

/*
* Object methods redefined for pangofontfamily 
*/

#ifdef PangoFontFamily_Private 
static int init_pangofontfamily(NspPangoFontFamily *o,NspTypePangoFontFamily *type);
static char *pangofontfamily_type_as_string(void);
static char *pangofontfamily_type_short_string(void);
static AttrTab pangofontfamily_attrs[];
/* static int int_pangofontfamily_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pangofontfamily_get_methods(void); 
#endif /* PangoFontFamily_Private */

#define NULLPANGOFONTFAMILY (NspPangoFontFamily*) 0

NspPangoFontFamily *pangofontfamily_create(char *name,NspTypeBase *type);

/* from PangoFontFamilyObj.c */

extern NspPangoFontFamily *pangofontfamily_object (NspObject *O); 
extern int IsPangoFontFamilyObj (Stack stack, int i); 
extern int IsPangoFontFamily(NspObject *O);
extern NspPangoFontFamily *GetPangoFontFamilyCopy (Stack stack, int i); 
extern NspPangoFontFamily *GetPangoFontFamily (Stack stack, int i); 

#endif 
