/* -*- Mode: C -*- */
#ifndef INC_NSP_PangoFontDescription
#define INC_NSP_PangoFontDescription

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspPangoFontDescription inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspPangoFontDescription ;
typedef NspTypeGBoxed NspTypePangoFontDescription ;

extern int nsp_type_pangofontdescription_id;
extern NspTypePangoFontDescription *nsp_type_pangofontdescription;

/* type instances for gboxed */

NspTypePangoFontDescription *new_type_pangofontdescription(type_mode mode);

/* instance for PangoFontDescription */

NspPangoFontDescription *new_pangofontdescription();

/*
* Object methods redefined for pangofontdescription 
*/

#define NULLPANGOFONTDESCRIPTION (NspPangoFontDescription*) 0

NspPangoFontDescription *pangofontdescription_create(char *name,NspTypeBase *type);

/* from PangoFontDescriptionObj.c */

extern NspPangoFontDescription *pangofontdescription_object (NspObject *O); 
extern int IsPangoFontDescriptionObj (Stack stack, int i); 
extern int IsPangoFontDescription(NspObject *O);
extern NspPangoFontDescription *GetPangoFontDescriptionCopy (Stack stack, int i); 
extern NspPangoFontDescription *GetPangoFontDescription (Stack stack, int i); 

#endif 

#ifdef PangoFontDescription_Private 
static int init_pangofontdescription(NspPangoFontDescription *o,NspTypePangoFontDescription *type);
static char *pangofontdescription_type_as_string(void);
static char *pangofontdescription_type_short_string(NspObject *v);
static AttrTab pangofontdescription_attrs[];
/* static int int_pangofontdescription_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pangofontdescription_get_methods(void); 
#endif /* PangoFontDescription_Private */
