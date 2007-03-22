/* -*- Mode: C -*- */
#ifndef INC_NSP_PangoLayout
#define INC_NSP_PangoLayout

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspPangoLayout inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspPangoLayout ;
typedef NspTypeGObject NspTypePangoLayout ;

extern int nsp_type_pangolayout_id;
extern NspTypePangoLayout *nsp_type_pangolayout;

/* type instances for gobject */

NspTypePangoLayout *new_type_pangolayout(type_mode mode);

/* instance for PangoLayout */

NspPangoLayout *new_pangolayout();

/*
* Object methods redefined for pangolayout 
*/

#define NULLPANGOLAYOUT (NspPangoLayout*) 0

NspPangoLayout *pangolayout_create(char *name,NspTypeBase *type);

/* from PangoLayoutObj.c */

extern NspPangoLayout *pangolayout_object (NspObject *O); 
extern int IsPangoLayoutObj (Stack stack, int i); 
extern int IsPangoLayout(NspObject *O);
extern NspPangoLayout *GetPangoLayoutCopy (Stack stack, int i); 
extern NspPangoLayout *GetPangoLayout (Stack stack, int i); 

#endif 

#ifdef PangoLayout_Private 
static int init_pangolayout(NspPangoLayout *o,NspTypePangoLayout *type);
static char *pangolayout_type_as_string(void);
static char *pangolayout_type_short_string(NspObject *v);
static AttrTab pangolayout_attrs[];
/* static int int_pangolayout_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pangolayout_get_methods(void); 
#endif /* PangoLayout_Private */
