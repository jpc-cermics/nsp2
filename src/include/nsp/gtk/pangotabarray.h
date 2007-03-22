/* -*- Mode: C -*- */
#ifndef INC_NSP_PangoTabArray
#define INC_NSP_PangoTabArray

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspPangoTabArray inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspPangoTabArray ;
typedef NspTypeGBoxed NspTypePangoTabArray ;

extern int nsp_type_pangotabarray_id;
extern NspTypePangoTabArray *nsp_type_pangotabarray;

/* type instances for gboxed */

NspTypePangoTabArray *new_type_pangotabarray(type_mode mode);

/* instance for PangoTabArray */

NspPangoTabArray *new_pangotabarray();

/*
* Object methods redefined for pangotabarray 
*/

#define NULLPANGOTABARRAY (NspPangoTabArray*) 0

NspPangoTabArray *pangotabarray_create(char *name,NspTypeBase *type);

/* from PangoTabArrayObj.c */

extern NspPangoTabArray *pangotabarray_object (NspObject *O); 
extern int IsPangoTabArrayObj (Stack stack, int i); 
extern int IsPangoTabArray(NspObject *O);
extern NspPangoTabArray *GetPangoTabArrayCopy (Stack stack, int i); 
extern NspPangoTabArray *GetPangoTabArray (Stack stack, int i); 

#endif 

#ifdef PangoTabArray_Private 
static int init_pangotabarray(NspPangoTabArray *o,NspTypePangoTabArray *type);
static char *pangotabarray_type_as_string(void);
static char *pangotabarray_type_short_string(NspObject *v);
static AttrTab pangotabarray_attrs[];
/* static int int_pangotabarray_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pangotabarray_get_methods(void); 
#endif /* PangoTabArray_Private */
