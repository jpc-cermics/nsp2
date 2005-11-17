/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkIconSet
#define INC_NSP_GtkIconSet

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGtkIconSet inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGtkIconSet ;
typedef NspTypeGBoxed NspTypeGtkIconSet ;

extern int nsp_type_gtkiconset_id;
extern NspTypeGtkIconSet *nsp_type_gtkiconset;

/* type instances for gboxed */

NspTypeGtkIconSet *new_type_gtkiconset(type_mode mode);

/* instance for GtkIconSet */

NspGtkIconSet *new_gtkiconset();

/*
* Object methods redefined for gtkiconset 
*/

#define NULLGTKICONSET (NspGtkIconSet*) 0

NspGtkIconSet *gtkiconset_create(char *name,NspTypeBase *type);

/* from GtkIconSetObj.c */

extern NspGtkIconSet *gtkiconset_object (NspObject *O); 
extern int IsGtkIconSetObj (Stack stack, int i); 
extern int IsGtkIconSet(NspObject *O);
extern NspGtkIconSet *GetGtkIconSetCopy (Stack stack, int i); 
extern NspGtkIconSet *GetGtkIconSet (Stack stack, int i); 

#endif 

#ifdef GtkIconSet_Private 
static int init_gtkiconset(NspGtkIconSet *o,NspTypeGtkIconSet *type);
static char *gtkiconset_type_as_string(void);
static char *gtkiconset_type_short_string(void);
static AttrTab gtkiconset_attrs[];
/* static int int_gtkiconset_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkiconset_get_methods(void); 
#endif /* GtkIconSet_Private */
