/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkIconSource
#define INC_NSP_GtkIconSource

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGtkIconSource inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGtkIconSource ;
typedef NspTypeGBoxed NspTypeGtkIconSource ;

extern int nsp_type_gtkiconsource_id;
extern NspTypeGtkIconSource *nsp_type_gtkiconsource;

/* type instances for gboxed */

NspTypeGtkIconSource *new_type_gtkiconsource(type_mode mode);

/* instance for GtkIconSource */

NspGtkIconSource *new_gtkiconsource();

/*
* Object methods redefined for gtkiconsource 
*/

#define NULLGTKICONSOURCE (NspGtkIconSource*) 0

NspGtkIconSource *gtkiconsource_create(char *name,NspTypeBase *type);

/* from GtkIconSourceObj.c */

extern NspGtkIconSource *gtkiconsource_object (NspObject *O); 
extern int IsGtkIconSourceObj (Stack stack, int i); 
extern int IsGtkIconSource(NspObject *O);
extern NspGtkIconSource *GetGtkIconSourceCopy (Stack stack, int i); 
extern NspGtkIconSource *GetGtkIconSource (Stack stack, int i); 

#endif 

#ifdef GtkIconSource_Private 
static int init_gtkiconsource(NspGtkIconSource *o,NspTypeGtkIconSource *type);
static char *gtkiconsource_type_as_string(void);
static char *gtkiconsource_type_short_string(NspObject *v);
static AttrTab gtkiconsource_attrs[];
/* static int int_gtkiconsource_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkiconsource_get_methods(void); 
#endif /* GtkIconSource_Private */
