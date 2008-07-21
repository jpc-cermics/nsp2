/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkFileFilter
#define INC_NSP_GtkFileFilter

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkobject.h"

/*
* NspGtkFileFilter inherits from NspGtkObject
* just change some type attributes 
*/

typedef NspGtkObject NspGtkFileFilter ;
typedef NspTypeGtkObject NspTypeGtkFileFilter ;

extern int nsp_type_gtkfilefilter_id;
extern NspTypeGtkFileFilter *nsp_type_gtkfilefilter;

/* type instances for gtkobject */

NspTypeGtkFileFilter *new_type_gtkfilefilter(type_mode mode);

/* instance for GtkFileFilter */

NspGtkFileFilter *new_gtkfilefilter();

/*
* Object methods redefined for gtkfilefilter 
*/

#define NULLGTKFILEFILTER (NspGtkFileFilter*) 0

NspGtkFileFilter *gtkfilefilter_create(char *name,NspTypeBase *type);

/* from GtkFileFilterObj.c */

extern NspGtkFileFilter *gtkfilefilter_object (NspObject *O); 
extern int IsGtkFileFilterObj (Stack stack, int i); 
extern int IsGtkFileFilter(NspObject *O);
extern NspGtkFileFilter *GetGtkFileFilterCopy (Stack stack, int i); 
extern NspGtkFileFilter *GetGtkFileFilter (Stack stack, int i); 

#endif 

#ifdef GtkFileFilter_Private 
static int init_gtkfilefilter(NspGtkFileFilter *o,NspTypeGtkFileFilter *type);
static char *gtkfilefilter_type_as_string(void);
static char *gtkfilefilter_type_short_string(NspObject *v);
static AttrTab gtkfilefilter_attrs[];
/* static int int_gtkfilefilter_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkfilefilter_get_methods(void); 
#endif /* GtkFileFilter_Private */
