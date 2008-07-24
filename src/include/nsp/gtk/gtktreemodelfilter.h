/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTreeModelFilter
#define INC_NSP_GtkTreeModelFilter

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkTreeModelFilter inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkTreeModelFilter ;
typedef NspTypeGObject NspTypeGtkTreeModelFilter ;

extern int nsp_type_gtktreemodelfilter_id;
extern NspTypeGtkTreeModelFilter *nsp_type_gtktreemodelfilter;

/* type instances for gobject */

NspTypeGtkTreeModelFilter *new_type_gtktreemodelfilter(type_mode mode);

/* instance for GtkTreeModelFilter */

NspGtkTreeModelFilter *new_gtktreemodelfilter();

/*
* Object methods redefined for gtktreemodelfilter 
*/

#define NULLGTKTREEMODELFILTER (NspGtkTreeModelFilter*) 0

NspGtkTreeModelFilter *gtktreemodelfilter_create(char *name,NspTypeBase *type);

/* from GtkTreeModelFilterObj.c */

extern NspGtkTreeModelFilter *gtktreemodelfilter_object (NspObject *O); 
extern int IsGtkTreeModelFilterObj (Stack stack, int i); 
extern int IsGtkTreeModelFilter(NspObject *O);
extern NspGtkTreeModelFilter *GetGtkTreeModelFilterCopy (Stack stack, int i); 
extern NspGtkTreeModelFilter *GetGtkTreeModelFilter (Stack stack, int i); 

#endif 

#ifdef GtkTreeModelFilter_Private 
static int init_gtktreemodelfilter(NspGtkTreeModelFilter *o,NspTypeGtkTreeModelFilter *type);
static char *gtktreemodelfilter_type_as_string(void);
static char *gtktreemodelfilter_type_short_string(NspObject *v);
static AttrTab gtktreemodelfilter_attrs[];
/* static int int_gtktreemodelfilter_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktreemodelfilter_get_methods(void); 
#endif /* GtkTreeModelFilter_Private */
