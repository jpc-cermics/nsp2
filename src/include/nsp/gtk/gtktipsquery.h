/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTipsQuery
#define INC_NSP_GtkTipsQuery

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkobject.h"

/*
* NspGtkTipsQuery inherits from NspGtkObject
* just change some type attributes 
*/

typedef NspGtkObject NspGtkTipsQuery ;
typedef NspTypeGtkObject NspTypeGtkTipsQuery ;

extern int nsp_type_gtktipsquery_id;
extern NspTypeGtkTipsQuery *nsp_type_gtktipsquery;

/* type instances for gtkobject */

NspTypeGtkTipsQuery *new_type_gtktipsquery(type_mode mode);

/* instance for GtkTipsQuery */

NspGtkTipsQuery *new_gtktipsquery();

/*
* Object methods redefined for gtktipsquery 
*/

#ifdef GtkTipsQuery_Private 
static int init_gtktipsquery(NspGtkTipsQuery *o,NspTypeGtkTipsQuery *type);
static char *gtktipsquery_type_as_string(void);
static char *gtktipsquery_type_short_string(void);
static AttrTab gtktipsquery_attrs[];
/* static int int_gtktipsquery_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktipsquery_get_methods(void); 
#endif /* GtkTipsQuery_Private */

#define NULLGTKTIPSQUERY (NspGtkTipsQuery*) 0

NspGtkTipsQuery *gtktipsquery_create(char *name,NspTypeBase *type);

/* from GtkTipsQueryObj.c */

extern NspGtkTipsQuery *gtktipsquery_object (NspObject *O); 
extern int IsGtkTipsQueryObj (Stack stack, int i); 
extern int IsGtkTipsQuery(NspObject *O);
extern NspGtkTipsQuery *GetGtkTipsQueryCopy (Stack stack, int i); 
extern NspGtkTipsQuery *GetGtkTipsQuery (Stack stack, int i); 

#endif 
