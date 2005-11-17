/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkLayout
#define INC_NSP_GtkLayout

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspGtkLayout inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspGtkLayout ;
typedef NspTypeGtkContainer NspTypeGtkLayout ;

extern int nsp_type_gtklayout_id;
extern NspTypeGtkLayout *nsp_type_gtklayout;

/* type instances for gtkcontainer */

NspTypeGtkLayout *new_type_gtklayout(type_mode mode);

/* instance for GtkLayout */

NspGtkLayout *new_gtklayout();

/*
* Object methods redefined for gtklayout 
*/

#define NULLGTKLAYOUT (NspGtkLayout*) 0

NspGtkLayout *gtklayout_create(char *name,NspTypeBase *type);

/* from GtkLayoutObj.c */

extern NspGtkLayout *gtklayout_object (NspObject *O); 
extern int IsGtkLayoutObj (Stack stack, int i); 
extern int IsGtkLayout(NspObject *O);
extern NspGtkLayout *GetGtkLayoutCopy (Stack stack, int i); 
extern NspGtkLayout *GetGtkLayout (Stack stack, int i); 

#endif 

#ifdef GtkLayout_Private 
static int init_gtklayout(NspGtkLayout *o,NspTypeGtkLayout *type);
static char *gtklayout_type_as_string(void);
static char *gtklayout_type_short_string(void);
static AttrTab gtklayout_attrs[];
/* static int int_gtklayout_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtklayout_get_methods(void); 
#endif /* GtkLayout_Private */
