/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkLabel
#define INC_NSP_GtkLabel

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkmisc.h"

/*
* NspGtkLabel inherits from NspGtkMisc
* just change some type attributes 
*/

typedef NspGtkMisc NspGtkLabel ;
typedef NspTypeGtkMisc NspTypeGtkLabel ;

extern int nsp_type_gtklabel_id;
extern NspTypeGtkLabel *nsp_type_gtklabel;

/* type instances for gtkmisc */

NspTypeGtkLabel *new_type_gtklabel(type_mode mode);

/* instance for GtkLabel */

NspGtkLabel *new_gtklabel();

/*
* Object methods redefined for gtklabel 
*/

#define NULLGTKLABEL (NspGtkLabel*) 0

NspGtkLabel *gtklabel_create(char *name,NspTypeBase *type);

/* from GtkLabelObj.c */

extern NspGtkLabel *gtklabel_object (NspObject *O); 
extern int IsGtkLabelObj (Stack stack, int i); 
extern int IsGtkLabel(NspObject *O);
extern NspGtkLabel *GetGtkLabelCopy (Stack stack, int i); 
extern NspGtkLabel *GetGtkLabel (Stack stack, int i); 

#endif 

#ifdef GtkLabel_Private 
static int init_gtklabel(NspGtkLabel *o,NspTypeGtkLabel *type);
static char *gtklabel_type_as_string(void);
static char *gtklabel_type_short_string(NspObject *v);
static AttrTab gtklabel_attrs[];
/* static int int_gtklabel_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtklabel_get_methods(void); 
#endif /* GtkLabel_Private */
