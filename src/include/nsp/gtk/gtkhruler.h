/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkHRuler
#define INC_NSP_GtkHRuler

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkruler.h"

/*
* NspGtkHRuler inherits from NspGtkRuler
* just change some type attributes 
*/

typedef NspGtkRuler NspGtkHRuler ;
typedef NspTypeGtkRuler NspTypeGtkHRuler ;

extern int nsp_type_gtkhruler_id;
extern NspTypeGtkHRuler *nsp_type_gtkhruler;

/* type instances for gtkruler */

NspTypeGtkHRuler *new_type_gtkhruler(type_mode mode);

/* instance for GtkHRuler */

NspGtkHRuler *new_gtkhruler();

/*
* Object methods redefined for gtkhruler 
*/

#ifdef GtkHRuler_Private 
static int init_gtkhruler(NspGtkHRuler *o,NspTypeGtkHRuler *type);
static char *gtkhruler_type_as_string(void);
static char *gtkhruler_type_short_string(void);
static AttrTab gtkhruler_attrs[];
/* static int int_gtkhruler_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkhruler_get_methods(void); 
#endif /* GtkHRuler_Private */

#define NULLGTKHRULER (NspGtkHRuler*) 0

NspGtkHRuler *gtkhruler_create(char *name,NspTypeBase *type);

/* from GtkHRulerObj.c */

extern NspGtkHRuler *gtkhruler_object (NspObject *O); 
extern int IsGtkHRulerObj (Stack stack, int i); 
extern int IsGtkHRuler(NspObject *O);
extern NspGtkHRuler *GetGtkHRulerCopy (Stack stack, int i); 
extern NspGtkHRuler *GetGtkHRuler (Stack stack, int i); 

#endif 
