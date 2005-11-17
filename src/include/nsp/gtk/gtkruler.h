/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkRuler
#define INC_NSP_GtkRuler

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkwidget.h"

/*
* NspGtkRuler inherits from NspGtkWidget
* just change some type attributes 
*/

typedef NspGtkWidget NspGtkRuler ;
typedef NspTypeGtkWidget NspTypeGtkRuler ;

extern int nsp_type_gtkruler_id;
extern NspTypeGtkRuler *nsp_type_gtkruler;

/* type instances for gtkwidget */

NspTypeGtkRuler *new_type_gtkruler(type_mode mode);

/* instance for GtkRuler */

NspGtkRuler *new_gtkruler();

/*
* Object methods redefined for gtkruler 
*/

#define NULLGTKRULER (NspGtkRuler*) 0

NspGtkRuler *gtkruler_create(char *name,NspTypeBase *type);

/* from GtkRulerObj.c */

extern NspGtkRuler *gtkruler_object (NspObject *O); 
extern int IsGtkRulerObj (Stack stack, int i); 
extern int IsGtkRuler(NspObject *O);
extern NspGtkRuler *GetGtkRulerCopy (Stack stack, int i); 
extern NspGtkRuler *GetGtkRuler (Stack stack, int i); 

#endif 

#ifdef GtkRuler_Private 
static int init_gtkruler(NspGtkRuler *o,NspTypeGtkRuler *type);
static char *gtkruler_type_as_string(void);
static char *gtkruler_type_short_string(void);
static AttrTab gtkruler_attrs[];
/* static int int_gtkruler_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkruler_get_methods(void); 
#endif /* GtkRuler_Private */
