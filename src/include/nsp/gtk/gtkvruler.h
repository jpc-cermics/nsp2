/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkVRuler
#define INC_NSP_GtkVRuler

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkruler.h"

/*
* NspGtkVRuler inherits from NspGtkRuler
* just change some type attributes 
*/

typedef NspGtkRuler NspGtkVRuler ;
typedef NspTypeGtkRuler NspTypeGtkVRuler ;

extern int nsp_type_gtkvruler_id;
extern NspTypeGtkVRuler *nsp_type_gtkvruler;

/* type instances for gtkruler */

NspTypeGtkVRuler *new_type_gtkvruler(type_mode mode);

/* instance for GtkVRuler */

NspGtkVRuler *new_gtkvruler();

/*
* Object methods redefined for gtkvruler 
*/

#ifdef GtkVRuler_Private 
static int init_gtkvruler(NspGtkVRuler *o,NspTypeGtkVRuler *type);
static char *gtkvruler_type_as_string(void);
static char *gtkvruler_type_short_string(void);
static AttrTab gtkvruler_attrs[];
/* static int int_gtkvruler_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkvruler_get_methods(void); 
#endif /* GtkVRuler_Private */

#define NULLGTKVRULER (NspGtkVRuler*) 0

NspGtkVRuler *gtkvruler_create(char *name,NspTypeBase *type);

/* from GtkVRulerObj.c */

extern NspGtkVRuler *gtkvruler_object (NspObject *O); 
extern int IsGtkVRulerObj (Stack stack, int i); 
extern int IsGtkVRuler(NspObject *O);
extern NspGtkVRuler *GetGtkVRulerCopy (Stack stack, int i); 
extern NspGtkVRuler *GetGtkVRuler (Stack stack, int i); 

#endif 
