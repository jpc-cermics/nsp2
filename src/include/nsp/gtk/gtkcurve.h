/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCurve
#define INC_NSP_GtkCurve

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkdrawingarea.h"

/*
* NspGtkCurve inherits from NspGtkDrawingArea
* just change some type attributes 
*/

typedef NspGtkDrawingArea NspGtkCurve ;
typedef NspTypeGtkDrawingArea NspTypeGtkCurve ;

extern int nsp_type_gtkcurve_id;
extern NspTypeGtkCurve *nsp_type_gtkcurve;

/* type instances for gtkdrawingarea */

NspTypeGtkCurve *new_type_gtkcurve(type_mode mode);

/* instance for GtkCurve */

NspGtkCurve *new_gtkcurve();

/*
* Object methods redefined for gtkcurve 
*/

#define NULLGTKCURVE (NspGtkCurve*) 0

NspGtkCurve *gtkcurve_create(char *name,NspTypeBase *type);

/* from GtkCurveObj.c */

extern NspGtkCurve *gtkcurve_object (NspObject *O); 
extern int IsGtkCurveObj (Stack stack, int i); 
extern int IsGtkCurve(NspObject *O);
extern NspGtkCurve *GetGtkCurveCopy (Stack stack, int i); 
extern NspGtkCurve *GetGtkCurve (Stack stack, int i); 

#endif 

#ifdef GtkCurve_Private 
static int init_gtkcurve(NspGtkCurve *o,NspTypeGtkCurve *type);
static char *gtkcurve_type_as_string(void);
static char *gtkcurve_type_short_string(NspObject *v);
static AttrTab gtkcurve_attrs[];
/* static int int_gtkcurve_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcurve_get_methods(void); 
#endif /* GtkCurve_Private */
