/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkGammaCurve
#define INC_NSP_GtkGammaCurve

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkvbox.h"

/*
* NspGtkGammaCurve inherits from NspGtkVBox
* just change some type attributes 
*/

typedef NspGtkVBox NspGtkGammaCurve ;
typedef NspTypeGtkVBox NspTypeGtkGammaCurve ;

extern int nsp_type_gtkgammacurve_id;
extern NspTypeGtkGammaCurve *nsp_type_gtkgammacurve;

/* type instances for gtkvbox */

NspTypeGtkGammaCurve *new_type_gtkgammacurve(type_mode mode);

/* instance for GtkGammaCurve */

NspGtkGammaCurve *new_gtkgammacurve();

/*
* Object methods redefined for gtkgammacurve 
*/

#ifdef GtkGammaCurve_Private 
static int init_gtkgammacurve(NspGtkGammaCurve *o,NspTypeGtkGammaCurve *type);
static char *gtkgammacurve_type_as_string(void);
static char *gtkgammacurve_type_short_string(void);
static AttrTab gtkgammacurve_attrs[];
/* static int int_gtkgammacurve_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkgammacurve_get_methods(void); 
#endif /* GtkGammaCurve_Private */

#define NULLGTKGAMMACURVE (NspGtkGammaCurve*) 0

NspGtkGammaCurve *gtkgammacurve_create(char *name,NspTypeBase *type);

/* from GtkGammaCurveObj.c */

extern NspGtkGammaCurve *gtkgammacurve_object (NspObject *O); 
extern int IsGtkGammaCurveObj (Stack stack, int i); 
extern int IsGtkGammaCurve(NspObject *O);
extern NspGtkGammaCurve *GetGtkGammaCurveCopy (Stack stack, int i); 
extern NspGtkGammaCurve *GetGtkGammaCurve (Stack stack, int i); 

#endif 
