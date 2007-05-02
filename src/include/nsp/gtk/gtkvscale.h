/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkVScale
#define INC_NSP_GtkVScale

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkscale.h"

/*
* NspGtkVScale inherits from NspGtkScale
* just change some type attributes 
*/

typedef NspGtkScale NspGtkVScale ;
typedef NspTypeGtkScale NspTypeGtkVScale ;

extern int nsp_type_gtkvscale_id;
extern NspTypeGtkVScale *nsp_type_gtkvscale;

/* type instances for gtkscale */

NspTypeGtkVScale *new_type_gtkvscale(type_mode mode);

/* instance for GtkVScale */

NspGtkVScale *new_gtkvscale();

/*
* Object methods redefined for gtkvscale 
*/

#define NULLGTKVSCALE (NspGtkVScale*) 0

NspGtkVScale *gtkvscale_create(char *name,NspTypeBase *type);

/* from GtkVScaleObj.c */

extern NspGtkVScale *gtkvscale_object (NspObject *O); 
extern int IsGtkVScaleObj (Stack stack, int i); 
extern int IsGtkVScale(NspObject *O);
extern NspGtkVScale *GetGtkVScaleCopy (Stack stack, int i); 
extern NspGtkVScale *GetGtkVScale (Stack stack, int i); 

#endif 

#ifdef GtkVScale_Private 
static int init_gtkvscale(NspGtkVScale *o,NspTypeGtkVScale *type);
static char *gtkvscale_type_as_string(void);
static char *gtkvscale_type_short_string(NspObject *v);
static AttrTab gtkvscale_attrs[];
/* static int int_gtkvscale_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkvscale_get_methods(void); 
#endif /* GtkVScale_Private */
