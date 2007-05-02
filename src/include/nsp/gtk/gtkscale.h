/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkScale
#define INC_NSP_GtkScale

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkrange.h"

/*
* NspGtkScale inherits from NspGtkRange
* just change some type attributes 
*/

typedef NspGtkRange NspGtkScale ;
typedef NspTypeGtkRange NspTypeGtkScale ;

extern int nsp_type_gtkscale_id;
extern NspTypeGtkScale *nsp_type_gtkscale;

/* type instances for gtkrange */

NspTypeGtkScale *new_type_gtkscale(type_mode mode);

/* instance for GtkScale */

NspGtkScale *new_gtkscale();

/*
* Object methods redefined for gtkscale 
*/

#define NULLGTKSCALE (NspGtkScale*) 0

NspGtkScale *gtkscale_create(char *name,NspTypeBase *type);

/* from GtkScaleObj.c */

extern NspGtkScale *gtkscale_object (NspObject *O); 
extern int IsGtkScaleObj (Stack stack, int i); 
extern int IsGtkScale(NspObject *O);
extern NspGtkScale *GetGtkScaleCopy (Stack stack, int i); 
extern NspGtkScale *GetGtkScale (Stack stack, int i); 

#endif 

#ifdef GtkScale_Private 
static int init_gtkscale(NspGtkScale *o,NspTypeGtkScale *type);
static char *gtkscale_type_as_string(void);
static char *gtkscale_type_short_string(NspObject *v);
static AttrTab gtkscale_attrs[];
/* static int int_gtkscale_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkscale_get_methods(void); 
#endif /* GtkScale_Private */
