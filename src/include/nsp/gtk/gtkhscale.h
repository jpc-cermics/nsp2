/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkHScale
#define INC_NSP_GtkHScale

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkscale.h"

/*
* NspGtkHScale inherits from NspGtkScale
* just change some type attributes 
*/

typedef NspGtkScale NspGtkHScale ;
typedef NspTypeGtkScale NspTypeGtkHScale ;

extern int nsp_type_gtkhscale_id;
extern NspTypeGtkHScale *nsp_type_gtkhscale;

/* type instances for gtkscale */

NspTypeGtkHScale *new_type_gtkhscale(type_mode mode);

/* instance for GtkHScale */

NspGtkHScale *new_gtkhscale();

/*
* Object methods redefined for gtkhscale 
*/

#ifdef GtkHScale_Private 
static int init_gtkhscale(NspGtkHScale *o,NspTypeGtkHScale *type);
static char *gtkhscale_type_as_string(void);
static char *gtkhscale_type_short_string(void);
static AttrTab gtkhscale_attrs[];
/* static int int_gtkhscale_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkhscale_get_methods(void); 
#endif /* GtkHScale_Private */

#define NULLGTKHSCALE (NspGtkHScale*) 0

NspGtkHScale *gtkhscale_create(char *name,NspTypeBase *type);

/* from GtkHScaleObj.c */

extern NspGtkHScale *gtkhscale_object (NspObject *O); 
extern int IsGtkHScaleObj (Stack stack, int i); 
extern int IsGtkHScale(NspObject *O);
extern NspGtkHScale *GetGtkHScaleCopy (Stack stack, int i); 
extern NspGtkHScale *GetGtkHScale (Stack stack, int i); 

#endif 
