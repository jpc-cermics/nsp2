/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkAdjustment
#define INC_NSP_GtkAdjustment

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkobject.h"

/*
* NspGtkAdjustment inherits from NspGtkObject
* just change some type attributes 
*/

typedef NspGtkObject NspGtkAdjustment ;
typedef NspTypeGtkObject NspTypeGtkAdjustment ;

extern int nsp_type_gtkadjustment_id;
extern NspTypeGtkAdjustment *nsp_type_gtkadjustment;

/* type instances for gtkobject */

NspTypeGtkAdjustment *new_type_gtkadjustment(type_mode mode);

/* instance for GtkAdjustment */

NspGtkAdjustment *new_gtkadjustment();

/*
* Object methods redefined for gtkadjustment 
*/

#ifdef GtkAdjustment_Private 
static int init_gtkadjustment(NspGtkAdjustment *o,NspTypeGtkAdjustment *type);
static char *gtkadjustment_type_as_string(void);
static char *gtkadjustment_type_short_string(void);
static AttrTab gtkadjustment_attrs[];
/* static int int_gtkadjustment_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkadjustment_get_methods(void); 
#endif /* GtkAdjustment_Private */

#define NULLGTKADJUSTMENT (NspGtkAdjustment*) 0

NspGtkAdjustment *gtkadjustment_create(char *name,NspTypeBase *type);

/* from GtkAdjustmentObj.c */

extern NspGtkAdjustment *gtkadjustment_object (NspObject *O); 
extern int IsGtkAdjustmentObj (Stack stack, int i); 
extern int IsGtkAdjustment(NspObject *O);
extern NspGtkAdjustment *GetGtkAdjustmentCopy (Stack stack, int i); 
extern NspGtkAdjustment *GetGtkAdjustment (Stack stack, int i); 

#endif 
