/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkRange
#define INC_NSP_GtkRange

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkwidget.h"

/*
* NspGtkRange inherits from NspGtkWidget
* just change some type attributes 
*/

typedef NspGtkWidget NspGtkRange ;
typedef NspTypeGtkWidget NspTypeGtkRange ;

extern int nsp_type_gtkrange_id;
extern NspTypeGtkRange *nsp_type_gtkrange;

/* type instances for gtkwidget */

NspTypeGtkRange *new_type_gtkrange(type_mode mode);

/* instance for GtkRange */

NspGtkRange *new_gtkrange();

/*
* Object methods redefined for gtkrange 
*/

#ifdef GtkRange_Private 
static int init_gtkrange(NspGtkRange *o,NspTypeGtkRange *type);
static char *gtkrange_type_as_string(void);
static char *gtkrange_type_short_string(void);
static AttrTab gtkrange_attrs[];
/* static int int_gtkrange_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkrange_get_methods(void); 
#endif /* GtkRange_Private */

#define NULLGTKRANGE (NspGtkRange*) 0

NspGtkRange *gtkrange_create(char *name,NspTypeBase *type);

/* from GtkRangeObj.c */

extern NspGtkRange *gtkrange_object (NspObject *O); 
extern int IsGtkRangeObj (Stack stack, int i); 
extern int IsGtkRange(NspObject *O);
extern NspGtkRange *GetGtkRangeCopy (Stack stack, int i); 
extern NspGtkRange *GetGtkRange (Stack stack, int i); 

#endif 
