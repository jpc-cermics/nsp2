/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCellView
#define INC_NSP_GtkCellView

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkwidget.h"

/*
* NspGtkCellView inherits from NspGtkWidget
* just change some type attributes 
*/

typedef NspGtkWidget NspGtkCellView ;
typedef NspTypeGtkWidget NspTypeGtkCellView ;

extern int nsp_type_gtkcellview_id;
extern NspTypeGtkCellView *nsp_type_gtkcellview;

/* type instances for gtkwidget */

NspTypeGtkCellView *new_type_gtkcellview(type_mode mode);

/* instance for GtkCellView */

NspGtkCellView *new_gtkcellview();

/*
* Object methods redefined for gtkcellview 
*/

#define NULLGTKCELLVIEW (NspGtkCellView*) 0

NspGtkCellView *gtkcellview_create(char *name,NspTypeBase *type);

/* from GtkCellViewObj.c */

extern NspGtkCellView *gtkcellview_object (NspObject *O); 
extern int IsGtkCellViewObj (Stack stack, int i); 
extern int IsGtkCellView(NspObject *O);
extern NspGtkCellView *GetGtkCellViewCopy (Stack stack, int i); 
extern NspGtkCellView *GetGtkCellView (Stack stack, int i); 

#endif 

#ifdef GtkCellView_Private 
static int init_gtkcellview(NspGtkCellView *o,NspTypeGtkCellView *type);
static char *gtkcellview_type_as_string(void);
static char *gtkcellview_type_short_string(void);
static AttrTab gtkcellview_attrs[];
/* static int int_gtkcellview_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcellview_get_methods(void); 
#endif /* GtkCellView_Private */
