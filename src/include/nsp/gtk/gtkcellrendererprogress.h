/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCellRendererProgress
#define INC_NSP_GtkCellRendererProgress

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcellrenderer.h"

/*
* NspGtkCellRendererProgress inherits from NspGtkCellRenderer
* just change some type attributes 
*/

typedef NspGtkCellRenderer NspGtkCellRendererProgress ;
typedef NspTypeGtkCellRenderer NspTypeGtkCellRendererProgress ;

extern int nsp_type_gtkcellrendererprogress_id;
extern NspTypeGtkCellRendererProgress *nsp_type_gtkcellrendererprogress;

/* type instances for gtkcellrenderer */

NspTypeGtkCellRendererProgress *new_type_gtkcellrendererprogress(type_mode mode);

/* instance for GtkCellRendererProgress */

NspGtkCellRendererProgress *new_gtkcellrendererprogress();

/*
* Object methods redefined for gtkcellrendererprogress 
*/

#define NULLGTKCELLRENDERERPROGRESS (NspGtkCellRendererProgress*) 0

NspGtkCellRendererProgress *gtkcellrendererprogress_create(char *name,NspTypeBase *type);

/* from GtkCellRendererProgressObj.c */

extern NspGtkCellRendererProgress *gtkcellrendererprogress_object (NspObject *O); 
extern int IsGtkCellRendererProgressObj (Stack stack, int i); 
extern int IsGtkCellRendererProgress(NspObject *O);
extern NspGtkCellRendererProgress *GetGtkCellRendererProgressCopy (Stack stack, int i); 
extern NspGtkCellRendererProgress *GetGtkCellRendererProgress (Stack stack, int i); 

#endif 

#ifdef GtkCellRendererProgress_Private 
static int init_gtkcellrendererprogress(NspGtkCellRendererProgress *o,NspTypeGtkCellRendererProgress *type);
static char *gtkcellrendererprogress_type_as_string(void);
static char *gtkcellrendererprogress_type_short_string(NspObject *v);
static AttrTab gtkcellrendererprogress_attrs[];
/* static int int_gtkcellrendererprogress_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcellrendererprogress_get_methods(void); 
#endif /* GtkCellRendererProgress_Private */
