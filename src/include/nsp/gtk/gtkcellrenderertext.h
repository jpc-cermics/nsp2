/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCellRendererText
#define INC_NSP_GtkCellRendererText

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcellrenderer.h"

/*
* NspGtkCellRendererText inherits from NspGtkCellRenderer
* just change some type attributes 
*/

typedef NspGtkCellRenderer NspGtkCellRendererText ;
typedef NspTypeGtkCellRenderer NspTypeGtkCellRendererText ;

extern int nsp_type_gtkcellrenderertext_id;
extern NspTypeGtkCellRendererText *nsp_type_gtkcellrenderertext;

/* type instances for gtkcellrenderer */

NspTypeGtkCellRendererText *new_type_gtkcellrenderertext(type_mode mode);

/* instance for GtkCellRendererText */

NspGtkCellRendererText *new_gtkcellrenderertext();

/*
* Object methods redefined for gtkcellrenderertext 
*/

#ifdef GtkCellRendererText_Private 
static int init_gtkcellrenderertext(NspGtkCellRendererText *o,NspTypeGtkCellRendererText *type);
static char *gtkcellrenderertext_type_as_string(void);
static char *gtkcellrenderertext_type_short_string(void);
static AttrTab gtkcellrenderertext_attrs[];
/* static int int_gtkcellrenderertext_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcellrenderertext_get_methods(void); 
#endif /* GtkCellRendererText_Private */

#define NULLGTKCELLRENDERERTEXT (NspGtkCellRendererText*) 0

NspGtkCellRendererText *gtkcellrenderertext_create(char *name,NspTypeBase *type);

/* from GtkCellRendererTextObj.c */

extern NspGtkCellRendererText *gtkcellrenderertext_object (NspObject *O); 
extern int IsGtkCellRendererTextObj (Stack stack, int i); 
extern int IsGtkCellRendererText(NspObject *O);
extern NspGtkCellRendererText *GetGtkCellRendererTextCopy (Stack stack, int i); 
extern NspGtkCellRendererText *GetGtkCellRendererText (Stack stack, int i); 

#endif 
