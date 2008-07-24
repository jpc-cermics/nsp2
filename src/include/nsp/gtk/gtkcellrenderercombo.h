/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCellRendererCombo
#define INC_NSP_GtkCellRendererCombo

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcellrenderertext.h"

/*
* NspGtkCellRendererCombo inherits from NspGtkCellRendererText
* just change some type attributes 
*/

typedef NspGtkCellRendererText NspGtkCellRendererCombo ;
typedef NspTypeGtkCellRendererText NspTypeGtkCellRendererCombo ;

extern int nsp_type_gtkcellrenderercombo_id;
extern NspTypeGtkCellRendererCombo *nsp_type_gtkcellrenderercombo;

/* type instances for gtkcellrenderertext */

NspTypeGtkCellRendererCombo *new_type_gtkcellrenderercombo(type_mode mode);

/* instance for GtkCellRendererCombo */

NspGtkCellRendererCombo *new_gtkcellrenderercombo();

/*
* Object methods redefined for gtkcellrenderercombo 
*/

#define NULLGTKCELLRENDERERCOMBO (NspGtkCellRendererCombo*) 0

NspGtkCellRendererCombo *gtkcellrenderercombo_create(char *name,NspTypeBase *type);

/* from GtkCellRendererComboObj.c */

extern NspGtkCellRendererCombo *gtkcellrenderercombo_object (NspObject *O); 
extern int IsGtkCellRendererComboObj (Stack stack, int i); 
extern int IsGtkCellRendererCombo(NspObject *O);
extern NspGtkCellRendererCombo *GetGtkCellRendererComboCopy (Stack stack, int i); 
extern NspGtkCellRendererCombo *GetGtkCellRendererCombo (Stack stack, int i); 

#endif 

#ifdef GtkCellRendererCombo_Private 
static int init_gtkcellrenderercombo(NspGtkCellRendererCombo *o,NspTypeGtkCellRendererCombo *type);
static char *gtkcellrenderercombo_type_as_string(void);
static char *gtkcellrenderercombo_type_short_string(NspObject *v);
static AttrTab gtkcellrenderercombo_attrs[];
/* static int int_gtkcellrenderercombo_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcellrenderercombo_get_methods(void); 
#endif /* GtkCellRendererCombo_Private */
