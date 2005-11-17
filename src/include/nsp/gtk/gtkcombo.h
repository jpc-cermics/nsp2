/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCombo
#define INC_NSP_GtkCombo

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkhbox.h"

/*
* NspGtkCombo inherits from NspGtkHBox
* just change some type attributes 
*/

typedef NspGtkHBox NspGtkCombo ;
typedef NspTypeGtkHBox NspTypeGtkCombo ;

extern int nsp_type_gtkcombo_id;
extern NspTypeGtkCombo *nsp_type_gtkcombo;

/* type instances for gtkhbox */

NspTypeGtkCombo *new_type_gtkcombo(type_mode mode);

/* instance for GtkCombo */

NspGtkCombo *new_gtkcombo();

/*
* Object methods redefined for gtkcombo 
*/

#define NULLGTKCOMBO (NspGtkCombo*) 0

NspGtkCombo *gtkcombo_create(char *name,NspTypeBase *type);

/* from GtkComboObj.c */

extern NspGtkCombo *gtkcombo_object (NspObject *O); 
extern int IsGtkComboObj (Stack stack, int i); 
extern int IsGtkCombo(NspObject *O);
extern NspGtkCombo *GetGtkComboCopy (Stack stack, int i); 
extern NspGtkCombo *GetGtkCombo (Stack stack, int i); 

#endif 

#ifdef GtkCombo_Private 
static int init_gtkcombo(NspGtkCombo *o,NspTypeGtkCombo *type);
static char *gtkcombo_type_as_string(void);
static char *gtkcombo_type_short_string(void);
static AttrTab gtkcombo_attrs[];
/* static int int_gtkcombo_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcombo_get_methods(void); 
#endif /* GtkCombo_Private */
