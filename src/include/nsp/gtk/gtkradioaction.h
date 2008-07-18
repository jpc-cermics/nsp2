/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkRadioAction
#define INC_NSP_GtkRadioAction

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtktoggleaction.h"

/*
* NspGtkRadioAction inherits from NspGtkToggleAction
* just change some type attributes 
*/

typedef NspGtkToggleAction NspGtkRadioAction ;
typedef NspTypeGtkToggleAction NspTypeGtkRadioAction ;

extern int nsp_type_gtkradioaction_id;
extern NspTypeGtkRadioAction *nsp_type_gtkradioaction;

/* type instances for gtktoggleaction */

NspTypeGtkRadioAction *new_type_gtkradioaction(type_mode mode);

/* instance for GtkRadioAction */

NspGtkRadioAction *new_gtkradioaction();

/*
* Object methods redefined for gtkradioaction 
*/

#define NULLGTKRADIOACTION (NspGtkRadioAction*) 0

NspGtkRadioAction *gtkradioaction_create(char *name,NspTypeBase *type);

/* from GtkRadioActionObj.c */

extern NspGtkRadioAction *gtkradioaction_object (NspObject *O); 
extern int IsGtkRadioActionObj (Stack stack, int i); 
extern int IsGtkRadioAction(NspObject *O);
extern NspGtkRadioAction *GetGtkRadioActionCopy (Stack stack, int i); 
extern NspGtkRadioAction *GetGtkRadioAction (Stack stack, int i); 

#endif 

#ifdef GtkRadioAction_Private 
static int init_gtkradioaction(NspGtkRadioAction *o,NspTypeGtkRadioAction *type);
static char *gtkradioaction_type_as_string(void);
static char *gtkradioaction_type_short_string(NspObject *v);
static AttrTab gtkradioaction_attrs[];
/* static int int_gtkradioaction_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkradioaction_get_methods(void); 
#endif /* GtkRadioAction_Private */
