/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkToggleAction
#define INC_NSP_GtkToggleAction

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkaction.h"

/*
* NspGtkToggleAction inherits from NspGtkAction
* just change some type attributes 
*/

typedef NspGtkAction NspGtkToggleAction ;
typedef NspTypeGtkAction NspTypeGtkToggleAction ;

extern int nsp_type_gtktoggleaction_id;
extern NspTypeGtkToggleAction *nsp_type_gtktoggleaction;

/* type instances for gtkaction */

NspTypeGtkToggleAction *new_type_gtktoggleaction(type_mode mode);

/* instance for GtkToggleAction */

NspGtkToggleAction *new_gtktoggleaction();

/*
* Object methods redefined for gtktoggleaction 
*/

#define NULLGTKTOGGLEACTION (NspGtkToggleAction*) 0

NspGtkToggleAction *gtktoggleaction_create(char *name,NspTypeBase *type);

/* from GtkToggleActionObj.c */

extern NspGtkToggleAction *gtktoggleaction_object (NspObject *O); 
extern int IsGtkToggleActionObj (Stack stack, int i); 
extern int IsGtkToggleAction(NspObject *O);
extern NspGtkToggleAction *GetGtkToggleActionCopy (Stack stack, int i); 
extern NspGtkToggleAction *GetGtkToggleAction (Stack stack, int i); 

#endif 

#ifdef GtkToggleAction_Private 
static int init_gtktoggleaction(NspGtkToggleAction *o,NspTypeGtkToggleAction *type);
static char *gtktoggleaction_type_as_string(void);
static char *gtktoggleaction_type_short_string(NspObject *v);
static AttrTab gtktoggleaction_attrs[];
/* static int int_gtktoggleaction_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktoggleaction_get_methods(void); 
#endif /* GtkToggleAction_Private */
