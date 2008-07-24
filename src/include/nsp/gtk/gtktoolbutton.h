/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkToolButton
#define INC_NSP_GtkToolButton

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtktoolitem.h"

/*
* NspGtkToolButton inherits from NspGtkToolItem
* just change some type attributes 
*/

typedef NspGtkToolItem NspGtkToolButton ;
typedef NspTypeGtkToolItem NspTypeGtkToolButton ;

extern int nsp_type_gtktoolbutton_id;
extern NspTypeGtkToolButton *nsp_type_gtktoolbutton;

/* type instances for gtktoolitem */

NspTypeGtkToolButton *new_type_gtktoolbutton(type_mode mode);

/* instance for GtkToolButton */

NspGtkToolButton *new_gtktoolbutton();

/*
* Object methods redefined for gtktoolbutton 
*/

#define NULLGTKTOOLBUTTON (NspGtkToolButton*) 0

NspGtkToolButton *gtktoolbutton_create(char *name,NspTypeBase *type);

/* from GtkToolButtonObj.c */

extern NspGtkToolButton *gtktoolbutton_object (NspObject *O); 
extern int IsGtkToolButtonObj (Stack stack, int i); 
extern int IsGtkToolButton(NspObject *O);
extern NspGtkToolButton *GetGtkToolButtonCopy (Stack stack, int i); 
extern NspGtkToolButton *GetGtkToolButton (Stack stack, int i); 

#endif 

#ifdef GtkToolButton_Private 
static int init_gtktoolbutton(NspGtkToolButton *o,NspTypeGtkToolButton *type);
static char *gtktoolbutton_type_as_string(void);
static char *gtktoolbutton_type_short_string(NspObject *v);
static AttrTab gtktoolbutton_attrs[];
/* static int int_gtktoolbutton_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktoolbutton_get_methods(void); 
#endif /* GtkToolButton_Private */
