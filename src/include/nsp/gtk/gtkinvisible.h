/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkInvisible
#define INC_NSP_GtkInvisible

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkwidget.h"

/*
* NspGtkInvisible inherits from NspGtkWidget
* just change some type attributes 
*/

typedef NspGtkWidget NspGtkInvisible ;
typedef NspTypeGtkWidget NspTypeGtkInvisible ;

extern int nsp_type_gtkinvisible_id;
extern NspTypeGtkInvisible *nsp_type_gtkinvisible;

/* type instances for gtkwidget */

NspTypeGtkInvisible *new_type_gtkinvisible(type_mode mode);

/* instance for GtkInvisible */

NspGtkInvisible *new_gtkinvisible();

/*
* Object methods redefined for gtkinvisible 
*/

#ifdef GtkInvisible_Private 
static int init_gtkinvisible(NspGtkInvisible *o,NspTypeGtkInvisible *type);
static char *gtkinvisible_type_as_string(void);
static char *gtkinvisible_type_short_string(void);
static AttrTab gtkinvisible_attrs[];
/* static int int_gtkinvisible_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkinvisible_get_methods(void); 
#endif /* GtkInvisible_Private */

#define NULLGTKINVISIBLE (NspGtkInvisible*) 0

NspGtkInvisible *gtkinvisible_create(char *name,NspTypeBase *type);

/* from GtkInvisibleObj.c */

extern NspGtkInvisible *gtkinvisible_object (NspObject *O); 
extern int IsGtkInvisibleObj (Stack stack, int i); 
extern int IsGtkInvisible(NspObject *O);
extern NspGtkInvisible *GetGtkInvisibleCopy (Stack stack, int i); 
extern NspGtkInvisible *GetGtkInvisible (Stack stack, int i); 

#endif 
