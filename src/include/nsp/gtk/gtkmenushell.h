/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkMenuShell
#define INC_NSP_GtkMenuShell

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspGtkMenuShell inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspGtkMenuShell ;
typedef NspTypeGtkContainer NspTypeGtkMenuShell ;

extern int nsp_type_gtkmenushell_id;
extern NspTypeGtkMenuShell *nsp_type_gtkmenushell;

/* type instances for gtkcontainer */

NspTypeGtkMenuShell *new_type_gtkmenushell(type_mode mode);

/* instance for GtkMenuShell */

NspGtkMenuShell *new_gtkmenushell();

/*
* Object methods redefined for gtkmenushell 
*/

#ifdef GtkMenuShell_Private 
static int init_gtkmenushell(NspGtkMenuShell *o,NspTypeGtkMenuShell *type);
static char *gtkmenushell_type_as_string(void);
static char *gtkmenushell_type_short_string(void);
static AttrTab gtkmenushell_attrs[];
/* static int int_gtkmenushell_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkmenushell_get_methods(void); 
#endif /* GtkMenuShell_Private */

#define NULLGTKMENUSHELL (NspGtkMenuShell*) 0

NspGtkMenuShell *gtkmenushell_create(char *name,NspTypeBase *type);

/* from GtkMenuShellObj.c */

extern NspGtkMenuShell *gtkmenushell_object (NspObject *O); 
extern int IsGtkMenuShellObj (Stack stack, int i); 
extern int IsGtkMenuShell(NspObject *O);
extern NspGtkMenuShell *GetGtkMenuShellCopy (Stack stack, int i); 
extern NspGtkMenuShell *GetGtkMenuShell (Stack stack, int i); 

#endif 
