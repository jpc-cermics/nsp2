/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkNotebook
#define INC_NSP_GtkNotebook

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspGtkNotebook inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspGtkNotebook ;
typedef NspTypeGtkContainer NspTypeGtkNotebook ;

extern int nsp_type_gtknotebook_id;
extern NspTypeGtkNotebook *nsp_type_gtknotebook;

/* type instances for gtkcontainer */

NspTypeGtkNotebook *new_type_gtknotebook(type_mode mode);

/* instance for GtkNotebook */

NspGtkNotebook *new_gtknotebook();

/*
* Object methods redefined for gtknotebook 
*/

#ifdef GtkNotebook_Private 
static int init_gtknotebook(NspGtkNotebook *o,NspTypeGtkNotebook *type);
static char *gtknotebook_type_as_string(void);
static char *gtknotebook_type_short_string(void);
static AttrTab gtknotebook_attrs[];
/* static int int_gtknotebook_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtknotebook_get_methods(void); 
#endif /* GtkNotebook_Private */

#define NULLGTKNOTEBOOK (NspGtkNotebook*) 0

NspGtkNotebook *gtknotebook_create(char *name,NspTypeBase *type);

/* from GtkNotebookObj.c */

extern NspGtkNotebook *gtknotebook_object (NspObject *O); 
extern int IsGtkNotebookObj (Stack stack, int i); 
extern int IsGtkNotebook(NspObject *O);
extern NspGtkNotebook *GetGtkNotebookCopy (Stack stack, int i); 
extern NspGtkNotebook *GetGtkNotebook (Stack stack, int i); 

#endif 
