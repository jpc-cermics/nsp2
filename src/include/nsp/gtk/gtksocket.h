/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkSocket
#define INC_NSP_GtkSocket

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspGtkSocket inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspGtkSocket ;
typedef NspTypeGtkContainer NspTypeGtkSocket ;

extern int nsp_type_gtksocket_id;
extern NspTypeGtkSocket *nsp_type_gtksocket;

/* type instances for gtkcontainer */

NspTypeGtkSocket *new_type_gtksocket(type_mode mode);

/* instance for GtkSocket */

NspGtkSocket *new_gtksocket();

/*
* Object methods redefined for gtksocket 
*/

#define NULLGTKSOCKET (NspGtkSocket*) 0

NspGtkSocket *gtksocket_create(char *name,NspTypeBase *type);

/* from GtkSocketObj.c */

extern NspGtkSocket *gtksocket_object (NspObject *O); 
extern int IsGtkSocketObj (Stack stack, int i); 
extern int IsGtkSocket(NspObject *O);
extern NspGtkSocket *GetGtkSocketCopy (Stack stack, int i); 
extern NspGtkSocket *GetGtkSocket (Stack stack, int i); 

#endif 

#ifdef GtkSocket_Private 
static int init_gtksocket(NspGtkSocket *o,NspTypeGtkSocket *type);
static char *gtksocket_type_as_string(void);
static char *gtksocket_type_short_string(NspObject *v);
static AttrTab gtksocket_attrs[];
/* static int int_gtksocket_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtksocket_get_methods(void); 
#endif /* GtkSocket_Private */
