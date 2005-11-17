/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkBin
#define INC_NSP_GtkBin

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspGtkBin inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspGtkBin ;
typedef NspTypeGtkContainer NspTypeGtkBin ;

extern int nsp_type_gtkbin_id;
extern NspTypeGtkBin *nsp_type_gtkbin;

/* type instances for gtkcontainer */

NspTypeGtkBin *new_type_gtkbin(type_mode mode);

/* instance for GtkBin */

NspGtkBin *new_gtkbin();

/*
* Object methods redefined for gtkbin 
*/

#define NULLGTKBIN (NspGtkBin*) 0

NspGtkBin *gtkbin_create(char *name,NspTypeBase *type);

/* from GtkBinObj.c */

extern NspGtkBin *gtkbin_object (NspObject *O); 
extern int IsGtkBinObj (Stack stack, int i); 
extern int IsGtkBin(NspObject *O);
extern NspGtkBin *GetGtkBinCopy (Stack stack, int i); 
extern NspGtkBin *GetGtkBin (Stack stack, int i); 

#endif 

#ifdef GtkBin_Private 
static int init_gtkbin(NspGtkBin *o,NspTypeGtkBin *type);
static char *gtkbin_type_as_string(void);
static char *gtkbin_type_short_string(void);
static AttrTab gtkbin_attrs[];
/* static int int_gtkbin_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkbin_get_methods(void); 
#endif /* GtkBin_Private */
