/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkAlignment
#define INC_NSP_GtkAlignment

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbin.h"

/*
* NspGtkAlignment inherits from NspGtkBin
* just change some type attributes 
*/

typedef NspGtkBin NspGtkAlignment ;
typedef NspTypeGtkBin NspTypeGtkAlignment ;

extern int nsp_type_gtkalignment_id;
extern NspTypeGtkAlignment *nsp_type_gtkalignment;

/* type instances for gtkbin */

NspTypeGtkAlignment *new_type_gtkalignment(type_mode mode);

/* instance for GtkAlignment */

NspGtkAlignment *new_gtkalignment();

/*
* Object methods redefined for gtkalignment 
*/

#ifdef GtkAlignment_Private 
static int init_gtkalignment(NspGtkAlignment *o,NspTypeGtkAlignment *type);
static char *gtkalignment_type_as_string(void);
static char *gtkalignment_type_short_string(void);
static AttrTab gtkalignment_attrs[];
/* static int int_gtkalignment_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkalignment_get_methods(void); 
#endif /* GtkAlignment_Private */

#define NULLGTKALIGNMENT (NspGtkAlignment*) 0

NspGtkAlignment *gtkalignment_create(char *name,NspTypeBase *type);

/* from GtkAlignmentObj.c */

extern NspGtkAlignment *gtkalignment_object (NspObject *O); 
extern int IsGtkAlignmentObj (Stack stack, int i); 
extern int IsGtkAlignment(NspObject *O);
extern NspGtkAlignment *GetGtkAlignmentCopy (Stack stack, int i); 
extern NspGtkAlignment *GetGtkAlignment (Stack stack, int i); 

#endif 
