/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTreeRowReference
#define INC_NSP_GtkTreeRowReference

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGtkTreeRowReference inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGtkTreeRowReference ;
typedef NspTypeGBoxed NspTypeGtkTreeRowReference ;

extern int nsp_type_gtktreerowreference_id;
extern NspTypeGtkTreeRowReference *nsp_type_gtktreerowreference;

/* type instances for gboxed */

NspTypeGtkTreeRowReference *new_type_gtktreerowreference(type_mode mode);

/* instance for GtkTreeRowReference */

NspGtkTreeRowReference *new_gtktreerowreference();

/*
* Object methods redefined for gtktreerowreference 
*/

#define NULLGTKTREEROWREFERENCE (NspGtkTreeRowReference*) 0

NspGtkTreeRowReference *gtktreerowreference_create(char *name,NspTypeBase *type);

/* from GtkTreeRowReferenceObj.c */

extern NspGtkTreeRowReference *gtktreerowreference_object (NspObject *O); 
extern int IsGtkTreeRowReferenceObj (Stack stack, int i); 
extern int IsGtkTreeRowReference(NspObject *O);
extern NspGtkTreeRowReference *GetGtkTreeRowReferenceCopy (Stack stack, int i); 
extern NspGtkTreeRowReference *GetGtkTreeRowReference (Stack stack, int i); 

#endif 

#ifdef GtkTreeRowReference_Private 
static int init_gtktreerowreference(NspGtkTreeRowReference *o,NspTypeGtkTreeRowReference *type);
static char *gtktreerowreference_type_as_string(void);
static char *gtktreerowreference_type_short_string(NspObject *v);
static AttrTab gtktreerowreference_attrs[];
/* static int int_gtktreerowreference_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktreerowreference_get_methods(void); 
#endif /* GtkTreeRowReference_Private */
