/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTreeIter
#define INC_NSP_GtkTreeIter

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGtkTreeIter inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGtkTreeIter ;
typedef NspTypeGBoxed NspTypeGtkTreeIter ;

extern int nsp_type_gtktreeiter_id;
extern NspTypeGtkTreeIter *nsp_type_gtktreeiter;

/* type instances for gboxed */

NspTypeGtkTreeIter *new_type_gtktreeiter(type_mode mode);

/* instance for GtkTreeIter */

NspGtkTreeIter *new_gtktreeiter();

/*
* Object methods redefined for gtktreeiter 
*/

#define NULLGTKTREEITER (NspGtkTreeIter*) 0

NspGtkTreeIter *gtktreeiter_create(char *name,NspTypeBase *type);

/* from GtkTreeIterObj.c */

extern NspGtkTreeIter *gtktreeiter_object (NspObject *O); 
extern int IsGtkTreeIterObj (Stack stack, int i); 
extern int IsGtkTreeIter(NspObject *O);
extern NspGtkTreeIter *GetGtkTreeIterCopy (Stack stack, int i); 
extern NspGtkTreeIter *GetGtkTreeIter (Stack stack, int i); 

#endif 

#ifdef GtkTreeIter_Private 
static int init_gtktreeiter(NspGtkTreeIter *o,NspTypeGtkTreeIter *type);
static char *gtktreeiter_type_as_string(void);
static char *gtktreeiter_type_short_string(NspObject *v);
static AttrTab gtktreeiter_attrs[];
/* static int int_gtktreeiter_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktreeiter_get_methods(void); 
#endif /* GtkTreeIter_Private */
