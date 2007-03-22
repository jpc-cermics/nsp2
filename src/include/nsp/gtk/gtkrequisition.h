/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkRequisition
#define INC_NSP_GtkRequisition

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGtkRequisition inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGtkRequisition ;
typedef NspTypeGBoxed NspTypeGtkRequisition ;

extern int nsp_type_gtkrequisition_id;
extern NspTypeGtkRequisition *nsp_type_gtkrequisition;

/* type instances for gboxed */

NspTypeGtkRequisition *new_type_gtkrequisition(type_mode mode);

/* instance for GtkRequisition */

NspGtkRequisition *new_gtkrequisition();

/*
* Object methods redefined for gtkrequisition 
*/

#define NULLGTKREQUISITION (NspGtkRequisition*) 0

NspGtkRequisition *gtkrequisition_create(char *name,NspTypeBase *type);

/* from GtkRequisitionObj.c */

extern NspGtkRequisition *gtkrequisition_object (NspObject *O); 
extern int IsGtkRequisitionObj (Stack stack, int i); 
extern int IsGtkRequisition(NspObject *O);
extern NspGtkRequisition *GetGtkRequisitionCopy (Stack stack, int i); 
extern NspGtkRequisition *GetGtkRequisition (Stack stack, int i); 

#endif 

#ifdef GtkRequisition_Private 
static int init_gtkrequisition(NspGtkRequisition *o,NspTypeGtkRequisition *type);
static char *gtkrequisition_type_as_string(void);
static char *gtkrequisition_type_short_string(NspObject *v);
static AttrTab gtkrequisition_attrs[];
/* static int int_gtkrequisition_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkrequisition_get_methods(void); 
#endif /* GtkRequisition_Private */
