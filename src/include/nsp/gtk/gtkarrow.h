/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkArrow
#define INC_NSP_GtkArrow

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkmisc.h"

/*
* NspGtkArrow inherits from NspGtkMisc
* just change some type attributes 
*/

typedef NspGtkMisc NspGtkArrow ;
typedef NspTypeGtkMisc NspTypeGtkArrow ;

extern int nsp_type_gtkarrow_id;
extern NspTypeGtkArrow *nsp_type_gtkarrow;

/* type instances for gtkmisc */

NspTypeGtkArrow *new_type_gtkarrow(type_mode mode);

/* instance for GtkArrow */

NspGtkArrow *new_gtkarrow();

/*
* Object methods redefined for gtkarrow 
*/

#ifdef GtkArrow_Private 
static int init_gtkarrow(NspGtkArrow *o,NspTypeGtkArrow *type);
static char *gtkarrow_type_as_string(void);
static char *gtkarrow_type_short_string(void);
static AttrTab gtkarrow_attrs[];
/* static int int_gtkarrow_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkarrow_get_methods(void); 
#endif /* GtkArrow_Private */

#define NULLGTKARROW (NspGtkArrow*) 0

NspGtkArrow *gtkarrow_create(char *name,NspTypeBase *type);

/* from GtkArrowObj.c */

extern NspGtkArrow *gtkarrow_object (NspObject *O); 
extern int IsGtkArrowObj (Stack stack, int i); 
extern int IsGtkArrow(NspObject *O);
extern NspGtkArrow *GetGtkArrowCopy (Stack stack, int i); 
extern NspGtkArrow *GetGtkArrow (Stack stack, int i); 

#endif 
