/* -*- Mode: C -*- */
#ifndef INC_NSP_ClassC
#define INC_NSP_ClassC

/*-----------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )
 * Jean-Philippe Chancelier Enpc/Cermics
 *-----------------------------------------------------------------*/

#include "classa.h"

/*
 * NspClassC inherits from NspClassA
 * just change some type attributes 
 */

typedef NspClassA NspClassC ;
typedef NspTypeClassA NspTypeClassC ;

extern int nsp_type_classc_id;
extern NspTypeClassC *nsp_type_classc;

/* type instances for classa */

NspTypeClassC *new_type_classc(type_mode mode);

/* instance for ClassC */

NspClassC *new_classc();

/*
 * Object methods redefined for classc 
 */


#define NULLCLC (NspClassC*) 0

NspClassC *classc_create(char *name,int color,int thickness);

/* from ClassCObj.c */

extern NspClassC *classc_object (NspObject *O); 
extern int IsClassCObj (Stack stack, int i); 
extern int IsClassC(NspObject *O);
extern NspClassC *GetClassCCopy (Stack stack, int i); 
extern NspClassC *GetClassC (Stack stack, int i); 

#endif 

#ifdef ClassC_Private 
static int init_classc(NspClassC *o,NspTypeClassC *type);
static char *classc_type_as_string(void);
static char *classc_type_short_string(void);
static NspMethods *classc_get_methods(void); 
#endif /* ClassC_Private */
