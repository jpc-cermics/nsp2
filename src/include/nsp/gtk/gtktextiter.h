/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTextIter
#define INC_NSP_GtkTextIter

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGtkTextIter inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGtkTextIter ;
typedef NspTypeGBoxed NspTypeGtkTextIter ;

extern int nsp_type_gtktextiter_id;
extern NspTypeGtkTextIter *nsp_type_gtktextiter;

/* type instances for gboxed */

NspTypeGtkTextIter *new_type_gtktextiter(type_mode mode);

/* instance for GtkTextIter */

NspGtkTextIter *new_gtktextiter();

/*
* Object methods redefined for gtktextiter 
*/

#define NULLGTKTEXTITER (NspGtkTextIter*) 0

NspGtkTextIter *gtktextiter_create(char *name,NspTypeBase *type);

/* from GtkTextIterObj.c */

extern NspGtkTextIter *gtktextiter_object (NspObject *O); 
extern int IsGtkTextIterObj (Stack stack, int i); 
extern int IsGtkTextIter(NspObject *O);
extern NspGtkTextIter *GetGtkTextIterCopy (Stack stack, int i); 
extern NspGtkTextIter *GetGtkTextIter (Stack stack, int i); 

#endif 

#ifdef GtkTextIter_Private 
static int init_gtktextiter(NspGtkTextIter *o,NspTypeGtkTextIter *type);
static char *gtktextiter_type_as_string(void);
static char *gtktextiter_type_short_string(void);
static AttrTab gtktextiter_attrs[];
/* static int int_gtktextiter_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktextiter_get_methods(void); 
#endif /* GtkTextIter_Private */
