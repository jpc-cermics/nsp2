/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkIMMulticontext
#define INC_NSP_GtkIMMulticontext

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkimcontext.h"

/*
* NspGtkIMMulticontext inherits from NspGtkIMContext
* just change some type attributes 
*/

typedef NspGtkIMContext NspGtkIMMulticontext ;
typedef NspTypeGtkIMContext NspTypeGtkIMMulticontext ;

extern int nsp_type_gtkimmulticontext_id;
extern NspTypeGtkIMMulticontext *nsp_type_gtkimmulticontext;

/* type instances for gtkimcontext */

NspTypeGtkIMMulticontext *new_type_gtkimmulticontext(type_mode mode);

/* instance for GtkIMMulticontext */

NspGtkIMMulticontext *new_gtkimmulticontext();

/*
* Object methods redefined for gtkimmulticontext 
*/

#define NULLGTKIMMULTICONTEXT (NspGtkIMMulticontext*) 0

NspGtkIMMulticontext *gtkimmulticontext_create(char *name,NspTypeBase *type);

/* from GtkIMMulticontextObj.c */

extern NspGtkIMMulticontext *gtkimmulticontext_object (NspObject *O); 
extern int IsGtkIMMulticontextObj (Stack stack, int i); 
extern int IsGtkIMMulticontext(NspObject *O);
extern NspGtkIMMulticontext *GetGtkIMMulticontextCopy (Stack stack, int i); 
extern NspGtkIMMulticontext *GetGtkIMMulticontext (Stack stack, int i); 

#endif 

#ifdef GtkIMMulticontext_Private 
static int init_gtkimmulticontext(NspGtkIMMulticontext *o,NspTypeGtkIMMulticontext *type);
static char *gtkimmulticontext_type_as_string(void);
static char *gtkimmulticontext_type_short_string(NspObject *v);
static AttrTab gtkimmulticontext_attrs[];
/* static int int_gtkimmulticontext_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkimmulticontext_get_methods(void); 
#endif /* GtkIMMulticontext_Private */
