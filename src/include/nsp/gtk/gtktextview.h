/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTextView
#define INC_NSP_GtkTextView

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspGtkTextView inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspGtkTextView ;
typedef NspTypeGtkContainer NspTypeGtkTextView ;

extern int nsp_type_gtktextview_id;
extern NspTypeGtkTextView *nsp_type_gtktextview;

/* type instances for gtkcontainer */

NspTypeGtkTextView *new_type_gtktextview(type_mode mode);

/* instance for GtkTextView */

NspGtkTextView *new_gtktextview();

/*
* Object methods redefined for gtktextview 
*/

#define NULLGTKTEXTVIEW (NspGtkTextView*) 0

NspGtkTextView *gtktextview_create(char *name,NspTypeBase *type);

/* from GtkTextViewObj.c */

extern NspGtkTextView *gtktextview_object (NspObject *O); 
extern int IsGtkTextViewObj (Stack stack, int i); 
extern int IsGtkTextView(NspObject *O);
extern NspGtkTextView *GetGtkTextViewCopy (Stack stack, int i); 
extern NspGtkTextView *GetGtkTextView (Stack stack, int i); 

#endif 

#ifdef GtkTextView_Private 
static int init_gtktextview(NspGtkTextView *o,NspTypeGtkTextView *type);
static char *gtktextview_type_as_string(void);
static char *gtktextview_type_short_string(void);
static AttrTab gtktextview_attrs[];
/* static int int_gtktextview_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktextview_get_methods(void); 
#endif /* GtkTextView_Private */
