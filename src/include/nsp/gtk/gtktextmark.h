/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTextMark
#define INC_NSP_GtkTextMark

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkTextMark inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkTextMark ;
typedef NspTypeGObject NspTypeGtkTextMark ;

extern int nsp_type_gtktextmark_id;
extern NspTypeGtkTextMark *nsp_type_gtktextmark;

/* type instances for gobject */

NspTypeGtkTextMark *new_type_gtktextmark(type_mode mode);

/* instance for GtkTextMark */

NspGtkTextMark *new_gtktextmark();

/*
* Object methods redefined for gtktextmark 
*/

#define NULLGTKTEXTMARK (NspGtkTextMark*) 0

NspGtkTextMark *gtktextmark_create(char *name,NspTypeBase *type);

/* from GtkTextMarkObj.c */

extern NspGtkTextMark *gtktextmark_object (NspObject *O); 
extern int IsGtkTextMarkObj (Stack stack, int i); 
extern int IsGtkTextMark(NspObject *O);
extern NspGtkTextMark *GetGtkTextMarkCopy (Stack stack, int i); 
extern NspGtkTextMark *GetGtkTextMark (Stack stack, int i); 

#endif 

#ifdef GtkTextMark_Private 
static int init_gtktextmark(NspGtkTextMark *o,NspTypeGtkTextMark *type);
static char *gtktextmark_type_as_string(void);
static char *gtktextmark_type_short_string(void);
static AttrTab gtktextmark_attrs[];
/* static int int_gtktextmark_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktextmark_get_methods(void); 
#endif /* GtkTextMark_Private */
