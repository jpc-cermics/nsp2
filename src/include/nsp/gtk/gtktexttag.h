/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTextTag
#define INC_NSP_GtkTextTag

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkTextTag inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkTextTag ;
typedef NspTypeGObject NspTypeGtkTextTag ;

extern int nsp_type_gtktexttag_id;
extern NspTypeGtkTextTag *nsp_type_gtktexttag;

/* type instances for gobject */

NspTypeGtkTextTag *new_type_gtktexttag(type_mode mode);

/* instance for GtkTextTag */

NspGtkTextTag *new_gtktexttag();

/*
* Object methods redefined for gtktexttag 
*/

#ifdef GtkTextTag_Private 
static int init_gtktexttag(NspGtkTextTag *o,NspTypeGtkTextTag *type);
static char *gtktexttag_type_as_string(void);
static char *gtktexttag_type_short_string(void);
static AttrTab gtktexttag_attrs[];
/* static int int_gtktexttag_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktexttag_get_methods(void); 
#endif /* GtkTextTag_Private */

#define NULLGTKTEXTTAG (NspGtkTextTag*) 0

NspGtkTextTag *gtktexttag_create(char *name,NspTypeBase *type);

/* from GtkTextTagObj.c */

extern NspGtkTextTag *gtktexttag_object (NspObject *O); 
extern int IsGtkTextTagObj (Stack stack, int i); 
extern int IsGtkTextTag(NspObject *O);
extern NspGtkTextTag *GetGtkTextTagCopy (Stack stack, int i); 
extern NspGtkTextTag *GetGtkTextTag (Stack stack, int i); 

#endif 
