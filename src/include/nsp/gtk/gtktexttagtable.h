/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTextTagTable
#define INC_NSP_GtkTextTagTable

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkTextTagTable inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkTextTagTable ;
typedef NspTypeGObject NspTypeGtkTextTagTable ;

extern int nsp_type_gtktexttagtable_id;
extern NspTypeGtkTextTagTable *nsp_type_gtktexttagtable;

/* type instances for gobject */

NspTypeGtkTextTagTable *new_type_gtktexttagtable(type_mode mode);

/* instance for GtkTextTagTable */

NspGtkTextTagTable *new_gtktexttagtable();

/*
* Object methods redefined for gtktexttagtable 
*/

#define NULLGTKTEXTTAGTABLE (NspGtkTextTagTable*) 0

NspGtkTextTagTable *gtktexttagtable_create(char *name,NspTypeBase *type);

/* from GtkTextTagTableObj.c */

extern NspGtkTextTagTable *gtktexttagtable_object (NspObject *O); 
extern int IsGtkTextTagTableObj (Stack stack, int i); 
extern int IsGtkTextTagTable(NspObject *O);
extern NspGtkTextTagTable *GetGtkTextTagTableCopy (Stack stack, int i); 
extern NspGtkTextTagTable *GetGtkTextTagTable (Stack stack, int i); 

#endif 

#ifdef GtkTextTagTable_Private 
static int init_gtktexttagtable(NspGtkTextTagTable *o,NspTypeGtkTextTagTable *type);
static char *gtktexttagtable_type_as_string(void);
static char *gtktexttagtable_type_short_string(NspObject *v);
static AttrTab gtktexttagtable_attrs[];
/* static int int_gtktexttagtable_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktexttagtable_get_methods(void); 
#endif /* GtkTextTagTable_Private */
