/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkComboBoxEntry
#define INC_NSP_GtkComboBoxEntry

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcombobox.h"

/*
* NspGtkComboBoxEntry inherits from NspGtkComboBox
* just change some type attributes 
*/

typedef NspGtkComboBox NspGtkComboBoxEntry ;
typedef NspTypeGtkComboBox NspTypeGtkComboBoxEntry ;

extern int nsp_type_gtkcomboboxentry_id;
extern NspTypeGtkComboBoxEntry *nsp_type_gtkcomboboxentry;

/* type instances for gtkcombobox */

NspTypeGtkComboBoxEntry *new_type_gtkcomboboxentry(type_mode mode);

/* instance for GtkComboBoxEntry */

NspGtkComboBoxEntry *new_gtkcomboboxentry();

/*
* Object methods redefined for gtkcomboboxentry 
*/

#define NULLGTKCOMBOBOXENTRY (NspGtkComboBoxEntry*) 0

NspGtkComboBoxEntry *gtkcomboboxentry_create(char *name,NspTypeBase *type);

/* from GtkComboBoxEntryObj.c */

extern NspGtkComboBoxEntry *gtkcomboboxentry_object (NspObject *O); 
extern int IsGtkComboBoxEntryObj (Stack stack, int i); 
extern int IsGtkComboBoxEntry(NspObject *O);
extern NspGtkComboBoxEntry *GetGtkComboBoxEntryCopy (Stack stack, int i); 
extern NspGtkComboBoxEntry *GetGtkComboBoxEntry (Stack stack, int i); 

#endif 

#ifdef GtkComboBoxEntry_Private 
static int init_gtkcomboboxentry(NspGtkComboBoxEntry *o,NspTypeGtkComboBoxEntry *type);
static char *gtkcomboboxentry_type_as_string(void);
static char *gtkcomboboxentry_type_short_string(void);
static AttrTab gtkcomboboxentry_attrs[];
/* static int int_gtkcomboboxentry_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcomboboxentry_get_methods(void); 
#endif /* GtkComboBoxEntry_Private */
