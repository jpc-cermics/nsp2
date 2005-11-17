/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkComboBox
#define INC_NSP_GtkComboBox

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbin.h"

/*
* NspGtkComboBox inherits from NspGtkBin
* just change some type attributes 
*/

typedef NspGtkBin NspGtkComboBox ;
typedef NspTypeGtkBin NspTypeGtkComboBox ;

extern int nsp_type_gtkcombobox_id;
extern NspTypeGtkComboBox *nsp_type_gtkcombobox;

/* type instances for gtkbin */

NspTypeGtkComboBox *new_type_gtkcombobox(type_mode mode);

/* instance for GtkComboBox */

NspGtkComboBox *new_gtkcombobox();

/*
* Object methods redefined for gtkcombobox 
*/

#define NULLGTKCOMBOBOX (NspGtkComboBox*) 0

NspGtkComboBox *gtkcombobox_create(char *name,NspTypeBase *type);

/* from GtkComboBoxObj.c */

extern NspGtkComboBox *gtkcombobox_object (NspObject *O); 
extern int IsGtkComboBoxObj (Stack stack, int i); 
extern int IsGtkComboBox(NspObject *O);
extern NspGtkComboBox *GetGtkComboBoxCopy (Stack stack, int i); 
extern NspGtkComboBox *GetGtkComboBox (Stack stack, int i); 

#endif 

#ifdef GtkComboBox_Private 
static int init_gtkcombobox(NspGtkComboBox *o,NspTypeGtkComboBox *type);
static char *gtkcombobox_type_as_string(void);
static char *gtkcombobox_type_short_string(void);
static AttrTab gtkcombobox_attrs[];
/* static int int_gtkcombobox_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcombobox_get_methods(void); 
#endif /* GtkComboBox_Private */
