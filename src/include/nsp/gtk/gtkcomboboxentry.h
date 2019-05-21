/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkComboBoxEntry
#define NSP_INC_NspGtkComboBoxEntry

/*
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/* NspGtkComboBoxEntry */

#include <nsp/gtk/gtkcombobox.h>

/*
 * NspGtkComboBoxEntry inherits from GtkComboBox
 * just change some type attributes 
 */

typedef NspGtkComboBox NspGtkComboBoxEntry ;
typedef NspTypeGtkComboBox NspTypeGtkComboBoxEntry ;

extern int nsp_type_gtkcomboboxentry_id;
extern NspTypeGtkComboBoxEntry *nsp_type_gtkcomboboxentry;

/* type instances for gtkcombobox */

NspTypeGtkComboBoxEntry *new_type_gtkcomboboxentry(type_mode mode);

/* instance for NspGtkComboBoxEntry */

NspGtkComboBoxEntry *new_gtkcomboboxentry();

/*
 * Object methods redefined for gtkcomboboxentry 
 */

#define NULLGTKCOMBOBOXENTRY (NspGtkComboBoxEntry*) 0


/* from NspGtkComboBoxEntryObj.c */

extern NspGtkComboBoxEntry *nsp_gtkcomboboxentry_object (NspObject *O);
extern int IsGtkComboBoxEntryObj (Stack stack, int i);
extern int IsGtkComboBoxEntry(NspObject *O);
extern NspGtkComboBoxEntry *GetGtkComboBoxEntryCopy (Stack stack, int i);
extern NspGtkComboBoxEntry *GetGtkComboBoxEntry (Stack stack, int i);

#endif /* NSP_INC_NspGtkComboBoxEntry */ 

#ifdef NspGtkComboBoxEntry_Private 
static int init_gtkcomboboxentry(NspGtkComboBoxEntry *o,NspTypeGtkComboBoxEntry *type);
static char *nsp_gtkcomboboxentry_type_as_string(void);
static char *nsp_gtkcomboboxentry_type_short_string(NspObject *v);
static AttrTab gtkcomboboxentry_attrs[];
static NspMethods *gtkcomboboxentry_get_methods(void);
/* static int int_gtkcomboboxentry_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkComboBoxEntry_Private */
