/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkComboBox
#define NSP_INC_NspGtkComboBox

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

/* NspGtkComboBox */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkComboBox inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkComboBox ;
typedef NspTypeGtkBin NspTypeGtkComboBox ;

extern int nsp_type_gtkcombobox_id;
extern NspTypeGtkComboBox *nsp_type_gtkcombobox;

/* type instances for gtkbin */

NspTypeGtkComboBox *new_type_gtkcombobox(type_mode mode);

/* instance for NspGtkComboBox */

NspGtkComboBox *new_gtkcombobox();

/*
 * Object methods redefined for gtkcombobox 
 */

#define NULLGTKCOMBOBOX (NspGtkComboBox*) 0


/* from NspGtkComboBoxObj.c */

extern NspGtkComboBox *nsp_gtkcombobox_object (NspObject *O);
extern int IsGtkComboBoxObj (Stack stack, int i);
extern int IsGtkComboBox(NspObject *O);
extern NspGtkComboBox *GetGtkComboBoxCopy (Stack stack, int i);
extern NspGtkComboBox *GetGtkComboBox (Stack stack, int i);

#endif /* NSP_INC_NspGtkComboBox */ 

#ifdef NspGtkComboBox_Private 
static int init_gtkcombobox(NspGtkComboBox *o,NspTypeGtkComboBox *type);
static char *nsp_gtkcombobox_type_as_string(void);
static char *nsp_gtkcombobox_type_short_string(NspObject *v);
static AttrTab gtkcombobox_attrs[];
static NspMethods *gtkcombobox_get_methods(void);
/* static int int_gtkcombobox_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkComboBox_Private */
