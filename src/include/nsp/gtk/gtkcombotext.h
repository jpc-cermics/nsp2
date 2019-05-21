/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkComboBoxText
#define NSP_INC_NspGtkComboBoxText

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

/* NspGtkComboBoxText */

#include <nsp/gtk/gtkcombobox.h>

/*
 * NspGtkComboBoxText inherits from GtkComboBox
 * just change some type attributes 
 */

typedef NspGtkComboBox NspGtkComboBoxText ;
typedef NspTypeGtkComboBox NspTypeGtkComboText ;

extern int nsp_type_gtkcombotext_id;
extern NspTypeGtkComboText *nsp_type_gtkcombotext;

/* type instances for gtkcombobox */

NspTypeGtkComboText *new_type_gtkcombotext(type_mode mode);

/* instance for NspGtkComboBoxText */

NspGtkComboBoxText *new_gtkcombotext();

/*
 * Object methods redefined for gtkcombotext 
 */

#define NULLGTKCOMBOTEXT (NspGtkComboBoxText*) 0


/* from NspGtkComboBoxTextObj.c */

extern NspGtkComboBoxText *nsp_gtkcombotext_object (NspObject *O);
extern int IsGtkComboTextObj (Stack stack, int i);
extern int IsGtkComboText(NspObject *O);
extern NspGtkComboBoxText *GetGtkComboTextCopy (Stack stack, int i);
extern NspGtkComboBoxText *GetGtkComboText (Stack stack, int i);

#endif /* NSP_INC_NspGtkComboBoxText */ 

#ifdef NspGtkComboBoxText_Private 
static int init_gtkcombotext(NspGtkComboBoxText *o,NspTypeGtkComboText *type);
static char *nsp_gtkcombotext_type_as_string(void);
static char *nsp_gtkcombotext_type_short_string(NspObject *v);
static AttrTab gtkcombotext_attrs[];
static NspMethods *gtkcombotext_get_methods(void);
/* static int int_gtkcombotext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkComboBoxText_Private */
