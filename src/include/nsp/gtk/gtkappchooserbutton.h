/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkAppChooserButton
#define NSP_INC_NspGtkAppChooserButton

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

/* NspGtkAppChooserButton */

#include <nsp/gtk/gtkbox.h>

/*
 * NspGtkAppChooserButton inherits from GtkBox
 * just change some type attributes 
 */

typedef NspGtkBox NspGtkAppChooserButton ;
typedef NspTypeGtkBox NspTypeGtkAppChooserButton ;

extern int nsp_type_gtkappchooserbutton_id;
extern NspTypeGtkAppChooserButton *nsp_type_gtkappchooserbutton;

/* type instances for gtkbox */

NspTypeGtkAppChooserButton *new_type_gtkappchooserbutton(type_mode mode);

/* instance for NspGtkAppChooserButton */

NspGtkAppChooserButton *new_gtkappchooserbutton();

/*
 * Object methods redefined for gtkappchooserbutton 
 */

#define NULLGTKAPPCHOOSERBUTTON (NspGtkAppChooserButton*) 0


/* from NspGtkAppChooserButtonObj.c */

extern NspGtkAppChooserButton *nsp_gtkappchooserbutton_object (NspObject *O);
extern int IsGtkAppChooserButtonObj (Stack stack, int i);
extern int IsGtkAppChooserButton(NspObject *O);
extern NspGtkAppChooserButton *GetGtkAppChooserButtonCopy (Stack stack, int i);
extern NspGtkAppChooserButton *GetGtkAppChooserButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkAppChooserButton */ 

#ifdef NspGtkAppChooserButton_Private 
static int init_gtkappchooserbutton(NspGtkAppChooserButton *o,NspTypeGtkAppChooserButton *type);
static char *nsp_gtkappchooserbutton_type_as_string(void);
static char *nsp_gtkappchooserbutton_type_short_string(NspObject *v);
static AttrTab gtkappchooserbutton_attrs[];
static NspMethods *gtkappchooserbutton_get_methods(void);
/* static int int_gtkappchooserbutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkAppChooserButton_Private */
