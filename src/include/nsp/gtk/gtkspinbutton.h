/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSpinButton
#define NSP_INC_NspGtkSpinButton

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

/* NspGtkSpinButton */

#include <nsp/gtk/gtkentry.h>

/*
 * NspGtkSpinButton inherits from GtkEntry
 * just change some type attributes 
 */

typedef NspGtkEntry NspGtkSpinButton ;
typedef NspTypeGtkEntry NspTypeGtkSpinButton ;

extern int nsp_type_gtkspinbutton_id;
extern NspTypeGtkSpinButton *nsp_type_gtkspinbutton;

/* type instances for gtkentry */

NspTypeGtkSpinButton *new_type_gtkspinbutton(type_mode mode);

/* instance for NspGtkSpinButton */

NspGtkSpinButton *new_gtkspinbutton();

/*
 * Object methods redefined for gtkspinbutton 
 */

#define NULLGTKSPINBUTTON (NspGtkSpinButton*) 0


/* from NspGtkSpinButtonObj.c */

extern NspGtkSpinButton *nsp_gtkspinbutton_object (NspObject *O);
extern int IsGtkSpinButtonObj (Stack stack, int i);
extern int IsGtkSpinButton(NspObject *O);
extern NspGtkSpinButton *GetGtkSpinButtonCopy (Stack stack, int i);
extern NspGtkSpinButton *GetGtkSpinButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkSpinButton */ 

#ifdef NspGtkSpinButton_Private 
static int init_gtkspinbutton(NspGtkSpinButton *o,NspTypeGtkSpinButton *type);
static char *nsp_gtkspinbutton_type_as_string(void);
static char *nsp_gtkspinbutton_type_short_string(NspObject *v);
static AttrTab gtkspinbutton_attrs[];
static NspMethods *gtkspinbutton_get_methods(void);
/* static int int_gtkspinbutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSpinButton_Private */
