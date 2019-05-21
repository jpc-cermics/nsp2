/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkRadioButton
#define NSP_INC_NspGtkRadioButton

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

/* NspGtkRadioButton */

#include <nsp/gtk/gtkcheckbutton.h>

/*
 * NspGtkRadioButton inherits from GtkCheckButton
 * just change some type attributes 
 */

typedef NspGtkCheckButton NspGtkRadioButton ;
typedef NspTypeGtkCheckButton NspTypeGtkRadioButton ;

extern int nsp_type_gtkradiobutton_id;
extern NspTypeGtkRadioButton *nsp_type_gtkradiobutton;

/* type instances for gtkcheckbutton */

NspTypeGtkRadioButton *new_type_gtkradiobutton(type_mode mode);

/* instance for NspGtkRadioButton */

NspGtkRadioButton *new_gtkradiobutton();

/*
 * Object methods redefined for gtkradiobutton 
 */

#define NULLGTKRADIOBUTTON (NspGtkRadioButton*) 0


/* from NspGtkRadioButtonObj.c */

extern NspGtkRadioButton *nsp_gtkradiobutton_object (NspObject *O);
extern int IsGtkRadioButtonObj (Stack stack, int i);
extern int IsGtkRadioButton(NspObject *O);
extern NspGtkRadioButton *GetGtkRadioButtonCopy (Stack stack, int i);
extern NspGtkRadioButton *GetGtkRadioButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkRadioButton */ 

#ifdef NspGtkRadioButton_Private 
static int init_gtkradiobutton(NspGtkRadioButton *o,NspTypeGtkRadioButton *type);
static char *nsp_gtkradiobutton_type_as_string(void);
static char *nsp_gtkradiobutton_type_short_string(NspObject *v);
static AttrTab gtkradiobutton_attrs[];
static NspMethods *gtkradiobutton_get_methods(void);
/* static int int_gtkradiobutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkRadioButton_Private */
