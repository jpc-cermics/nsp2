/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkMenuButton
#define NSP_INC_NspGtkMenuButton

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

/* NspGtkMenuButton */

#include <nsp/gtk/gtktogglebutton.h>

/*
 * NspGtkMenuButton inherits from GtkToggleButton
 * just change some type attributes 
 */

typedef NspGtkToggleButton NspGtkMenuButton ;
typedef NspTypeGtkToggleButton NspTypeGtkMenuButton ;

extern int nsp_type_gtkmenubutton_id;
extern NspTypeGtkMenuButton *nsp_type_gtkmenubutton;

/* type instances for gtktogglebutton */

NspTypeGtkMenuButton *new_type_gtkmenubutton(type_mode mode);

/* instance for NspGtkMenuButton */

NspGtkMenuButton *new_gtkmenubutton();

/*
 * Object methods redefined for gtkmenubutton 
 */

#define NULLGTKMENUBUTTON (NspGtkMenuButton*) 0


/* from NspGtkMenuButtonObj.c */

extern NspGtkMenuButton *nsp_gtkmenubutton_object (NspObject *O);
extern int IsGtkMenuButtonObj (Stack stack, int i);
extern int IsGtkMenuButton(NspObject *O);
extern NspGtkMenuButton *GetGtkMenuButtonCopy (Stack stack, int i);
extern NspGtkMenuButton *GetGtkMenuButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkMenuButton */ 

#ifdef NspGtkMenuButton_Private 
static int init_gtkmenubutton(NspGtkMenuButton *o,NspTypeGtkMenuButton *type);
static char *nsp_gtkmenubutton_type_as_string(void);
static char *nsp_gtkmenubutton_type_short_string(NspObject *v);
static AttrTab gtkmenubutton_attrs[];
static NspMethods *gtkmenubutton_get_methods(void);
/* static int int_gtkmenubutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkMenuButton_Private */
