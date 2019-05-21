/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkMenuToolButton
#define NSP_INC_NspGtkMenuToolButton

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

/* NspGtkMenuToolButton */

#include <nsp/gtk/gtktoolbutton.h>

/*
 * NspGtkMenuToolButton inherits from GtkToolButton
 * just change some type attributes 
 */

typedef NspGtkToolButton NspGtkMenuToolButton ;
typedef NspTypeGtkToolButton NspTypeGtkMenuToolButton ;

extern int nsp_type_gtkmenutoolbutton_id;
extern NspTypeGtkMenuToolButton *nsp_type_gtkmenutoolbutton;

/* type instances for gtktoolbutton */

NspTypeGtkMenuToolButton *new_type_gtkmenutoolbutton(type_mode mode);

/* instance for NspGtkMenuToolButton */

NspGtkMenuToolButton *new_gtkmenutoolbutton();

/*
 * Object methods redefined for gtkmenutoolbutton 
 */

#define NULLGTKMENUTOOLBUTTON (NspGtkMenuToolButton*) 0


/* from NspGtkMenuToolButtonObj.c */

extern NspGtkMenuToolButton *nsp_gtkmenutoolbutton_object (NspObject *O);
extern int IsGtkMenuToolButtonObj (Stack stack, int i);
extern int IsGtkMenuToolButton(NspObject *O);
extern NspGtkMenuToolButton *GetGtkMenuToolButtonCopy (Stack stack, int i);
extern NspGtkMenuToolButton *GetGtkMenuToolButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkMenuToolButton */ 

#ifdef NspGtkMenuToolButton_Private 
static int init_gtkmenutoolbutton(NspGtkMenuToolButton *o,NspTypeGtkMenuToolButton *type);
static char *nsp_gtkmenutoolbutton_type_as_string(void);
static char *nsp_gtkmenutoolbutton_type_short_string(NspObject *v);
static AttrTab gtkmenutoolbutton_attrs[];
static NspMethods *gtkmenutoolbutton_get_methods(void);
/* static int int_gtkmenutoolbutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkMenuToolButton_Private */
