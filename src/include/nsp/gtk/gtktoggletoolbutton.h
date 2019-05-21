/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkToggleToolButton
#define NSP_INC_NspGtkToggleToolButton

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

/* NspGtkToggleToolButton */

#include <nsp/gtk/gtktoolbutton.h>

/*
 * NspGtkToggleToolButton inherits from GtkToolButton
 * just change some type attributes 
 */

typedef NspGtkToolButton NspGtkToggleToolButton ;
typedef NspTypeGtkToolButton NspTypeGtkToggleToolButton ;

extern int nsp_type_gtktoggletoolbutton_id;
extern NspTypeGtkToggleToolButton *nsp_type_gtktoggletoolbutton;

/* type instances for gtktoolbutton */

NspTypeGtkToggleToolButton *new_type_gtktoggletoolbutton(type_mode mode);

/* instance for NspGtkToggleToolButton */

NspGtkToggleToolButton *new_gtktoggletoolbutton();

/*
 * Object methods redefined for gtktoggletoolbutton 
 */

#define NULLGTKTOGGLETOOLBUTTON (NspGtkToggleToolButton*) 0


/* from NspGtkToggleToolButtonObj.c */

extern NspGtkToggleToolButton *nsp_gtktoggletoolbutton_object (NspObject *O);
extern int IsGtkToggleToolButtonObj (Stack stack, int i);
extern int IsGtkToggleToolButton(NspObject *O);
extern NspGtkToggleToolButton *GetGtkToggleToolButtonCopy (Stack stack, int i);
extern NspGtkToggleToolButton *GetGtkToggleToolButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkToggleToolButton */ 

#ifdef NspGtkToggleToolButton_Private 
static int init_gtktoggletoolbutton(NspGtkToggleToolButton *o,NspTypeGtkToggleToolButton *type);
static char *nsp_gtktoggletoolbutton_type_as_string(void);
static char *nsp_gtktoggletoolbutton_type_short_string(NspObject *v);
static AttrTab gtktoggletoolbutton_attrs[];
static NspMethods *gtktoggletoolbutton_get_methods(void);
/* static int int_gtktoggletoolbutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkToggleToolButton_Private */
