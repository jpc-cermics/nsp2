/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkRadioToolButton
#define NSP_INC_NspGtkRadioToolButton

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

/* NspGtkRadioToolButton */

#include <nsp/gtk/gtktoggletoolbutton.h>

/*
 * NspGtkRadioToolButton inherits from GtkToggleToolButton
 * just change some type attributes 
 */

typedef NspGtkToggleToolButton NspGtkRadioToolButton ;
typedef NspTypeGtkToggleToolButton NspTypeGtkRadioToolButton ;

extern int nsp_type_gtkradiotoolbutton_id;
extern NspTypeGtkRadioToolButton *nsp_type_gtkradiotoolbutton;

/* type instances for gtktoggletoolbutton */

NspTypeGtkRadioToolButton *new_type_gtkradiotoolbutton(type_mode mode);

/* instance for NspGtkRadioToolButton */

NspGtkRadioToolButton *new_gtkradiotoolbutton();

/*
 * Object methods redefined for gtkradiotoolbutton 
 */

#define NULLGTKRADIOTOOLBUTTON (NspGtkRadioToolButton*) 0


/* from NspGtkRadioToolButtonObj.c */

extern NspGtkRadioToolButton *nsp_gtkradiotoolbutton_object (NspObject *O);
extern int IsGtkRadioToolButtonObj (Stack stack, int i);
extern int IsGtkRadioToolButton(NspObject *O);
extern NspGtkRadioToolButton *GetGtkRadioToolButtonCopy (Stack stack, int i);
extern NspGtkRadioToolButton *GetGtkRadioToolButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkRadioToolButton */ 

#ifdef NspGtkRadioToolButton_Private 
static int init_gtkradiotoolbutton(NspGtkRadioToolButton *o,NspTypeGtkRadioToolButton *type);
static char *nsp_gtkradiotoolbutton_type_as_string(void);
static char *nsp_gtkradiotoolbutton_type_short_string(NspObject *v);
static AttrTab gtkradiotoolbutton_attrs[];
static NspMethods *gtkradiotoolbutton_get_methods(void);
/* static int int_gtkradiotoolbutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkRadioToolButton_Private */
