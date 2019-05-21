/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCheckButton
#define NSP_INC_NspGtkCheckButton

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

/* NspGtkCheckButton */

#include <nsp/gtk/gtktogglebutton.h>

/*
 * NspGtkCheckButton inherits from GtkToggleButton
 * just change some type attributes 
 */

typedef NspGtkToggleButton NspGtkCheckButton ;
typedef NspTypeGtkToggleButton NspTypeGtkCheckButton ;

extern int nsp_type_gtkcheckbutton_id;
extern NspTypeGtkCheckButton *nsp_type_gtkcheckbutton;

/* type instances for gtktogglebutton */

NspTypeGtkCheckButton *new_type_gtkcheckbutton(type_mode mode);

/* instance for NspGtkCheckButton */

NspGtkCheckButton *new_gtkcheckbutton();

/*
 * Object methods redefined for gtkcheckbutton 
 */

#define NULLGTKCHECKBUTTON (NspGtkCheckButton*) 0


/* from NspGtkCheckButtonObj.c */

extern NspGtkCheckButton *nsp_gtkcheckbutton_object (NspObject *O);
extern int IsGtkCheckButtonObj (Stack stack, int i);
extern int IsGtkCheckButton(NspObject *O);
extern NspGtkCheckButton *GetGtkCheckButtonCopy (Stack stack, int i);
extern NspGtkCheckButton *GetGtkCheckButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkCheckButton */ 

#ifdef NspGtkCheckButton_Private 
static int init_gtkcheckbutton(NspGtkCheckButton *o,NspTypeGtkCheckButton *type);
static char *nsp_gtkcheckbutton_type_as_string(void);
static char *nsp_gtkcheckbutton_type_short_string(NspObject *v);
static AttrTab gtkcheckbutton_attrs[];
static NspMethods *gtkcheckbutton_get_methods(void);
/* static int int_gtkcheckbutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCheckButton_Private */
