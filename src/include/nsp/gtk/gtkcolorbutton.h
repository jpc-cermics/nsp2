/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkColorButton
#define NSP_INC_NspGtkColorButton

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

/* NspGtkColorButton */

#include <nsp/gtk/gtkbutton.h>

/*
 * NspGtkColorButton inherits from GtkButton
 * just change some type attributes 
 */

typedef NspGtkButton NspGtkColorButton ;
typedef NspTypeGtkButton NspTypeGtkColorButton ;

extern int nsp_type_gtkcolorbutton_id;
extern NspTypeGtkColorButton *nsp_type_gtkcolorbutton;

/* type instances for gtkbutton */

NspTypeGtkColorButton *new_type_gtkcolorbutton(type_mode mode);

/* instance for NspGtkColorButton */

NspGtkColorButton *new_gtkcolorbutton();

/*
 * Object methods redefined for gtkcolorbutton 
 */

#define NULLGTKCOLORBUTTON (NspGtkColorButton*) 0


/* from NspGtkColorButtonObj.c */

extern NspGtkColorButton *nsp_gtkcolorbutton_object (NspObject *O);
extern int IsGtkColorButtonObj (Stack stack, int i);
extern int IsGtkColorButton(NspObject *O);
extern NspGtkColorButton *GetGtkColorButtonCopy (Stack stack, int i);
extern NspGtkColorButton *GetGtkColorButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkColorButton */ 

#ifdef NspGtkColorButton_Private 
static int init_gtkcolorbutton(NspGtkColorButton *o,NspTypeGtkColorButton *type);
static char *nsp_gtkcolorbutton_type_as_string(void);
static char *nsp_gtkcolorbutton_type_short_string(NspObject *v);
static AttrTab gtkcolorbutton_attrs[];
static NspMethods *gtkcolorbutton_get_methods(void);
/* static int int_gtkcolorbutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkColorButton_Private */
