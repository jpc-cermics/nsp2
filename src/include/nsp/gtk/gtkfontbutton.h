/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkFontButton
#define NSP_INC_NspGtkFontButton

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

/* NspGtkFontButton */

#include <nsp/gtk/gtkbutton.h>

/*
 * NspGtkFontButton inherits from GtkButton
 * just change some type attributes 
 */

typedef NspGtkButton NspGtkFontButton ;
typedef NspTypeGtkButton NspTypeGtkFontButton ;

extern int nsp_type_gtkfontbutton_id;
extern NspTypeGtkFontButton *nsp_type_gtkfontbutton;

/* type instances for gtkbutton */

NspTypeGtkFontButton *new_type_gtkfontbutton(type_mode mode);

/* instance for NspGtkFontButton */

NspGtkFontButton *new_gtkfontbutton();

/*
 * Object methods redefined for gtkfontbutton 
 */

#define NULLGTKFONTBUTTON (NspGtkFontButton*) 0


/* from NspGtkFontButtonObj.c */

extern NspGtkFontButton *nsp_gtkfontbutton_object (NspObject *O);
extern int IsGtkFontButtonObj (Stack stack, int i);
extern int IsGtkFontButton(NspObject *O);
extern NspGtkFontButton *GetGtkFontButtonCopy (Stack stack, int i);
extern NspGtkFontButton *GetGtkFontButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkFontButton */ 

#ifdef NspGtkFontButton_Private 
static int init_gtkfontbutton(NspGtkFontButton *o,NspTypeGtkFontButton *type);
static char *nsp_gtkfontbutton_type_as_string(void);
static char *nsp_gtkfontbutton_type_short_string(NspObject *v);
static AttrTab gtkfontbutton_attrs[];
static NspMethods *gtkfontbutton_get_methods(void);
/* static int int_gtkfontbutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkFontButton_Private */
