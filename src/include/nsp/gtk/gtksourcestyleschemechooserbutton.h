/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceStyleSchemeChooserButton
#define NSP_INC_NspGtkSourceStyleSchemeChooserButton

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

/* NspGtkSourceStyleSchemeChooserButton */

#include <nsp/gtk/gtkbutton.h>

/*
 * NspGtkSourceStyleSchemeChooserButton inherits from GtkButton
 * just change some type attributes 
 */

typedef NspGtkButton NspGtkSourceStyleSchemeChooserButton ;
typedef NspTypeGtkButton NspTypeGtkSourceStyleSchemeChooserButton ;

extern int nsp_type_gtksourcestyleschemechooserbutton_id;
extern NspTypeGtkSourceStyleSchemeChooserButton *nsp_type_gtksourcestyleschemechooserbutton;

/* type instances for gtkbutton */

NspTypeGtkSourceStyleSchemeChooserButton *new_type_gtksourcestyleschemechooserbutton(type_mode mode);

/* instance for NspGtkSourceStyleSchemeChooserButton */

NspGtkSourceStyleSchemeChooserButton *new_gtksourcestyleschemechooserbutton();

/*
 * Object methods redefined for gtksourcestyleschemechooserbutton 
 */

#define NULLGTKSOURCESTYLESCHEMECHOOSERBUTTON (NspGtkSourceStyleSchemeChooserButton*) 0


/* from NspGtkSourceStyleSchemeChooserButtonObj.c */

extern NspGtkSourceStyleSchemeChooserButton *nsp_gtksourcestyleschemechooserbutton_object (NspObject *O);
extern int IsGtkSourceStyleSchemeChooserButtonObj (Stack stack, int i);
extern int IsGtkSourceStyleSchemeChooserButton(NspObject *O);
extern NspGtkSourceStyleSchemeChooserButton *GetGtkSourceStyleSchemeChooserButtonCopy (Stack stack, int i);
extern NspGtkSourceStyleSchemeChooserButton *GetGtkSourceStyleSchemeChooserButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceStyleSchemeChooserButton */ 

#ifdef NspGtkSourceStyleSchemeChooserButton_Private 
static int init_gtksourcestyleschemechooserbutton(NspGtkSourceStyleSchemeChooserButton *o,NspTypeGtkSourceStyleSchemeChooserButton *type);
static char *nsp_gtksourcestyleschemechooserbutton_type_as_string(void);
static char *nsp_gtksourcestyleschemechooserbutton_type_short_string(NspObject *v);
static AttrTab gtksourcestyleschemechooserbutton_attrs[];
static NspMethods *gtksourcestyleschemechooserbutton_get_methods(void);
/* static int int_gtksourcestyleschemechooserbutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceStyleSchemeChooserButton_Private */
