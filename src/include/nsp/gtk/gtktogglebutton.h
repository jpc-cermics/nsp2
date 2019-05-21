/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkToggleButton
#define NSP_INC_NspGtkToggleButton

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

/* NspGtkToggleButton */

#include <nsp/gtk/gtkbutton.h>

/*
 * NspGtkToggleButton inherits from GtkButton
 * just change some type attributes 
 */

typedef NspGtkButton NspGtkToggleButton ;
typedef NspTypeGtkButton NspTypeGtkToggleButton ;

extern int nsp_type_gtktogglebutton_id;
extern NspTypeGtkToggleButton *nsp_type_gtktogglebutton;

/* type instances for gtkbutton */

NspTypeGtkToggleButton *new_type_gtktogglebutton(type_mode mode);

/* instance for NspGtkToggleButton */

NspGtkToggleButton *new_gtktogglebutton();

/*
 * Object methods redefined for gtktogglebutton 
 */

#define NULLGTKTOGGLEBUTTON (NspGtkToggleButton*) 0


/* from NspGtkToggleButtonObj.c */

extern NspGtkToggleButton *nsp_gtktogglebutton_object (NspObject *O);
extern int IsGtkToggleButtonObj (Stack stack, int i);
extern int IsGtkToggleButton(NspObject *O);
extern NspGtkToggleButton *GetGtkToggleButtonCopy (Stack stack, int i);
extern NspGtkToggleButton *GetGtkToggleButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkToggleButton */ 

#ifdef NspGtkToggleButton_Private 
static int init_gtktogglebutton(NspGtkToggleButton *o,NspTypeGtkToggleButton *type);
static char *nsp_gtktogglebutton_type_as_string(void);
static char *nsp_gtktogglebutton_type_short_string(NspObject *v);
static AttrTab gtktogglebutton_attrs[];
static NspMethods *gtktogglebutton_get_methods(void);
/* static int int_gtktogglebutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkToggleButton_Private */
