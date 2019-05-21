/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkScaleButton
#define NSP_INC_NspGtkScaleButton

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

/* NspGtkScaleButton */

#include <nsp/gtk/gtkbutton.h>

/*
 * NspGtkScaleButton inherits from GtkButton
 * just change some type attributes 
 */

typedef NspGtkButton NspGtkScaleButton ;
typedef NspTypeGtkButton NspTypeGtkScaleButton ;

extern int nsp_type_gtkscalebutton_id;
extern NspTypeGtkScaleButton *nsp_type_gtkscalebutton;

/* type instances for gtkbutton */

NspTypeGtkScaleButton *new_type_gtkscalebutton(type_mode mode);

/* instance for NspGtkScaleButton */

NspGtkScaleButton *new_gtkscalebutton();

/*
 * Object methods redefined for gtkscalebutton 
 */

#define NULLGTKSCALEBUTTON (NspGtkScaleButton*) 0


/* from NspGtkScaleButtonObj.c */

extern NspGtkScaleButton *nsp_gtkscalebutton_object (NspObject *O);
extern int IsGtkScaleButtonObj (Stack stack, int i);
extern int IsGtkScaleButton(NspObject *O);
extern NspGtkScaleButton *GetGtkScaleButtonCopy (Stack stack, int i);
extern NspGtkScaleButton *GetGtkScaleButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkScaleButton */ 

#ifdef NspGtkScaleButton_Private 
static int init_gtkscalebutton(NspGtkScaleButton *o,NspTypeGtkScaleButton *type);
static char *nsp_gtkscalebutton_type_as_string(void);
static char *nsp_gtkscalebutton_type_short_string(NspObject *v);
static AttrTab gtkscalebutton_attrs[];
static NspMethods *gtkscalebutton_get_methods(void);
/* static int int_gtkscalebutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkScaleButton_Private */
