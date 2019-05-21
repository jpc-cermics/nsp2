/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkButton
#define NSP_INC_NspGtkButton

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

/* NspGtkButton */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkButton inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkButton ;
typedef NspTypeGtkBin NspTypeGtkButton ;

extern int nsp_type_gtkbutton_id;
extern NspTypeGtkButton *nsp_type_gtkbutton;

/* type instances for gtkbin */

NspTypeGtkButton *new_type_gtkbutton(type_mode mode);

/* instance for NspGtkButton */

NspGtkButton *new_gtkbutton();

/*
 * Object methods redefined for gtkbutton 
 */

#define NULLGTKBUTTON (NspGtkButton*) 0


/* from NspGtkButtonObj.c */

extern NspGtkButton *nsp_gtkbutton_object (NspObject *O);
extern int IsGtkButtonObj (Stack stack, int i);
extern int IsGtkButton(NspObject *O);
extern NspGtkButton *GetGtkButtonCopy (Stack stack, int i);
extern NspGtkButton *GetGtkButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkButton */ 

#ifdef NspGtkButton_Private 
static int init_gtkbutton(NspGtkButton *o,NspTypeGtkButton *type);
static char *nsp_gtkbutton_type_as_string(void);
static char *nsp_gtkbutton_type_short_string(NspObject *v);
static AttrTab gtkbutton_attrs[];
static NspMethods *gtkbutton_get_methods(void);
/* static int int_gtkbutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkButton_Private */
