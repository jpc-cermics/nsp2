/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkModelButton
#define NSP_INC_NspGtkModelButton

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

/* NspGtkModelButton */

#include <nsp/gtk/gtkbutton.h>

/*
 * NspGtkModelButton inherits from GtkButton
 * just change some type attributes 
 */

typedef NspGtkButton NspGtkModelButton ;
typedef NspTypeGtkButton NspTypeGtkModelButton ;

extern int nsp_type_gtkmodelbutton_id;
extern NspTypeGtkModelButton *nsp_type_gtkmodelbutton;

/* type instances for gtkbutton */

NspTypeGtkModelButton *new_type_gtkmodelbutton(type_mode mode);

/* instance for NspGtkModelButton */

NspGtkModelButton *new_gtkmodelbutton();

/*
 * Object methods redefined for gtkmodelbutton 
 */

#define NULLGTKMODELBUTTON (NspGtkModelButton*) 0


/* from NspGtkModelButtonObj.c */

extern NspGtkModelButton *nsp_gtkmodelbutton_object (NspObject *O);
extern int IsGtkModelButtonObj (Stack stack, int i);
extern int IsGtkModelButton(NspObject *O);
extern NspGtkModelButton *GetGtkModelButtonCopy (Stack stack, int i);
extern NspGtkModelButton *GetGtkModelButton (Stack stack, int i);

#endif /* NSP_INC_NspGtkModelButton */ 

#ifdef NspGtkModelButton_Private 
static int init_gtkmodelbutton(NspGtkModelButton *o,NspTypeGtkModelButton *type);
static char *nsp_gtkmodelbutton_type_as_string(void);
static char *nsp_gtkmodelbutton_type_short_string(NspObject *v);
static AttrTab gtkmodelbutton_attrs[];
static NspMethods *gtkmodelbutton_get_methods(void);
/* static int int_gtkmodelbutton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkModelButton_Private */
