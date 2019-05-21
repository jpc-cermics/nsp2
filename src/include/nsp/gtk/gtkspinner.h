/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSpinner
#define NSP_INC_NspGtkSpinner

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

/* NspGtkSpinner */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkSpinner inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkSpinner ;
typedef NspTypeGtkWidget NspTypeGtkSpinner ;

extern int nsp_type_gtkspinner_id;
extern NspTypeGtkSpinner *nsp_type_gtkspinner;

/* type instances for gtkwidget */

NspTypeGtkSpinner *new_type_gtkspinner(type_mode mode);

/* instance for NspGtkSpinner */

NspGtkSpinner *new_gtkspinner();

/*
 * Object methods redefined for gtkspinner 
 */

#define NULLGTKSPINNER (NspGtkSpinner*) 0


/* from NspGtkSpinnerObj.c */

extern NspGtkSpinner *nsp_gtkspinner_object (NspObject *O);
extern int IsGtkSpinnerObj (Stack stack, int i);
extern int IsGtkSpinner(NspObject *O);
extern NspGtkSpinner *GetGtkSpinnerCopy (Stack stack, int i);
extern NspGtkSpinner *GetGtkSpinner (Stack stack, int i);

#endif /* NSP_INC_NspGtkSpinner */ 

#ifdef NspGtkSpinner_Private 
static int init_gtkspinner(NspGtkSpinner *o,NspTypeGtkSpinner *type);
static char *nsp_gtkspinner_type_as_string(void);
static char *nsp_gtkspinner_type_short_string(NspObject *v);
static AttrTab gtkspinner_attrs[];
static NspMethods *gtkspinner_get_methods(void);
/* static int int_gtkspinner_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSpinner_Private */
