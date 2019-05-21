/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkVSeparator
#define NSP_INC_NspGtkVSeparator

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

/* NspGtkVSeparator */

#include <nsp/gtk/gtkseparator.h>

/*
 * NspGtkVSeparator inherits from GtkSeparator
 * just change some type attributes 
 */

typedef NspGtkSeparator NspGtkVSeparator ;
typedef NspTypeGtkSeparator NspTypeGtkVSeparator ;

extern int nsp_type_gtkvseparator_id;
extern NspTypeGtkVSeparator *nsp_type_gtkvseparator;

/* type instances for gtkseparator */

NspTypeGtkVSeparator *new_type_gtkvseparator(type_mode mode);

/* instance for NspGtkVSeparator */

NspGtkVSeparator *new_gtkvseparator();

/*
 * Object methods redefined for gtkvseparator 
 */

#define NULLGTKVSEPARATOR (NspGtkVSeparator*) 0


/* from NspGtkVSeparatorObj.c */

extern NspGtkVSeparator *nsp_gtkvseparator_object (NspObject *O);
extern int IsGtkVSeparatorObj (Stack stack, int i);
extern int IsGtkVSeparator(NspObject *O);
extern NspGtkVSeparator *GetGtkVSeparatorCopy (Stack stack, int i);
extern NspGtkVSeparator *GetGtkVSeparator (Stack stack, int i);

#endif /* NSP_INC_NspGtkVSeparator */ 

#ifdef NspGtkVSeparator_Private 
static int init_gtkvseparator(NspGtkVSeparator *o,NspTypeGtkVSeparator *type);
static char *nsp_gtkvseparator_type_as_string(void);
static char *nsp_gtkvseparator_type_short_string(NspObject *v);
static AttrTab gtkvseparator_attrs[];
static NspMethods *gtkvseparator_get_methods(void);
/* static int int_gtkvseparator_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkVSeparator_Private */
