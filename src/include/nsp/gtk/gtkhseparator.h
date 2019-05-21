/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkHSeparator
#define NSP_INC_NspGtkHSeparator

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

/* NspGtkHSeparator */

#include <nsp/gtk/gtkseparator.h>

/*
 * NspGtkHSeparator inherits from GtkSeparator
 * just change some type attributes 
 */

typedef NspGtkSeparator NspGtkHSeparator ;
typedef NspTypeGtkSeparator NspTypeGtkHSeparator ;

extern int nsp_type_gtkhseparator_id;
extern NspTypeGtkHSeparator *nsp_type_gtkhseparator;

/* type instances for gtkseparator */

NspTypeGtkHSeparator *new_type_gtkhseparator(type_mode mode);

/* instance for NspGtkHSeparator */

NspGtkHSeparator *new_gtkhseparator();

/*
 * Object methods redefined for gtkhseparator 
 */

#define NULLGTKHSEPARATOR (NspGtkHSeparator*) 0


/* from NspGtkHSeparatorObj.c */

extern NspGtkHSeparator *nsp_gtkhseparator_object (NspObject *O);
extern int IsGtkHSeparatorObj (Stack stack, int i);
extern int IsGtkHSeparator(NspObject *O);
extern NspGtkHSeparator *GetGtkHSeparatorCopy (Stack stack, int i);
extern NspGtkHSeparator *GetGtkHSeparator (Stack stack, int i);

#endif /* NSP_INC_NspGtkHSeparator */ 

#ifdef NspGtkHSeparator_Private 
static int init_gtkhseparator(NspGtkHSeparator *o,NspTypeGtkHSeparator *type);
static char *nsp_gtkhseparator_type_as_string(void);
static char *nsp_gtkhseparator_type_short_string(NspObject *v);
static AttrTab gtkhseparator_attrs[];
static NspMethods *gtkhseparator_get_methods(void);
/* static int int_gtkhseparator_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkHSeparator_Private */
