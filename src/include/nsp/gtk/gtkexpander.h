/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkExpander
#define NSP_INC_NspGtkExpander

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

/* NspGtkExpander */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkExpander inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkExpander ;
typedef NspTypeGtkBin NspTypeGtkExpander ;

extern int nsp_type_gtkexpander_id;
extern NspTypeGtkExpander *nsp_type_gtkexpander;

/* type instances for gtkbin */

NspTypeGtkExpander *new_type_gtkexpander(type_mode mode);

/* instance for NspGtkExpander */

NspGtkExpander *new_gtkexpander();

/*
 * Object methods redefined for gtkexpander 
 */

#define NULLGTKEXPANDER (NspGtkExpander*) 0


/* from NspGtkExpanderObj.c */

extern NspGtkExpander *nsp_gtkexpander_object (NspObject *O);
extern int IsGtkExpanderObj (Stack stack, int i);
extern int IsGtkExpander(NspObject *O);
extern NspGtkExpander *GetGtkExpanderCopy (Stack stack, int i);
extern NspGtkExpander *GetGtkExpander (Stack stack, int i);

#endif /* NSP_INC_NspGtkExpander */ 

#ifdef NspGtkExpander_Private 
static int init_gtkexpander(NspGtkExpander *o,NspTypeGtkExpander *type);
static char *nsp_gtkexpander_type_as_string(void);
static char *nsp_gtkexpander_type_short_string(NspObject *v);
static AttrTab gtkexpander_attrs[];
static NspMethods *gtkexpander_get_methods(void);
/* static int int_gtkexpander_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkExpander_Private */
