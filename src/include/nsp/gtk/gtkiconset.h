/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkIconSet
#define NSP_INC_NspGtkIconSet

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

/* NspGtkIconSet */

#include <nsp/gtk/gboxed.h>

/*
 * NspGtkIconSet inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGtkIconSet ;
typedef NspTypeGBoxed NspTypeGtkIconSet ;

extern int nsp_type_gtkiconset_id;
extern NspTypeGtkIconSet *nsp_type_gtkiconset;

/* type instances for gboxed */

NspTypeGtkIconSet *new_type_gtkiconset(type_mode mode);

/* instance for NspGtkIconSet */

NspGtkIconSet *new_gtkiconset();

/*
 * Object methods redefined for gtkiconset 
 */

#define NULLGTKICONSET (NspGtkIconSet*) 0


/* from NspGtkIconSetObj.c */

extern NspGtkIconSet *nsp_gtkiconset_object (NspObject *O);
extern int IsGtkIconSetObj (Stack stack, int i);
extern int IsGtkIconSet(NspObject *O);
extern NspGtkIconSet *GetGtkIconSetCopy (Stack stack, int i);
extern NspGtkIconSet *GetGtkIconSet (Stack stack, int i);

#endif /* NSP_INC_NspGtkIconSet */ 

#ifdef NspGtkIconSet_Private 
static int init_gtkiconset(NspGtkIconSet *o,NspTypeGtkIconSet *type);
static char *nsp_gtkiconset_type_as_string(void);
static char *nsp_gtkiconset_type_short_string(NspObject *v);
static AttrTab gtkiconset_attrs[];
static NspMethods *gtkiconset_get_methods(void);
/* static int int_gtkiconset_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkIconSet_Private */
