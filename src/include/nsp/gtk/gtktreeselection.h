/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTreeSelection
#define NSP_INC_NspGtkTreeSelection

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

/* NspGtkTreeSelection */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkTreeSelection inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkTreeSelection ;
typedef NspTypeGObject NspTypeGtkTreeSelection ;

extern int nsp_type_gtktreeselection_id;
extern NspTypeGtkTreeSelection *nsp_type_gtktreeselection;

/* type instances for gobject */

NspTypeGtkTreeSelection *new_type_gtktreeselection(type_mode mode);

/* instance for NspGtkTreeSelection */

NspGtkTreeSelection *new_gtktreeselection();

/*
 * Object methods redefined for gtktreeselection 
 */

#define NULLGTKTREESELECTION (NspGtkTreeSelection*) 0


/* from NspGtkTreeSelectionObj.c */

extern NspGtkTreeSelection *nsp_gtktreeselection_object (NspObject *O);
extern int IsGtkTreeSelectionObj (Stack stack, int i);
extern int IsGtkTreeSelection(NspObject *O);
extern NspGtkTreeSelection *GetGtkTreeSelectionCopy (Stack stack, int i);
extern NspGtkTreeSelection *GetGtkTreeSelection (Stack stack, int i);

#endif /* NSP_INC_NspGtkTreeSelection */ 

#ifdef NspGtkTreeSelection_Private 
static int init_gtktreeselection(NspGtkTreeSelection *o,NspTypeGtkTreeSelection *type);
static char *nsp_gtktreeselection_type_as_string(void);
static char *nsp_gtktreeselection_type_short_string(NspObject *v);
static AttrTab gtktreeselection_attrs[];
static NspMethods *gtktreeselection_get_methods(void);
/* static int int_gtktreeselection_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTreeSelection_Private */
