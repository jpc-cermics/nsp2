/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTreeViewColumn
#define NSP_INC_NspGtkTreeViewColumn

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

/* NspGtkTreeViewColumn */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkTreeViewColumn inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkTreeViewColumn ;
typedef NspTypeGObject NspTypeGtkTreeViewColumn ;

extern int nsp_type_gtktreeviewcolumn_id;
extern NspTypeGtkTreeViewColumn *nsp_type_gtktreeviewcolumn;

/* type instances for gobject */

NspTypeGtkTreeViewColumn *new_type_gtktreeviewcolumn(type_mode mode);

/* instance for NspGtkTreeViewColumn */

NspGtkTreeViewColumn *new_gtktreeviewcolumn();

/*
 * Object methods redefined for gtktreeviewcolumn 
 */

#define NULLGTKTREEVIEWCOLUMN (NspGtkTreeViewColumn*) 0


/* from NspGtkTreeViewColumnObj.c */

extern NspGtkTreeViewColumn *nsp_gtktreeviewcolumn_object (NspObject *O);
extern int IsGtkTreeViewColumnObj (Stack stack, int i);
extern int IsGtkTreeViewColumn(NspObject *O);
extern NspGtkTreeViewColumn *GetGtkTreeViewColumnCopy (Stack stack, int i);
extern NspGtkTreeViewColumn *GetGtkTreeViewColumn (Stack stack, int i);

#endif /* NSP_INC_NspGtkTreeViewColumn */ 

#ifdef NspGtkTreeViewColumn_Private 
static int init_gtktreeviewcolumn(NspGtkTreeViewColumn *o,NspTypeGtkTreeViewColumn *type);
static char *nsp_gtktreeviewcolumn_type_as_string(void);
static char *nsp_gtktreeviewcolumn_type_short_string(NspObject *v);
static AttrTab gtktreeviewcolumn_attrs[];
static NspMethods *gtktreeviewcolumn_get_methods(void);
/* static int int_gtktreeviewcolumn_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTreeViewColumn_Private */
