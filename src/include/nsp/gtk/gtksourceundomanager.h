/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceUndoManager
#define NSP_INC_NspGtkSourceUndoManager

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

/* NspGtkSourceUndoManager */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceUndoManager inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceUndoManager ;
typedef NspTypeGObject NspTypeGtkSourceUndoManager ;

extern int nsp_type_gtksourceundomanager_id;
extern NspTypeGtkSourceUndoManager *nsp_type_gtksourceundomanager;

/* type instances for gobject */

NspTypeGtkSourceUndoManager *new_type_gtksourceundomanager(type_mode mode);

/* instance for NspGtkSourceUndoManager */

NspGtkSourceUndoManager *new_gtksourceundomanager();

/*
 * Object methods redefined for gtksourceundomanager 
 */

#define NULLGTKSOURCEUNDOMANAGER (NspGtkSourceUndoManager*) 0


/* from NspGtkSourceUndoManagerObj.c */

extern NspGtkSourceUndoManager *nsp_gtksourceundomanager_object (NspObject *O);
extern int IsGtkSourceUndoManagerObj (Stack stack, int i);
extern int IsGtkSourceUndoManager(NspObject *O);
extern NspGtkSourceUndoManager *GetGtkSourceUndoManagerCopy (Stack stack, int i);
extern NspGtkSourceUndoManager *GetGtkSourceUndoManager (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceUndoManager */ 

#ifdef NspGtkSourceUndoManager_Private 
static int init_gtksourceundomanager(NspGtkSourceUndoManager *o,NspTypeGtkSourceUndoManager *type);
static char *nsp_gtksourceundomanager_type_as_string(void);
static char *nsp_gtksourceundomanager_type_short_string(NspObject *v);
static AttrTab gtksourceundomanager_attrs[];
static NspMethods *gtksourceundomanager_get_methods(void);
/* static int int_gtksourceundomanager_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceUndoManager_Private */
