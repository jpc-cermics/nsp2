/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceUndoManagerDefault
#define NSP_INC_NspGtkSourceUndoManagerDefault

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

/* NspGtkSourceUndoManagerDefault */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceUndoManagerDefault inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceUndoManagerDefault ;
typedef NspTypeGObject NspTypeGtkSourceUndoManagerDefault ;

extern int nsp_type_gtksourceundomanagerdefault_id;
extern NspTypeGtkSourceUndoManagerDefault *nsp_type_gtksourceundomanagerdefault;

/* type instances for gobject */

NspTypeGtkSourceUndoManagerDefault *new_type_gtksourceundomanagerdefault(type_mode mode);

/* instance for NspGtkSourceUndoManagerDefault */

NspGtkSourceUndoManagerDefault *new_gtksourceundomanagerdefault();

/*
 * Object methods redefined for gtksourceundomanagerdefault 
 */

#define NULLGTKSOURCEUNDOMANAGERDEFAULT (NspGtkSourceUndoManagerDefault*) 0


/* from NspGtkSourceUndoManagerDefaultObj.c */

extern NspGtkSourceUndoManagerDefault *nsp_gtksourceundomanagerdefault_object (NspObject *O);
extern int IsGtkSourceUndoManagerDefaultObj (Stack stack, int i);
extern int IsGtkSourceUndoManagerDefault(NspObject *O);
extern NspGtkSourceUndoManagerDefault *GetGtkSourceUndoManagerDefaultCopy (Stack stack, int i);
extern NspGtkSourceUndoManagerDefault *GetGtkSourceUndoManagerDefault (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceUndoManagerDefault */ 

#ifdef NspGtkSourceUndoManagerDefault_Private 
static int init_gtksourceundomanagerdefault(NspGtkSourceUndoManagerDefault *o,NspTypeGtkSourceUndoManagerDefault *type);
static char *nsp_gtksourceundomanagerdefault_type_as_string(void);
static char *nsp_gtksourceundomanagerdefault_type_short_string(NspObject *v);
static AttrTab gtksourceundomanagerdefault_attrs[];
static NspMethods *gtksourceundomanagerdefault_get_methods(void);
/* static int int_gtksourceundomanagerdefault_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceUndoManagerDefault_Private */
