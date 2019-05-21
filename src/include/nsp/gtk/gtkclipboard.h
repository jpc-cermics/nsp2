/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkClipboard
#define NSP_INC_NspGtkClipboard

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

/* NspGtkClipboard */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkClipboard inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkClipboard ;
typedef NspTypeGObject NspTypeGtkClipboard ;

extern int nsp_type_gtkclipboard_id;
extern NspTypeGtkClipboard *nsp_type_gtkclipboard;

/* type instances for gobject */

NspTypeGtkClipboard *new_type_gtkclipboard(type_mode mode);

/* instance for NspGtkClipboard */

NspGtkClipboard *new_gtkclipboard();

/*
 * Object methods redefined for gtkclipboard 
 */

#define NULLGTKCLIPBOARD (NspGtkClipboard*) 0


/* from NspGtkClipboardObj.c */

extern NspGtkClipboard *nsp_gtkclipboard_object (NspObject *O);
extern int IsGtkClipboardObj (Stack stack, int i);
extern int IsGtkClipboard(NspObject *O);
extern NspGtkClipboard *GetGtkClipboardCopy (Stack stack, int i);
extern NspGtkClipboard *GetGtkClipboard (Stack stack, int i);

#endif /* NSP_INC_NspGtkClipboard */ 

#ifdef NspGtkClipboard_Private 
static int init_gtkclipboard(NspGtkClipboard *o,NspTypeGtkClipboard *type);
static char *nsp_gtkclipboard_type_as_string(void);
static char *nsp_gtkclipboard_type_short_string(NspObject *v);
static AttrTab gtkclipboard_attrs[];
static NspMethods *gtkclipboard_get_methods(void);
/* static int int_gtkclipboard_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkClipboard_Private */
