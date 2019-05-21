/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkRecentChooser
#define NSP_INC_NspGtkRecentChooser

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

/* NspGtkRecentChooser */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkRecentChooser inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkRecentChooser ;
typedef NspTypeGObject NspTypeGtkRecentChooser ;

extern int nsp_type_gtkrecentchooser_id;
extern NspTypeGtkRecentChooser *nsp_type_gtkrecentchooser;

/* type instances for gobject */

NspTypeGtkRecentChooser *new_type_gtkrecentchooser(type_mode mode);

/* instance for NspGtkRecentChooser */

NspGtkRecentChooser *new_gtkrecentchooser();

/*
 * Object methods redefined for gtkrecentchooser 
 */

#define NULLGTKRECENTCHOOSER (NspGtkRecentChooser*) 0


/* from NspGtkRecentChooserObj.c */

extern NspGtkRecentChooser *nsp_gtkrecentchooser_object (NspObject *O);
extern int IsGtkRecentChooserObj (Stack stack, int i);
extern int IsGtkRecentChooser(NspObject *O);
extern NspGtkRecentChooser *GetGtkRecentChooserCopy (Stack stack, int i);
extern NspGtkRecentChooser *GetGtkRecentChooser (Stack stack, int i);

#endif /* NSP_INC_NspGtkRecentChooser */ 

#ifdef NspGtkRecentChooser_Private 
static int init_gtkrecentchooser(NspGtkRecentChooser *o,NspTypeGtkRecentChooser *type);
static char *nsp_gtkrecentchooser_type_as_string(void);
static char *nsp_gtkrecentchooser_type_short_string(NspObject *v);
static AttrTab gtkrecentchooser_attrs[];
static NspMethods *gtkrecentchooser_get_methods(void);
/* static int int_gtkrecentchooser_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkRecentChooser_Private */
