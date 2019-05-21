/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkAction
#define NSP_INC_NspGtkAction

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

/* NspGtkAction */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkAction inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkAction ;
typedef NspTypeGObject NspTypeGtkAction ;

extern int nsp_type_gtkaction_id;
extern NspTypeGtkAction *nsp_type_gtkaction;

/* type instances for gobject */

NspTypeGtkAction *new_type_gtkaction(type_mode mode);

/* instance for NspGtkAction */

NspGtkAction *new_gtkaction();

/*
 * Object methods redefined for gtkaction 
 */

#define NULLGTKACTION (NspGtkAction*) 0


/* from NspGtkActionObj.c */

extern NspGtkAction *nsp_gtkaction_object (NspObject *O);
extern int IsGtkActionObj (Stack stack, int i);
extern int IsGtkAction(NspObject *O);
extern NspGtkAction *GetGtkActionCopy (Stack stack, int i);
extern NspGtkAction *GetGtkAction (Stack stack, int i);

#endif /* NSP_INC_NspGtkAction */ 

#ifdef NspGtkAction_Private 
static int init_gtkaction(NspGtkAction *o,NspTypeGtkAction *type);
static char *nsp_gtkaction_type_as_string(void);
static char *nsp_gtkaction_type_short_string(NspObject *v);
static AttrTab gtkaction_attrs[];
static NspMethods *gtkaction_get_methods(void);
/* static int int_gtkaction_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkAction_Private */
