/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGAction
#define NSP_INC_NspGAction

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

/* NspGAction */

#include <nsp/gtk/gobject.h>

/*
 * NspGAction inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGAction ;
typedef NspTypeGObject NspTypeGAction ;

extern int nsp_type_gaction_id;
extern NspTypeGAction *nsp_type_gaction;

/* type instances for gobject */

NspTypeGAction *new_type_gaction(type_mode mode);

/* instance for NspGAction */

NspGAction *new_gaction();

/*
 * Object methods redefined for gaction 
 */

#define NULLGACTION (NspGAction*) 0


/* from NspGActionObj.c */

extern NspGAction *nsp_gaction_object (NspObject *O);
extern int IsGActionObj (Stack stack, int i);
extern int IsGAction(NspObject *O);
extern NspGAction *GetGActionCopy (Stack stack, int i);
extern NspGAction *GetGAction (Stack stack, int i);

#endif /* NSP_INC_NspGAction */ 

#ifdef NspGAction_Private 
static int init_gaction(NspGAction *o,NspTypeGAction *type);
static char *nsp_gaction_type_as_string(void);
static char *nsp_gaction_type_short_string(NspObject *v);
static AttrTab gaction_attrs[];
static NspMethods *gaction_get_methods(void);
/* static int int_gaction_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGAction_Private */
