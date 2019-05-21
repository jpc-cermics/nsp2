/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGSimpleAction
#define NSP_INC_NspGSimpleAction

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

/* NspGSimpleAction */

#include <nsp/gtk/gobject.h>

/*
 * NspGSimpleAction inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGSimpleAction ;
typedef NspTypeGObject NspTypeGSimpleAction ;

extern int nsp_type_gsimpleaction_id;
extern NspTypeGSimpleAction *nsp_type_gsimpleaction;

/* type instances for gobject */

NspTypeGSimpleAction *new_type_gsimpleaction(type_mode mode);

/* instance for NspGSimpleAction */

NspGSimpleAction *new_gsimpleaction();

/*
 * Object methods redefined for gsimpleaction 
 */

#define NULLGSIMPLEACTION (NspGSimpleAction*) 0


/* from NspGSimpleActionObj.c */

extern NspGSimpleAction *nsp_gsimpleaction_object (NspObject *O);
extern int IsGSimpleActionObj (Stack stack, int i);
extern int IsGSimpleAction(NspObject *O);
extern NspGSimpleAction *GetGSimpleActionCopy (Stack stack, int i);
extern NspGSimpleAction *GetGSimpleAction (Stack stack, int i);

#endif /* NSP_INC_NspGSimpleAction */ 

#ifdef NspGSimpleAction_Private 
static int init_gsimpleaction(NspGSimpleAction *o,NspTypeGSimpleAction *type);
static char *nsp_gsimpleaction_type_as_string(void);
static char *nsp_gsimpleaction_type_short_string(NspObject *v);
static AttrTab gsimpleaction_attrs[];
static NspMethods *gsimpleaction_get_methods(void);
/* static int int_gsimpleaction_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGSimpleAction_Private */
