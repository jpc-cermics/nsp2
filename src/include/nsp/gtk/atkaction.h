/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkAction
#define NSP_INC_NspAtkAction

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

/* NspAtkAction */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkAction inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkAction ;
typedef NspTypeGObject NspTypeAtkAction ;

extern int nsp_type_atkaction_id;
extern NspTypeAtkAction *nsp_type_atkaction;

/* type instances for gobject */

NspTypeAtkAction *new_type_atkaction(type_mode mode);

/* instance for NspAtkAction */

NspAtkAction *new_atkaction();

/*
 * Object methods redefined for atkaction 
 */

#define NULLATKACTION (NspAtkAction*) 0


/* from NspAtkActionObj.c */

extern NspAtkAction *nsp_atkaction_object (NspObject *O);
extern int IsAtkActionObj (Stack stack, int i);
extern int IsAtkAction(NspObject *O);
extern NspAtkAction *GetAtkActionCopy (Stack stack, int i);
extern NspAtkAction *GetAtkAction (Stack stack, int i);

#endif /* NSP_INC_NspAtkAction */ 

#ifdef NspAtkAction_Private 
static int init_atkaction(NspAtkAction *o,NspTypeAtkAction *type);
static char *nsp_atkaction_type_as_string(void);
static char *nsp_atkaction_type_short_string(NspObject *v);
static AttrTab atkaction_attrs[];
static NspMethods *atkaction_get_methods(void);
/* static int int_atkaction_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkAction_Private */
