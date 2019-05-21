/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkStack
#define NSP_INC_NspGtkStack

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

/* NspGtkStack */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkStack inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkStack ;
typedef NspTypeGtkContainer NspTypeGtkStack ;

extern int nsp_type_gtkstack_id;
extern NspTypeGtkStack *nsp_type_gtkstack;

/* type instances for gtkcontainer */

NspTypeGtkStack *new_type_gtkstack(type_mode mode);

/* instance for NspGtkStack */

NspGtkStack *new_gtkstack();

/*
 * Object methods redefined for gtkstack 
 */

#define NULLGTKSTACK (NspGtkStack*) 0


/* from NspGtkStackObj.c */

extern NspGtkStack *nsp_gtkstack_object (NspObject *O);
extern int IsGtkStackObj (Stack stack, int i);
extern int IsGtkStack(NspObject *O);
extern NspGtkStack *GetGtkStackCopy (Stack stack, int i);
extern NspGtkStack *GetGtkStack (Stack stack, int i);

#endif /* NSP_INC_NspGtkStack */ 

#ifdef NspGtkStack_Private 
static int init_gtkstack(NspGtkStack *o,NspTypeGtkStack *type);
static char *nsp_gtkstack_type_as_string(void);
static char *nsp_gtkstack_type_short_string(NspObject *v);
static AttrTab gtkstack_attrs[];
static NspMethods *gtkstack_get_methods(void);
/* static int int_gtkstack_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkStack_Private */
