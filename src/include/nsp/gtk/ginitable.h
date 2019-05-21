/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGInitable
#define NSP_INC_NspGInitable

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

/* NspGInitable */

#include <nsp/gtk/gobject.h>

/*
 * NspGInitable inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGInitable ;
typedef NspTypeGObject NspTypeGInitable ;

extern int nsp_type_ginitable_id;
extern NspTypeGInitable *nsp_type_ginitable;

/* type instances for gobject */

NspTypeGInitable *new_type_ginitable(type_mode mode);

/* instance for NspGInitable */

NspGInitable *new_ginitable();

/*
 * Object methods redefined for ginitable 
 */

#define NULLGINITABLE (NspGInitable*) 0


/* from NspGInitableObj.c */

extern NspGInitable *nsp_ginitable_object (NspObject *O);
extern int IsGInitableObj (Stack stack, int i);
extern int IsGInitable(NspObject *O);
extern NspGInitable *GetGInitableCopy (Stack stack, int i);
extern NspGInitable *GetGInitable (Stack stack, int i);

#endif /* NSP_INC_NspGInitable */ 

#ifdef NspGInitable_Private 
static int init_ginitable(NspGInitable *o,NspTypeGInitable *type);
static char *nsp_ginitable_type_as_string(void);
static char *nsp_ginitable_type_short_string(NspObject *v);
static AttrTab ginitable_attrs[];
static NspMethods *ginitable_get_methods(void);
/* static int int_ginitable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGInitable_Private */
