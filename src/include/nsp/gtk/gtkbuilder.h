/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkBuilder
#define NSP_INC_NspGtkBuilder

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

/* NspGtkBuilder */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkBuilder inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkBuilder ;
typedef NspTypeGObject NspTypeGtkBuilder ;

extern int nsp_type_gtkbuilder_id;
extern NspTypeGtkBuilder *nsp_type_gtkbuilder;

/* type instances for gobject */

NspTypeGtkBuilder *new_type_gtkbuilder(type_mode mode);

/* instance for NspGtkBuilder */

NspGtkBuilder *new_gtkbuilder();

/*
 * Object methods redefined for gtkbuilder 
 */

#define NULLGTKBUILDER (NspGtkBuilder*) 0


/* from NspGtkBuilderObj.c */

extern NspGtkBuilder *nsp_gtkbuilder_object (NspObject *O);
extern int IsGtkBuilderObj (Stack stack, int i);
extern int IsGtkBuilder(NspObject *O);
extern NspGtkBuilder *GetGtkBuilderCopy (Stack stack, int i);
extern NspGtkBuilder *GetGtkBuilder (Stack stack, int i);

#endif /* NSP_INC_NspGtkBuilder */ 

#ifdef NspGtkBuilder_Private 
static int init_gtkbuilder(NspGtkBuilder *o,NspTypeGtkBuilder *type);
static char *nsp_gtkbuilder_type_as_string(void);
static char *nsp_gtkbuilder_type_short_string(NspObject *v);
static AttrTab gtkbuilder_attrs[];
static NspMethods *gtkbuilder_get_methods(void);
/* static int int_gtkbuilder_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkBuilder_Private */
