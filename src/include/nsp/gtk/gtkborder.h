/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkBorder
#define NSP_INC_NspGtkBorder

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

/* NspGtkBorder */

#include <nsp/gtk/gboxed.h>

/*
 * NspGtkBorder inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGtkBorder ;
typedef NspTypeGBoxed NspTypeGtkBorder ;

extern int nsp_type_gtkborder_id;
extern NspTypeGtkBorder *nsp_type_gtkborder;

/* type instances for gboxed */

NspTypeGtkBorder *new_type_gtkborder(type_mode mode);

/* instance for NspGtkBorder */

NspGtkBorder *new_gtkborder();

/*
 * Object methods redefined for gtkborder 
 */

#define NULLGTKBORDER (NspGtkBorder*) 0


/* from NspGtkBorderObj.c */

extern NspGtkBorder *nsp_gtkborder_object (NspObject *O);
extern int IsGtkBorderObj (Stack stack, int i);
extern int IsGtkBorder(NspObject *O);
extern NspGtkBorder *GetGtkBorderCopy (Stack stack, int i);
extern NspGtkBorder *GetGtkBorder (Stack stack, int i);

#endif /* NSP_INC_NspGtkBorder */ 

#ifdef NspGtkBorder_Private 
static int init_gtkborder(NspGtkBorder *o,NspTypeGtkBorder *type);
static char *nsp_gtkborder_type_as_string(void);
static char *nsp_gtkborder_type_short_string(NspObject *v);
static AttrTab gtkborder_attrs[];
static NspMethods *gtkborder_get_methods(void);
/* static int int_gtkborder_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkBorder_Private */
