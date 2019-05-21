/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTable
#define NSP_INC_NspGtkTable

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

/* NspGtkTable */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkTable inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkTable ;
typedef NspTypeGtkContainer NspTypeGtkTable ;

extern int nsp_type_gtktable_id;
extern NspTypeGtkTable *nsp_type_gtktable;

/* type instances for gtkcontainer */

NspTypeGtkTable *new_type_gtktable(type_mode mode);

/* instance for NspGtkTable */

NspGtkTable *new_gtktable();

/*
 * Object methods redefined for gtktable 
 */

#define NULLGTKTABLE (NspGtkTable*) 0


/* from NspGtkTableObj.c */

extern NspGtkTable *nsp_gtktable_object (NspObject *O);
extern int IsGtkTableObj (Stack stack, int i);
extern int IsGtkTable(NspObject *O);
extern NspGtkTable *GetGtkTableCopy (Stack stack, int i);
extern NspGtkTable *GetGtkTable (Stack stack, int i);

#endif /* NSP_INC_NspGtkTable */ 

#ifdef NspGtkTable_Private 
static int init_gtktable(NspGtkTable *o,NspTypeGtkTable *type);
static char *nsp_gtktable_type_as_string(void);
static char *nsp_gtktable_type_short_string(NspObject *v);
static AttrTab gtktable_attrs[];
static NspMethods *gtktable_get_methods(void);
/* static int int_gtktable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTable_Private */
