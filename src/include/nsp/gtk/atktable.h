/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkTable
#define NSP_INC_NspAtkTable

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

/* NspAtkTable */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkTable inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkTable ;
typedef NspTypeGObject NspTypeAtkTable ;

extern int nsp_type_atktable_id;
extern NspTypeAtkTable *nsp_type_atktable;

/* type instances for gobject */

NspTypeAtkTable *new_type_atktable(type_mode mode);

/* instance for NspAtkTable */

NspAtkTable *new_atktable();

/*
 * Object methods redefined for atktable 
 */

#define NULLATKTABLE (NspAtkTable*) 0


/* from NspAtkTableObj.c */

extern NspAtkTable *nsp_atktable_object (NspObject *O);
extern int IsAtkTableObj (Stack stack, int i);
extern int IsAtkTable(NspObject *O);
extern NspAtkTable *GetAtkTableCopy (Stack stack, int i);
extern NspAtkTable *GetAtkTable (Stack stack, int i);

#endif /* NSP_INC_NspAtkTable */ 

#ifdef NspAtkTable_Private 
static int init_atktable(NspAtkTable *o,NspTypeAtkTable *type);
static char *nsp_atktable_type_as_string(void);
static char *nsp_atktable_type_short_string(NspObject *v);
static AttrTab atktable_attrs[];
static NspMethods *atktable_get_methods(void);
/* static int int_atktable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkTable_Private */
