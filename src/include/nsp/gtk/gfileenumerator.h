/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGFileEnumerator
#define NSP_INC_NspGFileEnumerator

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

/* NspGFileEnumerator */

#include <nsp/gtk/gobject.h>

/*
 * NspGFileEnumerator inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGFileEnumerator ;
typedef NspTypeGObject NspTypeGFileEnumerator ;

extern int nsp_type_gfileenumerator_id;
extern NspTypeGFileEnumerator *nsp_type_gfileenumerator;

/* type instances for gobject */

NspTypeGFileEnumerator *new_type_gfileenumerator(type_mode mode);

/* instance for NspGFileEnumerator */

NspGFileEnumerator *new_gfileenumerator();

/*
 * Object methods redefined for gfileenumerator 
 */

#define NULLGFILEENUMERATOR (NspGFileEnumerator*) 0


/* from NspGFileEnumeratorObj.c */

extern NspGFileEnumerator *nsp_gfileenumerator_object (NspObject *O);
extern int IsGFileEnumeratorObj (Stack stack, int i);
extern int IsGFileEnumerator(NspObject *O);
extern NspGFileEnumerator *GetGFileEnumeratorCopy (Stack stack, int i);
extern NspGFileEnumerator *GetGFileEnumerator (Stack stack, int i);

#endif /* NSP_INC_NspGFileEnumerator */ 

#ifdef NspGFileEnumerator_Private 
static int init_gfileenumerator(NspGFileEnumerator *o,NspTypeGFileEnumerator *type);
static char *nsp_gfileenumerator_type_as_string(void);
static char *nsp_gfileenumerator_type_short_string(NspObject *v);
static AttrTab gfileenumerator_attrs[];
static NspMethods *gfileenumerator_get_methods(void);
/* static int int_gfileenumerator_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGFileEnumerator_Private */
