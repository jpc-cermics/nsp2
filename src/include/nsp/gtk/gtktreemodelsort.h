/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTreeModelSort
#define NSP_INC_NspGtkTreeModelSort

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

/* NspGtkTreeModelSort */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkTreeModelSort inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkTreeModelSort ;
typedef NspTypeGObject NspTypeGtkTreeModelSort ;

extern int nsp_type_gtktreemodelsort_id;
extern NspTypeGtkTreeModelSort *nsp_type_gtktreemodelsort;

/* type instances for gobject */

NspTypeGtkTreeModelSort *new_type_gtktreemodelsort(type_mode mode);

/* instance for NspGtkTreeModelSort */

NspGtkTreeModelSort *new_gtktreemodelsort();

/*
 * Object methods redefined for gtktreemodelsort 
 */

#define NULLGTKTREEMODELSORT (NspGtkTreeModelSort*) 0


/* from NspGtkTreeModelSortObj.c */

extern NspGtkTreeModelSort *nsp_gtktreemodelsort_object (NspObject *O);
extern int IsGtkTreeModelSortObj (Stack stack, int i);
extern int IsGtkTreeModelSort(NspObject *O);
extern NspGtkTreeModelSort *GetGtkTreeModelSortCopy (Stack stack, int i);
extern NspGtkTreeModelSort *GetGtkTreeModelSort (Stack stack, int i);

#endif /* NSP_INC_NspGtkTreeModelSort */ 

#ifdef NspGtkTreeModelSort_Private 
static int init_gtktreemodelsort(NspGtkTreeModelSort *o,NspTypeGtkTreeModelSort *type);
static char *nsp_gtktreemodelsort_type_as_string(void);
static char *nsp_gtktreemodelsort_type_short_string(NspObject *v);
static AttrTab gtktreemodelsort_attrs[];
static NspMethods *gtktreemodelsort_get_methods(void);
/* static int int_gtktreemodelsort_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTreeModelSort_Private */
