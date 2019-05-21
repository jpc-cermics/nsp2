/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTreeIter
#define NSP_INC_NspGtkTreeIter

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

/* NspGtkTreeIter */

#include <nsp/gtk/gboxed.h>

/*
 * NspGtkTreeIter inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGtkTreeIter ;
typedef NspTypeGBoxed NspTypeGtkTreeIter ;

extern int nsp_type_gtktreeiter_id;
extern NspTypeGtkTreeIter *nsp_type_gtktreeiter;

/* type instances for gboxed */

NspTypeGtkTreeIter *new_type_gtktreeiter(type_mode mode);

/* instance for NspGtkTreeIter */

NspGtkTreeIter *new_gtktreeiter();

/*
 * Object methods redefined for gtktreeiter 
 */

#define NULLGTKTREEITER (NspGtkTreeIter*) 0


/* from NspGtkTreeIterObj.c */

extern NspGtkTreeIter *nsp_gtktreeiter_object (NspObject *O);
extern int IsGtkTreeIterObj (Stack stack, int i);
extern int IsGtkTreeIter(NspObject *O);
extern NspGtkTreeIter *GetGtkTreeIterCopy (Stack stack, int i);
extern NspGtkTreeIter *GetGtkTreeIter (Stack stack, int i);

#endif /* NSP_INC_NspGtkTreeIter */ 

#ifdef NspGtkTreeIter_Private 
static int init_gtktreeiter(NspGtkTreeIter *o,NspTypeGtkTreeIter *type);
static char *nsp_gtktreeiter_type_as_string(void);
static char *nsp_gtktreeiter_type_short_string(NspObject *v);
static AttrTab gtktreeiter_attrs[];
static NspMethods *gtktreeiter_get_methods(void);
/* static int int_gtktreeiter_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTreeIter_Private */
