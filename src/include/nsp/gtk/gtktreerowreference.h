/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTreeRowReference
#define NSP_INC_NspGtkTreeRowReference

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

/* NspGtkTreeRowReference */

#include <nsp/gtk/gboxed.h>

/*
 * NspGtkTreeRowReference inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGtkTreeRowReference ;
typedef NspTypeGBoxed NspTypeGtkTreeRowReference ;

extern int nsp_type_gtktreerowreference_id;
extern NspTypeGtkTreeRowReference *nsp_type_gtktreerowreference;

/* type instances for gboxed */

NspTypeGtkTreeRowReference *new_type_gtktreerowreference(type_mode mode);

/* instance for NspGtkTreeRowReference */

NspGtkTreeRowReference *new_gtktreerowreference();

/*
 * Object methods redefined for gtktreerowreference 
 */

#define NULLGTKTREEROWREFERENCE (NspGtkTreeRowReference*) 0


/* from NspGtkTreeRowReferenceObj.c */

extern NspGtkTreeRowReference *nsp_gtktreerowreference_object (NspObject *O);
extern int IsGtkTreeRowReferenceObj (Stack stack, int i);
extern int IsGtkTreeRowReference(NspObject *O);
extern NspGtkTreeRowReference *GetGtkTreeRowReferenceCopy (Stack stack, int i);
extern NspGtkTreeRowReference *GetGtkTreeRowReference (Stack stack, int i);

#endif /* NSP_INC_NspGtkTreeRowReference */ 

#ifdef NspGtkTreeRowReference_Private 
static int init_gtktreerowreference(NspGtkTreeRowReference *o,NspTypeGtkTreeRowReference *type);
static char *nsp_gtktreerowreference_type_as_string(void);
static char *nsp_gtktreerowreference_type_short_string(NspObject *v);
static AttrTab gtktreerowreference_attrs[];
static NspMethods *gtktreerowreference_get_methods(void);
/* static int int_gtktreerowreference_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTreeRowReference_Private */
