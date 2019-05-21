/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPangoTabArray
#define NSP_INC_NspPangoTabArray

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

/* NspPangoTabArray */

#include <nsp/gtk/gboxed.h>

/*
 * NspPangoTabArray inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspPangoTabArray ;
typedef NspTypeGBoxed NspTypePangoTabArray ;

extern int nsp_type_pangotabarray_id;
extern NspTypePangoTabArray *nsp_type_pangotabarray;

/* type instances for gboxed */

NspTypePangoTabArray *new_type_pangotabarray(type_mode mode);

/* instance for NspPangoTabArray */

NspPangoTabArray *new_pangotabarray();

/*
 * Object methods redefined for pangotabarray 
 */

#define NULLPANGOTABARRAY (NspPangoTabArray*) 0


/* from NspPangoTabArrayObj.c */

extern NspPangoTabArray *nsp_pangotabarray_object (NspObject *O);
extern int IsPangoTabArrayObj (Stack stack, int i);
extern int IsPangoTabArray(NspObject *O);
extern NspPangoTabArray *GetPangoTabArrayCopy (Stack stack, int i);
extern NspPangoTabArray *GetPangoTabArray (Stack stack, int i);

#endif /* NSP_INC_NspPangoTabArray */ 

#ifdef NspPangoTabArray_Private 
static int init_pangotabarray(NspPangoTabArray *o,NspTypePangoTabArray *type);
static char *nsp_pangotabarray_type_as_string(void);
static char *nsp_pangotabarray_type_short_string(NspObject *v);
static AttrTab pangotabarray_attrs[];
static NspMethods *pangotabarray_get_methods(void);
/* static int int_pangotabarray_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspPangoTabArray_Private */
