/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGError
#define NSP_INC_NspGError

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

/* NspGError */

#include <nsp/gtk/gboxed.h>

/*
 * NspGError inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGError ;
typedef NspTypeGBoxed NspTypeGError ;

extern int nsp_type_gerror_id;
extern NspTypeGError *nsp_type_gerror;

/* type instances for gboxed */

NspTypeGError *new_type_gerror(type_mode mode);

/* instance for NspGError */

NspGError *new_gerror();

/*
 * Object methods redefined for gerror 
 */

#define NULLGERROR (NspGError*) 0


/* from NspGErrorObj.c */

extern NspGError *nsp_gerror_object (NspObject *O);
extern int IsGErrorObj (Stack stack, int i);
extern int IsGError(NspObject *O);
extern NspGError *GetGErrorCopy (Stack stack, int i);
extern NspGError *GetGError (Stack stack, int i);

#endif /* NSP_INC_NspGError */ 

#ifdef NspGError_Private 
static int init_gerror(NspGError *o,NspTypeGError *type);
static char *nsp_gerror_type_as_string(void);
static char *nsp_gerror_type_short_string(NspObject *v);
static AttrTab gerror_attrs[];
static NspMethods *gerror_get_methods(void);
/* static int int_gerror_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGError_Private */
