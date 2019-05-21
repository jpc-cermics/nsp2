/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGPermission
#define NSP_INC_NspGPermission

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

/* NspGPermission */

#include <nsp/gtk/gobject.h>

/*
 * NspGPermission inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGPermission ;
typedef NspTypeGObject NspTypeGPermission ;

extern int nsp_type_gpermission_id;
extern NspTypeGPermission *nsp_type_gpermission;

/* type instances for gobject */

NspTypeGPermission *new_type_gpermission(type_mode mode);

/* instance for NspGPermission */

NspGPermission *new_gpermission();

/*
 * Object methods redefined for gpermission 
 */

#define NULLGPERMISSION (NspGPermission*) 0


/* from NspGPermissionObj.c */

extern NspGPermission *nsp_gpermission_object (NspObject *O);
extern int IsGPermissionObj (Stack stack, int i);
extern int IsGPermission(NspObject *O);
extern NspGPermission *GetGPermissionCopy (Stack stack, int i);
extern NspGPermission *GetGPermission (Stack stack, int i);

#endif /* NSP_INC_NspGPermission */ 

#ifdef NspGPermission_Private 
static int init_gpermission(NspGPermission *o,NspTypeGPermission *type);
static char *nsp_gpermission_type_as_string(void);
static char *nsp_gpermission_type_short_string(NspObject *v);
static AttrTab gpermission_attrs[];
static NspMethods *gpermission_get_methods(void);
/* static int int_gpermission_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGPermission_Private */
