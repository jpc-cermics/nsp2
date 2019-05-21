/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGInetAddressMask
#define NSP_INC_NspGInetAddressMask

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

/* NspGInetAddressMask */

#include <nsp/gtk/gobject.h>

/*
 * NspGInetAddressMask inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGInetAddressMask ;
typedef NspTypeGObject NspTypeGInetAddressMask ;

extern int nsp_type_ginetaddressmask_id;
extern NspTypeGInetAddressMask *nsp_type_ginetaddressmask;

/* type instances for gobject */

NspTypeGInetAddressMask *new_type_ginetaddressmask(type_mode mode);

/* instance for NspGInetAddressMask */

NspGInetAddressMask *new_ginetaddressmask();

/*
 * Object methods redefined for ginetaddressmask 
 */

#define NULLGINETADDRESSMASK (NspGInetAddressMask*) 0


/* from NspGInetAddressMaskObj.c */

extern NspGInetAddressMask *nsp_ginetaddressmask_object (NspObject *O);
extern int IsGInetAddressMaskObj (Stack stack, int i);
extern int IsGInetAddressMask(NspObject *O);
extern NspGInetAddressMask *GetGInetAddressMaskCopy (Stack stack, int i);
extern NspGInetAddressMask *GetGInetAddressMask (Stack stack, int i);

#endif /* NSP_INC_NspGInetAddressMask */ 

#ifdef NspGInetAddressMask_Private 
static int init_ginetaddressmask(NspGInetAddressMask *o,NspTypeGInetAddressMask *type);
static char *nsp_ginetaddressmask_type_as_string(void);
static char *nsp_ginetaddressmask_type_short_string(NspObject *v);
static AttrTab ginetaddressmask_attrs[];
static NspMethods *ginetaddressmask_get_methods(void);
/* static int int_ginetaddressmask_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGInetAddressMask_Private */
