/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkUtil
#define NSP_INC_NspAtkUtil

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

/* NspAtkUtil */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkUtil inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkUtil ;
typedef NspTypeGObject NspTypeAtkUtil ;

extern int nsp_type_atkutil_id;
extern NspTypeAtkUtil *nsp_type_atkutil;

/* type instances for gobject */

NspTypeAtkUtil *new_type_atkutil(type_mode mode);

/* instance for NspAtkUtil */

NspAtkUtil *new_atkutil();

/*
 * Object methods redefined for atkutil 
 */

#define NULLATKUTIL (NspAtkUtil*) 0


/* from NspAtkUtilObj.c */

extern NspAtkUtil *nsp_atkutil_object (NspObject *O);
extern int IsAtkUtilObj (Stack stack, int i);
extern int IsAtkUtil(NspObject *O);
extern NspAtkUtil *GetAtkUtilCopy (Stack stack, int i);
extern NspAtkUtil *GetAtkUtil (Stack stack, int i);

#endif /* NSP_INC_NspAtkUtil */ 

#ifdef NspAtkUtil_Private 
static int init_atkutil(NspAtkUtil *o,NspTypeAtkUtil *type);
static char *nsp_atkutil_type_as_string(void);
static char *nsp_atkutil_type_short_string(NspObject *v);
static AttrTab atkutil_attrs[];
static NspMethods *atkutil_get_methods(void);
/* static int int_atkutil_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkUtil_Private */
