/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGAppInfo
#define NSP_INC_NspGAppInfo

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

/* NspGAppInfo */

#include <nsp/gtk/gobject.h>

/*
 * NspGAppInfo inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGAppInfo ;
typedef NspTypeGObject NspTypeGAppInfo ;

extern int nsp_type_gappinfo_id;
extern NspTypeGAppInfo *nsp_type_gappinfo;

/* type instances for gobject */

NspTypeGAppInfo *new_type_gappinfo(type_mode mode);

/* instance for NspGAppInfo */

NspGAppInfo *new_gappinfo();

/*
 * Object methods redefined for gappinfo 
 */

#define NULLGAPPINFO (NspGAppInfo*) 0


/* from NspGAppInfoObj.c */

extern NspGAppInfo *nsp_gappinfo_object (NspObject *O);
extern int IsGAppInfoObj (Stack stack, int i);
extern int IsGAppInfo(NspObject *O);
extern NspGAppInfo *GetGAppInfoCopy (Stack stack, int i);
extern NspGAppInfo *GetGAppInfo (Stack stack, int i);

#endif /* NSP_INC_NspGAppInfo */ 

#ifdef NspGAppInfo_Private 
static int init_gappinfo(NspGAppInfo *o,NspTypeGAppInfo *type);
static char *nsp_gappinfo_type_as_string(void);
static char *nsp_gappinfo_type_short_string(NspObject *v);
static AttrTab gappinfo_attrs[];
static NspMethods *gappinfo_get_methods(void);
/* static int int_gappinfo_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGAppInfo_Private */
