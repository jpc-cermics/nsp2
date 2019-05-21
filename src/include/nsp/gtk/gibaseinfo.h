/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGIBaseInfo
#define NSP_INC_NspGIBaseInfo

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

/* NspGIBaseInfo */

#include <nsp/gtk/gboxed.h>

/*
 * NspGIBaseInfo inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGIBaseInfo ;
typedef NspTypeGBoxed NspTypeGIBaseInfo ;

extern int nsp_type_gibaseinfo_id;
extern NspTypeGIBaseInfo *nsp_type_gibaseinfo;

/* type instances for gboxed */

NspTypeGIBaseInfo *new_type_gibaseinfo(type_mode mode);

/* instance for NspGIBaseInfo */

NspGIBaseInfo *new_gibaseinfo();

/*
 * Object methods redefined for gibaseinfo 
 */

#define NULLGIBASEINFO (NspGIBaseInfo*) 0


/* from NspGIBaseInfoObj.c */

extern NspGIBaseInfo *nsp_gibaseinfo_object (NspObject *O);
extern int IsGIBaseInfoObj (Stack stack, int i);
extern int IsGIBaseInfo(NspObject *O);
extern NspGIBaseInfo *GetGIBaseInfoCopy (Stack stack, int i);
extern NspGIBaseInfo *GetGIBaseInfo (Stack stack, int i);

#endif /* NSP_INC_NspGIBaseInfo */ 

#ifdef NspGIBaseInfo_Private 
static int init_gibaseinfo(NspGIBaseInfo *o,NspTypeGIBaseInfo *type);
static char *nsp_gibaseinfo_type_as_string(void);
static char *nsp_gibaseinfo_type_short_string(NspObject *v);
static AttrTab gibaseinfo_attrs[];
static NspMethods *gibaseinfo_get_methods(void);
/* static int int_gibaseinfo_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGIBaseInfo_Private */
