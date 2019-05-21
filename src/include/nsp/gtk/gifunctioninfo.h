/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGIFunctionInfo
#define NSP_INC_NspGIFunctionInfo

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

/* NspGIFunctionInfo */

#include <nsp/gtk/gboxed.h>

/*
 * NspGIFunctionInfo inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGIFunctionInfo ;
typedef NspTypeGBoxed NspTypeGIFunctionInfo ;

extern int nsp_type_gifunctioninfo_id;
extern NspTypeGIFunctionInfo *nsp_type_gifunctioninfo;

/* type instances for gboxed */

NspTypeGIFunctionInfo *new_type_gifunctioninfo(type_mode mode);

/* instance for NspGIFunctionInfo */

NspGIFunctionInfo *new_gifunctioninfo();

/*
 * Object methods redefined for gifunctioninfo 
 */

#define NULLGIFUNCTIONINFO (NspGIFunctionInfo*) 0


/* from NspGIFunctionInfoObj.c */

extern NspGIFunctionInfo *nsp_gifunctioninfo_object (NspObject *O);
extern int IsGIFunctionInfoObj (Stack stack, int i);
extern int IsGIFunctionInfo(NspObject *O);
extern NspGIFunctionInfo *GetGIFunctionInfoCopy (Stack stack, int i);
extern NspGIFunctionInfo *GetGIFunctionInfo (Stack stack, int i);

#endif /* NSP_INC_NspGIFunctionInfo */ 

#ifdef NspGIFunctionInfo_Private 
static int init_gifunctioninfo(NspGIFunctionInfo *o,NspTypeGIFunctionInfo *type);
static char *nsp_gifunctioninfo_type_as_string(void);
static char *nsp_gifunctioninfo_type_short_string(NspObject *v);
static AttrTab gifunctioninfo_attrs[];
static NspMethods *gifunctioninfo_get_methods(void);
/* static int int_gifunctioninfo_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGIFunctionInfo_Private */
