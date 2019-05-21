/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkIconInfo
#define NSP_INC_NspGtkIconInfo

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

/* NspGtkIconInfo */

#include <nsp/gtk/gboxed.h>

/*
 * NspGtkIconInfo inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGtkIconInfo ;
typedef NspTypeGBoxed NspTypeGtkIconInfo ;

extern int nsp_type_gtkiconinfo_id;
extern NspTypeGtkIconInfo *nsp_type_gtkiconinfo;

/* type instances for gboxed */

NspTypeGtkIconInfo *new_type_gtkiconinfo(type_mode mode);

/* instance for NspGtkIconInfo */

NspGtkIconInfo *new_gtkiconinfo();

/*
 * Object methods redefined for gtkiconinfo 
 */

#define NULLGTKICONINFO (NspGtkIconInfo*) 0


/* from NspGtkIconInfoObj.c */

extern NspGtkIconInfo *nsp_gtkiconinfo_object (NspObject *O);
extern int IsGtkIconInfoObj (Stack stack, int i);
extern int IsGtkIconInfo(NspObject *O);
extern NspGtkIconInfo *GetGtkIconInfoCopy (Stack stack, int i);
extern NspGtkIconInfo *GetGtkIconInfo (Stack stack, int i);

#endif /* NSP_INC_NspGtkIconInfo */ 

#ifdef NspGtkIconInfo_Private 
static int init_gtkiconinfo(NspGtkIconInfo *o,NspTypeGtkIconInfo *type);
static char *nsp_gtkiconinfo_type_as_string(void);
static char *nsp_gtkiconinfo_type_short_string(NspObject *v);
static AttrTab gtkiconinfo_attrs[];
static NspMethods *gtkiconinfo_get_methods(void);
/* static int int_gtkiconinfo_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkIconInfo_Private */
