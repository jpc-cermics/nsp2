/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPangoAttrList
#define NSP_INC_NspPangoAttrList

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

/* NspPangoAttrList */

#include <nsp/gtk/gboxed.h>

/*
 * NspPangoAttrList inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspPangoAttrList ;
typedef NspTypeGBoxed NspTypePangoAttrList ;

extern int nsp_type_pangoattrlist_id;
extern NspTypePangoAttrList *nsp_type_pangoattrlist;

/* type instances for gboxed */

NspTypePangoAttrList *new_type_pangoattrlist(type_mode mode);

/* instance for NspPangoAttrList */

NspPangoAttrList *new_pangoattrlist();

/*
 * Object methods redefined for pangoattrlist 
 */

#define NULLPANGOATTRLIST (NspPangoAttrList*) 0


/* from NspPangoAttrListObj.c */

extern NspPangoAttrList *nsp_pangoattrlist_object (NspObject *O);
extern int IsPangoAttrListObj (Stack stack, int i);
extern int IsPangoAttrList(NspObject *O);
extern NspPangoAttrList *GetPangoAttrListCopy (Stack stack, int i);
extern NspPangoAttrList *GetPangoAttrList (Stack stack, int i);

#endif /* NSP_INC_NspPangoAttrList */ 

#ifdef NspPangoAttrList_Private 
static int init_pangoattrlist(NspPangoAttrList *o,NspTypePangoAttrList *type);
static char *nsp_pangoattrlist_type_as_string(void);
static char *nsp_pangoattrlist_type_short_string(NspObject *v);
static AttrTab pangoattrlist_attrs[];
static NspMethods *pangoattrlist_get_methods(void);
/* static int int_pangoattrlist_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspPangoAttrList_Private */
