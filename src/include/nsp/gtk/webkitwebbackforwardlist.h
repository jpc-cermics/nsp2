/* -*- Mode: C -*- */
#ifndef NSP_INC_NspWebKitWebBackForwardList
#define NSP_INC_NspWebKitWebBackForwardList

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

/* NspWebKitWebBackForwardList */

#include <nsp/gtk/gobject.h>

/*
 * NspWebKitWebBackForwardList inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspWebKitWebBackForwardList ;
typedef NspTypeGObject NspTypeWebKitWebBackForwardList ;

extern int nsp_type_webkitwebbackforwardlist_id;
extern NspTypeWebKitWebBackForwardList *nsp_type_webkitwebbackforwardlist;

/* type instances for gobject */

NspTypeWebKitWebBackForwardList *new_type_webkitwebbackforwardlist(type_mode mode);

/* instance for NspWebKitWebBackForwardList */

NspWebKitWebBackForwardList *new_webkitwebbackforwardlist();

/*
 * Object methods redefined for webkitwebbackforwardlist 
 */

#define NULLWEBKITWEBBACKFORWARDLIST (NspWebKitWebBackForwardList*) 0


/* from NspWebKitWebBackForwardListObj.c */

extern NspWebKitWebBackForwardList *nsp_webkitwebbackforwardlist_object (NspObject *O);
extern int IsWebKitWebBackForwardListObj (Stack stack, int i);
extern int IsWebKitWebBackForwardList(NspObject *O);
extern NspWebKitWebBackForwardList *GetWebKitWebBackForwardListCopy (Stack stack, int i);
extern NspWebKitWebBackForwardList *GetWebKitWebBackForwardList (Stack stack, int i);

#endif /* NSP_INC_NspWebKitWebBackForwardList */ 

#ifdef NspWebKitWebBackForwardList_Private 
static int init_webkitwebbackforwardlist(NspWebKitWebBackForwardList *o,NspTypeWebKitWebBackForwardList *type);
static char *nsp_webkitwebbackforwardlist_type_as_string(void);
static char *nsp_webkitwebbackforwardlist_type_short_string(NspObject *v);
static AttrTab webkitwebbackforwardlist_attrs[];
static NspMethods *webkitwebbackforwardlist_get_methods(void);
/* static int int_webkitwebbackforwardlist_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspWebKitWebBackForwardList_Private */
