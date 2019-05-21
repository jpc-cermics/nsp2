/* -*- Mode: C -*- */
#ifndef NSP_INC_NspWebKitWebHistoryItem
#define NSP_INC_NspWebKitWebHistoryItem

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

/* NspWebKitWebHistoryItem */

#include <nsp/gtk/gobject.h>

/*
 * NspWebKitWebHistoryItem inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspWebKitWebHistoryItem ;
typedef NspTypeGObject NspTypeWebKitWebHistoryItem ;

extern int nsp_type_webkitwebhistoryitem_id;
extern NspTypeWebKitWebHistoryItem *nsp_type_webkitwebhistoryitem;

/* type instances for gobject */

NspTypeWebKitWebHistoryItem *new_type_webkitwebhistoryitem(type_mode mode);

/* instance for NspWebKitWebHistoryItem */

NspWebKitWebHistoryItem *new_webkitwebhistoryitem();

/*
 * Object methods redefined for webkitwebhistoryitem 
 */

#define NULLWEBKITWEBHISTORYITEM (NspWebKitWebHistoryItem*) 0


/* from NspWebKitWebHistoryItemObj.c */

extern NspWebKitWebHistoryItem *nsp_webkitwebhistoryitem_object (NspObject *O);
extern int IsWebKitWebHistoryItemObj (Stack stack, int i);
extern int IsWebKitWebHistoryItem(NspObject *O);
extern NspWebKitWebHistoryItem *GetWebKitWebHistoryItemCopy (Stack stack, int i);
extern NspWebKitWebHistoryItem *GetWebKitWebHistoryItem (Stack stack, int i);

#endif /* NSP_INC_NspWebKitWebHistoryItem */ 

#ifdef NspWebKitWebHistoryItem_Private 
static int init_webkitwebhistoryitem(NspWebKitWebHistoryItem *o,NspTypeWebKitWebHistoryItem *type);
static char *nsp_webkitwebhistoryitem_type_as_string(void);
static char *nsp_webkitwebhistoryitem_type_short_string(NspObject *v);
static AttrTab webkitwebhistoryitem_attrs[];
static NspMethods *webkitwebhistoryitem_get_methods(void);
/* static int int_webkitwebhistoryitem_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspWebKitWebHistoryItem_Private */
