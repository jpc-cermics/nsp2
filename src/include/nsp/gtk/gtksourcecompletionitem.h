/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceCompletionItem
#define NSP_INC_NspGtkSourceCompletionItem

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

/* NspGtkSourceCompletionItem */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceCompletionItem inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceCompletionItem ;
typedef NspTypeGObject NspTypeGtkSourceCompletionItem ;

extern int nsp_type_gtksourcecompletionitem_id;
extern NspTypeGtkSourceCompletionItem *nsp_type_gtksourcecompletionitem;

/* type instances for gobject */

NspTypeGtkSourceCompletionItem *new_type_gtksourcecompletionitem(type_mode mode);

/* instance for NspGtkSourceCompletionItem */

NspGtkSourceCompletionItem *new_gtksourcecompletionitem();

/*
 * Object methods redefined for gtksourcecompletionitem 
 */

#define NULLGTKSOURCECOMPLETIONITEM (NspGtkSourceCompletionItem*) 0


/* from NspGtkSourceCompletionItemObj.c */

extern NspGtkSourceCompletionItem *nsp_gtksourcecompletionitem_object (NspObject *O);
extern int IsGtkSourceCompletionItemObj (Stack stack, int i);
extern int IsGtkSourceCompletionItem(NspObject *O);
extern NspGtkSourceCompletionItem *GetGtkSourceCompletionItemCopy (Stack stack, int i);
extern NspGtkSourceCompletionItem *GetGtkSourceCompletionItem (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceCompletionItem */ 

#ifdef NspGtkSourceCompletionItem_Private 
static int init_gtksourcecompletionitem(NspGtkSourceCompletionItem *o,NspTypeGtkSourceCompletionItem *type);
static char *nsp_gtksourcecompletionitem_type_as_string(void);
static char *nsp_gtksourcecompletionitem_type_short_string(NspObject *v);
static AttrTab gtksourcecompletionitem_attrs[];
static NspMethods *gtksourcecompletionitem_get_methods(void);
/* static int int_gtksourcecompletionitem_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceCompletionItem_Private */
