/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSearchEntry
#define NSP_INC_NspGtkSearchEntry

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

/* NspGtkSearchEntry */

#include <nsp/gtk/gtkentry.h>

/*
 * NspGtkSearchEntry inherits from GtkEntry
 * just change some type attributes 
 */

typedef NspGtkEntry NspGtkSearchEntry ;
typedef NspTypeGtkEntry NspTypeGtkSearchEntry ;

extern int nsp_type_gtksearchentry_id;
extern NspTypeGtkSearchEntry *nsp_type_gtksearchentry;

/* type instances for gtkentry */

NspTypeGtkSearchEntry *new_type_gtksearchentry(type_mode mode);

/* instance for NspGtkSearchEntry */

NspGtkSearchEntry *new_gtksearchentry();

/*
 * Object methods redefined for gtksearchentry 
 */

#define NULLGTKSEARCHENTRY (NspGtkSearchEntry*) 0


/* from NspGtkSearchEntryObj.c */

extern NspGtkSearchEntry *nsp_gtksearchentry_object (NspObject *O);
extern int IsGtkSearchEntryObj (Stack stack, int i);
extern int IsGtkSearchEntry(NspObject *O);
extern NspGtkSearchEntry *GetGtkSearchEntryCopy (Stack stack, int i);
extern NspGtkSearchEntry *GetGtkSearchEntry (Stack stack, int i);

#endif /* NSP_INC_NspGtkSearchEntry */ 

#ifdef NspGtkSearchEntry_Private 
static int init_gtksearchentry(NspGtkSearchEntry *o,NspTypeGtkSearchEntry *type);
static char *nsp_gtksearchentry_type_as_string(void);
static char *nsp_gtksearchentry_type_short_string(NspObject *v);
static AttrTab gtksearchentry_attrs[];
static NspMethods *gtksearchentry_get_methods(void);
/* static int int_gtksearchentry_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSearchEntry_Private */
