/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkEntry
#define NSP_INC_NspGtkEntry

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

/* NspGtkEntry */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkEntry inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkEntry ;
typedef NspTypeGtkWidget NspTypeGtkEntry ;

extern int nsp_type_gtkentry_id;
extern NspTypeGtkEntry *nsp_type_gtkentry;

/* type instances for gtkwidget */

NspTypeGtkEntry *new_type_gtkentry(type_mode mode);

/* instance for NspGtkEntry */

NspGtkEntry *new_gtkentry();

/*
 * Object methods redefined for gtkentry 
 */

#define NULLGTKENTRY (NspGtkEntry*) 0


/* from NspGtkEntryObj.c */

extern NspGtkEntry *nsp_gtkentry_object (NspObject *O);
extern int IsGtkEntryObj (Stack stack, int i);
extern int IsGtkEntry(NspObject *O);
extern NspGtkEntry *GetGtkEntryCopy (Stack stack, int i);
extern NspGtkEntry *GetGtkEntry (Stack stack, int i);

#endif /* NSP_INC_NspGtkEntry */ 

#ifdef NspGtkEntry_Private 
static int init_gtkentry(NspGtkEntry *o,NspTypeGtkEntry *type);
static char *nsp_gtkentry_type_as_string(void);
static char *nsp_gtkentry_type_short_string(NspObject *v);
static AttrTab gtkentry_attrs[];
static NspMethods *gtkentry_get_methods(void);
/* static int int_gtkentry_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkEntry_Private */
