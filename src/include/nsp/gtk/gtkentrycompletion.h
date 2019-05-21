/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkEntryCompletion
#define NSP_INC_NspGtkEntryCompletion

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

/* NspGtkEntryCompletion */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkEntryCompletion inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkEntryCompletion ;
typedef NspTypeGObject NspTypeGtkEntryCompletion ;

extern int nsp_type_gtkentrycompletion_id;
extern NspTypeGtkEntryCompletion *nsp_type_gtkentrycompletion;

/* type instances for gobject */

NspTypeGtkEntryCompletion *new_type_gtkentrycompletion(type_mode mode);

/* instance for NspGtkEntryCompletion */

NspGtkEntryCompletion *new_gtkentrycompletion();

/*
 * Object methods redefined for gtkentrycompletion 
 */

#define NULLGTKENTRYCOMPLETION (NspGtkEntryCompletion*) 0


/* from NspGtkEntryCompletionObj.c */

extern NspGtkEntryCompletion *nsp_gtkentrycompletion_object (NspObject *O);
extern int IsGtkEntryCompletionObj (Stack stack, int i);
extern int IsGtkEntryCompletion(NspObject *O);
extern NspGtkEntryCompletion *GetGtkEntryCompletionCopy (Stack stack, int i);
extern NspGtkEntryCompletion *GetGtkEntryCompletion (Stack stack, int i);

#endif /* NSP_INC_NspGtkEntryCompletion */ 

#ifdef NspGtkEntryCompletion_Private 
static int init_gtkentrycompletion(NspGtkEntryCompletion *o,NspTypeGtkEntryCompletion *type);
static char *nsp_gtkentrycompletion_type_as_string(void);
static char *nsp_gtkentrycompletion_type_short_string(NspObject *v);
static AttrTab gtkentrycompletion_attrs[];
static NspMethods *gtkentrycompletion_get_methods(void);
/* static int int_gtkentrycompletion_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkEntryCompletion_Private */
