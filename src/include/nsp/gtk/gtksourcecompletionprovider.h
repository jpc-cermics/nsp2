/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceCompletionProvider
#define NSP_INC_NspGtkSourceCompletionProvider

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

/* NspGtkSourceCompletionProvider */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceCompletionProvider inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceCompletionProvider ;
typedef NspTypeGObject NspTypeGtkSourceCompletionProvider ;

extern int nsp_type_gtksourcecompletionprovider_id;
extern NspTypeGtkSourceCompletionProvider *nsp_type_gtksourcecompletionprovider;

/* type instances for gobject */

NspTypeGtkSourceCompletionProvider *new_type_gtksourcecompletionprovider(type_mode mode);

/* instance for NspGtkSourceCompletionProvider */

NspGtkSourceCompletionProvider *new_gtksourcecompletionprovider();

/*
 * Object methods redefined for gtksourcecompletionprovider 
 */

#define NULLGTKSOURCECOMPLETIONPROVIDER (NspGtkSourceCompletionProvider*) 0


/* from NspGtkSourceCompletionProviderObj.c */

extern NspGtkSourceCompletionProvider *nsp_gtksourcecompletionprovider_object (NspObject *O);
extern int IsGtkSourceCompletionProviderObj (Stack stack, int i);
extern int IsGtkSourceCompletionProvider(NspObject *O);
extern NspGtkSourceCompletionProvider *GetGtkSourceCompletionProviderCopy (Stack stack, int i);
extern NspGtkSourceCompletionProvider *GetGtkSourceCompletionProvider (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceCompletionProvider */ 

#ifdef NspGtkSourceCompletionProvider_Private 
static int init_gtksourcecompletionprovider(NspGtkSourceCompletionProvider *o,NspTypeGtkSourceCompletionProvider *type);
static char *nsp_gtksourcecompletionprovider_type_as_string(void);
static char *nsp_gtksourcecompletionprovider_type_short_string(NspObject *v);
static AttrTab gtksourcecompletionprovider_attrs[];
static NspMethods *gtksourcecompletionprovider_get_methods(void);
/* static int int_gtksourcecompletionprovider_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceCompletionProvider_Private */
