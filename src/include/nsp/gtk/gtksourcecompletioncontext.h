/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceCompletionContext
#define NSP_INC_NspGtkSourceCompletionContext

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

/* NspGtkSourceCompletionContext */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceCompletionContext inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceCompletionContext ;
typedef NspTypeGObject NspTypeGtkSourceCompletionContext ;

extern int nsp_type_gtksourcecompletioncontext_id;
extern NspTypeGtkSourceCompletionContext *nsp_type_gtksourcecompletioncontext;

/* type instances for gobject */

NspTypeGtkSourceCompletionContext *new_type_gtksourcecompletioncontext(type_mode mode);

/* instance for NspGtkSourceCompletionContext */

NspGtkSourceCompletionContext *new_gtksourcecompletioncontext();

/*
 * Object methods redefined for gtksourcecompletioncontext 
 */

#define NULLGTKSOURCECOMPLETIONCONTEXT (NspGtkSourceCompletionContext*) 0


/* from NspGtkSourceCompletionContextObj.c */

extern NspGtkSourceCompletionContext *nsp_gtksourcecompletioncontext_object (NspObject *O);
extern int IsGtkSourceCompletionContextObj (Stack stack, int i);
extern int IsGtkSourceCompletionContext(NspObject *O);
extern NspGtkSourceCompletionContext *GetGtkSourceCompletionContextCopy (Stack stack, int i);
extern NspGtkSourceCompletionContext *GetGtkSourceCompletionContext (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceCompletionContext */ 

#ifdef NspGtkSourceCompletionContext_Private 
static int init_gtksourcecompletioncontext(NspGtkSourceCompletionContext *o,NspTypeGtkSourceCompletionContext *type);
static char *nsp_gtksourcecompletioncontext_type_as_string(void);
static char *nsp_gtksourcecompletioncontext_type_short_string(NspObject *v);
static AttrTab gtksourcecompletioncontext_attrs[];
static NspMethods *gtksourcecompletioncontext_get_methods(void);
/* static int int_gtksourcecompletioncontext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceCompletionContext_Private */
