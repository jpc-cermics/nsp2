/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceSearchContext
#define NSP_INC_NspGtkSourceSearchContext

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

/* NspGtkSourceSearchContext */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceSearchContext inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceSearchContext ;
typedef NspTypeGObject NspTypeGtkSourceSearchContext ;

extern int nsp_type_gtksourcesearchcontext_id;
extern NspTypeGtkSourceSearchContext *nsp_type_gtksourcesearchcontext;

/* type instances for gobject */

NspTypeGtkSourceSearchContext *new_type_gtksourcesearchcontext(type_mode mode);

/* instance for NspGtkSourceSearchContext */

NspGtkSourceSearchContext *new_gtksourcesearchcontext();

/*
 * Object methods redefined for gtksourcesearchcontext 
 */

#define NULLGTKSOURCESEARCHCONTEXT (NspGtkSourceSearchContext*) 0


/* from NspGtkSourceSearchContextObj.c */

extern NspGtkSourceSearchContext *nsp_gtksourcesearchcontext_object (NspObject *O);
extern int IsGtkSourceSearchContextObj (Stack stack, int i);
extern int IsGtkSourceSearchContext(NspObject *O);
extern NspGtkSourceSearchContext *GetGtkSourceSearchContextCopy (Stack stack, int i);
extern NspGtkSourceSearchContext *GetGtkSourceSearchContext (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceSearchContext */ 

#ifdef NspGtkSourceSearchContext_Private 
static int init_gtksourcesearchcontext(NspGtkSourceSearchContext *o,NspTypeGtkSourceSearchContext *type);
static char *nsp_gtksourcesearchcontext_type_as_string(void);
static char *nsp_gtksourcesearchcontext_type_short_string(NspObject *v);
static AttrTab gtksourcesearchcontext_attrs[];
static NspMethods *gtksourcesearchcontext_get_methods(void);
/* static int int_gtksourcesearchcontext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceSearchContext_Private */
