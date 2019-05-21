/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceMap
#define NSP_INC_NspGtkSourceMap

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

/* NspGtkSourceMap */

#include <nsp/gtk/gtksourceview.h>

/*
 * NspGtkSourceMap inherits from GtkSourceView
 * just change some type attributes 
 */

typedef NspGtkSourceView NspGtkSourceMap ;
typedef NspTypeGtkSourceView NspTypeGtkSourceMap ;

extern int nsp_type_gtksourcemap_id;
extern NspTypeGtkSourceMap *nsp_type_gtksourcemap;

/* type instances for gtksourceview */

NspTypeGtkSourceMap *new_type_gtksourcemap(type_mode mode);

/* instance for NspGtkSourceMap */

NspGtkSourceMap *new_gtksourcemap();

/*
 * Object methods redefined for gtksourcemap 
 */

#define NULLGTKSOURCEMAP (NspGtkSourceMap*) 0


/* from NspGtkSourceMapObj.c */

extern NspGtkSourceMap *nsp_gtksourcemap_object (NspObject *O);
extern int IsGtkSourceMapObj (Stack stack, int i);
extern int IsGtkSourceMap(NspObject *O);
extern NspGtkSourceMap *GetGtkSourceMapCopy (Stack stack, int i);
extern NspGtkSourceMap *GetGtkSourceMap (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceMap */ 

#ifdef NspGtkSourceMap_Private 
static int init_gtksourcemap(NspGtkSourceMap *o,NspTypeGtkSourceMap *type);
static char *nsp_gtksourcemap_type_as_string(void);
static char *nsp_gtksourcemap_type_short_string(NspObject *v);
static AttrTab gtksourcemap_attrs[];
static NspMethods *gtksourcemap_get_methods(void);
/* static int int_gtksourcemap_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceMap_Private */
