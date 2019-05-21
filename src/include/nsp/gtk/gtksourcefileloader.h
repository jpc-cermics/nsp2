/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceFileLoader
#define NSP_INC_NspGtkSourceFileLoader

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

/* NspGtkSourceFileLoader */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceFileLoader inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceFileLoader ;
typedef NspTypeGObject NspTypeGtkSourceFileLoader ;

extern int nsp_type_gtksourcefileloader_id;
extern NspTypeGtkSourceFileLoader *nsp_type_gtksourcefileloader;

/* type instances for gobject */

NspTypeGtkSourceFileLoader *new_type_gtksourcefileloader(type_mode mode);

/* instance for NspGtkSourceFileLoader */

NspGtkSourceFileLoader *new_gtksourcefileloader();

/*
 * Object methods redefined for gtksourcefileloader 
 */

#define NULLGTKSOURCEFILELOADER (NspGtkSourceFileLoader*) 0


/* from NspGtkSourceFileLoaderObj.c */

extern NspGtkSourceFileLoader *nsp_gtksourcefileloader_object (NspObject *O);
extern int IsGtkSourceFileLoaderObj (Stack stack, int i);
extern int IsGtkSourceFileLoader(NspObject *O);
extern NspGtkSourceFileLoader *GetGtkSourceFileLoaderCopy (Stack stack, int i);
extern NspGtkSourceFileLoader *GetGtkSourceFileLoader (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceFileLoader */ 

#ifdef NspGtkSourceFileLoader_Private 
static int init_gtksourcefileloader(NspGtkSourceFileLoader *o,NspTypeGtkSourceFileLoader *type);
static char *nsp_gtksourcefileloader_type_as_string(void);
static char *nsp_gtksourcefileloader_type_short_string(NspObject *v);
static AttrTab gtksourcefileloader_attrs[];
static NspMethods *gtksourcefileloader_get_methods(void);
/* static int int_gtksourcefileloader_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceFileLoader_Private */
