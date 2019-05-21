/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceFileSaver
#define NSP_INC_NspGtkSourceFileSaver

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

/* NspGtkSourceFileSaver */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceFileSaver inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceFileSaver ;
typedef NspTypeGObject NspTypeGtkSourceFileSaver ;

extern int nsp_type_gtksourcefilesaver_id;
extern NspTypeGtkSourceFileSaver *nsp_type_gtksourcefilesaver;

/* type instances for gobject */

NspTypeGtkSourceFileSaver *new_type_gtksourcefilesaver(type_mode mode);

/* instance for NspGtkSourceFileSaver */

NspGtkSourceFileSaver *new_gtksourcefilesaver();

/*
 * Object methods redefined for gtksourcefilesaver 
 */

#define NULLGTKSOURCEFILESAVER (NspGtkSourceFileSaver*) 0


/* from NspGtkSourceFileSaverObj.c */

extern NspGtkSourceFileSaver *nsp_gtksourcefilesaver_object (NspObject *O);
extern int IsGtkSourceFileSaverObj (Stack stack, int i);
extern int IsGtkSourceFileSaver(NspObject *O);
extern NspGtkSourceFileSaver *GetGtkSourceFileSaverCopy (Stack stack, int i);
extern NspGtkSourceFileSaver *GetGtkSourceFileSaver (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceFileSaver */ 

#ifdef NspGtkSourceFileSaver_Private 
static int init_gtksourcefilesaver(NspGtkSourceFileSaver *o,NspTypeGtkSourceFileSaver *type);
static char *nsp_gtksourcefilesaver_type_as_string(void);
static char *nsp_gtksourcefilesaver_type_short_string(NspObject *v);
static AttrTab gtksourcefilesaver_attrs[];
static NspMethods *gtksourcefilesaver_get_methods(void);
/* static int int_gtksourcefilesaver_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceFileSaver_Private */
