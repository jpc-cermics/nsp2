/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceFile
#define NSP_INC_NspGtkSourceFile

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

/* NspGtkSourceFile */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceFile inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceFile ;
typedef NspTypeGObject NspTypeGtkSourceFile ;

extern int nsp_type_gtksourcefile_id;
extern NspTypeGtkSourceFile *nsp_type_gtksourcefile;

/* type instances for gobject */

NspTypeGtkSourceFile *new_type_gtksourcefile(type_mode mode);

/* instance for NspGtkSourceFile */

NspGtkSourceFile *new_gtksourcefile();

/*
 * Object methods redefined for gtksourcefile 
 */

#define NULLGTKSOURCEFILE (NspGtkSourceFile*) 0


/* from NspGtkSourceFileObj.c */

extern NspGtkSourceFile *nsp_gtksourcefile_object (NspObject *O);
extern int IsGtkSourceFileObj (Stack stack, int i);
extern int IsGtkSourceFile(NspObject *O);
extern NspGtkSourceFile *GetGtkSourceFileCopy (Stack stack, int i);
extern NspGtkSourceFile *GetGtkSourceFile (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceFile */ 

#ifdef NspGtkSourceFile_Private 
static int init_gtksourcefile(NspGtkSourceFile *o,NspTypeGtkSourceFile *type);
static char *nsp_gtksourcefile_type_as_string(void);
static char *nsp_gtksourcefile_type_short_string(NspObject *v);
static AttrTab gtksourcefile_attrs[];
static NspMethods *gtksourcefile_get_methods(void);
/* static int int_gtksourcefile_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceFile_Private */
