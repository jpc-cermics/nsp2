/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGFile
#define NSP_INC_NspGFile

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

/* NspGFile */

#include <nsp/gtk/gobject.h>

/*
 * NspGFile inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGFile ;
typedef NspTypeGObject NspTypeGFile ;

extern int nsp_type_gfile_id;
extern NspTypeGFile *nsp_type_gfile;

/* type instances for gobject */

NspTypeGFile *new_type_gfile(type_mode mode);

/* instance for NspGFile */

NspGFile *new_gfile();

/*
 * Object methods redefined for gfile 
 */

#define NULLGFILE (NspGFile*) 0


/* from NspGFileObj.c */

extern NspGFile *nsp_gfile_object (NspObject *O);
extern int IsGFileObj (Stack stack, int i);
extern int IsGFile(NspObject *O);
extern NspGFile *GetGFileCopy (Stack stack, int i);
extern NspGFile *GetGFile (Stack stack, int i);

#endif /* NSP_INC_NspGFile */ 

#ifdef NspGFile_Private 
static int init_gfile(NspGFile *o,NspTypeGFile *type);
static char *nsp_gfile_type_as_string(void);
static char *nsp_gfile_type_short_string(NspObject *v);
static AttrTab gfile_attrs[];
static NspMethods *gfile_get_methods(void);
/* static int int_gfile_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGFile_Private */
