/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkImage
#define NSP_INC_NspGtkImage

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

/* NspGtkImage */

#include <nsp/gtk/gtkmisc.h>

/*
 * NspGtkImage inherits from GtkMisc
 * just change some type attributes 
 */

typedef NspGtkMisc NspGtkImage ;
typedef NspTypeGtkMisc NspTypeGtkImage ;

extern int nsp_type_gtkimage_id;
extern NspTypeGtkImage *nsp_type_gtkimage;

/* type instances for gtkmisc */

NspTypeGtkImage *new_type_gtkimage(type_mode mode);

/* instance for NspGtkImage */

NspGtkImage *new_gtkimage();

/*
 * Object methods redefined for gtkimage 
 */

#define NULLGTKIMAGE (NspGtkImage*) 0


/* from NspGtkImageObj.c */

extern NspGtkImage *nsp_gtkimage_object (NspObject *O);
extern int IsGtkImageObj (Stack stack, int i);
extern int IsGtkImage(NspObject *O);
extern NspGtkImage *GetGtkImageCopy (Stack stack, int i);
extern NspGtkImage *GetGtkImage (Stack stack, int i);

#endif /* NSP_INC_NspGtkImage */ 

#ifdef NspGtkImage_Private 
static int init_gtkimage(NspGtkImage *o,NspTypeGtkImage *type);
static char *nsp_gtkimage_type_as_string(void);
static char *nsp_gtkimage_type_short_string(NspObject *v);
static AttrTab gtkimage_attrs[];
static NspMethods *gtkimage_get_methods(void);
/* static int int_gtkimage_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkImage_Private */
