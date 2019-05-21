/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkImage
#define NSP_INC_NspAtkImage

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

/* NspAtkImage */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkImage inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkImage ;
typedef NspTypeGObject NspTypeAtkImage ;

extern int nsp_type_atkimage_id;
extern NspTypeAtkImage *nsp_type_atkimage;

/* type instances for gobject */

NspTypeAtkImage *new_type_atkimage(type_mode mode);

/* instance for NspAtkImage */

NspAtkImage *new_atkimage();

/*
 * Object methods redefined for atkimage 
 */

#define NULLATKIMAGE (NspAtkImage*) 0


/* from NspAtkImageObj.c */

extern NspAtkImage *nsp_atkimage_object (NspObject *O);
extern int IsAtkImageObj (Stack stack, int i);
extern int IsAtkImage(NspObject *O);
extern NspAtkImage *GetAtkImageCopy (Stack stack, int i);
extern NspAtkImage *GetAtkImage (Stack stack, int i);

#endif /* NSP_INC_NspAtkImage */ 

#ifdef NspAtkImage_Private 
static int init_atkimage(NspAtkImage *o,NspTypeAtkImage *type);
static char *nsp_atkimage_type_as_string(void);
static char *nsp_atkimage_type_short_string(NspObject *v);
static AttrTab atkimage_attrs[];
static NspMethods *atkimage_get_methods(void);
/* static int int_atkimage_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkImage_Private */
