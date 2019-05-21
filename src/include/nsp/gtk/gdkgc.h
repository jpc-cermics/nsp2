/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkGC
#define NSP_INC_NspGdkGC

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

/* NspGdkGC */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkGC inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkGC ;
typedef NspTypeGObject NspTypeGdkGC ;

extern int nsp_type_gdkgc_id;
extern NspTypeGdkGC *nsp_type_gdkgc;

/* type instances for gobject */

NspTypeGdkGC *new_type_gdkgc(type_mode mode);

/* instance for NspGdkGC */

NspGdkGC *new_gdkgc();

/*
 * Object methods redefined for gdkgc 
 */

#define NULLGDKGC (NspGdkGC*) 0


/* from NspGdkGCObj.c */

extern NspGdkGC *nsp_gdkgc_object (NspObject *O);
extern int IsGdkGCObj (Stack stack, int i);
extern int IsGdkGC(NspObject *O);
extern NspGdkGC *GetGdkGCCopy (Stack stack, int i);
extern NspGdkGC *GetGdkGC (Stack stack, int i);

#endif /* NSP_INC_NspGdkGC */ 

#ifdef NspGdkGC_Private 
static int init_gdkgc(NspGdkGC *o,NspTypeGdkGC *type);
static char *nsp_gdkgc_type_as_string(void);
static char *nsp_gdkgc_type_short_string(NspObject *v);
static AttrTab gdkgc_attrs[];
static NspMethods *gdkgc_get_methods(void);
/* static int int_gdkgc_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkGC_Private */
