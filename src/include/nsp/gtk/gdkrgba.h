/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkRGBA
#define NSP_INC_NspGdkRGBA

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

/* NspGdkRGBA */

#include <nsp/gtk/gboxed.h>

/*
 * NspGdkRGBA inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGdkRGBA ;
typedef NspTypeGBoxed NspTypeGdkRGBA ;

extern int nsp_type_gdkrgba_id;
extern NspTypeGdkRGBA *nsp_type_gdkrgba;

/* type instances for gboxed */

NspTypeGdkRGBA *new_type_gdkrgba(type_mode mode);

/* instance for NspGdkRGBA */

NspGdkRGBA *new_gdkrgba();

/*
 * Object methods redefined for gdkrgba 
 */

#define NULLGDKRGBA (NspGdkRGBA*) 0


/* from NspGdkRGBAObj.c */

extern NspGdkRGBA *nsp_gdkrgba_object (NspObject *O);
extern int IsGdkRGBAObj (Stack stack, int i);
extern int IsGdkRGBA(NspObject *O);
extern NspGdkRGBA *GetGdkRGBACopy (Stack stack, int i);
extern NspGdkRGBA *GetGdkRGBA (Stack stack, int i);

#endif /* NSP_INC_NspGdkRGBA */ 

#ifdef NspGdkRGBA_Private 
static int init_gdkrgba(NspGdkRGBA *o,NspTypeGdkRGBA *type);
static char *nsp_gdkrgba_type_as_string(void);
static char *nsp_gdkrgba_type_short_string(NspObject *v);
static AttrTab gdkrgba_attrs[];
static NspMethods *gdkrgba_get_methods(void);
/* static int int_gdkrgba_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkRGBA_Private */
