/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkColor
#define NSP_INC_NspGdkColor

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

/* NspGdkColor */

#include <nsp/gtk/gboxed.h>

/*
 * NspGdkColor inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGdkColor ;
typedef NspTypeGBoxed NspTypeGdkColor ;

extern int nsp_type_gdkcolor_id;
extern NspTypeGdkColor *nsp_type_gdkcolor;

/* type instances for gboxed */

NspTypeGdkColor *new_type_gdkcolor(type_mode mode);

/* instance for NspGdkColor */

NspGdkColor *new_gdkcolor();

/*
 * Object methods redefined for gdkcolor 
 */

#define NULLGDKCOLOR (NspGdkColor*) 0


/* from NspGdkColorObj.c */

extern NspGdkColor *nsp_gdkcolor_object (NspObject *O);
extern int IsGdkColorObj (Stack stack, int i);
extern int IsGdkColor(NspObject *O);
extern NspGdkColor *GetGdkColorCopy (Stack stack, int i);
extern NspGdkColor *GetGdkColor (Stack stack, int i);

#endif /* NSP_INC_NspGdkColor */ 

#ifdef NspGdkColor_Private 
static int init_gdkcolor(NspGdkColor *o,NspTypeGdkColor *type);
static char *nsp_gdkcolor_type_as_string(void);
static char *nsp_gdkcolor_type_short_string(NspObject *v);
static AttrTab gdkcolor_attrs[];
static NspMethods *gdkcolor_get_methods(void);
/* static int int_gdkcolor_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkColor_Private */
