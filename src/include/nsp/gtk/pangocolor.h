/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPangoColor
#define NSP_INC_NspPangoColor

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

/* NspPangoColor */

#include <nsp/gtk/gboxed.h>

/*
 * NspPangoColor inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspPangoColor ;
typedef NspTypeGBoxed NspTypePangoColor ;

extern int nsp_type_pangocolor_id;
extern NspTypePangoColor *nsp_type_pangocolor;

/* type instances for gboxed */

NspTypePangoColor *new_type_pangocolor(type_mode mode);

/* instance for NspPangoColor */

NspPangoColor *new_pangocolor();

/*
 * Object methods redefined for pangocolor 
 */

#define NULLPANGOCOLOR (NspPangoColor*) 0


/* from NspPangoColorObj.c */

extern NspPangoColor *nsp_pangocolor_object (NspObject *O);
extern int IsPangoColorObj (Stack stack, int i);
extern int IsPangoColor(NspObject *O);
extern NspPangoColor *GetPangoColorCopy (Stack stack, int i);
extern NspPangoColor *GetPangoColor (Stack stack, int i);

#endif /* NSP_INC_NspPangoColor */ 

#ifdef NspPangoColor_Private 
static int init_pangocolor(NspPangoColor *o,NspTypePangoColor *type);
static char *nsp_pangocolor_type_as_string(void);
static char *nsp_pangocolor_type_short_string(NspObject *v);
static AttrTab pangocolor_attrs[];
static NspMethods *pangocolor_get_methods(void);
/* static int int_pangocolor_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspPangoColor_Private */
