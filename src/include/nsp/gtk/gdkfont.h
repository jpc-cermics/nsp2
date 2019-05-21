/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkFont
#define NSP_INC_NspGdkFont

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

/* NspGdkFont */

#include <nsp/gtk/gboxed.h>

/*
 * NspGdkFont inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGdkFont ;
typedef NspTypeGBoxed NspTypeGdkFont ;

extern int nsp_type_gdkfont_id;
extern NspTypeGdkFont *nsp_type_gdkfont;

/* type instances for gboxed */

NspTypeGdkFont *new_type_gdkfont(type_mode mode);

/* instance for NspGdkFont */

NspGdkFont *new_gdkfont();

/*
 * Object methods redefined for gdkfont 
 */

#define NULLGDKFONT (NspGdkFont*) 0


/* from NspGdkFontObj.c */

extern NspGdkFont *nsp_gdkfont_object (NspObject *O);
extern int IsGdkFontObj (Stack stack, int i);
extern int IsGdkFont(NspObject *O);
extern NspGdkFont *GetGdkFontCopy (Stack stack, int i);
extern NspGdkFont *GetGdkFont (Stack stack, int i);

#endif /* NSP_INC_NspGdkFont */ 

#ifdef NspGdkFont_Private 
static int init_gdkfont(NspGdkFont *o,NspTypeGdkFont *type);
static char *nsp_gdkfont_type_as_string(void);
static char *nsp_gdkfont_type_short_string(NspObject *v);
static AttrTab gdkfont_attrs[];
static NspMethods *gdkfont_get_methods(void);
/* static int int_gdkfont_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkFont_Private */
