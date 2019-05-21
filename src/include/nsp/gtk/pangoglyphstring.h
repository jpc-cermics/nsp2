/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPangoGlyphString
#define NSP_INC_NspPangoGlyphString

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

/* NspPangoGlyphString */

#include <nsp/gtk/gboxed.h>

/*
 * NspPangoGlyphString inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspPangoGlyphString ;
typedef NspTypeGBoxed NspTypePangoGlyphString ;

extern int nsp_type_pangoglyphstring_id;
extern NspTypePangoGlyphString *nsp_type_pangoglyphstring;

/* type instances for gboxed */

NspTypePangoGlyphString *new_type_pangoglyphstring(type_mode mode);

/* instance for NspPangoGlyphString */

NspPangoGlyphString *new_pangoglyphstring();

/*
 * Object methods redefined for pangoglyphstring 
 */

#define NULLPANGOGLYPHSTRING (NspPangoGlyphString*) 0


/* from NspPangoGlyphStringObj.c */

extern NspPangoGlyphString *nsp_pangoglyphstring_object (NspObject *O);
extern int IsPangoGlyphStringObj (Stack stack, int i);
extern int IsPangoGlyphString(NspObject *O);
extern NspPangoGlyphString *GetPangoGlyphStringCopy (Stack stack, int i);
extern NspPangoGlyphString *GetPangoGlyphString (Stack stack, int i);

#endif /* NSP_INC_NspPangoGlyphString */ 

#ifdef NspPangoGlyphString_Private 
static int init_pangoglyphstring(NspPangoGlyphString *o,NspTypePangoGlyphString *type);
static char *nsp_pangoglyphstring_type_as_string(void);
static char *nsp_pangoglyphstring_type_short_string(NspObject *v);
static AttrTab pangoglyphstring_attrs[];
static NspMethods *pangoglyphstring_get_methods(void);
/* static int int_pangoglyphstring_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspPangoGlyphString_Private */
